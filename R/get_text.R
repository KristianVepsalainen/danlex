#' Retrieve the full text of a document
#'
#' Returns the document's operative text as a single string, rendered from the
#' structured XML.
#'
#' @section Text availability:
#'
#' Full content is available only for material published from late September
#' 2007 onward; see [dlx_get_doc()] for the boundary. Earlier documents return
#' `status = "metadata_only"` and `text = NA`.
#'
#' @section How the text is rendered:
#'
#' The source encodes text as `Linea` (lines) containing `Char` runs, with
#' `Explicatus` elements carrying structural markers such as `"§ 1"` or
#' `"Stk. 2."`. A naive `xml_text()` on the whole document runs these together
#' without separation, producing strings like `"§ 1I bekendtgørelse nr. ..."`.
#'
#' danlex instead walks `Explicatus` and `Linea` in document order, joins them
#' with single spaces, and inserts a blank line before each marker that opens a
#' `Paragraf` or `Stk`. The result is readable running text with section
#' breaks preserved. Inline formatting (`formaChar`) is discarded.
#'
#' For structured access to individual provisions, use [dlx_get_paragraphs()].
#'
#' @inheritParams dlx_get_doc
#'
#' @return A one-row [tibble][tibble::tibble] with `eli`, `status`
#'   (`"ok"`, `"metadata_only"` or `"not_found"`), `n_char` and `text`.
#'
#' @seealso [dlx_get_paragraphs()] for provision-level output.
#'
#' @source Retsinformation, <https://www.retsinformation.dk>.
#'
#' @examples
#' if (FALSE) {
#'   txt <- dlx_get_text("lta", 2025, 50)
#'   cat(substr(txt$text, 1, 500))
#'
#'   # Pre-2008 documents carry metadata only
#'   dlx_get_text("lta", 1998, 763)$status
#' }
#'
#' @export
dlx_get_text <- function(collection = NULL, year = NULL, number = NULL,
                         accn = NULL, eli = NULL) {

  eli_uri <- dlx_build_eli(
    collection = collection, year = year, number = number,
    accn = accn, eli = eli
  )

  res <- perform_eli_request(paste0(eli_uri, "/xml"))

  if (identical(res$status, "not_found")) {
    return(tibble::tibble(eli = eli_uri, status = "not_found",
                          n_char = NA_integer_, text = NA_character_))
  }

  content <- xml2::xml_find_first(xml2::xml_root(res$body), "//DokumentIndhold")

  if (inherits(content, "xml_missing")) {
    return(tibble::tibble(eli = eli_uri, status = "metadata_only",
                          n_char = NA_integer_, text = NA_character_))
  }

  text <- render_content(content)

  tibble::tibble(
    eli    = eli_uri,
    status = "ok",
    n_char = nchar(text),
    text   = text
  )
}

#' Retrieve a document's provisions
#'
#' Returns one row per *stykke* (subsection), or one row per section where a
#' section has no subsections. This gives provision-level access to Danish
#' legislation, which is not possible from the plain text alone.
#'
#' @section Amending provisions:
#'
#' Danish amending instruments (`BEK Æ`, `LOV Æ`, `ANG`) contain `Paragraf`
#' elements that are **not their own**: they are new text to be inserted into
#' another instrument, nested under `AendringNyTekst`. An amending regulation
#' may therefore appear to have sections that in fact belong to the act it
#' amends.
#'
#' The `is_amendment` column distinguishes the two. In a consolidated act it
#' is `FALSE` throughout; in an amending instrument it is typically `TRUE`
#' throughout. Filter on it before treating the output as the document's own
#' structure.
#'
#' @section Known limitation:
#'
#' Only `Stk` elements that sit inside a `Paragraf` are returned. A small
#' number of amending instruments place `Stk` directly under
#' `AendringNyTekst` with no enclosing section; those are currently omitted.
#'
#' @inheritParams dlx_get_doc
#'
#' @return A [tibble][tibble::tibble] with one row per provision:
#'   \describe{
#'     \item{`eli`}{ELI URI of the document.}
#'     \item{`paragraf_index`}{Position of the section in document order.}
#'     \item{`paragraf_id`, `paragraf_local_id`}{Source identifiers.}
#'     \item{`paragraf`}{Section marker, e.g. `"§ 1"`.}
#'     \item{`stk_index`}{Position of the subsection within its section.}
#'     \item{`stk`}{Subsection marker, e.g. `"Stk. 2."`, or `NA`.}
#'     \item{`is_amendment`}{Whether this provision is new text destined for
#'       another instrument.}
#'     \item{`text`}{Provision text, including any nested list items.}
#'   }
#'
#'   Zero rows for metadata-only or non-existent documents.
#'
#' @seealso [dlx_get_text()] for the whole document as one string.
#'
#' @source Retsinformation, <https://www.retsinformation.dk>.
#'
#' @examples
#' if (FALSE) {
#'   # A consolidated regulation: all provisions are its own
#'   p <- dlx_get_paragraphs("lta", 2025, 50)
#'   table(p$is_amendment)
#'
#'   # An amending regulation: the sections belong to the amended instrument
#'   a <- dlx_get_paragraphs("lta", 2025, 1)
#'   table(a$is_amendment)
#' }
#'
#' @export
dlx_get_paragraphs <- function(collection = NULL, year = NULL, number = NULL,
                               accn = NULL, eli = NULL) {

  eli_uri <- dlx_build_eli(
    collection = collection, year = year, number = number,
    accn = accn, eli = eli
  )

  res <- perform_eli_request(paste0(eli_uri, "/xml"))

  if (identical(res$status, "not_found")) {
    warning("No document found at ", eli_uri, "; returning zero rows.",
            call. = FALSE)
    return(dlx_empty_paragraphs())
  }

  content <- xml2::xml_find_first(xml2::xml_root(res$body), "//DokumentIndhold")
  if (inherits(content, "xml_missing")) {
    return(dlx_empty_paragraphs())
  }

  # Top-level structure varies by instrument type — Indledning/Afsnit,
  # Hymne/Bog, AendringCentreretParagraf and so on — but Paragraf and Stk
  # appear in all of them. A descendant search sidesteps the variation
  # entirely, so no per-type branching is needed.
  paras <- xml2::xml_find_all(content, ".//Paragraf")

  if (length(paras) == 0L) {
    return(dlx_empty_paragraphs())
  }

  rows <- lapply(seq_along(paras), function(i) {
    para_rows(paras[[i]], eli_uri, i)
  })

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

#' Render a content node as readable text
#'
#' Walks Explicatus and Linea in document order. Char runs within a Linea are
#' concatenated without a separator, which is correct — they are inline
#' formatting spans, not separate words.
#' @noRd
render_content <- function(node) {

  nodes <- xml2::xml_find_all(node, ".//Explicatus | .//Linea")
  if (length(nodes) == 0L) return(NA_character_)

  txt <- gsub("\\s+", " ", trimws(xml2::xml_text(nodes)))

  keep <- nzchar(txt)
  nodes <- nodes[keep]
  txt   <- txt[keep]
  if (length(txt) == 0L) return(NA_character_)

  is_marker <- xml2::xml_name(nodes) == "Explicatus"
  parents   <- vapply(nodes, function(n) xml2::xml_name(xml2::xml_parent(n)),
                      character(1))

  # Structural markers that open a new block. The Aendring* variants matter
  # because amending instruments never use bare Paragraf at the top level.
  block_parents <- c("Paragraf", "Stk", "AendringCentreretParagraf",
                     "IkraftCentreretParagraf", "AendringsNummer")

  sep <- rep(" ", length(nodes))
  sep[is_marker & parents %in% block_parents]  <- "\n\n"
  sep[is_marker & parents == "Indentatio"]     <- "\n"
  sep[!is_marker & parents == "Rubrica"] <- "\n"
  sep[1] <- ""

  trimws(paste0(sep, txt, collapse = ""))
}

#' Is this node new text destined for another instrument?
#' @noRd
is_amendment_node <- function(node) {
  !inherits(
    xml2::xml_find_first(node, "ancestor::AendringNyTekst"),
    "xml_missing"
  )
}

#' Marker text of a node, i.e. its first direct Explicatus child
#' @noRd
explicatus_of <- function(node) {
  ex <- xml2::xml_find_first(node, "./Explicatus")
  if (inherits(ex, "xml_missing")) return(NA_character_)
  value <- gsub("\\s+", " ", trimws(xml2::xml_text(ex)))
  if (!nzchar(value)) NA_character_ else value
}

#' Build the rows for a single Paragraf
#' @noRd
para_rows <- function(para, eli_uri, index) {

  amendment <- is_amendment_node(para)
  marker    <- explicatus_of(para)
  stks      <- xml2::xml_find_all(para, "./Stk")

  base <- list(
    eli               = eli_uri,
    paragraf_index    = as.integer(index),
    paragraf_id       = xml2::xml_attr(para, "id"),
    paragraf_local_id = xml2::xml_attr(para, "localId"),
    paragraf          = marker,
    is_amendment      = amendment
  )

  if (length(stks) == 0L) {
    # A section with no subsections: render the section itself, minus its
    # own marker, which is already carried in `paragraf`.
    return(tibble::tibble(
      eli               = base$eli,
      paragraf_index    = base$paragraf_index,
      paragraf_id       = base$paragraf_id,
      paragraf_local_id = base$paragraf_local_id,
      paragraf          = base$paragraf,
      stk_index         = NA_integer_,
      stk               = NA_character_,
      is_amendment      = base$is_amendment,
      text              = render_lines(para)
    ))
  }

  tibble::tibble(
    eli               = base$eli,
    paragraf_index    = base$paragraf_index,
    paragraf_id       = base$paragraf_id,
    paragraf_local_id = base$paragraf_local_id,
    paragraf          = base$paragraf,
    stk_index         = seq_along(stks),
    stk               = vapply(stks, explicatus_of, character(1)),
    is_amendment      = base$is_amendment,
    text              = vapply(stks, render_lines, character(1))
  )
}

#' Text content of a node, excluding structural markers
#'
#' Unlike render_content(), this drops Explicatus entirely:
#' returned as their own columns, so repeating them inside `text` would be
#' redundant. Nested list items (Indentatio) are kept, since they form part of
#' the provision.
#' @noRd
render_lines <- function(node) {
  lines <- xml2::xml_find_all(node, ".//Linea")
  if (length(lines) == 0L) return(NA_character_)
  txt <- gsub("\\s+", " ", trimws(xml2::xml_text(lines)))
  txt <- txt[nzchar(txt)]
  if (length(txt) == 0L) return(NA_character_)
  paste(txt, collapse = " ")
}

#' Zero-row provision table with the correct columns and types
#' @noRd
dlx_empty_paragraphs <- function() {
  tibble::tibble(
    eli               = character(0),
    paragraf_index    = integer(0),
    paragraf_id       = character(0),
    paragraf_local_id = character(0),
    paragraf          = character(0),
    stk_index         = integer(0),
    stk               = character(0),
    is_amendment      = logical(0),
    text              = character(0)
  )
}
