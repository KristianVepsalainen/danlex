#' Retrieve the references a document makes to other instruments
#'
#' Returns the cross-references recorded in a document's metadata as an edge
#' table, one row per reference. These are typically the enabling provisions
#' (*hjemmel*) a regulation rests on.
#'
#' @section References are maintained, not historical:
#'
#' This is the single most important thing to understand about these edges.
#' Retsinformation keeps references pointing at the *current* consolidated
#' version of the parent instrument, not at the version in force when the
#' referring document was issued.
#'
#' A regulation from 1998, for instance, carries references to acts dated
#' 1999 — the links were updated when the parent acts were re-consolidated.
#' Anyone building a citation network and treating the edges as timestamped to
#' the source document's date will get a systematically wrong picture of what
#' the law looked like at any past moment.
#'
#' Use `ref_date` to see which version an edge currently points at. There is
#' no way to recover the reference as it stood at enactment from this API.
#'
#' @section Coverage:
#'
#' References are present on both sides of the 2007 text boundary — roughly
#' 43% of older and 59% of modern documents carry at least one. A document
#' with no enabling provision simply has none, which is not an error.
#'
#' @inheritParams dlx_get_doc
#'
#' @return A [tibble][tibble::tibble] with one row per reference:
#'   \describe{
#'     \item{`from_eli`}{ELI URI of the referring document.}
#'     \item{`from_accn`}{Accession number of the referring document.}
#'     \item{`ref_index`}{Position of the reference within the document.}
#'     \item{`ref_accn`}{Accession number of the referenced instrument. Pass
#'       this to `dlx_get_doc(accn = )` to follow the edge.}
#'     \item{`ref_date`}{Date of the referenced version.}
#'     \item{`ref_title`}{Title of the referenced instrument.}
#'   }
#'
#'   Zero rows if the document has no references. A document that does not
#'   exist yields zero rows and a warning.
#'
#' @seealso [dlx_get_doc()], whose `n_references` column reports the row count
#'   this function would return.
#'
#' @source Retsinformation, <https://www.retsinformation.dk>.
#'
#' @examples
#' if (FALSE) {
#'   # A 1998 regulation resting on two acts
#'   refs <- dlx_get_references("lta", 1998, 763)
#'
#'   # Follow the first edge
#'   dlx_get_doc(accn = refs$ref_accn[1])
#' }
#'
#' @export
dlx_get_references <- function(collection = NULL, year = NULL, number = NULL,
                               accn = NULL, eli = NULL) {

  eli_uri <- dlx_build_eli(
    collection = collection, year = year, number = number,
    accn = accn, eli = eli
  )

  res <- perform_eli_request(paste0(eli_uri, "/xml"))

  if (identical(res$status, "not_found")) {
    warning("No document found at ", eli_uri, "; returning zero rows.",
            call. = FALSE)
    return(dlx_empty_references())
  }

  meta <- xml2::xml_find_first(xml2::xml_root(res$body), "./Meta")
  if (inherits(meta, "xml_missing")) {
    dlx_abort(
      "Response contained no <Meta> element.",
      paste0("URI: ", eli_uri, ". The API may have changed."),
      class = "danlex_parse_error"
    )
  }

  from_accn <- meta_text(meta, "AccessionNumber")

  # Concerns is an empty marker element; Ref_Accn, Ref_Af and Ref_Text are its
  # SIBLINGS, not its children, so the blocks must be recovered positionally.
  blocks <- parse_marker_blocks(meta)

  if (length(blocks) == 0L) {
    return(dlx_empty_references())
  }

  tibble::tibble(
    from_eli  = eli_uri,
    from_accn = from_accn,
    ref_index = seq_along(blocks),
    ref_accn  = vapply(blocks, function(b) as.character(b["Ref_Accn"]  %|NA|% NA), character(1)),
    # parse_date_safe() rather than as.Date(): a non-ISO value would otherwise
    # abort the whole call. Date formats in older records are undocumented.
    ref_date  = do.call(c, lapply(blocks, function(b)
      parse_date_safe(as.character(b["Ref_Af"] %|NA|% NA_character_)))),
    ref_title = vapply(blocks, function(b) as.character(b["Ref_Text"]  %|NA|% NA), character(1))
  )
}

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

#' Replace an unnamed or missing lookup with a default
#'
#' `b["Ref_Accn"]` on a named character vector returns `NA` with the name
#' `NA` when the element is absent, which is awkward to test for.
#' @noRd
`%|NA|%` <- function(x, y) {
  if (length(x) == 0L || is.na(x)) y else x
}

#' Zero-row reference table with the correct columns and types
#'
#' Callers row-bind these, so an empty result must have the same shape as a
#' populated one.
#' @noRd
dlx_empty_references <- function() {
  tibble::tibble(
    from_eli  = character(0),
    from_accn = character(0),
    ref_index = integer(0),
    ref_accn  = character(0),
    ref_date  = as.Date(character(0)),
    ref_title = character(0)
  )
}
