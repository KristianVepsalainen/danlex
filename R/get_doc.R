#' Retrieve a Danish legal document
#'
#' Fetches a single document from Retsinformation and returns its metadata as
#' a one-row tibble. The document may be identified in any one of three ways:
#' by the deterministic triple of collection, year and number; by accession
#' number; or by a full ELI URI.
#'
#' @section Identifier forms:
#'
#' The deterministic form is the one users will normally have. A Danish act or
#' regulation is cited as, for example, "bekendtgørelse nr. 763 af 1998", which
#' maps directly onto `dlx_get_doc("lta", 1998, 763)`.
#'
#' The accession form exists to follow references. `dlx_get_references()`
#' returns accession numbers rather than ELI URIs, so the citation network can
#' be traversed without a lookup table.
#'
#' The ELI form is for URIs taken from `dlx_list_documents()` or
#' `dlx_get_changes()`.
#'
#' @section Text availability:
#'
#' Retsinformation's ELI service returns full document content only for
#' material published from roughly late September 2007 onward. The boundary
#' observed in Lovtidende A is act number 1081 of 2007, announced 25 September
#' 2007. Earlier documents return metadata only, and `status` is then
#' `"metadata_only"` rather than `"ok"`.
#'
#' This is a property of the source, not of danlex. Metadata for earlier
#' documents is complete and reaches back to 1852 for Lovtidende A.
#'
#' @section Instrument types:
#'
#' Lovtidende A carries both acts (*love*) and regulations
#' (*bekendtgørelser*) in a single number space, so the instrument type cannot
#' be inferred from the identifier. Always consult `document_type`.
#'
#' Records also use two different schemas either side of the 2007 boundary:
#' older records give a whole Danish word, newer ones a composite code. Both
#' are returned verbatim in `document_type`, with the internal code split out
#' into `document_type_code` and a harmonised label in `document_type_std`.
#'
#' @param collection Collection segment of the ELI URI. One of `"lta"`
#'   (Lovtidende A: acts and regulations), `"ltb"`, `"ltc"` (treaties) or
#'   `"mt"` (Ministerialtidende, discontinued in 2012). The collections
#'   `"retsinfo"`, `"fob"` and `"ft"` are recognised but not yet supported.
#' @param year Four-digit year.
#' @param number Document number *as published*. Character or numeric;
#'   stored as character, because a small number of documents use
#'   non-numeric forms such as `"1-1"`.
#' @param accn Accession number, e.g. `"A19990059229"`.
#' @param eli A full canonical ELI URI.
#'
#' @return A one-row [tibble][tibble::tibble] with, among others:
#'   \describe{
#'     \item{`status`}{`"ok"`, `"metadata_only"` or `"not_found"`.}
#'     \item{`eli`}{Canonical ELI URI.}
#'     \item{`collection`, `year`, `number`}{Identifier components.}
#'     \item{`accn`}{Accession number.}
#'     \item{`title`}{Document title.}
#'     \item{`document_type`}{Type as recorded, verbatim.}
#'     \item{`document_type_code`}{Internal code, post-2007 records only.}
#'     \item{`document_type_std`}{Harmonised type label where known.}
#'     \item{`legal_status`}{The source's `Status` field. Named
#'       `legal_status` to avoid collision with `status` above.}
#'     \item{`dies_signi`, `dies_edicti`}{Signing and publication dates.}
#'     \item{`start_date`, `end_date`}{Period of validity.}
#'     \item{`ministry`, `administrative_authority`}{Issuing body.}
#'     \item{`has_text`}{Whether full content is available.}
#'     \item{`n_references`}{Number of references to other instruments.}
#'   }
#'
#'   A document that does not exist yields a one-row tibble with
#'   `status = "not_found"` and all other fields missing, rather than an error.
#'
#' @source Retsinformation, <https://www.retsinformation.dk>. Terms of use:
#'   <https://www.retsinformation.dk/api>.
#'
#' @examples
#' if (FALSE) {
#'   # A regulation from 1998 — metadata only, as it predates the text boundary
#'   dlx_get_doc("lta", 1998, 763)
#'
#'   # A modern act — full content available
#'   dlx_get_doc("lta", 2025, 1)
#'
#'   # Following a reference by accession number
#'   dlx_get_doc(accn = "A19990059229")
#'
#'   # Straight from an ELI URI
#'   dlx_get_doc(eli = "https://retsinformation.dk/eli/lta/1998/763")
#' }
#'
#' @export
dlx_get_doc <- function(collection = NULL, year = NULL, number = NULL,
                        accn = NULL, eli = NULL) {

  eli_uri <- dlx_build_eli(
    collection = collection, year = year, number = number,
    accn = accn, eli = eli
  )

  res <- perform_eli_request(paste0(eli_uri, "/xml"))

  if (identical(res$status, "not_found")) {
    return(dlx_doc_row(eli_uri, status = "not_found"))
  }

  dlx_parse_document(res$body, eli_uri)
}

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

#' Construct a document row
#'
#' Both the parsed and the not-found paths run through this, which guarantees
#' identical columns and types in every case. That matters because callers
#' will row-bind these.
#'
#' @param values Named list of values overriding the template.
#' @noRd
dlx_doc_row <- function(eli_uri, status, values = list()) {

  ids <- dlx_parse_eli(eli_uri)

  template <- list(
    status                   = status,
    eli                      = eli_uri,
    collection               = ids$collection,
    year                     = ids$year,
    number                   = ids$number,
    accn                     = NA_character_,
    document_id              = NA_character_,
    unique_document_id       = NA_character_,
    title                    = NA_character_,
    document_type            = NA_character_,
    document_type_code       = NA_character_,
    document_type_std        = NA_character_,
    rank                     = NA_character_,
    legal_status             = NA_character_,
    dies_signi               = as.Date(NA),
    dies_edicti              = as.Date(NA),
    date_of_submit           = as.Date(NA),
    start_date               = as.Date(NA),
    end_date                 = as.Date(NA),
    date_of_historic_mark    = as.Date(NA),
    announced_in             = NA_character_,
    republished              = NA_character_,
    ministry                 = NA_character_,
    administrative_authority = NA_character_,
    journal_number           = NA_character_,
    place_of_signature       = NA_character_,
    has_text                 = NA,
    n_references             = NA_integer_
  )

  for (nm in names(values)) {
    template[[nm]] <- values[[nm]]
  }

  tibble::as_tibble(template)
}

#' Parse a document XML into a one-row tibble
#' @noRd
dlx_parse_document <- function(doc, eli_uri) {

  root <- xml2::xml_root(doc)
  meta <- xml2::xml_find_first(root, "./Meta")

  if (inherits(meta, "xml_missing")) {
    dlx_abort(
      "Response contained no <Meta> element.",
      paste0("URI: ", eli_uri, ". The API may have changed."),
      class = "danlex_parse_error"
    )
  }

  # Full content is present only for material from late 2007 onward; earlier
  # documents carry <Meta> alone.
  has_text <- "DokumentIndhold" %in% xml2::xml_name(xml2::xml_children(root))

  dtype <- split_document_type(meta_text(meta, "DocumentType"))
  refs  <- parse_marker_blocks(meta)

  # The accession number embeds the identifier, so recover the components from
  # it when the request was made by accession number rather than by triple.
  accn <- meta_text(meta, "AccessionNumber")
  ids  <- dlx_parse_eli(eli_uri)

  if (is.na(ids$year) && !is.na(accn) && grepl("^[A-Za-z][0-9]{11}$", accn)) {
    ids$year   <- as.integer(substr(accn, 2, 5))
    ids$number <- sub("^0+", "", substr(accn, 6, 10))
  }

  dlx_doc_row(
    eli_uri,
    status = if (has_text) "ok" else "metadata_only",
    values = list(
      collection               = ids$collection,
      year                     = ids$year,
      number                   = ids$number %||% meta_text(meta, "Number"),
      accn                     = accn,
      document_id              = meta_text(meta, "DocumentId"),
      unique_document_id       = meta_text(meta, "UniqueDocumentId"),
      title                    = meta_text(meta, "DocumentTitle"),
      document_type            = dtype$label,
      document_type_code       = dtype$code,
      document_type_std        = dtype$standard,
      rank                     = meta_text(meta, "Rank"),
      legal_status             = meta_text(meta, "Status"),
      dies_signi               = parse_date_safe(meta_text(meta, "DiesSigni")),
      dies_edicti              = parse_date_safe(meta_text(meta, "DiesEdicti")),
      date_of_submit           = parse_date_safe(meta_text(meta, "DateOfSubmit")),
      start_date               = parse_date_safe(meta_text(meta, "StartDate")),
      end_date                 = parse_date_safe(meta_text(meta, "EndDate")),
      date_of_historic_mark    = parse_date_safe(meta_text(meta, "DateOfHistoricMark")),
      announced_in             = meta_text(meta, "AnnouncedIn"),
      republished              = meta_text(meta, "Republished"),
      ministry                 = meta_text(meta, "Ministry"),
      administrative_authority = meta_text(meta, "AdministrativeAuthority"),
      journal_number           = meta_text(meta, "JournalNumber"),
      place_of_signature       = meta_text(meta, "PlaceOfSignature"),
      has_text                 = has_text,
      n_references             = length(refs)
    )
  )
}
