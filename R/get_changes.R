#' Retrieve recent changes to Danish legislation
#'
#' Reads the Retsinformation ELI update feed and returns the documents
#' published or amended within its retention window, one row per change.
#'
#' @section Retention window:
#'
#' The feed holds at least 60 days of history and is refreshed daily. It is a
#' static file with no server-side query interface, so `since`, `collection`
#' and `reason` filter the retrieved feed rather than the request. Asking for
#' a `since` date earlier than the feed reaches will therefore return a
#' truncated result, and a warning is issued when that happens.
#'
#' There is no way to obtain older changes from this API. Use
#' `dlx_list_documents()` and the sitemap's modification dates for anything
#' beyond the window.
#'
#' @section Identifying the collection:
#'
#' The feed itself does not record which collection a document belongs to; it
#' reports only `published_in`, the publication channel. danlex derives
#' `collection`, `year` and `number` from the ELI identifier, which makes it
#' possible to separate legislation (`lta`, `ltb`, `ltc`, `mt`) from
#' administrative decisions (`retsinfo`) and parliamentary material (`ft`).
#'
#' The two are related but not equivalent: `published_in` distinguishes
#' Lovtidende A, B and C from Retsinformation's own series, whereas
#' `collection` follows the ELI URI structure.
#'
#' @param since Optional date (or anything [as.Date()] accepts). Only changes
#'   on or after this date are returned.
#' @param collection Optional character vector of collection codes, e.g.
#'   `"lta"` or `c("lta", "ltc")`.
#' @param reason Optional character vector of change reasons. Recognised
#'   values are `"NewDocument"`, `"DocumentContentChanged"`,
#'   `"DocumentMetadataChanged"`,
#'   `"DocumentMetadataChangedAndDocumentContentChanged"`,
#'   `"RemovedDocument"` and `"Unknown"`.
#'
#' @return A [tibble][tibble::tibble] with one row per change:
#'   \describe{
#'     \item{`eli`}{Canonical ELI URI. Pass to [dlx_get_doc()] via `eli`.}
#'     \item{`collection`, `year`, `number`}{Derived from the ELI URI.}
#'     \item{`title`}{Document title.}
#'     \item{`updated`}{Date the entry was last published or updated.}
#'     \item{`change_date`}{Date of the change itself, from the source's
#'       `changeDate` element.}
#'     \item{`reason_for_change`}{Why the entry appears in the feed.}
#'     \item{`published_in`}{Publication channel.}
#'     \item{`n_images`}{Number of images attached to the document.}
#'   }
#'
#'   Zero rows if no change matches the filters.
#'
#' @seealso [dlx_get_doc()] to retrieve any of the changed documents.
#'
#' @source Retsinformation ELI update feed,
#'   <https://www.retsinformation.dk/eli/eli-update-feed.atom>.
#'
#' @examples
#' if (FALSE) {
#'   # Everything the feed holds
#'   dlx_get_changes()
#'
#'   # New acts and regulations only, from the past fortnight
#'   dlx_get_changes(
#'     since      = Sys.Date() - 14,
#'     collection = "lta",
#'     reason     = "NewDocument"
#'   )
#'
#'   # Fetch the first changed document in full
#'   ch <- dlx_get_changes(collection = "lta")
#'   dlx_get_doc(eli = ch$eli[1])
#' }
#'
#' @export
dlx_get_changes <- function(since = NULL, collection = NULL, reason = NULL) {

  res <- perform_eli_request(DLX_FEED_URL)

  if (identical(res$status, "not_found")) {
    dlx_abort(
      "The ELI update feed is not available.",
      paste0("Tried ", DLX_FEED_URL),
      class = "danlex_http_error"
    )
  }

  changes <- parse_change_feed(res$body)
  filter_changes(changes, since = since, collection = collection,
                 reason = reason)
}

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DLX_FEED_URL <- "https://retsinformation.dk/eli/eli-update-feed.atom"

# The namespace is declared explicitly rather than taken from xml_ns(), whose
# automatic d1/d2 prefixes depend on declaration order in the document.
#
# Note that the ELI extension elements (published_in, reasonForChange,
# changeDate, images) inherit Atom's default namespace instead of carrying
# one of their own. That is contrary to the ELI specification, so "./d1:x"
# matches and "./x" does not. If Civilstyrelsen ever corrects this, every
# extension column silently becomes NA — hence the test that guards it.
DLX_ATOM_NS <- c(d1 = "http://www.w3.org/2005/Atom")

DLX_CHANGE_REASONS <- c(
  "Unknown",
  "DocumentMetadataChanged",
  "DocumentContentChanged",
  "DocumentMetadataChangedAndDocumentContentChanged",
  "NewDocument",
  "RemovedDocument"
)

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

#' Text of a child element for each entry, with blanks as NA
#' @noRd
entry_text <- function(entries, field) {
  nodes <- xml2::xml_find_first(entries, paste0("./d1:", field), DLX_ATOM_NS)
  txt <- trimws(xml2::xml_text(nodes))
  txt[!nzchar(txt)] <- NA_character_
  txt
}

#' Parse an ELI update feed into a tibble
#'
#' Kept separate from the HTTP call so it can be tested against a fixture.
#' @noRd
parse_change_feed <- function(feed) {

  # Anchored at /d1:feed/d1:entry rather than //d1:entry: a bare //d1:id also
  # matches the feed's own identifier, which would shift every column by one.
  entries <- xml2::xml_find_all(feed, "/d1:feed/d1:entry", DLX_ATOM_NS)

  if (length(entries) == 0L) return(dlx_empty_changes())

  eli <- entry_text(entries, "id")

  # The feed carries no collection segment; recover it from the identifier.
  ids <- lapply(eli, function(x) {
    if (is.na(x)) {
      list(collection = NA_character_, year = NA_integer_, number = NA_character_)
    } else {
      dlx_parse_eli(x)
    }
  })

  n_images <- vapply(
    entries,
    function(e) length(xml2::xml_find_all(e, "./d1:images/d1:image", DLX_ATOM_NS)),
    integer(1)
  )

  tibble::tibble(
    eli               = eli,
    collection        = vapply(ids, function(x) x$collection, character(1)),
    year              = vapply(ids, function(x) x$year, integer(1)),
    number            = vapply(ids, function(x) x$number, character(1)),
    title             = entry_text(entries, "title"),
    updated           = parse_date_vec(entry_text(entries, "updated")),
    change_date       = parse_date_vec(entry_text(entries, "changeDate")),
    reason_for_change = entry_text(entries, "reasonForChange"),
    published_in      = entry_text(entries, "published_in"),
    n_images          = n_images
  )
}

#' Parse a vector of dates, tolerating timestamps and malformed values
#'
#' Atom `updated` is nominally a full timestamp, though this feed emits plain
#' dates. Taking the first ten characters handles both without warning.
#' @noRd
parse_date_vec <- function(x) {
  out <- suppressWarnings(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
  out
}

#' Apply the client-side filters
#' @noRd
filter_changes <- function(changes, since = NULL, collection = NULL,
                           reason = NULL) {

  if (!is.null(since)) {
    since <- tryCatch(
      as.Date(since),
      error = function(e) {
        dlx_abort("`since` must be a date or convertible to one.",
                  class = "danlex_bad_argument")
      }
    )
    if (length(since) != 1L || is.na(since)) {
      dlx_abort("`since` must be a single non-missing date.",
                class = "danlex_bad_argument")
    }

    if (nrow(changes) > 0L) {
      earliest <- suppressWarnings(min(changes$updated, na.rm = TRUE))
      if (is.finite(earliest) && since < earliest) {
        warning(
          "The feed reaches back only to ", format(earliest),
          ", so changes before that date cannot be returned. ",
          "The result is truncated.",
          call. = FALSE
        )
      }
    }

    keep <- !is.na(changes$updated) & changes$updated >= since
    changes <- changes[keep, , drop = FALSE]
  }

  if (!is.null(collection)) {
    unknown <- setdiff(collection, DLX_COLLECTIONS_ALL)
    if (length(unknown) > 0L) {
      warning("Unknown collection code(s): ", paste(unknown, collapse = ", "),
              ". Known codes: ", paste(DLX_COLLECTIONS_ALL, collapse = ", "),
              call. = FALSE)
    }
    changes <- changes[!is.na(changes$collection) &
                         changes$collection %in% collection, , drop = FALSE]
  }

  if (!is.null(reason)) {
    unknown <- setdiff(reason, DLX_CHANGE_REASONS)
    if (length(unknown) > 0L) {
      warning("Unrecognised change reason(s): ",
              paste(unknown, collapse = ", "), call. = FALSE)
    }
    changes <- changes[!is.na(changes$reason_for_change) &
                         changes$reason_for_change %in% reason, , drop = FALSE]
  }

  changes
}

#' Zero-row change table with the correct columns and types
#' @noRd
dlx_empty_changes <- function() {
  tibble::tibble(
    eli               = character(0),
    collection        = character(0),
    year              = integer(0),
    number            = character(0),
    title             = character(0),
    updated           = as.Date(character(0)),
    change_date       = as.Date(character(0)),
    reason_for_change = character(0),
    published_in      = character(0),
    n_images          = integer(0)
  )
}
