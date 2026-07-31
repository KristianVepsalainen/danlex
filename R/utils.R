# Internal helpers shared across danlex.
#
# Nothing here is exported. The HTTP layer, the ELI URI builder and the XML
# accessors all live here so that dlx_get_text(), dlx_get_metadata() and
# dlx_get_references() can reuse them without duplication.

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Canonical base. Retsinformation serves both with and without "www.", but the
# ELI sitemap and the Atom feed both emit the bare form, so that is canonical
# and avoids a redirect on every request.
DLX_BASE <- "https://retsinformation.dk"

# Collections observed in the sitemap (July 2026). v1 supports the four
# legislative ones; the others are recognised but flagged.
DLX_COLLECTIONS_V1  <- c("lta", "ltb", "ltc", "mt")
DLX_COLLECTIONS_ALL <- c("lta", "ltb", "ltc", "mt", "retsinfo", "fob", "ft")

# DocumentType composite codes, post-2007 schema. Incomplete: derived from a
# 39-document sample of Lovtidende A. See data-raw/api-exploration.R.
DLX_TYPE_CODES <- c(
  LOKDOK01 = "Lov",
  LOKDOK02 = "Lov (\u00e6ndring)",
  LOKDOK03 = "Lovbekendtg\u00f8relse",
  LOKDOK04 = "Bekendtg\u00f8relse",
  LOKDOK05 = "Bekendtg\u00f8relse (\u00e6ndring)",
  LOKDOK17 = "Anordning"
)

danlex_user_agent <- function() {
  paste0(
    "danlex/", utils::packageVersion("danlex"),
    " (https://github.com/kristianvepsalainen/danlex)"
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------------------
# ELI URI construction
# ---------------------------------------------------------------------------

#' Build a canonical ELI URI
#'
#' Accepts exactly one of three identifier forms and returns the canonical
#' ELI URI (without a format suffix).
#'
#' @param collection,year,number Components of the deterministic form.
#' @param accn Accession number, e.g. `"A19990059229"`.
#' @param eli A full ELI URI.
#'
#' @return A length-one character vector.
#' @noRd
dlx_build_eli <- function(collection = NULL, year = NULL, number = NULL,
                          accn = NULL, eli = NULL) {

  triple <- !is.null(collection) || !is.null(year) || !is.null(number)
  modes  <- c(triple = triple, accn = !is.null(accn), eli = !is.null(eli))

  if (sum(modes) == 0L) {
    dlx_abort(
      "No identifier supplied.",
      "Supply either `collection`, `year` and `number`, or `accn`, or `eli`.",
      class = "danlex_bad_identifier"
    )
  }
  if (sum(modes) > 1L) {
    dlx_abort(
      "More than one identifier form supplied.",
      "Use exactly one of: the (collection, year, number) triple, `accn`, or `eli`.",
      class = "danlex_bad_identifier"
    )
  }

  if (modes[["eli"]]) {
    if (!is.character(eli) || length(eli) != 1L || is.na(eli)) {
      dlx_abort("`eli` must be a single non-missing string.",
                class = "danlex_bad_identifier")
    }
    if (!grepl("^https?://[^/]*retsinformation\\.dk/eli/", eli)) {
      dlx_abort(
        "`eli` does not look like a Retsinformation ELI URI.",
        paste0("Expected something like ", DLX_BASE, "/eli/lta/1998/763"),
        class = "danlex_bad_identifier"
      )
    }
    return(sub("/xml$", "", eli))
  }

  if (modes[["accn"]]) {
    if (!is.character(accn) || length(accn) != 1L || is.na(accn)) {
      dlx_abort("`accn` must be a single non-missing string.",
                class = "danlex_bad_identifier")
    }
    # Format is {letter}{year:4}{number:5}{suffix:2} = 12 characters. We warn
    # rather than abort, because the pattern is empirical, not documented.
    if (!grepl("^[A-Za-z][0-9]{11}$", accn)) {
      warning("`accn` does not match the expected 12-character pattern; ",
              "attempting the request anyway.", call. = FALSE)
    }
    return(paste0(DLX_BASE, "/eli/accn/", accn))
  }

  # Deterministic triple ------------------------------------------------------

  if (is.null(collection) || is.null(year) || is.null(number)) {
    dlx_abort(
      "Incomplete identifier.",
      "`collection`, `year` and `number` must all be supplied together.",
      class = "danlex_bad_identifier"
    )
  }

  collection <- as.character(collection)
  if (length(collection) != 1L || is.na(collection)) {
    dlx_abort("`collection` must be a single non-missing string.",
              class = "danlex_bad_identifier")
  }
  if (!collection %in% DLX_COLLECTIONS_ALL) {
    dlx_abort(
      paste0("Unknown collection: ", collection, "."),
      paste0("Known collections: ", paste(DLX_COLLECTIONS_ALL, collapse = ", ")),
      class = "danlex_bad_identifier"
    )
  }
  if (!collection %in% DLX_COLLECTIONS_V1) {
    warning("Collection '", collection, "' is outside the collections danlex ",
            "currently targets (", paste(DLX_COLLECTIONS_V1, collapse = ", "),
            "). Identifiers may not be deterministic.", call. = FALSE)
  }

  year <- suppressWarnings(as.integer(year))
  if (length(year) != 1L || is.na(year)) {
    dlx_abort("`year` must be a single four-digit year.",
              class = "danlex_bad_identifier")
  }
  if (year < 1600L || year > 2200L) {
    dlx_abort(
      paste0("`year` = ", year, " is outside the plausible range."),
      "The corpus reaches back to 1665.",
      class = "danlex_bad_identifier"
    )
  }

  # number is deliberately CHARACTER: /eli/fob/2009/1-1 exists.
  number <- as.character(number)
  if (length(number) != 1L || is.na(number) || !nzchar(number)) {
    dlx_abort("`number` must be a single non-empty value.",
              class = "danlex_bad_identifier")
  }

  paste0(DLX_BASE, "/eli/", collection, "/", year, "/", number)
}

#' Split a canonical ELI URI back into its components
#' @noRd
dlx_parse_eli <- function(eli) {
  m <- regmatches(
    eli,
    regexec("/eli/([^/]+)/([0-9]{4})/([^/]+)$", eli)
  )[[1]]

  if (length(m) == 4L) {
    return(list(collection = m[2], year = as.integer(m[3]), number = m[4]))
  }

  # accn form, or the ft collection, which uses a different scheme entirely
  m2 <- regmatches(eli, regexec("/eli/(accn|ft)/([^/]+)$", eli))[[1]]
  if (length(m2) == 3L) {
    return(list(collection = m2[2], year = NA_integer_, number = m2[3]))
  }

  list(collection = NA_character_, year = NA_integer_, number = NA_character_)
}

# ---------------------------------------------------------------------------
# HTTP layer
# ---------------------------------------------------------------------------

#' Perform a request against an ELI resource
#'
#' Retrieval from `/eli/` is not rate limited (unlike the harvest service at
#' `api.retsinformation.dk`, which danlex does not use), so no throttling is
#' applied here. Batch functions must throttle themselves.
#'
#' @param url Full URL including any format suffix.
#'
#' @return A list with `status` (`"ok"` or `"not_found"`) and `body`
#'   (an `xml_document`, or `NULL` when not found).
#' @noRd
perform_eli_request <- function(url) {

  req <- httr2::request(url) |>
    httr2::req_user_agent(danlex_user_agent()) |>
    httr2::req_retry(max_tries = 3, is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
    }) |>
    # 404 is a normal, expected answer, so it must not abort. Anything from
    # 500 up is a genuine failure and is allowed through to req_retry / abort.
    httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 500L)

  resp <- tryCatch(
    httr2::req_perform(req),
    httr2_failure = function(e) {
      dlx_abort(
        "Could not connect to Retsinformation.",
        conditionMessage(e),
        class = "danlex_connection_error"
      )
    },
    httr2_http = function(e) {
      dlx_abort(
        "Retsinformation returned a server error.",
        conditionMessage(e),
        class = "danlex_http_error"
      )
    }
  )

  status <- httr2::resp_status(resp)

  if (status == 404L) {
    return(list(status = "not_found", body = NULL))
  }
  if (status != 200L) {
    dlx_abort(
      paste0("Unexpected HTTP status ", status, " from Retsinformation."),
      class = "danlex_http_error"
    )
  }

  # Encoding is forced to UTF-8 deliberately. The Atom feed declares utf-16
  # while emitting UTF-8, and relying on libxml2's guess makes parsing
  # platform-dependent. Document XML appears to be UTF-8 as well, though that
  # has not been verified across the whole corpus (see ROADMAP, open question 7).
  body <- httr2::resp_body_xml(resp, encoding = "UTF-8")

  list(status = "ok", body = body)
}

#' Abort with a danlex condition class
#' @noRd
dlx_abort <- function(message, info = NULL, class = "danlex_error") {
  msg <- c(message)
  if (!is.null(info)) msg <- c(msg, i = info)
  rlang::abort(msg, class = c(class, "danlex_error"), call = NULL)
}

# ---------------------------------------------------------------------------
# XML accessors
# ---------------------------------------------------------------------------
#
# Document XML carries NO namespace, so plain XPath works. (The Atom feed is
# the opposite and needs the d1 prefix throughout — do not copy patterns
# between the two.)

#' Text of the first matching element, or NA
#' @noRd
meta_text <- function(x, field) {
  node <- xml2::xml_find_first(x, paste0("./", field))
  if (inherits(node, "xml_missing")) return(NA_character_)
  value <- xml2::xml_text(node)
  if (is.na(value) || !nzchar(trimws(value))) return(NA_character_)
  trimws(value)
}

#' Parse an ISO date, returning NA rather than warning on anything else
#'
#' Date formats in older records are not documented and may vary, so failure
#' must be silent and recoverable.
#' @noRd
parse_date_safe <- function(x) {
  if (is.na(x)) return(as.Date(NA))
  out <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  out
}

#' Split a DocumentType value into label and code
#'
#' Pre-2008 records hold a whole Danish word ("Bekendtgørelse"); post-2007
#' records hold a composite ("BEK Æ#LOKDOK05").
#'
#' @return A list of `label`, `code` and `standard`.
#' @noRd
split_document_type <- function(x) {
  if (is.na(x)) {
    return(list(label = NA_character_, code = NA_character_,
                standard = NA_character_))
  }

  parts <- strsplit(x, "#", fixed = TRUE)[[1]]
  label <- trimws(parts[1])
  code  <- if (length(parts) > 1L) gsub("\\s+", "", parts[2]) else NA_character_

  standard <- if (!is.na(code) && code %in% names(DLX_TYPE_CODES)) {
    unname(DLX_TYPE_CODES[[code]])
  } else if (is.na(code)) {
    # Old schema: the label is already the full Danish term.
    label
  } else {
    NA_character_
  }

  list(label = label, code = code, standard = standard)
}

#' Extract reference blocks from a Meta node
#'
#' `Concerns` is an empty marker element and `Ref_Accn`, `Ref_Af` and
#' `Ref_Text` are its SIBLINGS, not its children. Grouping is therefore
#' positional: split Meta's children at each marker.
#'
#' The same shape applies to Sign -> Signature and Sub-Sign -> Signature.
#'
#' Note that these references are MAINTAINED, not historical: they point at
#' the current consolidated version of the parent act, not at the version in
#' force when the referring document was issued.
#'
#' @param meta An `xml_node` for `<Meta>`.
#' @param marker Name of the marker element.
#' @param fields Element names to collect within each block.
#'
#' @return A list of named character vectors, one per marker.
#' @noRd
parse_marker_blocks <- function(meta, marker = "Concerns",
                                fields = c("Ref_Accn", "Ref_Af", "Ref_Text")) {

  kids <- xml2::xml_children(meta)
  nm   <- xml2::xml_name(kids)

  marks <- which(nm == marker)
  if (length(marks) == 0L) return(list())

  lapply(seq_along(marks), function(i) {
    from  <- marks[i] + 1L
    to    <- if (i < length(marks)) marks[i + 1L] - 1L else length(nm)
    if (from > to) return(stats::setNames(character(0), character(0)))
    block <- kids[from:to]
    block <- block[xml2::xml_name(block) %in% fields]
    stats::setNames(trimws(xml2::xml_text(block)), xml2::xml_name(block))
  })
}
