#' List the documents in the Retsinformation corpus
#'
#' Enumerates every document published through the ELI service, one row per
#' document, with the collection, year, number and last modification date.
#'
#' @section The first call is slow:
#'
#' The corpus index is published as a sitemap split across 21 pages of up to
#' 10,000 entries each, and the pages are not ordered in any way that permits
#' fetching a single year. The whole index must therefore be retrieved, which
#' takes roughly 40 seconds at a polite request rate and around 50 MB of
#' memory.
#'
#' The result is held for the remainder of the session, so subsequent calls
#' return immediately. `refresh = TRUE` forces a re-fetch, and
#' [dlx_clear_index()] discards it.
#'
#' danlex deliberately does not write the index to disk. To keep it between
#' sessions, save it yourself:
#'
#' ```
#' idx <- dlx_list_documents()
#' saveRDS(idx, "danlex-index.rds")
#' ```
#'
#' @section What the index does and does not contain:
#'
#' The sitemap carries only identifiers and modification dates. Titles,
#' document types and dates of enactment are not present; retrieve those with
#' [dlx_get_doc()] for the documents you actually need.
#'
#' `lastmod` records when Retsinformation last modified its record, which is
#' unrelated to when the instrument was enacted, amended or repealed. A 1950s
#' act may carry a recent `lastmod` simply because its record was revised.
#'
#' The sitemap is refreshed monthly, so documents published in the last few
#' weeks may be missing. Use [dlx_get_changes()] for recent material.
#'
#' @param year Optional year or vector of years.
#' @param collection Optional character vector of collection codes. See
#'   [dlx_get_doc()] for what each covers.
#' @param refresh Re-fetch the index even if this session already holds one.
#' @param progress Show a progress bar during retrieval. Defaults to
#'   [interactive()].
#'
#' @return A [tibble][tibble::tibble] with one row per document:
#'   \describe{
#'     \item{`eli`}{Canonical ELI URI. Pass to [dlx_get_doc()] via `eli`.}
#'     \item{`collection`}{Collection code.}
#'     \item{`year`}{Year, or `NA` for `ft`, which uses parliamentary
#'       session identifiers instead.}
#'     \item{`number`}{Document number, as character.}
#'     \item{`lastmod`}{Date the record was last modified.}
#'   }
#'
#' @seealso [dlx_index_status()], [dlx_clear_index()], [dlx_get_changes()]
#'
#' @examples
#' if (FALSE) {
#'   # Whole corpus (slow on first call)
#'   idx <- dlx_list_documents()
#'
#'   # Acts and regulations from a single year
#'   dlx_list_documents(year = 2025, collection = "lta")
#'
#'   # How many documents per collection
#'   table(idx$collection)
#' }
#'
#' @export
dlx_list_documents <- function(year = NULL, collection = NULL,
                               refresh = FALSE, progress = interactive()) {

  idx <- dlx_index(refresh = refresh, progress = progress)

  if (!is.null(year)) {
    year <- suppressWarnings(as.integer(year))
    if (length(year) == 0L || anyNA(year)) {
      dlx_abort("`year` must be one or more four-digit years.",
                class = "danlex_bad_argument")
    }
    idx <- idx[!is.na(idx$year) & idx$year %in% year, , drop = FALSE]
  }

  if (!is.null(collection)) {
    unknown <- setdiff(collection, DLX_COLLECTIONS_ALL)
    if (length(unknown) > 0L) {
      warning("Unknown collection code(s): ", paste(unknown, collapse = ", "),
              ". Known codes: ", paste(DLX_COLLECTIONS_ALL, collapse = ", "),
              call. = FALSE)
    }
    idx <- idx[idx$collection %in% collection, , drop = FALSE]
  }

  idx
}

#' Report on the session's corpus index
#'
#' @return A one-row [tibble][tibble::tibble] with `loaded`, `n_documents`,
#'   `n_collections`, `retrieved_at` and `size_mb`. When no index has been
#'   retrieved, `loaded` is `FALSE` and the rest are missing.
#'
#' @seealso [dlx_list_documents()]
#'
#' @examples
#' dlx_index_status()
#'
#' @export
dlx_index_status <- function() {
  if (!dlx_index_loaded()) {
    return(tibble::tibble(
      loaded        = FALSE,
      n_documents   = NA_integer_,
      n_collections = NA_integer_,
      retrieved_at  = as.POSIXct(NA),
      size_mb       = NA_real_
    ))
  }

  idx <- .dlx_cache$index
  tibble::tibble(
    loaded        = TRUE,
    n_documents   = nrow(idx),
    n_collections = length(unique(idx$collection)),
    retrieved_at  = .dlx_cache$retrieved_at,
    size_mb       = round(as.numeric(utils::object.size(idx)) / 1024^2, 1)
  )
}

#' Discard the session's corpus index
#'
#' Frees the memory held by [dlx_list_documents()]. The next call will
#' retrieve the index again.
#'
#' @return Invisibly, `TRUE` if an index was discarded and `FALSE` if none
#'   was held.
#'
#' @seealso [dlx_list_documents()]
#'
#' @examples
#' dlx_clear_index()
#'
#' @export
dlx_clear_index <- function() {
  had <- dlx_index_loaded()
  if (had) {
    rm(list = c("index", "retrieved_at"), envir = .dlx_cache)
  }
  invisible(had)
}

# ---------------------------------------------------------------------------
# Session cache
# ---------------------------------------------------------------------------

# Session-scoped only: nothing is written to disk, so no CRAN policy on user
# directories applies and no consent prompt is needed. Users who want the
# index to persist can saveRDS() it themselves.
.dlx_cache <- new.env(parent = emptyenv())

#' @noRd
dlx_index_loaded <- function() {
  exists("index", envir = .dlx_cache, inherits = FALSE)
}

#' Retrieve the index, using the session copy when available
#' @noRd
dlx_index <- function(refresh = FALSE, progress = interactive()) {
  if (!refresh && dlx_index_loaded()) {
    return(.dlx_cache$index)
  }

  idx <- harvest_sitemap(progress = progress)

  assign("index", idx, envir = .dlx_cache)
  assign("retrieved_at", Sys.time(), envir = .dlx_cache)

  idx
}

# ---------------------------------------------------------------------------
# Sitemap harvesting
# ---------------------------------------------------------------------------

DLX_SITEMAP_URL <- "https://retsinformation.dk/eli/sitemap.xml"

# The sitemap protocol namespace, declared explicitly for the same reason as
# the Atom one: xml_ns()'s automatic prefixes depend on declaration order.
DLX_SITEMAP_NS <- c(d1 = "http://www.sitemaps.org/schemas/sitemap/0.9")

#' Fetch the sitemap index and every page it lists
#'
#' The top-level document is a <sitemapindex> whose entries are page URLs of
#' the form sitemap.xml?page=N. Those URLs are used verbatim: they omit the
#' "www." prefix, and normalising them would add a redirect to every request.
#'
#' @noRd
harvest_sitemap <- function(progress = interactive()) {

  res <- perform_eli_request(DLX_SITEMAP_URL)
  if (identical(res$status, "not_found")) {
    dlx_abort("The ELI sitemap is not available.",
              paste0("Tried ", DLX_SITEMAP_URL),
              class = "danlex_http_error")
  }

  root <- xml2::xml_root(res$body)
  if (xml2::xml_name(root) != "sitemapindex") {
    dlx_abort(
      "The sitemap is not an index as expected.",
      paste0("Found <", xml2::xml_name(root), ">. The API may have changed."),
      class = "danlex_parse_error"
    )
  }

  pages <- xml2::xml_text(
    xml2::xml_find_all(root, "/d1:sitemapindex/d1:sitemap/d1:loc",
                       DLX_SITEMAP_NS)
  )

  if (length(pages) == 0L) {
    dlx_abort("The sitemap index listed no pages.",
              class = "danlex_parse_error")
  }

  if (isTRUE(progress)) {
    message("Retrieving corpus index from ", length(pages),
            " sitemap pages. This takes about a minute.")
  }

  parts <- vector("list", length(pages))

  for (i in seq_along(pages)) {
    if (isTRUE(progress)) {
      message("  page ", i, "/", length(pages))
    }
    parts[[i]] <- harvest_sitemap_page(pages[i])
    # A courtesy pause: the ELI endpoints are not rate limited, but 21 rapid
    # requests for 30 MB deserves some restraint.
    if (i < length(pages)) Sys.sleep(0.3)
  }

  out <- do.call(rbind, parts)

  if (isTRUE(progress)) {
    message("Retrieved ", nrow(out), " documents.")
  }

  out
}

#' Fetch and parse one sitemap page
#' @noRd
harvest_sitemap_page <- function(url) {

  res <- perform_eli_request(url)
  if (identical(res$status, "not_found")) {
    dlx_abort(paste0("Sitemap page not found: ", url),
              class = "danlex_http_error")
  }

  urls <- xml2::xml_find_all(xml2::xml_root(res$body),
                             "/d1:urlset/d1:url", DLX_SITEMAP_NS)

  if (length(urls) == 0L) return(dlx_empty_index())

  loc     <- xml2::xml_text(xml2::xml_find_first(urls, "./d1:loc", DLX_SITEMAP_NS))
  lastmod <- xml2::xml_text(xml2::xml_find_first(urls, "./d1:lastmod", DLX_SITEMAP_NS))

  parse_index_locs(loc, lastmod)
}

#' Split ELI URIs into their components
#'
#' `number` is character throughout: /eli/fob/2009/1-1 is not numeric, and
#' coercing it to integer would turn it into a silent NA.
#'
#' The `ft` collection uses parliamentary session identifiers rather than
#' year/number, e.g. /eli/ft/202522L00009, so `year` is NA for those rows and
#' `number` holds the whole identifier.
#'
#' @noRd
parse_index_locs <- function(loc, lastmod) {

  parts <- utils::strcapture(
    "/eli/([^/]+)/([0-9]{4})/([^/]+)$",
    loc,
    proto = data.frame(
      collection = character(),
      year       = integer(),
      number     = character()
    )
  )

  # Anything that did not match the three-segment form: currently only `ft`.
  odd <- is.na(parts$collection)
  if (any(odd)) {
    two <- utils::strcapture(
      "/eli/([^/]+)/([^/]+)$",
      loc[odd],
      proto = data.frame(collection = character(), number = character())
    )
    parts$collection[odd] <- two$collection
    parts$number[odd]     <- two$number
  }

  tibble::tibble(
    eli        = loc,
    collection = parts$collection,
    year       = parts$year,
    number     = parts$number,
    lastmod    = parse_date_vec(lastmod)
  )
}

#' Zero-row index with the correct columns and types
#' @noRd
dlx_empty_index <- function() {
  tibble::tibble(
    eli        = character(0),
    collection = character(0),
    year       = integer(0),
    number     = character(0),
    lastmod    = as.Date(character(0))
  )
}
