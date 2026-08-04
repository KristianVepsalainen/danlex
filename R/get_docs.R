#' Retrieve many documents at once
#'
#' Fetches metadata for a vector of documents and returns one row per
#' identifier, in the order given. Intended for corpus-scale work: the output
#' of [dlx_list_documents()] or the `ref_accn` column of
#' [dlx_get_references()] can be passed straight in.
#'
#' @section Failures do not stop the run:
#'
#' A batch of any size will eventually meet a network hiccup, and losing an
#' hour of retrieval to one dropped connection would be unacceptable. Failed
#' documents are therefore returned as rows with `status = "error"` and the
#' reason in `error_message`, rather than raised as conditions. Inspect them
#' with `subset(out, status == "error")` and retry those identifiers.
#'
#' Missing documents behave as in [dlx_get_doc()]: `status` is
#' `"not_found"`, which is a normal answer rather than a failure.
#'
#' @section Rate limiting:
#'
#' The ELI endpoints are not rate limited — the one-call-per-ten-seconds
#' restriction applies to the harvest service at `api.retsinformation.dk`,
#' which danlex does not use. `delay` nevertheless defaults to a courtesy
#' pause of 0.2 seconds, since a batch of tens of thousands of requests
#' deserves some restraint. Retrieving 1,000 documents takes roughly five
#' minutes at the default.
#'
#' @param eli Character vector of canonical ELI URIs.
#' @param accn Character vector of accession numbers. Supply this or `eli`,
#'   not both.
#' @param delay Seconds to pause between requests.
#' @param progress Show progress. Defaults to [interactive()].
#'
#' @return A [tibble][tibble::tibble] with the columns of [dlx_get_doc()],
#'   plus `error_message`, which is `NA` except on failed rows. One row per
#'   input identifier, in input order. Duplicated identifiers are fetched
#'   once and repeated in the output.
#'
#' @seealso [dlx_get_doc()] for a single document, [dlx_list_documents()]
#'   for identifiers to feed in.
#'
#' @examples
#' if (FALSE) {
#'   # Every act and regulation from one year
#'   idx  <- dlx_list_documents(year = 2025, collection = "lta")
#'   docs <- dlx_get_docs(idx$eli)
#'
#'   # Anything that failed
#'   subset(docs, status == "error")
#'
#'   # Follow a whole set of references
#'   refs <- dlx_get_references("lta", 1998, 763)
#'   dlx_get_docs(accn = refs$ref_accn)
#' }
#'
#' @export
dlx_get_docs <- function(eli = NULL, accn = NULL, delay = 0.2,
                         progress = interactive()) {

  if (is.null(eli) == is.null(accn)) {
    dlx_abort(
      "Supply exactly one of `eli` or `accn`.",
      class = "danlex_bad_identifier"
    )
  }

  ids     <- if (is.null(eli)) accn else eli
  by_accn <- is.null(eli)

  if (!is.character(ids)) {
    dlx_abort("Identifiers must be a character vector.",
              class = "danlex_bad_identifier")
  }
  if (length(ids) == 0L) {
    return(dlx_empty_docs())
  }
  if (anyNA(ids)) {
    dlx_abort("Identifiers must not contain missing values.",
              class = "danlex_bad_identifier")
  }

  if (!is.numeric(delay) || length(delay) != 1L || is.na(delay) || delay < 0) {
    dlx_abort("`delay` must be a single non-negative number.",
              class = "danlex_bad_argument")
  }

  # Duplicates are fetched once and reassembled afterwards. Reference columns
  # commonly repeat the same parent act dozens of times.
  unique_ids <- unique(ids)

  if (isTRUE(progress)) {
    n <- length(unique_ids)
    message("Retrieving ", n, " document", if (n != 1L) "s" else "",
            if (n < length(ids)) paste0(" (", length(ids), " requested, ",
                                        length(ids) - n, " duplicated)")
            else "",
            ". Estimated time: ", format_duration(n * (delay + 0.3)), ".")
  }

  results <- vector("list", length(unique_ids))

  for (i in seq_along(unique_ids)) {
    results[[i]] <- fetch_one_safely(unique_ids[i], by_accn = by_accn)

    if (isTRUE(progress) && (i %% 50L == 0L || i == length(unique_ids))) {
      message("  ", i, "/", length(unique_ids))
    }
    if (delay > 0 && i < length(unique_ids)) Sys.sleep(delay)
  }

  out <- do.call(rbind, results)

  # Restore input order, repeating any duplicates.
  out <- out[match(ids, unique_ids), , drop = FALSE]

  if (isTRUE(progress)) {
    n_err <- sum(out$status == "error")
    n_nf  <- sum(out$status == "not_found")
    message("Done. ", nrow(out) - n_err - n_nf, " retrieved",
            if (n_nf > 0L) paste0(", ", n_nf, " not found") else "",
            if (n_err > 0L) paste0(", ", n_err, " failed") else "", ".")
  }

  out
}

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

#' Fetch one document, converting any condition into an error row
#'
#' Server errors, timeouts and malformed responses all become rows rather
#' than conditions, so a long batch survives them.
#' @noRd
fetch_one_safely <- function(id, by_accn) {

  row <- tryCatch(
    if (by_accn) dlx_get_doc(accn = id) else dlx_get_doc(eli = id),
    error = function(e) {
      structure(conditionMessage(e), class = "dlx_failed")
    }
  )

  if (inherits(row, "dlx_failed")) {
    eli_uri <- tryCatch(
      if (by_accn) dlx_build_eli(accn = id) else dlx_build_eli(eli = id),
      error = function(e) NA_character_
    )
    err <- dlx_doc_row(
      if (is.na(eli_uri)) id else eli_uri,
      status = "error"
    )
    err$error_message <- as.character(row)
    return(err)
  }

  row$error_message <- NA_character_
  row
}

#' Human-readable duration
#' @noRd
format_duration <- function(seconds) {
  if (seconds < 90) {
    paste0(round(seconds), " seconds")
  } else if (seconds < 5400) {
    paste0(round(seconds / 60), " minutes")
  } else {
    paste0(round(seconds / 3600, 1), " hours")
  }
}

#' Zero-row batch result with the correct columns and types
#' @noRd
dlx_empty_docs <- function() {
  template <- dlx_doc_row("", status = "")
  template$error_message <- NA_character_
  template[0, , drop = FALSE]
}
