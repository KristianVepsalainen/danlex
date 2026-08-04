# Tests for dlx_get_docs().
#
# The properties that matter for batch retrieval are order preservation,
# duplicate handling and — above all — that a failure partway through does not
# discard the work already done. The offline tests cover argument handling and
# the failure path; the live ones keep to small batches so the suite stays
# quick.

# ---------------------------------------------------------------------------
# Argument handling
# ---------------------------------------------------------------------------

test_that("exactly one identifier vector is required", {
  expect_error(dlx_get_docs(), class = "danlex_bad_identifier")
  expect_error(
    dlx_get_docs(eli = "https://retsinformation.dk/eli/lta/1998/763",
                 accn = "B19980076305"),
    class = "danlex_bad_identifier"
  )
})

test_that("identifiers must be a character vector without missing values", {
  expect_error(dlx_get_docs(eli = 1:3), class = "danlex_bad_identifier")
  expect_error(dlx_get_docs(eli = c("a", NA)), class = "danlex_bad_identifier")
})

test_that("an empty input returns the standard zero-row shape", {
  out <- dlx_get_docs(eli = character(0))
  expect_equal(nrow(out), 0L)
  expect_true("error_message" %in% names(out))
  expect_equal(names(out), names(dlx_empty_docs()))
})

test_that("delay is validated", {
  ids <- "https://retsinformation.dk/eli/lta/1998/763"
  expect_error(dlx_get_docs(eli = ids, delay = -1),
               class = "danlex_bad_argument")
  expect_error(dlx_get_docs(eli = ids, delay = c(1, 2)),
               class = "danlex_bad_argument")
  expect_error(dlx_get_docs(eli = ids, delay = NA),
               class = "danlex_bad_argument")
})

test_that("durations are reported in sensible units", {
  expect_match(format_duration(30), "seconds")
  expect_match(format_duration(600), "minutes")
  expect_match(format_duration(7200), "hours")
})

# ---------------------------------------------------------------------------
# Failure handling
# ---------------------------------------------------------------------------

test_that("a failed request becomes a row, not a condition", {
  # The whole point of the batch function: an hour of retrieval must not be
  # lost to one dropped connection.
  local_mocked_bindings(
    dlx_get_doc = function(...) stop("simulated network failure")
  )

  out <- dlx_get_docs(
    eli = "https://retsinformation.dk/eli/lta/1998/763",
    delay = 0, progress = FALSE
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$status, "error")
  expect_match(out$error_message, "simulated network failure")
  expect_equal(out$eli, "https://retsinformation.dk/eli/lta/1998/763")
})

test_that("a failure partway through does not discard earlier results", {
  calls <- 0L
  local_mocked_bindings(
    dlx_get_doc = function(eli = NULL, accn = NULL, ...) {
      calls <<- calls + 1L
      if (calls == 2L) stop("boom")
      row <- dlx_doc_row(eli, status = "ok")
      row$title <- paste("Document", calls)
      row
    }
  )

  out <- dlx_get_docs(
    eli = paste0("https://retsinformation.dk/eli/lta/2000/", 1:3),
    delay = 0, progress = FALSE
  )

  expect_equal(nrow(out), 3L)
  expect_equal(out$status, c("ok", "error", "ok"))
  expect_equal(out$title[1], "Document 1")
  expect_false(is.na(out$title[3]))
})

test_that("error rows have the same columns as successful ones", {
  local_mocked_bindings(
    dlx_get_doc = function(eli = NULL, accn = NULL, ...) {
      if (grepl("/2$", eli)) stop("boom")
      row <- dlx_doc_row(eli, status = "ok")
      row$error_message <- NA_character_
      row
    }
  )

  out <- dlx_get_docs(
    eli = paste0("https://retsinformation.dk/eli/lta/2000/", 1:2),
    delay = 0, progress = FALSE
  )

  expect_equal(nrow(out), 2L)
  expect_true(is.na(out$error_message[1]))
  expect_false(is.na(out$error_message[2]))
})

# ---------------------------------------------------------------------------
# Order and duplicates
# ---------------------------------------------------------------------------

test_that("output order matches input order", {
  local_mocked_bindings(
    dlx_get_doc = function(eli = NULL, accn = NULL, ...) {
      row <- dlx_doc_row(eli, status = "ok")
      row$title <- eli
      row
    }
  )

  ids <- paste0("https://retsinformation.dk/eli/lta/2000/", c(3, 1, 2))
  out <- dlx_get_docs(eli = ids, delay = 0, progress = FALSE)

  expect_equal(out$title, ids)
})

test_that("duplicates are fetched once but returned in full", {
  fetched <- character(0)
  local_mocked_bindings(
    dlx_get_doc = function(eli = NULL, accn = NULL, ...) {
      fetched <<- c(fetched, eli)
      row <- dlx_doc_row(eli, status = "ok")
      row$title <- eli
      row
    }
  )

  ids <- paste0("https://retsinformation.dk/eli/lta/2000/", c(1, 2, 1, 1))
  out <- dlx_get_docs(eli = ids, delay = 0, progress = FALSE)

  # Reference columns commonly repeat the same parent act many times.
  expect_length(fetched, 2L)
  expect_equal(nrow(out), 4L)
  expect_equal(out$title, ids)
})

# ---------------------------------------------------------------------------
# Live
# ---------------------------------------------------------------------------

test_that("a small batch retrieves by ELI", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  ids <- c(
    "https://retsinformation.dk/eli/lta/1998/763",
    "https://retsinformation.dk/eli/lta/2007/1081"
  )
  out <- dlx_get_docs(eli = ids, delay = 0.2, progress = FALSE)

  expect_equal(nrow(out), 2L)
  expect_equal(out$eli, ids)
  expect_equal(out$status, c("metadata_only", "ok"))
  expect_true(all(is.na(out$error_message)))
})

test_that("a small batch retrieves by accession number", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  out <- dlx_get_docs(
    accn = c("B19980076305", "A19990059229"),
    delay = 0.2, progress = FALSE
  )

  expect_equal(nrow(out), 2L)
  expect_equal(out$accn, c("B19980076305", "A19990059229"))
  expect_equal(out$document_type[2], "Lovbekendtg\u00f8relse")
})

test_that("missing documents are not_found rather than error", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  # A 404 is a normal answer from this API, distinct from a failed request.
  out <- dlx_get_docs(
    eli = c("https://retsinformation.dk/eli/lta/1998/763",
            "https://retsinformation.dk/eli/lta/1998/999999"),
    delay = 0.2, progress = FALSE
  )

  expect_equal(out$status, c("metadata_only", "not_found"))
  expect_true(all(is.na(out$error_message)))
})

test_that("references can be resolved in bulk", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  # The workflow the function exists for: get an edge list, then fetch every
  # instrument it points to.
  refs <- dlx_get_references("lta", 1998, 763)
  out  <- dlx_get_docs(accn = refs$ref_accn, delay = 0.2, progress = FALSE)

  expect_equal(nrow(out), nrow(refs))
  expect_true(all(out$status %in% c("ok", "metadata_only")))
  expect_equal(out$accn, refs$ref_accn)
})

test_that("batch and single retrieval agree", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  one   <- dlx_get_doc("lta", 1998, 763)
  batch <- dlx_get_docs(eli = "https://retsinformation.dk/eli/lta/1998/763",
                        delay = 0, progress = FALSE)

  # error_message is the only column the batch form adds.
  expect_equal(setdiff(names(batch), names(one)), "error_message")
  expect_equal(batch$accn, one$accn)
  expect_equal(batch$title, one$title)
  expect_equal(batch$status, one$status)
})
