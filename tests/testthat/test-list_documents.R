# Tests for dlx_list_documents() and the session index.
#
# Parsing is separated from retrieval so the URI-splitting logic — the part
# that actually broke during exploration — can be tested without the network.
#
# The live tests are the slowest in the package: retrieving the index takes
# about a minute. They fetch it once and reuse the session copy.

# ---------------------------------------------------------------------------
# URI parsing
# ---------------------------------------------------------------------------

test_that("three-segment ELI URIs split into components", {
  locs <- c(
    "https://retsinformation.dk/eli/lta/1998/763",
    "https://retsinformation.dk/eli/mt/1998/208",
    "https://retsinformation.dk/eli/retsinfo/1665/1"
  )
  idx <- parse_index_locs(locs, rep("2026-07-29", 3))

  expect_equal(idx$collection, c("lta", "mt", "retsinfo"))
  expect_equal(idx$year, c(1998L, 1998L, 1665L))
  expect_equal(idx$number, c("763", "208", "1"))
  expect_equal(idx$eli, locs)
})

test_that("non-numeric document numbers survive", {
  # /eli/fob/2009/1-1 is the only such case in 202,690 URIs, and it is exactly
  # the kind of single exception that an integer column turns into a silent NA.
  idx <- parse_index_locs("https://retsinformation.dk/eli/fob/2009/1-1",
                          "2026-07-29")
  expect_equal(idx$number, "1-1")
  expect_type(idx$number, "character")
})

test_that("ft identifiers fall back to the two-segment form", {
  # Folketinget uses session identifiers rather than year/number, so the
  # three-segment pattern cannot match. During exploration this silently
  # dropped 41,447 URIs — a fifth of the corpus — because table() omits NAs.
  locs <- c(
    "https://retsinformation.dk/eli/ft/202522L00009",
    "https://retsinformation.dk/eli/ft/202513LB0086"
  )
  idx <- parse_index_locs(locs, rep("2026-07-29", 2))

  expect_equal(idx$collection, c("ft", "ft"))
  expect_true(all(is.na(idx$year)))
  expect_equal(idx$number, c("202522L00009", "202513LB0086"))
})

test_that("every URI is accounted for, whatever its shape", {
  # The invariant that would have caught the exploration bug immediately.
  locs <- c(
    "https://retsinformation.dk/eli/lta/1998/763",
    "https://retsinformation.dk/eli/ft/202522L00009",
    "https://retsinformation.dk/eli/fob/2009/1-1"
  )
  idx <- parse_index_locs(locs, rep("2026-07-29", 3))

  expect_equal(nrow(idx), length(locs))
  expect_false(anyNA(idx$collection))
  expect_false(anyNA(idx$number))
})

test_that("lastmod parses and tolerates rubbish", {
  idx <- parse_index_locs(
    rep("https://retsinformation.dk/eli/lta/1998/763", 2),
    c("2026-07-29", "not-a-date")
  )
  expect_equal(idx$lastmod[1], as.Date("2026-07-29"))
  expect_true(is.na(idx$lastmod[2]))
})

# ---------------------------------------------------------------------------
# Session cache
# ---------------------------------------------------------------------------

test_that("status reports an empty cache before anything is retrieved", {
  dlx_clear_index()
  st <- dlx_index_status()

  expect_false(st$loaded)
  expect_true(is.na(st$n_documents))
  expect_equal(nrow(st), 1L)
})

test_that("clearing an empty cache is harmless", {
  dlx_clear_index()
  expect_false(dlx_clear_index())
})

test_that("the cache round-trips", {
  dlx_clear_index()

  fake <- parse_index_locs(
    c("https://retsinformation.dk/eli/lta/2000/1",
      "https://retsinformation.dk/eli/mt/2000/2"),
    c("2026-01-01", "2026-01-02")
  )
  assign("index", fake, envir = .dlx_cache)
  assign("retrieved_at", Sys.time(), envir = .dlx_cache)

  st <- dlx_index_status()
  expect_true(st$loaded)
  expect_equal(st$n_documents, 2L)
  expect_equal(st$n_collections, 2L)
  expect_gt(st$size_mb, -1)

  expect_true(dlx_clear_index())
  expect_false(dlx_index_status()$loaded)
})

# ---------------------------------------------------------------------------
# Filtering
# ---------------------------------------------------------------------------

test_that("year and collection filter independently and together", {
  dlx_clear_index()
  on.exit(dlx_clear_index(), add = TRUE)

  fake <- parse_index_locs(
    c("https://retsinformation.dk/eli/lta/2000/1",
      "https://retsinformation.dk/eli/lta/2001/2",
      "https://retsinformation.dk/eli/mt/2000/3",
      "https://retsinformation.dk/eli/ft/202522L00009"),
    rep("2026-01-01", 4)
  )
  assign("index", fake, envir = .dlx_cache)
  assign("retrieved_at", Sys.time(), envir = .dlx_cache)

  expect_equal(nrow(dlx_list_documents(progress = FALSE)), 4L)
  expect_equal(nrow(dlx_list_documents(year = 2000, progress = FALSE)), 2L)
  expect_equal(nrow(dlx_list_documents(collection = "lta", progress = FALSE)), 2L)
  expect_equal(
    nrow(dlx_list_documents(year = 2000, collection = "lta", progress = FALSE)),
    1L
  )
  expect_equal(nrow(dlx_list_documents(year = c(2000, 2001), progress = FALSE)), 3L)

  # ft has no year, so a year filter must exclude it rather than error
  expect_equal(nrow(dlx_list_documents(collection = "ft", progress = FALSE)), 1L)
  expect_equal(
    nrow(dlx_list_documents(year = 2000, collection = "ft", progress = FALSE)),
    0L
  )
})

test_that("filtering to nothing preserves the column shape", {
  dlx_clear_index()
  on.exit(dlx_clear_index(), add = TRUE)

  fake <- parse_index_locs("https://retsinformation.dk/eli/lta/2000/1",
                           "2026-01-01")
  assign("index", fake, envir = .dlx_cache)
  assign("retrieved_at", Sys.time(), envir = .dlx_cache)

  out <- dlx_list_documents(year = 1900, progress = FALSE)
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), names(dlx_empty_index()))
})

test_that("bad filter arguments are rejected or warned about", {
  dlx_clear_index()
  on.exit(dlx_clear_index(), add = TRUE)

  fake <- parse_index_locs("https://retsinformation.dk/eli/lta/2000/1",
                           "2026-01-01")
  assign("index", fake, envir = .dlx_cache)
  assign("retrieved_at", Sys.time(), envir = .dlx_cache)

  expect_error(dlx_list_documents(year = "abcd", progress = FALSE),
               class = "danlex_bad_argument")
  expect_warning(dlx_list_documents(collection = "nosuch", progress = FALSE),
                 "Unknown collection")
})

# ---------------------------------------------------------------------------
# Live
# ---------------------------------------------------------------------------

test_that("the corpus index retrieves and looks sane", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  dlx_clear_index()
  idx <- dlx_list_documents(progress = FALSE)

  expect_s3_class(idx, "tbl_df")
  expect_equal(names(idx), names(dlx_empty_index()))

  # 202,690 documents were observed in July 2026. The corpus grows, so this
  # is a floor rather than an equality; a large shortfall means pages were
  # dropped.
  expect_gt(nrow(idx), 190000L)

  expect_false(anyNA(idx$collection))
  expect_false(anyNA(idx$number))
  expect_true(all(grepl("^https://", idx$eli)))
})

test_that("all seven known collections are present", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  idx <- dlx_list_documents(progress = FALSE)
  found <- sort(unique(idx$collection))

  # If a new code appears, DLX_COLLECTIONS_ALL and the documentation need
  # updating — hence testing the set rather than a subset.
  expect_setequal(found, DLX_COLLECTIONS_ALL)
})

test_that("coverage reaches back to the seventeenth century", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  idx   <- dlx_list_documents(progress = FALSE)
  years <- idx$year[!is.na(idx$year)]

  expect_lt(min(years), 1700L)
  expect_gte(max(years), 2026L)
})

test_that("year is missing only for ft", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  idx <- dlx_list_documents(progress = FALSE)
  expect_equal(unique(idx$collection[is.na(idx$year)]), "ft")
})

test_that("the second call uses the session copy", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  dlx_list_documents(progress = FALSE)

  # Retrieval takes about a minute; a cached call should be effectively free.
  elapsed <- system.time(dlx_list_documents(progress = FALSE))[["elapsed"]]
  expect_lt(elapsed, 5)

  st <- dlx_index_status()
  expect_true(st$loaded)
  expect_gt(st$n_documents, 190000L)
})

test_that("indexed documents can be retrieved", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  idx <- dlx_list_documents(year = 2025, collection = "lta", progress = FALSE)
  skip_if(nrow(idx) == 0L, "No 2025 Lovtidende A documents in the index")

  d <- dlx_get_doc(eli = idx$eli[1])
  expect_true(d$status %in% c("ok", "metadata_only"))
  expect_equal(d$collection, "lta")
  expect_equal(d$year, 2025L)
})
