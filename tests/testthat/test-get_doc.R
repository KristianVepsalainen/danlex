# Tests for dlx_get_doc() and the identifier helpers.
#
# Offline tests come first: URI construction and validation need no network
# and therefore run everywhere, including on CRAN.
#
# Live tests are guarded by skip_on_cran() and skip_if_offline(). Document
# retrieval from /eli/ is stable — these identifiers are permanent — so
# recorded fixtures are unnecessary here. The Atom feed functions will need
# httptest2 mocks, because their content changes daily.

# ---------------------------------------------------------------------------
# URI construction
# ---------------------------------------------------------------------------

test_that("the deterministic triple builds a canonical ELI URI", {
  expect_equal(
    dlx_build_eli("lta", 1998, 763),
    "https://retsinformation.dk/eli/lta/1998/763"
  )
  # Numeric and character numbers must agree
  expect_equal(
    dlx_build_eli("lta", 1998, "763"),
    dlx_build_eli("lta", 1998, 763)
  )
})

test_that("non-numeric document numbers are preserved", {
  # /eli/fob/2009/1-1 is the only non-numeric number in 202,690 URIs.
  # If `number` is ever coerced to integer this becomes a silent NA.
  expect_warning(
    uri <- dlx_build_eli("fob", 2009, "1-1"),
    "outside the collections"
  )
  expect_equal(uri, "https://retsinformation.dk/eli/fob/2009/1-1")
})

test_that("accession numbers build an accn URI", {
  expect_equal(
    dlx_build_eli(accn = "A19990059229"),
    "https://retsinformation.dk/eli/accn/A19990059229"
  )
  expect_warning(dlx_build_eli(accn = "nonsense"), "12-character pattern")
})

test_that("a full ELI URI passes through, with any format suffix stripped", {
  uri <- "https://retsinformation.dk/eli/lta/1998/763"
  expect_equal(dlx_build_eli(eli = uri), uri)
  expect_equal(dlx_build_eli(eli = paste0(uri, "/xml")), uri)
})

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

test_that("exactly one identifier form is required", {
  expect_error(dlx_build_eli(), class = "danlex_bad_identifier")
  expect_error(
    dlx_build_eli("lta", 1998, 763, accn = "A19990059229"),
    class = "danlex_bad_identifier"
  )
  expect_error(dlx_build_eli("lta", 1998), class = "danlex_bad_identifier")
})

test_that("implausible identifier components are rejected", {
  expect_error(dlx_build_eli("nosuch", 1998, 763), class = "danlex_bad_identifier")
  expect_error(dlx_build_eli("lta", 1400, 763), class = "danlex_bad_identifier")
  expect_error(dlx_build_eli("lta", "abcd", 763), class = "danlex_bad_identifier")
  expect_error(dlx_build_eli("lta", 1998, ""), class = "danlex_bad_identifier")
  expect_error(dlx_build_eli(eli = "https://example.com/foo"),
               class = "danlex_bad_identifier")
})

test_that("ELI URIs round-trip through the parser", {
  p <- dlx_parse_eli("https://retsinformation.dk/eli/lta/1998/763")
  expect_equal(p$collection, "lta")
  expect_equal(p$year, 1998L)
  expect_equal(p$number, "763")

  p2 <- dlx_parse_eli("https://retsinformation.dk/eli/fob/2009/1-1")
  expect_equal(p2$number, "1-1")
})

# ---------------------------------------------------------------------------
# Metadata parsing helpers
# ---------------------------------------------------------------------------

test_that("DocumentType is split across both schema eras", {
  # Pre-2008: a whole Danish word, no internal code
  old <- split_document_type("Bekendtg\u00f8relse")
  expect_equal(old$label, "Bekendtg\u00f8relse")
  expect_true(is.na(old$code))
  expect_equal(old$standard, "Bekendtg\u00f8relse")

  # Post-2007: display abbreviation + internal code
  new <- split_document_type("BEK \u00c6#LOKDOK05")
  expect_equal(new$label, "BEK \u00c6")
  expect_equal(new$code, "LOKDOK05")
  expect_false(is.na(new$standard))

  # Unknown codes must not invent a standard label
  unk <- split_document_type("XXX#LOKDOK99")
  expect_equal(unk$code, "LOKDOK99")
  expect_true(is.na(unk$standard))

  expect_true(all(vapply(split_document_type(NA_character_), is.na, logical(1))))
})

test_that("date parsing fails silently rather than warning", {
  expect_equal(parse_date_safe("2007-09-25"), as.Date("2007-09-25"))
  expect_true(is.na(parse_date_safe(NA_character_)))
  expect_silent(parse_date_safe("not a date"))
})

# ---------------------------------------------------------------------------
# Live API
# ---------------------------------------------------------------------------

test_that("a pre-2008 document returns metadata only", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  d <- dlx_get_doc("lta", 1998, 763)

  expect_s3_class(d, "tbl_df")
  expect_equal(nrow(d), 1L)
  expect_equal(d$status, "metadata_only")
  expect_false(d$has_text)
  expect_equal(d$accn, "B19980076305")
  expect_equal(d$number, "763")
  expect_equal(d$year, 1998L)

  # Lovtidende A mixes acts and regulations in one number space, so this is a
  # Bekendtgoerelse despite sitting in the same series as acts.
  expect_equal(d$document_type, "Bekendtg\u00f8relse")

  # Two Concerns markers, parsed positionally rather than as nested children
  expect_equal(d$n_references, 2L)
})

test_that("a post-2007 document carries full content", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  # The first Lovtidende A document past the text boundary, announced
  # 2007-09-25. See danlex_ROADMAP.md section 4.
  d <- dlx_get_doc("lta", 2007, 1081)

  expect_equal(d$status, "ok")
  expect_true(d$has_text)
  expect_equal(d$dies_edicti, as.Date("2007-09-25"))
  expect_false(is.na(d$title))

  # Modern records use the composite DocumentType form
  expect_false(is.na(d$document_type_code))
})

test_that("the document immediately before the boundary has no text", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  d <- dlx_get_doc("lta", 2007, 1080)
  expect_equal(d$status, "metadata_only")
  expect_equal(d$dies_edicti, as.Date("2007-09-14"))
})

test_that("references can be followed by accession number", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  # No accn -> ELI lookup table is needed: the accession URI resolves directly.
  d <- dlx_get_doc(accn = "A19990059229")

  expect_equal(d$status, "metadata_only")
  expect_true(grepl("arbejdsl", d$title, fixed = TRUE))
  expect_equal(d$document_type, "Lovbekendtg\u00f8relse")

  # Components recovered from the accession number itself
  expect_equal(d$year, 1999L)
})

test_that("a missing document yields not_found rather than an error", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  d <- dlx_get_doc("lta", 1998, 999999)

  expect_equal(nrow(d), 1L)
  expect_equal(d$status, "not_found")
  expect_true(is.na(d$title))
  expect_true(is.na(d$accn))
})

test_that("all three identifier forms reach the same document", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  a <- dlx_get_doc("lta", 1998, 763)
  b <- dlx_get_doc(eli = "https://retsinformation.dk/eli/lta/1998/763")
  d <- dlx_get_doc(accn = "B19980076305")

  expect_equal(a$accn, b$accn)
  expect_equal(a$accn, d$accn)
  expect_equal(a$title, d$title)
})

test_that("every status value produces identically shaped output", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  ok    <- dlx_get_doc("lta", 2007, 1081)
  meta  <- dlx_get_doc("lta", 1998, 763)
  gone  <- dlx_get_doc("lta", 1998, 999999)

  expect_equal(names(ok), names(meta))
  expect_equal(names(ok), names(gone))
  expect_equal(vapply(ok, class, character(1))[["year"]], "integer")
  expect_equal(vapply(ok, class, character(1))[["number"]], "character")

  # Callers row-bind these, so the shapes must agree
  expect_equal(nrow(rbind(ok, meta, gone)), 3L)
})
