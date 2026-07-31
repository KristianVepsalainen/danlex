# Tests for dlx_get_changes().
#
# Parsing and filtering are separated from the HTTP call so both can be tested
# against a fixture. The synthetic feed below reproduces the two traps in the
# source: extension elements that inherit Atom's default namespace, and a
# feed-level <id> that a bare //d1:id would pick up alongside the entries.

synthetic_feed <- function() {
  xml2::read_xml(paste0(
    "<feed xmlns='http://www.w3.org/2005/Atom'>",
    "<id>urn:retsinformation-dk:eli:eli-update-feed</id>",
    "<title>Update feed</title>",
    "<updated>2026-07-29</updated>",
    "<link href='https://retsinformation.dk/eli/eli-update-feed.atom'/>",

    "<entry>",
    "<id>https://retsinformation.dk/eli/lta/2026/500</id>",
    "<title>Bekendtg\u00f8relse om noget</title>",
    "<updated>2026-07-29</updated>",
    "<link href='https://retsinformation.dk/eli/lta/2026/500'/>",
    "<published_in>Lovtidende A</published_in>",
    "<reasonForChange>NewDocument</reasonForChange>",
    "<changeDate>2026-07-29</changeDate>",
    "<images>",
    "<image><source>https://example.org/a.png</source><altText>A</altText></image>",
    "<image><source>https://example.org/b.png</source><altText>B</altText></image>",
    "</images>",
    "</entry>",

    "<entry>",
    "<id>https://retsinformation.dk/eli/retsinfo/2026/9737</id>",
    "<title>Afg\u00f8relse om noget andet</title>",
    "<updated>2026-06-15</updated>",
    "<link href='https://retsinformation.dk/eli/retsinfo/2026/9737'/>",
    "<published_in>Retsinformation</published_in>",
    "<reasonForChange>DocumentContentChanged</reasonForChange>",
    "<changeDate>2026-06-15</changeDate>",
    "<images/>",
    "</entry>",

    "<entry>",
    "<id>https://retsinformation.dk/eli/lta/2026/300</id>",
    "<title>Uden \u00e5rsag</title>",
    "<updated>2026-06-01</updated>",
    "<link href='https://retsinformation.dk/eli/lta/2026/300'/>",
    "<published_in>Lovtidende A</published_in>",
    "<reasonForChange></reasonForChange>",
    "<changeDate>2026-06-01</changeDate>",
    "</entry>",

    "</feed>"
  ))
}

# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

test_that("the feed-level id is not mistaken for an entry", {
  # A bare //d1:id matches four elements here — the feed's own identifier plus
  # three entries — which would shift every column by one.
  ch <- parse_change_feed(synthetic_feed())
  expect_equal(nrow(ch), 3L)
  expect_false(any(grepl("^urn:", ch$eli)))
})

test_that("extension elements inherit the Atom namespace", {
  # published_in, reasonForChange, changeDate and images carry no namespace of
  # their own, contrary to the ELI specification. If that is ever corrected
  # upstream, these columns become NA without any error being raised.
  ch <- parse_change_feed(synthetic_feed())
  expect_equal(ch$published_in[1], "Lovtidende A")
  expect_equal(ch$reason_for_change[1], "NewDocument")
  expect_equal(ch$change_date[1], as.Date("2026-07-29"))
})

test_that("collection is derived from the ELI identifier", {
  # The feed reports only published_in, which does not separate legislation
  # from administrative decisions. The identifier does.
  ch <- parse_change_feed(synthetic_feed())
  expect_equal(ch$collection, c("lta", "retsinfo", "lta"))
  expect_equal(ch$year, rep(2026L, 3))
  expect_equal(ch$number, c("500", "9737", "300"))
})

test_that("images are counted, including when absent or empty", {
  ch <- parse_change_feed(synthetic_feed())
  expect_equal(ch$n_images, c(2L, 0L, 0L))
  expect_type(ch$n_images, "integer")
})

test_that("an empty reasonForChange becomes NA", {
  ch <- parse_change_feed(synthetic_feed())
  expect_true(is.na(ch$reason_for_change[3]))
})

test_that("an empty feed yields the standard zero-row shape", {
  empty <- xml2::read_xml(
    "<feed xmlns='http://www.w3.org/2005/Atom'><id>x</id></feed>"
  )
  ch <- parse_change_feed(empty)
  expect_equal(nrow(ch), 0L)
  expect_equal(names(ch), names(dlx_empty_changes()))
})

test_that("timestamps and plain dates both parse", {
  expect_equal(parse_date_vec("2026-07-29"), as.Date("2026-07-29"))
  expect_equal(parse_date_vec("2026-07-29T13:45:00Z"), as.Date("2026-07-29"))
  expect_true(is.na(parse_date_vec(NA_character_)))
  expect_silent(parse_date_vec("rubbish"))
})

# ---------------------------------------------------------------------------
# Filtering
# ---------------------------------------------------------------------------

test_that("since filters on the update date", {
  ch <- parse_change_feed(synthetic_feed())
  out <- filter_changes(ch, since = as.Date("2026-06-10"))
  expect_equal(nrow(out), 2L)
  expect_true(all(out$updated >= as.Date("2026-06-10")))
})

test_that("since warns when it predates the feed window", {
  # The feed is a static file with no server-side query, so an early `since`
  # silently truncates unless the user is told.
  ch <- parse_change_feed(synthetic_feed())
  expect_warning(
    filter_changes(ch, since = as.Date("2026-01-01")),
    "reaches back only to"
  )
})

test_that("since accepts anything as.Date accepts", {
  ch <- parse_change_feed(synthetic_feed())
  expect_equal(
    nrow(filter_changes(ch, since = "2026-06-10")),
    nrow(filter_changes(ch, since = as.Date("2026-06-10")))
  )
})

test_that("invalid since values are rejected", {
  ch <- parse_change_feed(synthetic_feed())
  expect_error(filter_changes(ch, since = c("2026-01-01", "2026-02-01")),
               class = "danlex_bad_argument")
  expect_error(filter_changes(ch, since = NA), class = "danlex_bad_argument")
})

test_that("collection and reason filter independently and together", {
  ch <- parse_change_feed(synthetic_feed())

  expect_equal(nrow(filter_changes(ch, collection = "lta")), 2L)
  expect_equal(nrow(filter_changes(ch, collection = c("lta", "retsinfo"))), 3L)
  expect_equal(nrow(filter_changes(ch, reason = "NewDocument")), 1L)
  expect_equal(
    nrow(filter_changes(ch, collection = "lta", reason = "NewDocument")),
    1L
  )
})

test_that("unrecognised filter values warn rather than fail silently", {
  ch <- parse_change_feed(synthetic_feed())
  expect_warning(filter_changes(ch, collection = "nosuch"), "Unknown collection")
  expect_warning(filter_changes(ch, reason = "Whatever"), "Unrecognised change")
})

test_that("filtering to nothing preserves the column shape", {
  ch  <- parse_change_feed(synthetic_feed())
  out <- filter_changes(ch, reason = "RemovedDocument")
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), names(dlx_empty_changes()))
})

# ---------------------------------------------------------------------------
# Live
# ---------------------------------------------------------------------------

test_that("the live feed parses into the expected shape", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  ch <- dlx_get_changes()

  expect_s3_class(ch, "tbl_df")
  expect_gt(nrow(ch), 0L)
  expect_equal(names(ch), names(dlx_empty_changes()))
  expect_true(all(grepl("^https://", ch$eli)))
})

test_that("the feed holds at least the advertised 60 days", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  ch   <- dlx_get_changes()
  span <- as.integer(max(ch$updated, na.rm = TRUE) -
                       min(ch$updated, na.rm = TRUE))

  # Documented as "at least 60 days of history". Allow a little slack for
  # boundary effects; a large shortfall means the guarantee has changed.
  expect_gt(span, 45L)
})

test_that("Danish characters survive the feed's incorrect encoding declaration", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  # The feed declares utf-16 while emitting UTF-8. Without an explicit
  # override the result depends on libxml2's guess, which is exactly the kind
  # of defect that passes locally and fails on another platform.
  ch <- dlx_get_changes()
  expect_true(any(grepl("[\u00e6\u00f8\u00e5]", ch$title)))
})

test_that("live reason codes stay within the documented enumeration", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  ch      <- dlx_get_changes()
  present <- unique(stats::na.omit(ch$reason_for_change))
  expect_true(all(present %in% DLX_CHANGE_REASONS))
})

test_that("live collection codes stay within the observed set", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  ch      <- dlx_get_changes()
  present <- unique(stats::na.omit(ch$collection))
  expect_true(all(present %in% DLX_COLLECTIONS_ALL))
})

test_that("the feed's link duplicates its id", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  # danlex omits `link` on the assumption that it never differs from `id`.
  # This test exists to discover if that assumption ever breaks; if it fails,
  # add a `link` column rather than deleting the test.
  feed <- perform_eli_request(DLX_FEED_URL)$body
  entries <- xml2::xml_find_all(feed, "/d1:feed/d1:entry", DLX_ATOM_NS)
  ids   <- xml2::xml_text(xml2::xml_find_first(entries, "./d1:id", DLX_ATOM_NS))
  links <- xml2::xml_attr(
    xml2::xml_find_first(entries, "./d1:link", DLX_ATOM_NS), "href"
  )
  expect_equal(ids, links)
})

test_that("a change can be fetched in full", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  ch <- dlx_get_changes(collection = "lta")
  skip_if(nrow(ch) == 0L, "No Lovtidende A changes in the current window")

  d <- dlx_get_doc(eli = ch$eli[1])
  expect_true(d$status %in% c("ok", "metadata_only"))
  expect_equal(d$collection, "lta")
})
