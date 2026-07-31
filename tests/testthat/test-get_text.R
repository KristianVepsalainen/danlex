# Tests for dlx_get_references(), dlx_get_text() and dlx_get_paragraphs().
#
# The offline block uses a synthetic document that reproduces the two
# structural traps in the source format: Concerns markers whose Ref_* fields
# are siblings rather than children, and Char runs that must be concatenated
# without a separator. These run everywhere, including on CRAN.
#
# Live tests assert values confirmed during the API mapping; see
# data-raw/api-exploration.R.

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

synthetic_doc <- function() {
  xml2::read_xml(paste0(
    "<Dokument>",
    "<Meta>",
    "<DocumentType>BEK H#LOKDOK 04</DocumentType>",
    "<AccessionNumber>B20000000105</AccessionNumber>",
    "<DocumentTitle>Test</DocumentTitle>",
    "<Concerns id='c1'/>",
    "<Ref_Accn>A11111111111</Ref_Accn>",
    "<Ref_Af>2020-01-01</Ref_Af>",
    "<Ref_Text>First parent act</Ref_Text>",
    "<Concerns id='c2'/>",
    "<Ref_Accn>A22222222222</Ref_Accn>",
    "<Ref_Af>not-a-date</Ref_Af>",
    "<Ref_Text>Second parent act</Ref_Text>",
    "</Meta>",
    "<DokumentIndhold>",
    "<Paragraf id='p1' localId='1'>",
    "<Explicatus>\u00a7 1.</Explicatus>",
    "<Stk id='s1'><Exitus><Linea>",
    "<Char>First subsection.</Char>",
    "</Linea></Exitus></Stk>",
    "<Stk id='s2'>",
    "<Explicatus>Stk. 2.</Explicatus>",
    "<Exitus><Linea>",
    "<Char>Second </Char><Char formaChar='i'>subsection</Char><Char>.</Char>",
    "</Linea></Exitus></Stk>",
    "</Paragraf>",
    "</DokumentIndhold>",
    "</Dokument>"
  ))
}

# ---------------------------------------------------------------------------
# Offline: reference block parsing
# ---------------------------------------------------------------------------

test_that("Concerns markers are grouped positionally, not by nesting", {
  meta   <- xml2::xml_find_first(synthetic_doc(), "//Meta")
  blocks <- parse_marker_blocks(meta)

  # Two markers, each followed by three sibling Ref_* elements.
  expect_length(blocks, 2L)
  expect_equal(unname(blocks[[1]][["Ref_Accn"]]), "A11111111111")
  expect_equal(unname(blocks[[2]][["Ref_Text"]]), "Second parent act")

  # The trap this guards against: Ref_* are siblings of Concerns, so asking
  # for its children finds nothing at all.
  concerns <- xml2::xml_find_first(meta, "./Concerns")
  expect_length(xml2::xml_children(concerns), 0L)
})

test_that("a document with no Concerns markers yields no blocks", {
  doc <- xml2::read_xml("<Dokument><Meta><Number>1</Number></Meta></Dokument>")
  expect_length(parse_marker_blocks(xml2::xml_find_first(doc, "//Meta")), 0L)
})

test_that("DocumentType codes tolerate internal whitespace", {
  # Observed in the wild: "LOV H#LOKDOK 01" with a space, while most records
  # have none. Without stripping it the lookup silently misses.
  expect_equal(split_document_type("BEK H#LOKDOK 04")$code, "LOKDOK04")
  expect_equal(
    split_document_type("BEK H#LOKDOK 04")$standard,
    split_document_type("BEK H#LOKDOK04")$standard
  )
})

# ---------------------------------------------------------------------------
# Offline: text rendering
# ---------------------------------------------------------------------------

test_that("Char runs are joined without separators", {
  # Char elements are inline formatting spans, not words. Joining them with a
  # space would corrupt every emphasised phrase in the corpus.
  stk <- xml2::xml_find_first(synthetic_doc(), "//Stk[@id='s2']")
  expect_equal(render_lines(stk), "Second subsection.")
})

test_that("render_lines drops structural markers", {
  # Markers are returned as their own columns, so repeating them in `text`
  # would be redundant.
  stk <- xml2::xml_find_first(synthetic_doc(), "//Stk[@id='s2']")
  expect_false(grepl("Stk. 2.", render_lines(stk), fixed = TRUE))
})

test_that("render_content separates markers from the text that follows", {
  content <- xml2::xml_find_first(synthetic_doc(), "//DokumentIndhold")
  txt     <- render_content(content)

  # The regression this guards: a naive xml_text() produces
  # "\u00a7 1.First subsection." with no separation at all.
  expect_true(grepl("^\u00a7 1\\. First subsection\\.", txt))
  expect_true(grepl("Stk\\. 2\\. Second subsection\\.", txt))

  # A new block opens with a blank line
  expect_true(grepl("\n\nStk. 2.", txt, fixed = TRUE))
})

test_that("render_content handles empty content", {
  doc <- xml2::read_xml("<Dokument><DokumentIndhold/></Dokument>")
  expect_true(is.na(
    render_content(xml2::xml_find_first(doc, "//DokumentIndhold"))
  ))
})

# ---------------------------------------------------------------------------
# Offline: amendment detection
# ---------------------------------------------------------------------------

test_that("amendment detection tests the ancestor axis, not the document type", {
  # Instrument type does not predict structure: ANG I is labelled differently
  # from BEK AE but is structurally an amending instrument. Only the ancestor
  # test is reliable.
  own <- xml2::read_xml(
    "<DokumentIndhold><Paragraf id='p'/></DokumentIndhold>"
  )
  amd <- xml2::read_xml(paste0(
    "<DokumentIndhold><AendringsNummer><Aendring><AendringAktion>",
    "<AendringNyTekst><Paragraf id='p'/></AendringNyTekst>",
    "</AendringAktion></Aendring></AendringsNummer></DokumentIndhold>"
  ))

  expect_false(is_amendment_node(xml2::xml_find_first(own, "//Paragraf")))
  expect_true(is_amendment_node(xml2::xml_find_first(amd, "//Paragraf")))
})

# ---------------------------------------------------------------------------
# Offline: empty-result shapes
# ---------------------------------------------------------------------------

test_that("empty results have the same shape as populated ones", {
  # Callers row-bind these across many documents, so the columns must agree.
  expect_equal(nrow(dlx_empty_references()), 0L)
  expect_equal(nrow(dlx_empty_paragraphs()), 0L)
  expect_type(dlx_empty_references()$ref_index, "integer")
  expect_s3_class(dlx_empty_references()$ref_date, "Date")
  expect_type(dlx_empty_paragraphs()$is_amendment, "logical")
})

# ---------------------------------------------------------------------------
# Live: references
# ---------------------------------------------------------------------------

test_that("references are returned as an edge table", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  refs <- dlx_get_references("lta", 1998, 763)

  expect_equal(nrow(refs), 2L)
  expect_equal(refs$from_accn[1], "B19980076305")
  expect_equal(refs$ref_accn, c("A19990059229", "A19990066629"))
  expect_equal(refs$ref_date, as.Date(c("1999-07-14", "1999-08-19")))
  expect_equal(refs$ref_index, 1:2)
  expect_true(all(grepl("^Bekendtg", refs$ref_title)))
})

test_that("references point forward in time, not backward", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  # These edges are MAINTAINED, not historical: they track the current
  # consolidated parent. A 1998 instrument therefore cites 1999 acts. If this
  # ever stops holding, the semantics of the citation network have changed and
  # the documentation needs revisiting.
  refs <- dlx_get_references("lta", 1998, 763)
  expect_true(all(refs$ref_date > as.Date("1998-12-31")))
})

test_that("references can be followed by accession number", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  refs <- dlx_get_references("lta", 1998, 763)
  parent <- dlx_get_doc(accn = refs$ref_accn[1])

  expect_equal(parent$status, "metadata_only")
  expect_equal(parent$document_type, "Lovbekendtg\u00f8relse")
})

test_that("a missing document yields zero references with a warning", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  expect_warning(
    refs <- dlx_get_references("lta", 1998, 999999),
    "No document found"
  )
  expect_equal(nrow(refs), 0L)
  expect_equal(names(refs), names(dlx_empty_references()))
})

# ---------------------------------------------------------------------------
# Live: text
# ---------------------------------------------------------------------------

test_that("full text is returned for post-2007 documents", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  txt <- dlx_get_text("lta", 2025, 1)

  expect_equal(nrow(txt), 1L)
  expect_equal(txt$status, "ok")
  expect_gt(txt$n_char, 1000L)

  # Regression test for the marker-collision bug: a naive xml_text() produced
  # "\u00a7 1I bekendtg..." with no space between the marker and the text.
  expect_true(grepl("^\u00a7 1 I bekendtg", txt$text))
  expect_false(grepl("^\u00a7 1I", txt$text))
})

test_that("pre-2008 documents report metadata_only", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  txt <- dlx_get_text("lta", 1998, 763)

  expect_equal(txt$status, "metadata_only")
  expect_true(is.na(txt$text))
  expect_true(is.na(txt$n_char))
})

test_that("dlx_get_text agrees with dlx_get_doc on availability", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  for (n in c(1080, 1081)) {
    d <- dlx_get_doc("lta", 2007, n)
    t <- dlx_get_text("lta", 2007, n)
    expect_equal(d$has_text, t$status == "ok")
  }
})

# ---------------------------------------------------------------------------
# Live: provisions
# ---------------------------------------------------------------------------

test_that("a consolidated regulation yields its own provisions", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  p <- dlx_get_paragraphs("lta", 2025, 50)

  expect_equal(nrow(p), 25L)
  expect_true(all(!p$is_amendment))
  expect_equal(p$paragraf[1], "\u00a7 1.")
  expect_equal(p$paragraf_index[1], 1L)

  # The first subsection carries no marker: "Stk. 1." is implicit in Danish
  # drafting practice and is simply absent from the source.
  expect_true(is.na(p$stk[1]))
  expect_equal(p$stk[2], "Stk. 2.")

  expect_true(all(!is.na(p$text)))
})

test_that("an amending instrument yields provisions belonging elsewhere", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  # Every Paragraf sits under AendringNyTekst: this is new text destined for
  # the 2018 regulation, not structure of its own.
  p <- dlx_get_paragraphs("lta", 2025, 1)

  expect_gt(nrow(p), 0L)
  expect_true(all(p$is_amendment))
})

test_that("provision indices are well formed", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  p <- dlx_get_paragraphs("lta", 2025, 50)

  # paragraf_index increases monotonically and never repeats out of order
  expect_false(is.unsorted(p$paragraf_index))

  # stk_index restarts at 1 within each section
  first <- tapply(p$stk_index, p$paragraf_index, min)
  expect_true(all(first == 1L))
})

test_that("metadata-only documents yield zero provisions", {
  skip_on_cran()
  skip_if_offline("retsinformation.dk")

  p <- dlx_get_paragraphs("lta", 1998, 763)
  expect_equal(nrow(p), 0L)
  expect_equal(names(p), names(dlx_empty_paragraphs()))
})
