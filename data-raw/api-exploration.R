# ---------------------------------------------------------------------------
# danlex — empirical exploration of the Retsinformation ELI API
# ---------------------------------------------------------------------------
#
# This file is a record of how the API was mapped, not part of the package.
# It is excluded from the build via .Rbuildignore.
#
# Every block below was actually run against the live API, and the observed
# results are recorded as comments. Where a conclusion was later revised, the
# revision is noted rather than the original being deleted — the wrong turns
# are as informative as the right ones.
#
# Explored: July 2026
# Author:   Kristian Vepsäläinen
#
# Sources of documentation:
#   https://www.retsinformation.dk/api                       (terms, overview)
#   https://api.retsinformation.dk/                          (harvest service Swagger)
#   https://www.retsinformation.dk/eli/technicaldocumentation (ELI docs)
#   https://www.retsinformation.dk/eli/resource/authority     (NOT YET FETCHED)
#
# ---------------------------------------------------------------------------

library(httr2)
library(xml2)
library(tibble)

UA <- "danlex R package (exploration)"

# ---------------------------------------------------------------------------
# 1. Harvest service (api.retsinformation.dk) — evaluated and REJECTED
# ---------------------------------------------------------------------------
#
# The REST harvest service at https://api.retsinformation.dk exposes exactly
# one endpoint, GET /v1/Documents, which returns documents changed within the
# last 10 days.
#
# Constraints documented in its Swagger:
#   - callable only between 03:00 and 23:45 (time zone unstated, presumably CET)
#   - one call per 10 seconds; exceeding this returns HTTP 429
#   - `date` parameter must fall within the last 10 days
#
# DECISION: not used. The ELI Atom feed (section 3) supersedes it completely —
# 60 days of history instead of 10, more fields, no throttling, no opening
# hours. The only field unique to the harvest service is `documentId`.
#
# NOTE: the rate limit applies ONLY to api.retsinformation.dk. Document
# retrieval from www.retsinformation.dk/eli/... is unthrottled.

# ---------------------------------------------------------------------------
# 2. ELI sitemap — the corpus backbone
# ---------------------------------------------------------------------------

sm <- request("https://www.retsinformation.dk/eli/sitemap.xml") |>
  req_user_agent(UA) |>
  req_perform() |>
  resp_body_xml()

xml_name(xml_root(sm))    # "sitemapindex"  -> NOT a flat urlset
xml_ns(sm)                # d1 <-> http://www.sitemaps.org/schemas/sitemap/0.9
length(xml_children(sm))  # 21

xml_child(sm, 1)
# <loc>https://retsinformation.dk/eli/sitemap.xml?page=1</loc>
#
# NOTE 1: pages are query parameters, not separate files.
# NOTE 2: the index gives <loc> only — no <lastmod>, despite the ELI docs
#         promising it. <lastmod> IS present at page level (see below).
# NOTE 3: canonical URIs omit the "www." prefix even though the site serves
#         both. Use the bare form to avoid a redirect on every request.

# Harvest all 21 pages -------------------------------------------------------

harvest_page <- function(p) {
  x <- request(paste0("https://retsinformation.dk/eli/sitemap.xml?page=", p)) |>
    req_user_agent(UA) |>
    req_throttle(rate = 1 / 2) |>
    req_perform() |>
    resp_body_xml(encoding = "UTF-8")
  ns <- xml_ns(x)
  urls <- xml_find_all(x, "/d1:urlset/d1:url", ns)
  tibble(
    page    = p,
    loc     = xml_text(xml_find_first(urls, "./d1:loc", ns)),
    lastmod = as.Date(xml_text(xml_find_first(urls, "./d1:lastmod", ns)))
  )
}

idx <- do.call(rbind, lapply(1:21, harvest_page))

nrow(idx)  # 202690  (20 pages x 10000, plus 2690 on page 21)

# Page ordering --------------------------------------------------------------
#
# HYPOTHESIS (rejected): pages are ordered by lastmod descending, so a given
# year could be fetched by selecting one or two pages.
#
# Observed lastmod ranges per page:
#   page 1:  2024-03-15 .. 2026-07-29
#   page 2:  2021-12-07 .. 2026-07-28
#   page 3:  2019-12-19 .. 2026-07-16
#   ...
#   page 10: 1988-05-21 .. 2025-11-19
#
# The ranges overlap heavily, so the ordering is NOT a clean lastmod sort.
# Minima decline monotonically with page number, so there is *some*
# correlation, but no rule that permits selective fetching.
#
# CONSEQUENCE: dlx_list_documents() must harvest all 21 pages. A persistent
# cache is therefore mandatory, not optional. ~30 MB, ~40 s at a polite rate.

# ---------------------------------------------------------------------------
# 3. URI structure and the collection taxonomy
# ---------------------------------------------------------------------------

all_locs <- idx$loc

# First attempt (FLAWED — kept as a warning) ---------------------------------
#
# parts <- strcapture("/eli/([^/]+)/(\\d{4})/(\\d+)$", all_locs, ...)
#
# This silently produced 41,448 NAs (20.4% of the corpus) because table()
# drops NAs without comment. Two causes: (a) the `ft` collection uses an
# entirely different identifier scheme, (b) `number` is not always numeric.
#
# LESSON: always reconcile sum(table(x)) against length(x).

# Corrected: number as character, and check the residual -----------------------

parts <- strcapture(
  "/eli/([^/]+)/(\\d{4})/([^/]+)$",
  all_locs,
  proto = data.frame(collection = character(), year = integer(), number = character())
)
parts$loc <- all_locs

sum(is.na(parts$collection))  # 41447 — exactly the `ft` collection

# Collection segment, independent of the number format
seg <- sub("^https?://[^/]+/eli/([^/]+)/.*$", "\\1", all_locs)
table(seg)
#      fob       ft      lta      ltb      ltc       mt retsinfo
#     2956    41447    63179       45     4783     6438    83842

# Non-numeric numbers --------------------------------------------------------

nonnum <- parts[!is.na(parts$collection) & !grepl("^[0-9]+$", parts$number), ]
nonnum
#   collection year number
#          fob 2009    1-1
#
# Exactly ONE case in 202,690. It is enough: `number` must be character.
# This is a permanent regression test case.

# Deep-URI check
all_locs[grepl("^https?://[^/]+/eli/[^/]+/[^/]+/[^/]+/", all_locs)]
# character(0) — no URI has more than three segments after /eli/

# ---------------------------------------------------------------------------
# 4. Collection semantics and temporal coverage
# ---------------------------------------------------------------------------

tab <- table(parts$year, parts$collection)

# Coverage summary (derived from tab):
#
#   code      n        content                            coverage
#   --------- -------  ---------------------------------  -----------------
#   retsinfo  83,842   administrative decisions           1665–  (!)
#   ft        41,447   Folketinget documents              own session scheme
#   lta       63,179   Lovtidende A: acts AND regulations 1852–
#   mt         6,438   Ministerialtidende                 1871–2012 (ceased)
#   ltc        4,783   Lovtidende C: treaties             1936–, collapses 2008
#   fob        2,956   Ombudsman decisions                1980–
#   ltb           45   Lovtidende B                       effectively empty
#
# The corpus reaches back to 1665. Volumes become meaningful around 1920 and
# modern from the 1960s.
#
# `mt` is zero from 2013 onward — Ministerialtidende was discontinued. `fob`
# starts in 1980. `ltc` drops from ~40/year to single digits after 2008.
# These are genuine historical discontinuities, NOT data errors, and must be
# documented so users do not read an empty result as a bug.

# ---------------------------------------------------------------------------
# 5. The `ft` collection — deliberately excluded from v1
# ---------------------------------------------------------------------------

ft_ids <- sub("^.*/eli/ft/", "", all_locs[seg == "ft"])
table(nchar(ft_ids))  # all 12 characters

# Naive fixed-width split: session(5) + type(2) + sequence(5)
ft <- data.frame(
  session = substr(ft_ids, 1, 5),
  type    = substr(ft_ids, 6, 7),
  seq     = suppressWarnings(as.integer(substr(ft_ids, 8, 12)))
)

table(ft$type)
#   2K   2L   3L   4K   4L   4T   5L   5T   6K   6L   7L   8L   BA   BB
# 3390 6327 5829 2415 5724   12  635    3   31   10  466  463 1525 4976
#   CB   DA   DB   EA   EB   FB   SR   SS   XX
#  368  420 1785   47  411   19  127  263 6201

# REGIME CHANGE: type codes switch completely in 1998. Before: 2K, 4K, BA,
# DA, SR, EA. After: 2L, 3L, 4L, XX, BB, CB, DB. `2K` ends at session 19972
# and `2L` begins at 19981.

bad <- ft_ids[is.na(ft$seq)]
length(bad)  # 453
head(bad, 4)
# "202513LB0086" "202513LA0086" "202513LC0064" "202513LB0064"
table(substr(bad, 6, 7))
#  2K  3L  4K
#  31 416   6
#
# A THIRD pattern: session(5) + type(2) + variant letter(1) + sequence(4).
# LA/LB/LC are versions of the same bill (probably "som fremsat" /
# "som vedtaget" / etc.).
#
# Also note anomalous session codes with a single document each: 19531,
# 19742, 19982, 20089, 20102. Probably miskeyed records.
#
# DECISION: `ft` requires at least three parsing rules and is parliamentary
# rather than legislative material. Excluded from v1. Before implementing it
# at all, check whether oda.ft.dk (Folketinget's own OData API) provides the
# same material better structured.

# ---------------------------------------------------------------------------
# 6. ELI Atom update feed
# ---------------------------------------------------------------------------

resp <- request("https://www.retsinformation.dk/eli/eli-update-feed.atom") |>
  req_user_agent(UA) |>
  req_perform()

# ENCODING BUG: the XML declaration claims utf-16 but the bytes are UTF-8.
# Without an explicit override, read_xml() warns and relies on libxml2's
# guess. That guess may differ on another platform, so this is exactly the
# class of bug that passes locally and fails on win-builder. Force UTF-8.
feed <- resp_body_xml(resp, encoding = "UTF-8")

ns <- xml_ns(feed)
print(ns)  # d1 <-> http://www.w3.org/2005/Atom   (one namespace only)

# XPATH TRAP 1: the ELI extension elements (published_in, reasonForChange,
# changeDate, images) are NOT in their own namespace — they inherit Atom's
# default namespace. So "./d1:published_in" matches and "./published_in"
# does not. This is contrary to the ELI specification, and if Civilstyrelsen
# ever fixes it, every extension column silently becomes NA. Guard with a test.

e1 <- xml_find_first(feed, "/d1:feed/d1:entry", ns)
xml_find_first(e1, "./d1:published_in", ns)  # <published_in>
xml_find_first(e1, "./published_in", ns)     # {xml_missing}

# XPATH TRAP 2: "//d1:id" also matches the feed-level <id>, giving one extra
# element. Always anchor at /d1:feed/d1:entry.
entries <- xml_find_all(feed, "/d1:feed/d1:entry", ns)
length(entries)  # 482

feed_tbl <- tibble(
  eli           = xml_text(xml_find_first(entries, "./d1:id", ns)),
  title         = xml_text(xml_find_first(entries, "./d1:title", ns)),
  link          = xml_attr(xml_find_first(entries, "./d1:link", ns), "href"),
  updated       = as.Date(xml_text(xml_find_first(entries, "./d1:updated", ns))),
  published_in  = xml_text(xml_find_first(entries, "./d1:published_in", ns)),
  reason_change = xml_text(xml_find_first(entries, "./d1:reasonForChange", ns)),
  change_date   = as.Date(xml_text(xml_find_first(entries, "./d1:changeDate", ns)))
)

range(feed_tbl$updated)  # 2026-06-01 .. 2026-07-29  (59 days, as promised)

table(feed_tbl$published_in)
#    Lovtidende A    Lovtidende B    Lovtidende C Retsinformation
#             197               1               2             282
#
# `published_in` is the practical filter key, since the URI collection
# segment is not present in the feed.

table(feed_tbl$reason_change)
# DocumentContentChanged: 2
# DocumentMetadataChangedAndDocumentContentChanged: 44
# NewDocument: (majority)
#
# TODO: one entry appeared to have an empty reason_change. Confirm and
# handle as NA.
#
# Enum (from the harvest service schema, assumed shared):
#   Unknown, DocumentMetadataChanged, DocumentContentChanged,
#   DocumentMetadataChangedAndDocumentContentChanged, NewDocument,
#   RemovedDocument

# ---------------------------------------------------------------------------
# 7. Document XML — structure and schema drift
# ---------------------------------------------------------------------------

get_xml <- function(loc) {
  request(paste0(loc, "/xml")) |>
    req_user_agent(UA) |>
    req_throttle(rate = 1 / 2) |>
    req_perform() |>
    resp_body_xml(encoding = "UTF-8")
}

d_old <- get_xml("https://retsinformation.dk/eli/lta/1998/763")
d_new <- get_xml("https://retsinformation.dk/eli/lta/2025/...")  # any 2025 doc

xml_ns(d_old)  # empty — NO namespace. XPath needs no prefixes here, unlike
               # the Atom feed. Worth a comment in the code; the difference
               # between the two is otherwise confusing.

xml_name(xml_children(d_old))
# [1] "Meta"                      <- metadata ONLY

xml_name(xml_children(d_new))
# [1] "Meta" "TitelGruppe" "DokumentIndhold" "UnderskriftGruppe" "Bilag"

# Meta fields, 1998 document (32 elements):
#  DocumentType, Rank, AccessionNumber, DocumentId, UniqueDocumentId,
#  DocumentTitle, Year, DiesSigni, DateOfSubmit, StartDate, EndDate, Status,
#  Number, AnnouncedIn, DiesEdicti, DateOfHistoricMark,
#  Concerns, Ref_Accn, Ref_Af, Ref_Text,      <- reference block 1
#  Concerns, Ref_Accn, Ref_Af, Ref_Text,      <- reference block 2
#  Republished, Sign, Signature, Sub-Sign, Signature, PlaceOfSignature,
#  JournalNumber, Ministry
#
# Meta fields, 2007 document (25 elements): as above minus the 8 reference
# elements, plus AdministrativeAuthority.
#
# NOTE: element names are English and Latin (DiesSigni = signing date,
# DiesEdicti = publication date) while content elements are Danish
# (TitelGruppe, DokumentIndhold, Bilag). The source is bilingual; column
# names follow the source verbatim in snake_case and the inconsistency is
# documented rather than smoothed over.

# ---------------------------------------------------------------------------
# 8. Determinism — the decisive test
# ---------------------------------------------------------------------------

xml_text(xml_find_first(d_old, "//Number"))           # "763"
xml_text(xml_find_first(d_old, "//DocumentTitle"))    # "Bekendtgørelse om ..."
xml_text(xml_find_first(d_old, "//DocumentType"))     # "Bekendtgørelse"
xml_text(xml_find_first(d_old, "//AccessionNumber"))  # "B19980076305"
#
# CONFIRMED: the URI number IS the official act number. dlx_get_doc() can be
# built on (collection, year, number), the same pattern as finlex and swelex.
#
# CAVEAT: /eli/lta/1998/763 is a Bekendtgørelse, not a Lov. Lovtidende A
# carries both acts and regulations in ONE number space. Users cannot infer
# the instrument type from the URI, so document_type must always be returned.

# ---------------------------------------------------------------------------
# 9. Schema drift in DocumentType
# ---------------------------------------------------------------------------

xml_text(xml_find_first(d_old, "//DocumentType"))  # "Bekendtgørelse"
# 2007 document:                                   # "BEK Æ#LOKDOK05"
#
# HYPOTHESIS (rejected): xml_text() had concatenated two sibling elements.
# length(xml_find_all(d3, "//DocumentType")) == 1 and xml_structure() shows a
# single text node, so the "#" is genuinely part of the value.
#
# It is a composite: {display abbreviation}#{internal code}.
#
# Confirmed mapping (from a 39-document sample):
#   LOV Æ  / LOKDOK02  amending act
#   LBK H  / LOKDOK03  consolidated act
#   BEK H  / LOKDOK04  regulation
#   BEK Æ  / LOKDOK05  amending regulation
#
# Æ = ændring (amendment), H = hoveddokument (presumed).
# The mapping is INCOMPLETE — more codes certainly exist in ltc/mt/retsinfo.
#
# CONSEQUENCE: pre-2008 documents use whole Danish words, post-2008 use short
# codes. danlex must split this into document_type / document_type_code and
# normalise across the boundary, otherwise the same instrument type appears
# two different ways depending on the year.

# ---------------------------------------------------------------------------
# 10. Accession number decoded
# ---------------------------------------------------------------------------
#
# Format: {letter}{year:4}{number:5, zero-padded}{suffix:2}
#
#   B19980076305  B 1998 00763 05  Bekendtgørelse
#   B20070130205  B 2007 01302 05  BEK Æ
#   A19990059229  A 1999 00592 29  Lovbekendtgørelse
#
# The embedded number matches the URI number zero-padded to five digits.

meta_tbl <- ...  # 39 documents with text, fields accn + type
# table(letter, type):
#          BEK H  BEK Æ  LBK H  LOV Æ
#   A          0      0      1      7
#   B         24      7      0      0
#
# letter = normative rank. A = statute level (LOV, LBK), B = regulation
# level (BEK). No crossover in 39 observations.
#
# table(suffix, type):
#          BEK H  BEK Æ  LBK H  LOV Æ
#   05        24      7      0      0
#   29         0      0      1      0
#   30         0      0      0      7
#
# suffix = coarse instrument class. NOTE that 05 covers BOTH BEK H and BEK Æ,
# so suffix is NOT the harvest service's documentType.id — that hypothesis
# is rejected.
#
# CAUTION: the Swagger example C20210900609 / "LBK H" / "id": 0 does not fit
# this pattern (C is not observed for LBK). "id": 0 is an obvious placeholder,
# so the whole example is probably synthetic. It already caused one wrong
# inference here. Do not treat it as evidence.
#
# STILL OPEN: what the letter C means (likely tied to the ltc collection).
# Resolve via https://www.retsinformation.dk/eli/resource/authority — NOT YET
# FETCHED, and the last documented source not yet consulted.

# ---------------------------------------------------------------------------
# 11. Text coverage boundary
# ---------------------------------------------------------------------------

has_text <- function(loc) {
  r <- request(paste0(loc, "/xml")) |>
    req_user_agent(UA) |>
    req_throttle(rate = 1 / 2) |>
    req_error(is_error = function(resp) resp_status(resp) >= 500) |>
    req_perform()
  if (resp_status(r) == 404) return(NA)
  x <- resp_body_xml(r, encoding = "UTF-8")
  "DokumentIndhold" %in% xml_name(xml_children(x))
}

# Two lta documents per year, 1950 onward: text is absent for EVERY year
# 1950–2006, present for EVERY year 2008–2026, and mixed in 2007. A sharp
# boundary — no per-document has_text index is needed, only a documented cut-off.

# Binary search within 2007 (assumes monotonicity in number; supported by the
# 25-point sample but NOT proven):
#   1080  2007-09-14  Bekendtgørelse om tilskud til fremme af innovation ...   no text
#   1081  2007-09-25  Bekendtgørelse om udgang til indsatte ...                text
#
# BOUNDARY: lta/2007/1081, announced 2007-09-25.
#
# The schema change (section 9) coincides with this boundary.
#
# TODO: verify the same boundary for ltc and mt. mt ceased in 2012, so if the
# boundary is later than that, all of Ministerialtidende is metadata-only.

# ---------------------------------------------------------------------------
# 12. References (Concerns) — positional, not nested
# ---------------------------------------------------------------------------
#
# TRAP: Concerns, Ref_Accn, Ref_Af and Ref_Text are all SIBLINGS under Meta.
# Ref_* are NOT children of Concerns. Concerns is an empty marker element
# carrying only an id attribute. So
#   xml_children(xml_find_first(d, "//Concerns"))
# returns nothing, which reads as "no reference data" when there is plenty.
#
# Grouping requires splitting Meta's children at each Concerns marker.

kids  <- xml_children(xml_child(d_old, 1))
nm    <- xml_name(kids)
marks <- which(nm == "Concerns")

refs <- lapply(seq_along(marks), function(i) {
  from  <- marks[i] + 1
  to    <- if (i < length(marks)) marks[i + 1] - 1 else length(nm)
  block <- kids[from:to]
  block <- block[xml_name(block) %in% c("Ref_Accn", "Ref_Af", "Ref_Text")]
  setNames(xml_text(block), xml_name(block))
})

# [[1]] Ref_Accn "A19990059229"
#       Ref_Af   "1999-07-14"
#       Ref_Text "Bekendtgørelse af lov om arbejdsløshedsforsikring m.v."
# [[2]] Ref_Accn "A19990066629"
#       Ref_Af   "1999-08-19"
#       Ref_Text "Bekendtgørelse af lov om en aktiv arbejdsmarkedspolitik"
#
# The same positional pattern applies to Sign -> Signature and
# Sub-Sign -> Signature. Note that a bare //Signature conflates the signatory
# and the counter-signatory.
#
# SEMANTIC FINDING (important): the 1998 document references acts dated 1999,
# i.e. the references point FORWARD in time. These are not historical
# citations frozen at enactment; they are MAINTAINED links to the current
# consolidated version of the parent act. Anyone building a citation network
# and assuming the edges are timestamped to the source document's date will
# get a systematically wrong picture.
#
# For lexverse: this is concrete evidence that lex_citations needs a field
# distinguishing maintained from historical references. EUR-Lex CELEX
# references behave differently. A standard designed on one jurisdiction
# would miss this.
#
# Ref_Af is Danish "af" = "of": the date of the referenced act, as in
# "lov nr. 405 af 14. juli 1999". Naming the column ref_af would be opaque,
# so this is the one place where danlex deviates from source naming:
# ref_accn / ref_date / ref_title. Document the deviation.

# Availability across the text boundary --------------------------------------
#
# with(chk, table(text, nrefs > 0)):
#          FALSE TRUE
#   FALSE     66   49     <- metadata-only documents
#   TRUE      16   23     <- documents with text
#
# References are present in BOTH eras. Text and references are not mutually
# exclusive, so dlx_get_references() works across the whole corpus. Roughly
# 59% of modern and 43% of older documents carry at least one reference.

# Following a reference ------------------------------------------------------

ref <- get_xml("https://retsinformation.dk/eli/accn/A19990059229")
xml_text(xml_find_first(ref, "//DocumentTitle"))
#  "Bekendtgørelse af lov om arbejdsløshedsforsikring m.v."
xml_text(xml_find_first(ref, "//DocumentType"))
#  "Lovbekendtgørelse"
#
# CONFIRMED: /eli/accn/{accn}/xml resolves directly. No accn -> ELI lookup
# table is required, so the citation network can be traversed without first
# harvesting the whole corpus. (An earlier conclusion in the design notes
# claimed the opposite; it was wrong.)

# ---------------------------------------------------------------------------
# 13. Error handling
# ---------------------------------------------------------------------------

r <- request("https://retsinformation.dk/eli/lta/1998/999999/xml") |>
  req_user_agent(UA) |>
  req_error(is_error = function(resp) FALSE) |>
  req_perform()

resp_status(r)        # 404
resp_content_type(r)  # "text/plain"
substr(resp_body_string(r), 1, 300)
#  "No document found matching ELI request"
#
# Clean 404 with an explicit plain-text message. This is the best possible
# outcome: no need to detect emptiness from a 200 response body.

# ---------------------------------------------------------------------------
# 14. Summary of decisions
# ---------------------------------------------------------------------------
#
# v1 scope:    lta, ltb, ltc, mt  (74,445 documents)
# v2:          retsinfo, fob      (index-based lookup; internal numbering)
# not planned: ft                 (three parsing regimes; check oda.ft.dk)
#
# Base URI:    https://retsinformation.dk  (no www — matches canonical form)
# Identifiers: (collection, year, number), accn, or full ELI URI
# number:      CHARACTER, not integer   (fob/2009/1-1)
# Namespace:   none in document XML; Atom feed needs the d1 prefix
# Encoding:    force UTF-8 on the Atom feed (declaration is wrong); assumed
#              safe on document XML but not independently verified
# Errors:      404 -> status "not_found"; >= 500 -> abort with a condition class
# Throttling:  none needed for /eli/ requests; required only for the
#              harvest service, which is not used
