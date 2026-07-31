# danlex — roadmap

A living design document. Not part of the build (see `.Rbuildignore`).

`danlex` provides access to Danish legislation via the Retsinformation ELI
service. It is part of **lexverse**, alongside `eurlex`, `finlex` and `swelex`.

- Function prefix: `dlx_`
- Base URI: `https://retsinformation.dk` (canonical form, no `www.`)
- Licence terms: <https://www.retsinformation.dk/api>

---

## 1. What the API actually is

Three independent sources, three roles:

| Source | Role | Notes |
|---|---|---|
| `/eli/sitemap.xml` | corpus backbone | sitemapindex, 21 pages, 202,690 URIs, `lastmod` at page level; refreshed monthly |
| `/eli/eli-update-feed.atom` | change feed | ≥60 days of history, daily, no throttling |
| `/eli/{collection}/{year}/{number}/xml` | document retrieval | also `/eli/accn/{accn}/xml` |

The REST harvest service at `api.retsinformation.dk` is **not used**: it
offers only 10 days of history, one call per 10 seconds, and is unavailable
between 23:45 and 03:00. The Atom feed supersedes it on every axis.

---

## 2. Scope

### v1 — legislation

`lta`, `ltb`, `ltc`, `mt` — **74,445 documents**. One uniform identifier
scheme, no special cases.

### v2 — administrative decisions

`retsinfo` (83,842) and `fob` (2,956). Both use internal numbering that users
cannot construct, so these require the cached corpus index first.

### Not planned

`ft` (41,447), Folketinget documents. At least three parsing regimes:

```
202522L00009   session(5) + type(2) + seq(5)          post-1998
19972...       different type codes entirely           pre-1998
202513LB0086   session(5) + type(2) + variant(1) + seq(4)   453 cases
```

Before implementing this at all, check whether `oda.ft.dk` (Folketinget's own
OData API) provides the same material better structured.

---

## 3. Collections and coverage

| Code | n | Content | Coverage |
|---|---|---|---|
| `lta` | 63,179 | Lovtidende A — acts **and** regulations | 1852– |
| `ltb` | 45 | Lovtidende B | effectively empty |
| `ltc` | 4,783 | Lovtidende C — treaties | 1936–, collapses after 2008 |
| `mt` | 6,438 | Ministerialtidende | 1871–**2012** (discontinued) |
| `retsinfo` | 83,842 | administrative decisions | 1665– |
| `fob` | 2,956 | Ombudsman decisions | 1980– |
| `ft` | 41,447 | Folketinget documents | own session scheme |

The `mt` gap after 2012, the `fob` start in 1980 and the `ltc` collapse after
2008 are genuine historical discontinuities. They must be documented in the
README so an empty result is not read as a bug.

---

## 4. Confirmed findings

**Determinism.** `/eli/lta/1998/763` → `<Number>763</Number>`. The URI number
is the official act number, so `dlx_get_doc(collection, year, number)` works
exactly as in `finlex` and `swelex`.

**`number` is character, not integer.** `/eli/fob/2009/1-1` is the sole
non-numeric case in 202,690 URIs. Permanent regression test.

**Lovtidende A mixes instrument types.** `lta/1998/763` is a *Bekendtgørelse*,
not a *Lov*. Acts and regulations share one number space, so
`document_type` must always be returned.

**Text coverage boundary: `lta/2007/1081`, announced 2007-09-25.** Documents
before it contain `<Meta>` only; documents after also contain `TitelGruppe`,
`DokumentIndhold`, `UnderskriftGruppe` and `Bilag`. The boundary is sharp, so
no per-document `has_text` index is needed. *Caveat: located by binary search
assuming monotonicity in number — supported by a 25-point sample, not proven.*

**Schema drift at the same boundary.** `DocumentType` is a whole Danish word
before 2008 (`"Bekendtgørelse"`) and a composite after
(`"BEK Æ#LOKDOK05"`). Confirmed codes:

| Display | Code | Meaning |
|---|---|---|
| `LOV Æ` | `LOKDOK02` | amending act |
| `LBK H` | `LOKDOK03` | consolidated act |
| `BEK H` | `LOKDOK04` | regulation |
| `BEK Æ` | `LOKDOK05` | amending regulation |

Incomplete — more codes certainly exist in `ltc`, `mt` and `retsinfo`.

**Accession number:** `{letter}{year:4}{number:5 zero-padded}{suffix:2}`.
Letter = normative rank (A = statute, B = regulation). Suffix = coarse
instrument class (05 = Bekendtgørelse, 29 = Lovbekendtgørelse, 30 = Lov).
Suffix is *not* the harvest service's `documentType.id` — 05 covers two
distinct codes.

**References are positional, not nested.** `Concerns` is an empty marker
element; `Ref_Accn` / `Ref_Af` / `Ref_Text` are its *siblings*. Grouping
requires splitting `Meta`'s children at each marker. The same applies to
`Sign` → `Signature` and `Sub-Sign` → `Signature`.

**References are maintained, not historical.** A 1998 regulation references
acts dated 1999 — the links point to the *current* consolidated parent, not
to the version in force at enactment. Anyone treating these edges as
timestamped to the source document will get a systematically wrong network.

**References exist in both eras.** 23/39 modern and 49/115 older sampled
documents carry at least one. Text and references are not mutually exclusive.

**`/eli/accn/{accn}/xml` resolves directly.** The citation network can be
traversed without harvesting the corpus first.

**Errors are clean.** Unknown document → HTTP 404, `text/plain`,
`"No document found matching ELI request"`.

**No namespace in document XML** (plain XPath), but the Atom feed needs the
`d1` prefix — including for its ELI extension elements, which inherit Atom's
default namespace contrary to the ELI specification.

**The Atom feed declares `utf-16` but is UTF-8.** Must be overridden
explicitly or it will pass locally and fail on other platforms.

**Sitemap pages are not usefully ordered.** `lastmod` ranges overlap heavily
across pages, so year-selective fetching is impossible. `dlx_list_documents()`
must harvest all 21 pages → **a persistent cache is mandatory**, ~30 MB.

---

## 5. Conventions

- Column names follow the source verbatim, in `snake_case`. The source is
  bilingual (English/Latin metadata, Danish content elements); the
  inconsistency is documented, not smoothed over.
- **Exception:** `Ref_Af` → `ref_date`. Danish *af* = "of", i.e. the date of
  the referenced act. A literal `ref_af` column would be opaque.
- **Collision:** the XML has a `Status` field, and lexverse reserves `status`
  for `"ok"` / `"not_found"` / `"metadata_only"`. The XML field is returned as
  `legal_status`.
- Roxygen documentation in English, British spelling.
- Tibble returns throughout; `httr2` over `httr`.
- `status` values: `"ok"`, `"not_found"`, `"metadata_only"`.

---

## 6. Function plan

### v0.1

- [x] `dlx_get_doc()` — three identifier modes, metadata tibble
- [ ] `dlx_get_text()` — requires `DokumentIndhold` sub-structure analysis
- [ ] `dlx_get_metadata()` — thin wrapper
- [ ] `dlx_get_references()` — positional `Concerns` parser

### v0.2

- [ ] `dlx_get_changes(since, collection)` — Atom feed
- [ ] `dlx_list_documents(year, collection)` — cached sitemap index
- [ ] `dlx_cache_status()`, `dlx_clear_cache()`
- [ ] batch retrieval with internal throttling

### Cache design

`tools::R_user_dir("danlex", "cache")`, interactive consent on first write,
`dest` argument to override. CRAN policy forbids writing outside `tempdir()`
without explicit consent. This layer is the prototype for `lexcore`'s corpus
adapter — the same pattern will serve Norway (Lovdata bulk) and Iceland
(Althingi lagasafn zip).

---

## 7. Open questions

1. **`eli/resource/authority` not yet fetched.** The last documented source
   not consulted. Should give the authoritative collection and document-type
   code lists, and resolve what the accession letter `C` means.
2. `DokumentIndhold` internal structure — determines whether
   `dlx_get_paragraphs()` is possible. Investigate when writing
   `dlx_get_text()`.
3. Text boundary for `ltc` and `mt`. If later than 2012, all of
   Ministerialtidende is metadata-only.
4. Atom feed opening hours / rate limits — assumed none, not verified.
5. Harvest service time zone (assumed CET) — only relevant if it is ever used.
6. One Atom entry appeared to have an empty `reasonForChange`. Confirm and
   handle as `NA`.
7. Whether document XML is genuinely UTF-8 or merely happens to parse.
8. Complete `LOKDOK` code table beyond the four confirmed.

---

## 8. CRAN pre-flight

Following the lexverse checklist:

- [ ] `httptest2` mocks — appropriate here, unlike `swelex`. Keep fixture
      paths short from the start to avoid the path-length failures seen there.
- [ ] Pre-computed vignette (no live API calls at check time)
- [ ] `urlchecker::url_check()`
- [ ] `usethis::use_spell_check()` — will need a Danish word list
- [ ] `devtools::check(cran = TRUE)`
- [ ] win-builder + R-hub v2
- [ ] `cran-comments.md`
- [ ] `devtools::release()`

Per the lexverse release policy, the first submission bar is high: map the
API thoroughly and ship product-level coverage rather than a thin wrapper
that immediately needs updating.

---

## 9. Lexverse notes

Denmark shares ELI with `eurlex` and (prospectively) Estonia. That is the
strategic reason for this ordering — three independent ELI implementations
give `lexcore`'s `lex_documents` structure an empirical rather than
theoretical basis.

The maintained-versus-historical reference distinction (section 4) is the
first concrete evidence that `lex_citations` needs a field for it. A standard
designed on EUR-Lex alone would have missed it.

Sequence: **Denmark → Estonia → `lexcore` → Lithuania → (Norway + Iceland on
the corpus adapter)**. Latvia excluded: re-use of `likumi.lv` is a chargeable
service under Cabinet Regulation No. 536, with a 5 req/s automated-access
limit.
