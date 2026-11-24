# Input rows covering every resolution path and agreement class
fake_verify_input <- function() {
  tibble::tribble(
    ~chem_name,          ~casrn,     ~conc,
    "Benzene",           "71-43-2",  1,   # agree
    "Toluene",           "71-43-2",  2,   # disagree (expands to 2 rows)
    "Benzene",           NA,         3,   # single_id
    "Benzen",            "71-43-2",  4,   # agree, name via confirmed suggestion
    "Tolune",            "71-43-2",  5,   # partial, suggestion not confirmed
    "Mysterol",          NA,         6,   # single_id, via PubChem DTXSID
    "Ketonol old name",  "888-88-8", 7,   # agree, name via PubChem InChIKey
    "Novelide",          NA,         8,   # single_id, InChIKey only
    NA,                  NA,         9,   # no_match, nothing supplied
    "Glucose",           "71-43-2",  10,  # disagree, PubChem InChIKey vs CompTox
    "Unknownium",        "Nothing",  11   # no_match
  )
}

test_that("each row is resolved and classified (with PubChem fallback)", {
  local_fake_api()

  verified <- verify_chem_identifiers(
    fake_verify_input(),
    chem_id_cols = c(chem_name, casrn),
    pubchem_fallback = TRUE,
    progress = FALSE
  ) |>
    suppressMessages()

  expect_named(verified, c("chem_name", "casrn", "conc", .verify_output_cols))
  expect_equal(verified$conc, c(1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 11))
  expect_equal(
    verified$agreement,
    c(
      "agree", "disagree", "disagree", "single_id", "agree", "partial",
      "single_id", "agree", "single_id", "no_match", "disagree", "disagree", "no_match"
    )
  )

  # Disagreeing row: one output row per chemical, CompTox data attached
  toluene_row <- dplyr::filter(verified, conc == 2)
  expect_setequal(toluene_row$dtxsid, c("DTXSID7021360", "DTXSID3039242"))
  expect_setequal(toluene_row$preferredName, c("Toluene", "Benzene"))
  expect_match(toluene_row$resolution_detail[[1]], "chem_name 'Toluene' -> DTXSID7021360 \\(comptox\\)")

  # Suggestion confirmed by the CASRN
  expect_equal(verified$resolved_via[verified$conc == 4], "comptox; comptox_suggestion")

  # PubChem DTXSID re-queried in CompTox, PubChem fields kept
  mysterol <- dplyr::filter(verified, conc == 6)
  expect_equal(mysterol$dtxsid, "DTXSID9999999")
  expect_equal(mysterol$preferredName, "Mysterol acid")
  expect_equal(mysterol$comptox_casrn, "999-99-9")
  expect_equal(mysterol$inchikey, "AAAAAAAAAAAAAA-UHFFFAOYSA-N")
  expect_equal(mysterol$pubchem_cid, 5001L)
  expect_equal(mysterol$resolved_via, "pubchem")

  # PubChem InChIKey found in CompTox
  ketonol <- dplyr::filter(verified, conc == 7)
  expect_equal(ketonol$dtxsid, "DTXSID8888888")
  expect_equal(ketonol$resolved_via, "comptox; pubchem")

  # PubChem compound unknown to CompTox: keyed on its InChIKey
  novelide <- dplyr::filter(verified, conc == 8)
  expect_true(is.na(novelide$dtxsid))
  expect_equal(novelide$inchikey, "CCCCCCCCCCCCCC-UHFFFAOYSA-N")
  expect_equal(novelide$pubchem_smiles, "CCN")

  # Ambiguous PubChem name noted in the detail
  expect_match(verified$resolution_detail[verified$conc == 10][[1]], "1 of 2 CIDs")

  expect_equal(verified$resolution_detail[verified$conc == 9], "no identifiers supplied")
})

test_that("without PubChem fallback, PubChem-only identifiers do not resolve", {
  local_fake_api()

  verified <- verify_chem_identifiers(
    fake_verify_input(),
    chem_id_cols = c(chem_name, casrn),
    progress = FALSE
  ) |>
    suppressMessages()

  expect_equal(verified$agreement[verified$conc == 6], "no_match")
  expect_equal(verified$agreement[verified$conc == 10], "partial")
  expect_true(all(is.na(verified$pubchem_cid)))
})

test_that("an identifier matching several chemicals is 'ambiguous' unless the row narrows it", {
  resolved <- resolve_identifier(c("DTXSID1", "DTXSID2"), character(), character(), NULL)
  expect_equal(resolved$keys, c("DTXSID1", "DTXSID2"))

  narrowed <- resolve_identifier(c("DTXSID1", "DTXSID2"), character(), character(), "DTXSID2")
  expect_equal(narrowed$keys, "DTXSID2")

  unconfirmed <- resolve_identifier(character(), "DTXSID3", character(), "DTXSID2")
  expect_length(unconfirmed$keys, 0L)

  expect_false(has_disjoint_sets(list(c("A", "B"), "A")))
  expect_true(has_disjoint_sets(list("A", "B")))
})

test_that("inputs are validated", {
  withr::local_envvar(CTX_API_KEY = "k")
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")

  expect_error(verify_chem_identifiers(df, chem_id_cols = chem_name), "at least 2")
  expect_error(
    verify_chem_identifiers(dplyr::mutate(df, dtxsid = "x"), chem_id_cols = c(chem_name, casrn)),
    "clash"
  )
  expect_error(
    verify_chem_identifiers(df, chem_id_cols = c(chem_name, casrn), pubchem_fallback = "yes"),
    "pubchem_fallback"
  )
})
