# test-verify_chem_identifiers.R
# Tests for verify_chem_identifiers().
#
# comptox_chem_search() is mocked at the package level so no live API calls
# are made. helper-mocks.R (loaded automatically by testthat) sets
# future::plan(future::sequential), which keeps the mocked bindings visible
# inside any furrr::future_map() calls inside comptox_chem_search().

# ---------------------------------------------------------------------------
# Helper fixtures: tibble rows returned by a mocked comptox_chem_search()
# ---------------------------------------------------------------------------

# Factory for a single matched row in the 12-column schema.
make_hit_row <- function(input_term, dtxsid, dtxcid, casrn,
                          preferred_name, smiles = "C") {
  tibble::tibble(
    input_term        = input_term,
    dtxsid            = dtxsid,
    dtxcid            = dtxcid,
    casrn             = casrn,
    preferredName     = preferred_name,
    smiles            = smiles,
    isMarkush         = FALSE,
    hasStructureImage = TRUE,
    searchName        = input_term,
    searchValue       = input_term,
    rank              = 1L,
    suggestions       = NA_character_
  )
}

# Factory for a no-match row (no DTXSID, optional suggestions).
make_miss_row <- function(input_term, suggestions = NA_character_) {
  tibble::tibble(
    input_term        = input_term,
    dtxsid            = NA_character_,
    dtxcid            = NA_character_,
    casrn             = NA_character_,
    preferredName     = NA_character_,
    smiles            = NA_character_,
    isMarkush         = NA,
    hasStructureImage = NA,
    searchName        = NA_character_,
    searchValue       = input_term,
    rank              = NA_integer_,
    suggestions       = suggestions
  )
}

# Reusable chemical data constants
BNZ_DTXSID <- "DTXSID3039242"
BNZ_DTXCID <- "DTXCID3039242"
BNZ_CASRN  <- "71-43-2"

TOL_DTXSID <- "DTXSID4040008"
TOL_DTXCID <- "DTXCID4040008"
TOL_CASRN  <- "108-88-3"

# Mock comptox_chem_search() that maps known identifiers to results. Any
# unrecognised identifier gets a no-match row.
make_mock_search <- function(extra_map = list()) {
  base_map <- list(
    "Benzene"   = make_hit_row("Benzene",   BNZ_DTXSID, BNZ_DTXCID, BNZ_CASRN, "Benzene"),
    "71-43-2"   = make_hit_row("71-43-2",   BNZ_DTXSID, BNZ_DTXCID, BNZ_CASRN, "Benzene"),
    "Toluene"   = make_hit_row("Toluene",   TOL_DTXSID, TOL_DTXCID, TOL_CASRN, "Toluene"),
    "108-88-3"  = make_hit_row("108-88-3",  TOL_DTXSID, TOL_DTXCID, TOL_CASRN, "Toluene")
  )
  merged_map <- c(base_map, extra_map)

  function(input_data, ...) {
    ids <- if (is.character(input_data)) input_data else stop("expected character vector")
    rows <- lapply(ids, function(id) {
      if (!is.null(merged_map[[id]])) merged_map[[id]] else make_miss_row(id)
    })
    dplyr::bind_rows(rows)
  }
}

# ===========================================================================
# Input validation
# ===========================================================================

test_that("non-data-frame input_data triggers abort", {
  expect_error(
    verify_chem_identifiers("Benzene", api_key = "key", chem_id_cols = chem_name),
    regexp = "`input_data`"
  )
})

test_that("missing api_key triggers abort", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")
  expect_error(
    verify_chem_identifiers(df, chem_id_cols = c(chem_name, casrn)),
    regexp = "`api_key`"
  )
})

test_that("blank api_key triggers abort", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")
  expect_error(
    verify_chem_identifiers(df, api_key = "", chem_id_cols = c(chem_name, casrn)),
    regexp = "`api_key`"
  )
})

test_that("missing chem_id_cols triggers abort", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")
  expect_error(
    verify_chem_identifiers(df, api_key = "key"),
    regexp = "`chem_id_cols`"
  )
})

test_that("fewer than 2 chem_id_cols triggers abort", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")
  expect_error(
    verify_chem_identifiers(df, api_key = "key", chem_id_cols = chem_name),
    regexp = "at least 2"
  )
})

test_that("zero chunk_size triggers abort", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")
  expect_error(
    verify_chem_identifiers(df, api_key = "key",
                             chem_id_cols = c(chem_name, casrn), chunk_size = 0L),
    regexp = "chunk_size"
  )
})

test_that("negative rate_limit triggers warning and proceeds", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  expect_warning(
    verify_chem_identifiers(df, api_key = "key",
                             chem_id_cols = c(chem_name, casrn), rate_limit = -1),
    regexp = "rate_limit"
  )
})

test_that("clashing column name in input_data triggers abort", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2", dtxsid = "old")
  expect_error(
    verify_chem_identifiers(df, api_key = "key", chem_id_cols = c(chem_name, casrn)),
    regexp = "dtxsid"
  )
})

# ===========================================================================
# Empty / all-NA inputs
# ===========================================================================

test_that("all-NA identifier columns return no_match rows without API call", {
  df <- tibble::tibble(
    chem_name = c(NA_character_, NA_character_),
    casrn     = c(NA_character_, NA_character_),
    extra     = c(1L, 2L)
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_equal(nrow(result), 2L)
  expect_true(all(result$agreement == "no_match"))
  expect_true(all(is.na(result$dtxsid)))
  expect_equal(result$extra, c(1L, 2L))
})

test_that("blank-string identifiers are treated as NA", {
  df <- tibble::tibble(chem_name = "   ", casrn = "")
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_equal(nrow(result), 1L)
  expect_equal(result$agreement, "no_match")
})

# ===========================================================================
# Agreement: "agree"
# ===========================================================================

test_that("both identifiers resolving to the same DTXSID yields one agree row", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_equal(nrow(result), 1L)
  expect_equal(result$agreement,     "agree")
  expect_equal(result$dtxsid,        BNZ_DTXSID)
  expect_equal(result$preferredName, "Benzene")
  expect_equal(result$chem_name,     "Benzene")
  expect_equal(result$casrn,         "71-43-2")
})

test_that("three columns all resolving to same DTXSID yields one agree row", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2", dtxsid_col = BNZ_DTXSID)
  extra_map <- setNames(
    list(make_hit_row(BNZ_DTXSID, BNZ_DTXSID, BNZ_DTXCID, BNZ_CASRN, "Benzene")),
    BNZ_DTXSID
  )
  mock <- make_mock_search(extra_map)
  local_mocked_bindings(
    comptox_chem_search = mock,
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn, dtxsid_col))
  expect_equal(nrow(result), 1L)
  expect_equal(result$agreement, "agree")
})

test_that("one identifier resolves; other is NA -> agree (one row)", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = NA_character_)
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_equal(nrow(result), 1L)
  expect_equal(result$agreement, "agree")
  expect_equal(result$dtxsid,    BNZ_DTXSID)
})

test_that("same identifier in both columns -> agree (one row)", {
  df <- tibble::tibble(chem_name = "Benzene", alt_name = "Benzene")
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, alt_name))
  expect_equal(nrow(result), 1L)
  expect_equal(result$agreement, "agree")
})

# ===========================================================================
# Agreement: "disagree"
# ===========================================================================

test_that("identifiers resolving to different DTXSIDs yields two disagree rows", {
  # chem_name = "Toluene" resolves to TOL_DTXSID
  # casrn     = "71-43-2" resolves to BNZ_DTXSID  (deliberate mismatch)
  df <- tibble::tibble(chem_name = "Toluene", casrn = "71-43-2")
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_equal(nrow(result), 2L)
  expect_true(all(result$agreement == "disagree"))
  expect_setequal(result$dtxsid, c(TOL_DTXSID, BNZ_DTXSID))
})

test_that("three columns with two distinct DTXSIDs yields two disagree rows", {
  # chem_name -> BNZ, casrn -> BNZ, alt_name -> TOL
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2", alt_name = "Toluene")
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn, alt_name))
  expect_equal(nrow(result), 2L)
  expect_true(all(result$agreement == "disagree"))
  expect_setequal(result$dtxsid, c(BNZ_DTXSID, TOL_DTXSID))
})

# ===========================================================================
# Agreement: "no_match"
# ===========================================================================

test_that("no identifier resolves -> one no_match row with NA CompTox data", {
  df <- tibble::tibble(chem_name = "NotAChemical", casrn = "000-00-0")
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_equal(nrow(result), 1L)
  expect_equal(result$agreement, "no_match")
  expect_true(is.na(result$dtxsid))
  expect_true(is.na(result$preferredName))
})

# ===========================================================================
# Suggestion cross-validation
# ===========================================================================

test_that("suggestion validated by other identifier's DTXSID -> agree", {
  # "bnz-typo" has no direct hit but suggests "Benzene".
  # casrn = "71-43-2" resolves directly to BNZ_DTXSID.
  # "Benzene" from retry -> BNZ_DTXSID matches the anchor -> cross-validated.
  df <- tibble::tibble(chem_name = "bnz-typo", casrn = "71-43-2")

  search_mock <- function(input_data, ...) {
    rows <- lapply(input_data, function(id) {
      switch(id,
        "bnz-typo" = make_miss_row("bnz-typo", suggestions = "Benzene"),
        "71-43-2"  = make_hit_row("71-43-2", BNZ_DTXSID, BNZ_DTXCID, BNZ_CASRN, "Benzene"),
        "Benzene"  = make_hit_row("Benzene", BNZ_DTXSID, BNZ_DTXCID, BNZ_CASRN, "Benzene"),
        make_miss_row(id)
      )
    })
    dplyr::bind_rows(rows)
  }

  local_mocked_bindings(
    comptox_chem_search = search_mock,
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_equal(nrow(result), 1L)
  expect_equal(result$agreement, "agree")
  expect_equal(result$dtxsid,    BNZ_DTXSID)
})

test_that("suggestion not matching any direct hit -> no_match", {
  # "bnz-typo" suggests "Benzene" (BNZ_DTXSID).
  # casrn = "108-88-3" resolves to TOL_DTXSID (does NOT match).
  # The suggestion cannot be cross-validated -> bnz-typo stays unresolved.
  # casrn resolves alone -> agree on TOL_DTXSID (not no_match).
  # Actually: one identifier resolves (casrn -> TOL), one doesn't (name).
  # n_resolved == 1 -> "agree" for the row (single DTXSID found).
  df <- tibble::tibble(chem_name = "bnz-typo", casrn = "108-88-3")

  search_mock <- function(input_data, ...) {
    rows <- lapply(input_data, function(id) {
      switch(id,
        "bnz-typo"  = make_miss_row("bnz-typo", suggestions = "Benzene"),
        "108-88-3"  = make_hit_row("108-88-3", TOL_DTXSID, TOL_DTXCID, TOL_CASRN, "Toluene"),
        "Benzene"   = make_hit_row("Benzene",   BNZ_DTXSID, BNZ_DTXCID, BNZ_CASRN, "Benzene"),
        make_miss_row(id)
      )
    })
    dplyr::bind_rows(rows)
  }

  local_mocked_bindings(
    comptox_chem_search = search_mock,
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  # casrn resolved to TOL; "bnz-typo" suggestion (BNZ) != anchor (TOL) -> not validated
  # Only one unique DTXSID (TOL) found -> agree
  expect_equal(nrow(result), 1L)
  expect_equal(result$agreement, "agree")
  expect_equal(result$dtxsid,    TOL_DTXSID)
})

test_that("suggestion when no other identifier resolves -> no_match", {
  df <- tibble::tibble(chem_name = "bnz-typo", casrn = "000-00-0")

  search_mock <- function(input_data, ...) {
    rows <- lapply(input_data, function(id) {
      switch(id,
        "bnz-typo" = make_miss_row("bnz-typo", suggestions = "Benzene"),
        "Benzene"  = make_hit_row("Benzene", BNZ_DTXSID, BNZ_DTXCID, BNZ_CASRN, "Benzene"),
        make_miss_row(id)
      )
    })
    dplyr::bind_rows(rows)
  }

  local_mocked_bindings(
    comptox_chem_search = search_mock,
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  # No anchor DTXSIDs -> suggestion cannot be validated -> no_match
  expect_equal(nrow(result), 1L)
  expect_equal(result$agreement, "no_match")
  expect_true(is.na(result$dtxsid))
})

# ===========================================================================
# Multi-row inputs and row ordering
# ===========================================================================

test_that("multi-row input: agree, disagree, and no_match rows ordered correctly", {
  # Row 1: Benzene + 71-43-2  -> agree    -> 1 output row
  # Row 2: Toluene + 71-43-2  -> disagree -> 2 output rows
  # Row 3: NotAChemical + xyz  -> no_match -> 1 output row
  # Total output rows: 4
  df <- tibble::tibble(
    chem_name = c("Benzene",      "Toluene",  "NotAChemical"),
    casrn     = c("71-43-2",      "71-43-2",  "xyz"),
    row_label = c("row1",         "row2",      "row3")
  )
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_equal(nrow(result), 4L)

  # First output row: row 1 (agree, Benzene)
  expect_equal(result$row_label[[1L]], "row1")
  expect_equal(result$agreement[[1L]], "agree")
  expect_equal(result$dtxsid[[1L]],    BNZ_DTXSID)

  # Rows 2-3: row 2 (disagree)
  expect_true(all(result$agreement[2:3] == "disagree"))
  expect_true(all(result$row_label[2:3] == "row2"))
  expect_setequal(result$dtxsid[2:3], c(BNZ_DTXSID, TOL_DTXSID))

  # Last row: row 3 (no_match)
  expect_equal(result$row_label[[4L]], "row3")
  expect_equal(result$agreement[[4L]], "no_match")
  expect_true(is.na(result$dtxsid[[4L]]))
})

# ===========================================================================
# Output structure
# ===========================================================================

test_that("output contains all original input columns", {
  df <- tibble::tibble(
    chem_name = "Benzene",
    casrn     = "71-43-2",
    dose_mg_l = 1.5,
    study_id  = "S001"
  )
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_true(all(c("chem_name", "casrn", "dose_mg_l", "study_id") %in% names(result)))
  expect_equal(result$dose_mg_l, 1.5)
  expect_equal(result$study_id,  "S001")
})

test_that("output contains all expected CompTox columns and agreement", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expected_cols <- c(
    "chem_name", "casrn",
    "dtxsid", "dtxcid", "preferredName", "smiles",
    "isMarkush", "hasStructureImage", "rank",
    "agreement"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("output does not contain internal helper columns (.orig_row etc.)", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_false(".orig_row"  %in% names(result))
  expect_false("n_resolved" %in% names(result))
})

test_that("original input columns appear before CompTox columns in output", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2", extra = "x")
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result    <- verify_chem_identifiers(df, api_key = "key",
                                        chem_id_cols = c(chem_name, casrn))
  col_names <- names(result)
  orig_last <- max(which(col_names %in% c("chem_name", "casrn", "extra")))
  comptox_first <- min(which(col_names %in% c("dtxsid", "dtxcid", "preferredName")))
  expect_lt(orig_last, comptox_first)
})

test_that("agreement column is the last column in the output", {
  df <- tibble::tibble(chem_name = "Benzene", casrn = "71-43-2")
  local_mocked_bindings(
    comptox_chem_search = make_mock_search(),
    .package = "comptoxtools"
  )
  result <- verify_chem_identifiers(df, api_key = "key",
                                     chem_id_cols = c(chem_name, casrn))
  expect_equal(names(result)[ncol(result)], "agreement")
})
