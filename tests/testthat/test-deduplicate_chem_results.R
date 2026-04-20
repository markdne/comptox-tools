# test-deduplicate_chem_results.R
# Tests for deduplicate_chem_results().
#
# No API mocking is needed here — the function works on tibbles directly.

# ---------------------------------------------------------------------------
# Helpers: sample results tibbles
# ---------------------------------------------------------------------------

# Two rows for Benzene (same DTXSID, different input_term) plus one for Toluene.
# Mimics the output of comptox_chem_search() called with chem_name + casrn cols.
make_results_two_chems <- function() {
  tibble::tibble(
    input_term        = c("Benzene",        "71-43-2",       "Toluene"),
    dtxsid            = c("DTXSID3039242",  "DTXSID3039242", "DTXSID4040008"),
    dtxcid            = c("DTXCID3039242",  "DTXCID3039242", "DTXCID4040008"),
    casrn             = c("71-43-2",        "71-43-2",       "108-88-3"),
    preferredName     = c("Benzene",        "Benzene",       "Toluene"),
    smiles            = c("c1ccccc1",       "c1ccccc1",      "Cc1ccccc1"),
    isMarkush         = c(FALSE,            FALSE,           FALSE),
    hasStructureImage = c(TRUE,             TRUE,            TRUE),
    searchName        = c("Benzene",        "71-43-2",       "Toluene"),
    searchValue       = c("Benzene",        "71-43-2",       "Toluene"),
    rank              = c(1L,               1L,              1L),
    suggestions       = c(NA_character_,    NA_character_,   NA_character_)
  )
}

# One row that has NA in the `by` column (unmatched identifier).
make_results_with_unmatched <- function() {
  tibble::tibble(
    input_term        = c("Benzene",        "71-43-2",       "NotFound"),
    dtxsid            = c("DTXSID3039242",  "DTXSID3039242", NA_character_),
    dtxcid            = c("DTXCID3039242",  "DTXCID3039242", NA_character_),
    casrn             = c("71-43-2",        "71-43-2",       NA_character_),
    preferredName     = c("Benzene",        "Benzene",       NA_character_),
    smiles            = c("c1ccccc1",       "c1ccccc1",      NA_character_),
    isMarkush         = c(FALSE,            FALSE,           NA),
    hasStructureImage = c(TRUE,             TRUE,            NA),
    searchName        = c("Benzene",        "71-43-2",       NA_character_),
    searchValue       = c("Benzene",        "71-43-2",       "NotFound"),
    rank              = c(1L,               1L,              NA_integer_),
    suggestions       = c(NA_character_,    NA_character_,   "Not-Found-Suggestion")
  )
}

# ===========================================================================
# Input validation
# ===========================================================================

test_that("non-data-frame results triggers abort", {
  expect_error(
    deduplicate_chem_results("not a df"),
    regexp = "`results`"
  )
})

test_that("invalid `by` value triggers abort", {
  results <- make_results_two_chems()
  expect_error(
    deduplicate_chem_results(results, by = "inchikey")
  )
})

test_that("`by` column absent from results triggers abort", {
  # Remove dtxcid so that by = "dtxcid" fails
  results <- dplyr::select(make_results_two_chems(), -dtxcid)
  expect_error(
    deduplicate_chem_results(results, by = "dtxcid"),
    regexp = "dtxcid"
  )
})

test_that("missing chem_id_cols when input_data is a data frame triggers abort", {
  results  <- make_results_two_chems()
  df       <- tibble::tibble(chem_name = c("Benzene"), casrn = c("71-43-2"))
  expect_error(
    deduplicate_chem_results(results, input_data = df),
    regexp = "chem_id_cols"
  )
})

test_that("non-atomic, non-data-frame input_data triggers abort", {
  results <- make_results_two_chems()
  expect_error(
    deduplicate_chem_results(results, input_data = list("a")),
    regexp = "input_data"
  )
})

# ===========================================================================
# Empty results
# ===========================================================================

test_that("zero-row results tibble is returned unchanged", {
  empty   <- make_results_two_chems()[0, ]
  deduped <- deduplicate_chem_results(empty)
  expect_equal(nrow(deduped), 0L)
  expect_s3_class(deduped, "tbl_df")
})

# ===========================================================================
# Basic deduplication (no input_data)
# ===========================================================================

test_that("two rows with the same DTXSID are collapsed into one output row", {
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(results)
  expect_equal(nrow(deduped), 2L)  # Benzene (collapsed) + Toluene
})

test_that("collapsed input_term joins both search terms with ' | '", {
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(results)
  benzene_row <- dplyr::filter(deduped, dtxsid == "DTXSID3039242")
  # Letter-containing "Benzene" sorts before numeric "71-43-2"
  expect_equal(benzene_row$input_term, "Benzene | 71-43-2")
})

test_that("non-input_term CompTox columns retain the value from the first result row", {
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(results)
  benzene_row <- dplyr::filter(deduped, dtxsid == "DTXSID3039242")
  expect_equal(benzene_row$casrn,         "71-43-2")
  expect_equal(benzene_row$preferredName, "Benzene")
  expect_equal(benzene_row$smiles,        "c1ccccc1")
})

test_that("a unique chemical that has no duplicate rows is returned unchanged", {
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(results)
  toluene_row <- dplyr::filter(deduped, dtxsid == "DTXSID4040008")
  expect_equal(toluene_row$input_term, "Toluene")
})

# ===========================================================================
# input_term ordering (no input_data)
# ===========================================================================

test_that("without input_data, letter-containing terms sort before CAS-RNs", {
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(results)
  benzene_row <- dplyr::filter(deduped, dtxsid == "DTXSID3039242")
  terms <- strsplit(benzene_row$input_term, " | ", fixed = TRUE)[[1]]
  # "Benzene" contains letters; "71-43-2" does not
  expect_true(grepl("[A-Za-z]", terms[1]))
  expect_false(grepl("[A-Za-z]", terms[2]))
})

# ===========================================================================
# Row ordering (no input_data)
# ===========================================================================

test_that("output row order matches the order of the first occurrence in results", {
  # Toluene appears first in results; Benzene (collapsed) appears after
  results_reversed <- tibble::tibble(
    input_term        = c("Toluene",        "Benzene",       "71-43-2"),
    dtxsid            = c("DTXSID4040008",  "DTXSID3039242", "DTXSID3039242"),
    dtxcid            = c("DTXCID4040008",  "DTXCID3039242", "DTXCID3039242"),
    casrn             = c("108-88-3",       "71-43-2",       "71-43-2"),
    preferredName     = c("Toluene",        "Benzene",       "Benzene"),
    smiles            = c("Cc1ccccc1",      "c1ccccc1",      "c1ccccc1"),
    isMarkush         = c(FALSE,            FALSE,           FALSE),
    hasStructureImage = c(TRUE,             TRUE,            TRUE),
    searchName        = c("Toluene",        "Benzene",       "71-43-2"),
    searchValue       = c("Toluene",        "Benzene",       "71-43-2"),
    rank              = c(1L,               1L,              1L),
    suggestions       = c(NA_character_,    NA_character_,   NA_character_)
  )
  deduped <- deduplicate_chem_results(results_reversed)
  expect_equal(deduped$dtxsid[1], "DTXSID4040008")  # Toluene first
  expect_equal(deduped$dtxsid[2], "DTXSID3039242")  # Benzene second
})

# ===========================================================================
# Unmatched rows (NA in `by` column)
# ===========================================================================

test_that("rows with NA in the `by` column are returned at the end of the tibble", {
  results <- make_results_with_unmatched()
  deduped <- deduplicate_chem_results(results)
  # Expect: Benzene (collapsed) in row 1, unmatched NotFound at the end
  last_row <- dplyr::slice_tail(deduped, n = 1)
  expect_true(is.na(last_row$dtxsid))
  expect_equal(last_row$input_term, "NotFound")
})

test_that("unmatched rows are returned unchanged (not collapsed or modified)", {
  results <- make_results_with_unmatched()
  deduped <- deduplicate_chem_results(results)
  last_row <- dplyr::slice_tail(deduped, n = 1)
  # suggestions column should carry through unmodified
  expect_equal(last_row$suggestions, "Not-Found-Suggestion")
})

test_that("total output rows = unique matched chemicals + unmatched rows", {
  results <- make_results_with_unmatched()
  deduped <- deduplicate_chem_results(results)
  # 1 unique matched chemical (Benzene, collapsed from 2 rows) + 1 unmatched
  expect_equal(nrow(deduped), 2L)
})

# ===========================================================================
# `by` parameter
# ===========================================================================

test_that("by = 'casrn' deduplicates on CAS-RN instead of DTXSID", {
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(results, by = "casrn")
  expect_equal(nrow(deduped), 2L)
  benzene_row <- dplyr::filter(deduped, casrn == "71-43-2")
  expect_equal(benzene_row$input_term, "Benzene | 71-43-2")
})

test_that("by = 'preferredName' deduplicates on preferred name", {
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(results, by = "preferredName")
  expect_equal(nrow(deduped), 2L)
})

# ===========================================================================
# collapse_sep parameter
# ===========================================================================

test_that("custom collapse_sep is used as the separator in input_term", {
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(results, collapse_sep = " / ")
  benzene_row <- dplyr::filter(deduped, dtxsid == "DTXSID3039242")
  expect_true(grepl(" / ", benzene_row$input_term, fixed = TRUE))
  expect_false(grepl(" | ", benzene_row$input_term, fixed = TRUE))
})

# ===========================================================================
# With input_data (data.frame)
# ===========================================================================

test_that("non-identifier columns from input_data are joined onto the results", {
  df <- tibble::tibble(
    chem_name = c("Benzene", "Benzene"),
    casrn     = c("71-43-2", "71-43-2"),
    conc_mg_l = c(1.0,       1.0)
  )
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(
    results,
    input_data   = df,
    chem_id_cols = c(chem_name, casrn)
  )
  expect_true("conc_mg_l" %in% names(deduped))
})

test_that("with input_data, output row order matches the input_data row order", {
  # input_data: Toluene first, Benzene second (order reversed vs results tibble)
  df <- tibble::tibble(
    chem_name = c("Toluene", "Benzene"),
    casrn     = c("108-88-3", "71-43-2")
  )
  results <- make_results_two_chems()  # Benzene rows first in results
  deduped <- deduplicate_chem_results(
    results,
    input_data   = df,
    chem_id_cols = c(chem_name, casrn)
  )
  # Toluene should appear first because it is first in input_data
  expect_equal(deduped$dtxsid[1], "DTXSID4040008")  # Toluene
})

test_that("with input_data, chem_id_cols order controls input_term ordering", {
  # chem_name listed before casrn → chemical name should appear first in
  # the collapsed input_term string
  df <- tibble::tibble(
    chem_name = c("Benzene"),
    casrn     = c("71-43-2")
  )
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(
    results,
    input_data   = df,
    chem_id_cols = c(chem_name, casrn)
  )
  benzene_row <- dplyr::filter(deduped, dtxsid == "DTXSID3039242")
  terms <- strsplit(benzene_row$input_term, " | ", fixed = TRUE)[[1]]
  expect_equal(terms[1], "Benzene")   # chem_name comes first
  expect_equal(terms[2], "71-43-2")   # casrn comes second
})

test_that("with character vector input_data, function falls through to global dedup", {
  results <- make_results_two_chems()
  # Atomic vector input_data: no columns to join, same as calling without input_data
  deduped <- deduplicate_chem_results(
    results,
    input_data = c("Benzene", "71-43-2", "Toluene")
  )
  expect_equal(nrow(deduped), 2L)
})

# ===========================================================================
# Internal helper columns stripped from output
# ===========================================================================

test_that("internal helper columns (.orig_row, .result_row, .col_order) are not in output", {
  df <- tibble::tibble(chem_name = c("Benzene"), casrn = c("71-43-2"))
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(
    results,
    input_data   = df,
    chem_id_cols = c(chem_name, casrn)
  )
  expect_false(".orig_row"    %in% names(deduped))
  expect_false(".result_row"  %in% names(deduped))
  expect_false(".col_order"   %in% names(deduped))
})

test_that("internal helper columns are absent even without input_data", {
  results <- make_results_two_chems()
  deduped <- deduplicate_chem_results(results)
  expect_false(".result_row" %in% names(deduped))
  expect_false(".col_order"  %in% names(deduped))
})
