test_that("terms resolving to the same chemical are collapsed within each input row", {
  local_fake_api()

  df <- tibble::tibble(
    chem_name = c("Benzene ", "Toluene"),
    casrn = c("71-43-2", "71-43-2"),
    conc = c(1, 2)
  )

  results <- comptox_chem_search(df, chem_id_cols = c(chem_name, casrn), progress = FALSE)

  deduped <- deduplicate_chem_results(results, input_data = df, chem_id_cols = c(chem_name, casrn))

  # Row 1: name (with a stray space) and CASRN collapse to one benzene row;
  # row 2: toluene and benzene stay separate
  expect_equal(nrow(deduped), 3L)
  expect_equal(deduped$input_term[[1]], "Benzene | 71-43-2")
  expect_equal(deduped$conc, c(1, 2, 2))
})

test_that("without input_data, names come before CASRNs", {
  local_fake_api()

  results <- comptox_chem_search(c("71-43-2", "Benzene"), progress = FALSE)

  deduped <- deduplicate_chem_results(results)

  expect_equal(deduped$input_term, "Benzene | 71-43-2")
})

test_that("bad arguments are rejected", {
  expect_error(deduplicate_chem_results("x"), "results")
  expect_error(deduplicate_chem_results(tibble::tibble(a = 1)), "not found")
})
