test_that("batch search resolves names and CASRNs and keeps every identifier", {
  local_fake_api()

  results <- comptox_chem_search(
    c("Benzene", " 71-43-2 ", "Benzen", "Unknownium", NA, ""),
    progress = FALSE
  ) |>
    suppressMessages()

  expect_named(results, .comptox_output_cols)
  expect_equal(results$input_term, c("Benzene", "71-43-2", "Benzen", "Unknownium"))
  expect_equal(results$dtxsid, c("DTXSID3039242", "DTXSID3039242", NA, NA))
  expect_equal(results$search_status, c("found", "found", "not_found", "not_found"))
  expect_equal(results$suggestions[[3]], "Benzene")
})

test_that("identifiers ending in a hyphen are searched with GET", {
  local_fake_api()

  expect_message(
    results <- comptox_chem_search(c("Ethyl-", "Toluene"), progress = FALSE),
    "ending in"
  )
  expect_equal(results$dtxsid, c("DTXSID7021360", "DTXSID7021360"))
})

test_that("failed requests are reported as errors, not as 'not found'", {
  local_fake_api()

  expect_warning(
    results <- comptox_chem_search(c("Server Error Chem", "Toluene"), progress = FALSE) |>
      suppressMessages(),
    "could not be searched"
  )
  expect_equal(results$search_status, c("error", "found"))
})

test_that("batch results are matched to inputs whatever the case of searchValue", {
  local_fake_api()
  withr::local_options(fake_comptox_upper_case = TRUE)

  results <- comptox_chem_search(c("Benzene", "Toluene"), progress = FALSE)

  expect_equal(results$input_term, c("Benzene", "Toluene"))
  expect_equal(results$dtxsid, c("DTXSID3039242", "DTXSID7021360"))
})

test_that("data frame input searches all selected columns, numeric ones included", {
  local_fake_api()

  df <- tibble::tibble(chem_name = c("Benzene", "Toluene"), code = c(1, 2))

  results <- comptox_chem_search(df, chem_id_cols = c(chem_name, code), progress = FALSE) |>
    suppressMessages()

  expect_setequal(results$input_term, c("Benzene", "Toluene", "1", "2"))
})

test_that("GET-only search and empty input work", {
  local_fake_api()

  results <- comptox_chem_search(c("Toluene", "Nothing"), batch = FALSE, progress = FALSE)
  expect_equal(results$search_status, c("found", "not_found"))

  empty <- comptox_chem_search(c(NA_character_, " "), progress = FALSE)
  expect_equal(nrow(empty), 0L)
  expect_named(empty, .comptox_output_cols)
})

test_that("inputs are validated", {
  withr::local_envvar(CTX_API_KEY = "")

  expect_error(comptox_chem_search("Benzene"), "api_key")
  expect_error(comptox_chem_search(1:3, api_key = "k"), "input_data")
  expect_error(comptox_chem_search(tibble::tibble(x = "a"), api_key = "k"), "chem_id_cols")
  expect_error(comptox_chem_search("a", api_key = "k", chunk_size = 0), "chunk_size")
  expect_error(comptox_chem_search("a", api_key = "k", rate_limit = -1), "rate_limit")
})
