test_that("names resolve to CID, DTXSID, InChIKey and SMILES", {
  local_fake_api()

  results <- pubchem_dtxsid_search(
    c("Mysterol", "Novelide", "Glucose", "unknown_xyz"),
    progress = FALSE
  )

  expect_named(results, names(.pubchem_na_template))
  expect_equal(results$search_status, c("found", "found", "found", "not_found"))
  expect_equal(results$pubchem_cid, c(5001L, 7001L, 5793L, NA))
  expect_equal(results$pubchem_n_cids, c(1L, 1L, 2L, 0L))
  expect_equal(results$dtxsid, c("DTXSID9999999", NA, NA, NA))
  expect_equal(results$inchikey[[2]], "CCCCCCCCCCCCCC-UHFFFAOYSA-N")
  expect_equal(results$smiles[[1]], "CCO")
})

test_that("failed lookups are reported as errors", {
  local_fake_api()

  expect_warning(
    results <- pubchem_dtxsid_search(c("pubchem server error", "benzene"), progress = FALSE),
    "failed"
  )
  expect_equal(results$search_status, c("error", "found"))
})

test_that("the namespace of each identifier is detected", {
  expect_equal(
    detect_pubchem_namespace(c("InChI=1S/CH4/h1H4", "UHOVQNZJYSORNB-UHFFFAOYSA-N", "71-43-2", "benzene")),
    c("inchi", "inchikey", "name", "name")
  )
})

test_that("empty input gives an empty result with the usual columns", {
  results <- pubchem_dtxsid_search(character(), progress = FALSE)

  expect_equal(nrow(results), 0L)
  expect_named(results, names(.pubchem_na_template))
})
