test_that("results_type chooses the projection and columns follow the preferred order", {
  local_fake_api()

  identifiers <- comptox_chem_details(
    c("DTXSID7021360", "DTXSID3039242"),
    results_type = "chem_identifiers",
    progress = FALSE
  )

  # Listed columns in the preferred order, unlisted API fields kept after them
  expect_named(
    identifiers,
    c(
      "input_term", "dtxsid", "dtxcid", "casrn", "preferredName", "inchikey",
      "newApiField", "search_status", "error"
    )
  )
  expect_equal(identifiers$input_term, c("DTXSID7021360", "DTXSID3039242"))
  expect_equal(
    identifiers$inchikey,
    c("YXFVVABEGXRONW-UHFFFAOYSA-N", "UHOVQNZJYSORNB-UHFFFAOYSA-N")
  )

  # Default projection: structure fields only
  structures <- comptox_chem_details("DTXSID3039242", progress = FALSE)

  expect_named(
    structures,
    c("input_term", "dtxsid", "dtxcid", "smiles", "inchikey", "search_status", "error")
  )
})

test_that("every identifier is reported with its status", {
  local_fake_api()

  expect_warning(
    details <- comptox_chem_details(
      c(" dtxsid3039242", "DTXSID0000000", "71-43-2", "DTXSID0000500", NA, "DTXSID3039242"),
      chunk_size = 1,
      progress = FALSE
    ),
    "not a valid DTXSID"
  ) |>
    expect_warning("request failed")

  expect_equal(details$input_term, c("DTXSID3039242", "DTXSID0000000", "71-43-2", "DTXSID0000500"))
  expect_equal(details$search_status, c("found", "not_found", "invalid", "error"))
  expect_equal(
    details$error,
    c(NA, "No results returned", "Not a valid DTXSID", "HTTP 500")
  )
  expect_equal(details$dtxsid, c("DTXSID3039242", NA, NA, NA))
})

test_that("data frame input, DTXCID search and unsorted output work", {
  local_fake_api()

  df <- tibble::tibble(dtxcid = c("DTXCID999999", "DTXCID30182"), conc = 1:2)

  details <- comptox_chem_details(
    df,
    search_type = "dtxcid",
    results_type = "all",
    chem_id_col = dtxcid,
    sorted = FALSE,
    progress = FALSE
  )

  # Unsorted: found chemicals in API order (the fixture's order)
  expect_equal(details$input_term, c("DTXCID30182", "DTXCID999999"))
  expect_equal(details$preferredName, c("Benzene", "Mysterol acid"))
})

test_that("empty input gives an empty result with the key columns", {
  details <- comptox_chem_details(c(NA, " "), api_key = "k", search_type = "dtxcid")

  expect_equal(nrow(details), 0L)
  expect_named(details, c("input_term", "dtxcid", "search_status", "error"))
})

test_that("inputs are validated", {
  withr::local_envvar(CTX_API_KEY = "k")
  df <- tibble::tibble(a = "DTXSID1", b = "DTXSID2")

  expect_error(comptox_chem_details("DTXSID3039242", chunk_size = 1001), "1000")
  expect_error(comptox_chem_details(df), "chem_id_col")
  expect_error(comptox_chem_details(df, chem_id_col = c(a, b)), "exactly one")
  expect_error(comptox_chem_details("DTXSID1", results_type = "everything"), "should be one of")
  expect_error(comptox_chem_details(list("DTXSID1")), "input_data")
})
