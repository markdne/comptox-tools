test_that("rows are grouped by DTXSID, then by InChIKey", {
  chems <- tibble::tibble(
    chem_name = c("Benzene", "Toluene", "benzol (PubChem only)", "Novelide", "Novelide salt?", "Unknown", "Benzene"),
    dtxsid = c("DTXSID3039242", "DTXSID7021360", NA, NA, NA, NA, "DTXSID3039242"),
    inchikey = c(
      "UHOVQNZJYSORNB-UHFFFAOYSA-N", "YXFVVABEGXRONW-UHFFFAOYSA-N",
      "UHOVQNZJYSORNB-UHFFFAOYSA-N", "CCCCCCCCCCCCCC-UHFFFAOYSA-N",
      "CCCCCCCCCCCCCC-UHFFFAOYSA-N", NA, "UHOVQNZJYSORNB-UHFFFAOYSA-N"
    )
  )

  flagged <- flag_duplicate_chems(chems)

  expect_equal(
    flagged$dup_key,
    c(
      "DTXSID3039242", "DTXSID7021360", "DTXSID3039242",
      "inchikey:CCCCCCCCCCCCCC-UHFFFAOYSA-N", "inchikey:CCCCCCCCCCCCCC-UHFFFAOYSA-N",
      NA, "DTXSID3039242"
    )
  )
  expect_equal(flagged$dup_group, c(1L, 2L, 1L, 3L, 3L, NA, 1L))
  expect_equal(flagged$dup_n, c(3L, 1L, 3L, 2L, 2L, NA, 3L))
  expect_equal(flagged$dup_first_row, c(1L, 2L, 1L, 4L, 4L, NA, 1L))
  expect_equal(flagged$is_duplicate, c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
})

test_that("an InChIKey shared by two DTXSIDs is not mapped to either", {
  chems <- tibble::tibble(
    dtxsid = c("DTXSID1", "DTXSID2", NA),
    inchikey = c("SAMEKEY", "SAMEKEY", "SAMEKEY")
  )

  flagged <- flag_duplicate_chems(chems)

  expect_equal(flagged$dup_key, c("DTXSID1", "DTXSID2", "inchikey:SAMEKEY"))
  expect_false(any(flagged$is_duplicate))
})

test_that("DTXSID-only keys work and bad inputs are rejected", {
  chems <- tibble::tibble(dtxsid = c("DTXSID1", "DTXSID1"))

  expect_equal(flag_duplicate_chems(chems, inchikey_col = NULL)$is_duplicate, c(FALSE, TRUE))
  expect_error(flag_duplicate_chems(chems), "not found")
  expect_error(flag_duplicate_chems(dplyr::mutate(chems, is_duplicate = 1), inchikey_col = NULL), "already has")
})
