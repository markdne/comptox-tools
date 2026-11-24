test_that("is_valid_casrn() checks format and check digit", {
  expect_equal(
    is_valid_casrn(c("71-43-2", "50-00-0", "7732-18-5", "4292-10-8", "71-43-3", "0071-43-2", "Nothing", NA)),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("standardise_casrn() fixes spaces, leading zeros and missing hyphens", {
  expect_equal(
    standardise_casrn(c("0000071-43-2", "71432", " 50-00-0", "7732185", NA, "Nothing")),
    c("71-43-2", "71-43-2", "50-00-0", "7732-18-5", NA, "Nothing")
  )
})

test_that("non-atomic input is rejected", {
  expect_error(is_valid_casrn(list("71-43-2")), "atomic")
  expect_error(standardise_casrn(list("71-43-2")), "atomic")
})
