test_that("Excel dates in the common formats convert back to CASRNs", {
  dates <- c(
    "08/10/4292",       # CPDat v4.0 style, day/month/year
    "10/8/4292",        # US style, month/day/year
    "4292-10-08",       # ISO
    "08.10.4292",       # dotted
    "8-Oct-4292",       # day, month name, year
    "Oct 8, 4292",      # month name, day, year
    "08/10/4292 00:00"  # with a time
  )

  details <- parse_casrn_dates(dates)

  expect_equal(details$status, rep("converted", length(dates)))
  expect_equal(details$casrn, rep("4292-10-8", length(dates)))
})

test_that("values that are not dates are left alone", {
  details <- parse_casrn_dates(c("4292-10-8", "71-43-2", "71432", "benzene", NA, ""))

  expect_equal(details$status, c(rep("not_a_date", 4), NA, NA))
  expect_true(all(is.na(details$casrn)))
})

test_that("years from two-digit first blocks are recovered with the check digit", {
  # Excel reads 75-09-2 as 2 Sep 1975; only 75-09-2 (not 1975-09-2) is valid
  details <- parse_casrn_dates(c("02/09/1975", "02/09/75"))

  expect_equal(details$casrn, c("75-09-2", "75-09-2"))
  expect_equal(details$two_digit_year, c(FALSE, TRUE))
})

test_that("dates with several or no valid readings are not converted", {
  details <- parse_casrn_dates(c("01/06/1003", "12/10/2020", "Oct-92"))

  # 01/06/1003 fits 1003-06-1 (day first) and 1003-01-6 (month first);
  # 12/10/2020 has no single-digit day; Oct-92 has lost the day
  expect_equal(details$status, c("ambiguous", "no_valid_casrn", "no_valid_casrn"))
  expect_equal(details$candidates[[1]], "1003-06-1 | 1003-01-6")
  expect_true(all(is.na(details$casrn)))

  # Knowing the date order resolves the ambiguity
  expect_equal(parse_casrn_dates("01/06/1003", date_order = "dmy")$casrn, "1003-06-1")
})

test_that("Excel serial numbers convert only when asked and only when valid", {
  # 873943 is Excel's serial number for 8 Oct 4292
  expect_equal(parse_casrn_dates("873943")$status, "not_a_date")
  expect_equal(parse_casrn_dates("873943", excel_serial = TRUE)$casrn, "4292-10-8")

  # 71432 as a serial is 27 Jul 2095 (no valid CASRN), so it stays a number
  expect_equal(parse_casrn_dates("71432", excel_serial = TRUE)$status, "not_a_date")
})

test_that("convert_date_to_casrn() returns a corrected vector", {
  expect_message(
    expect_warning(
      converted <- convert_date_to_casrn(c("08/10/4292", "71-43-2", "12/10/2020", NA)),
      "could not be converted"
    ),
    "Converted 1"
  )

  expect_equal(converted, c("4292-10-8", "71-43-2", NA, NA))

  kept <- convert_date_to_casrn(c("08/10/4292", "12/10/2020"), keep_unconverted = TRUE) |>
    suppressWarnings() |>
    suppressMessages()

  expect_equal(kept, c("4292-10-8", "12/10/2020"))
})

test_that("year-first dates are read as year-month-day whatever date_order says", {
  expect_equal(parse_casrn_dates("4292-10-08", date_order = "mdy")$casrn, "4292-10-8")
})

test_that("arguments are validated", {
  expect_error(parse_casrn_dates(list("a")), "atomic")
  expect_error(parse_casrn_dates("a", date_order = "ymd"), "date_order")
  expect_error(convert_date_to_casrn("a", keep_unconverted = NA), "keep_unconverted")
  expect_error(parse_casrn_dates("a", excel_serial = NA), "excel_serial")
})
