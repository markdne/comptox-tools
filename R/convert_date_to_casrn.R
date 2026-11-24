# Day-month orders that parse_casrn_dates() can try for day- or month-first
# dates (year-first dates are always read as year-month-day)
.casrn_date_orders <- c("dmy", "mdy")

# Excel's date origin for serial numbers (allows for its 1900 leap-year bug)
.excel_date_origin <- as.Date("1899-12-30")

#' Recover CAS Registry Numbers that Excel turned into dates
#'
#' Excel reads CASRNs such as `4292-10-8` as dates (year 4292, month 10,
#' day 8) and shows them in the local date format, e.g. `"08/10/4292"` with
#' UK settings (as in the CPDat v4.0 download) or `"10/8/4292"` with US
#' settings. These functions turn such dates back into CASRNs: the year
#' becomes the first block, the month the middle block and the day the check
#' digit.
#'
#' `parse_casrn_dates()` returns one row per value with the reasoning;
#' `convert_date_to_casrn()` returns just the corrected vector, for use inside
#' `dplyr::mutate()`.
#'
#' @details
#' ## Which CASRNs Excel turns into dates
#'
#' Only CASRNs whose first block is a year Excel accepts, whose middle block
#' is 01-12 and whose check digit is 1-9. CASRNs ending in 0, with a middle
#' block above 12, or with a first block of 5 or more digits stay as text.
#' Two-digit first blocks (CASRNs start at 50-00-0) are expanded to 1950-1999,
#' e.g. `75-09-2` becomes 2 Sep 1975.
#'
#' ## What is recognised as a date
#'
#' * Year-first dates, always read as year-month-day: `"4292-10-08"` (this is
#'   how date cells of an `.xlsx` file look when read with their cell types).
#' * Day- or month-first dates with `/`, `-` or `.` separators, read in the
#'   orders given in `date_order`: `"08/10/4292"`, `"10/8/4292"`.
#' * Dates with English month names: `"8-Oct-4292"`, `"Oct 8, 4292"`.
#' * With `excel_serial = TRUE`, Excel serial day numbers (e.g. `"873943"`),
#'   which is how date cells arrive when an `.xlsx` file is read with
#'   `readxl::read_excel(col_types = "text")`.
#'
#' A trailing time (e.g. `" 00:00"`) is ignored. Values already shaped like a
#' CASRN (e.g. `"4292-10-8"`) are never treated as dates.
#'
#' ## Choosing between readings
#'
#' A date can often be read more than one way: day and month may be swapped
#' (when `date_order` has both), and a year from 1950 to 1999 may come from a
#' two-digit first block. Every reading whose day is 1-9 becomes a candidate
#' CASRN, and only candidates with a correct check digit are kept:
#'
#' * exactly one valid candidate: `status = "converted"`;
#' * several: `status = "ambiguous"` (candidates listed, `casrn` is `NA`);
#' * none: `status = "no_valid_casrn"`. This includes month-year displays
#'   such as `"Oct-92"`, which have lost the check digit.
#'
#' A date shown with a two-digit year (e.g. `"02/09/75"`) has lost the rest
#' of the first block unless the CASRN started with two digits (50-99). Such
#' values are only converted when a two-digit first block gives a valid
#' CASRN, and are flagged `two_digit_year = TRUE`; check them by hand.
#'
#' A serial number could also be a CASRN written without hyphens. Serial
#' numbers are only treated as dates when that gives a valid CASRN; if the
#' digits are a valid CASRN as well, the value is `"ambiguous"`.
#'
#' With 1 in 10 random values passing the check digit, a conversion is only
#' as good as the evidence behind it: confirm converted CASRNs against the
#' chemical name where you can (e.g. with [verify_chem_identifiers()]).
#'
#' @param x Character vector of CASRNs, some of which may be dates.
#' @param date_order Character. How day- or month-first dates are read: `"dmy"`
#'   (UK and most of Europe, e.g. `"08/10/4292"`), `"mdy"` (US, e.g.
#'   `"10/8/4292"`), or both (default), in which case readings are kept only
#'   if they give a valid CASRN. Use `"dmy"` for the CPDat v4.0 download.
#' @param excel_serial Logical. Also treat 5-7 digit numbers as Excel serial
#'   dates. Default is `FALSE`.
#' @param keep_unconverted Logical. For `convert_date_to_casrn()`: what to
#'   return for dates that cannot be converted to a single valid CASRN. `FALSE`
#'   (default) gives `NA`; `TRUE` keeps the original value, so the problem
#'   stays visible downstream.
#'
#' @returns `parse_casrn_dates()`: a tibble with one row per value of `x` and
#'   columns `input`, `is_date`, `two_digit_year`, `status` (`"not_a_date"`,
#'   `"converted"`, `"ambiguous"`, `"no_valid_casrn"`, or `NA` for missing or
#'   blank input), `casrn` (the converted CASRN, otherwise `NA`) and
#'   `candidates` (valid candidate CASRNs, `" | "`-separated).
#'
#'   `convert_date_to_casrn()`: a character vector the same length as `x`.
#'   Converted dates are replaced by their CASRN and values that are not dates
#'   are returned unchanged. Dates that cannot be converted become `NA` (or
#'   stay as they were with `keep_unconverted = TRUE`), with a warning.
#'
#' @seealso [standardise_casrn()] for leading zeros and missing hyphens,
#'   [is_valid_casrn()] for the check-digit test.
#'
#' @export
#'
#' @examples
#' casrns <- c("08/10/4292", "4292-10-8", "02/09/1975", "Oct-92", "71-43-2", NA)
#'
#' parse_casrn_dates(casrns)
#' convert_date_to_casrn(casrns, date_order = "dmy") |> suppressWarnings()
#'
#' # Excel serial number for 8 Oct 4292
#' convert_date_to_casrn("873943", excel_serial = TRUE)
parse_casrn_dates <- function(
    x,
    date_order   = c("dmy", "mdy"),
    excel_serial = FALSE
) {

  ## Input validation ----

  if (!is.atomic(x)) {
    cli::cli_abort("{.arg x} must be an atomic vector, not {.cls {class(x)}}.")
  }

  if (!is.character(date_order) || !length(date_order) || !all(date_order %in% .casrn_date_orders)) {
    cli::cli_abort("{.arg date_order} must be {.val dmy}, {.val mdy} or both.")
  }

  if (!rlang::is_bool(excel_serial)) {
    cli::cli_abort("{.arg excel_serial} must be {.val TRUE} or {.val FALSE}.")
  }

  ## Parse each distinct value once ----

  values <- stringr::str_squish(as.character(x))
  unique_values <- unique(values[!is.na(values) & nzchar(values)])

  parsed <- purrr::map(
    unique_values,
    \(value) casrn_candidates_from_date(value, unique(date_order), excel_serial)
  )

  lookup <- tibble::tibble(
    value          = unique_values,
    is_date        = purrr::map_lgl(parsed, "is_date"),
    two_digit_year = purrr::map_lgl(parsed, "two_digit_year"),
    valid          = purrr::map(parsed, "valid")
  ) |>
    dplyr::mutate(
      n_valid = lengths(.data$valid),
      status = dplyr::case_when(
        !.data$is_date ~ "not_a_date",
        .data$n_valid == 1L ~ "converted",
        .data$n_valid > 1L ~ "ambiguous",
        .default = "no_valid_casrn"
      ),
      casrn = dplyr::if_else(
        .data$status == "converted",
        purrr::map_chr(.data$valid, \(valid) valid[1]),
        NA_character_
      ),
      candidates = dplyr::if_else(
        .data$n_valid > 0L,
        purrr::map_chr(.data$valid, \(valid) stringr::str_c(valid, collapse = " | ")),
        NA_character_
      )
    ) |>
    dplyr::select("value", "is_date", "two_digit_year", "status", "casrn", "candidates")

  ## One row per input value ----

  # Missing and blank values have no lookup row, so all their details are NA
  details <- tibble::tibble(input = as.character(x), value = values) |>
    dplyr::left_join(lookup, by = dplyr::join_by("value")) |>
    dplyr::select(-"value")

  return(details)
}

#' @rdname parse_casrn_dates
#' @export
convert_date_to_casrn <- function(
    x,
    date_order       = c("dmy", "mdy"),
    excel_serial     = FALSE,
    keep_unconverted = FALSE
) {
  if (!rlang::is_bool(keep_unconverted)) {
    cli::cli_abort("{.arg keep_unconverted} must be {.val TRUE} or {.val FALSE}.")
  }

  details <- parse_casrn_dates(x, date_order = date_order, excel_serial = excel_serial)

  is_failed <- details$status %in% c("ambiguous", "no_valid_casrn")

  converted <- dplyr::case_when(
    details$status %in% "converted" ~ details$casrn,
    is_failed & !keep_unconverted ~ NA_character_,
    .default = details$input
  )

  n_converted <- sum(details$status %in% "converted")

  if (n_converted > 0) {
    cli::cli_inform("Converted {n_converted} Excel date{?s} back to CASRN{?s}.")
  }

  if (any(is_failed)) {
    outcome <- if (keep_unconverted) "left unchanged" else "set to {.val NA}"
    cli::cli_warn(c(
      stringr::str_c(
        "{sum(is_failed)} date{?s} could not be converted to a single valid CASRN and {?was/were} ",
        outcome, "."
      ),
      "i" = "Use {.fn parse_casrn_dates} to see the candidates."
    ))
  }

  return(converted)
}

# Candidate CASRNs from one value. Returns a list: is_date (the value is
# shaped like a date), two_digit_year (a date shown with a two-digit year) and
# valid (candidate CASRNs with a correct check digit).
casrn_candidates_from_date <- function(value, date_order, excel_serial) {
  not_a_date <- list(is_date = FALSE, two_digit_year = FALSE, valid = character())

  # Already shaped like a CASRN: not a date
  if (stringr::str_detect(value, "^[0-9]{2,7}-[0-9]{2}-[0-9]$")) {
    return(not_a_date)
  }

  # Drop a trailing time, e.g. "08/10/4292 00:00" or "4292-10-08T00:00:00"
  value <- stringr::str_remove(
    value,
    "[ T][0-9]{1,2}:[0-9]{2}(:[0-9]{2}(\\.[0-9]+)?)?( ?[AaPp][Mm])?$"
  )

  ## Excel serial day numbers ----

  if (excel_serial && stringr::str_detect(value, "^[0-9]{5,7}(\\.0+)?$")) {
    return(casrn_candidates_from_serial(value))
  }

  ## Month-year displays ("Oct-92"): the day, and so the check digit, is lost ----

  month_year <- stringr::str_match(value, "^([A-Za-z]{3,9})[-/ ]([0-9]{2}|[0-9]{4})$")

  if (!is.na(month_year[1, 1]) && !is.na(month_from_name(month_year[1, 2]))) {
    return(list(is_date = TRUE, two_digit_year = nchar(month_year[1, 3]) == 2L, valid = character()))
  }

  ## Dates written out ----

  # Each reading is (year text, month, day)
  readings <- list()

  numeric_date <- stringr::str_match(value, "^([0-9]{1,4})[/.-]([0-9]{1,2})[/.-]([0-9]{1,4})$")

  if (!is.na(numeric_date[1, 1])) {
    parts <- numeric_date[1, 2:4]

    # Year first (ISO, or .xlsx date cells): always year-month-day. Otherwise
    # the year is last and day / month order comes from date_order.
    order_positions <- if (nchar(parts[[1]]) == 4L) {
      list(ymd = c(year = 1, month = 2, day = 3))
    } else {
      list(
        dmy = c(year = 3, month = 2, day = 1),
        mdy = c(year = 3, month = 1, day = 2)
      )[date_order]
    }

    readings <- purrr::map(
      order_positions,
      \(positions) {
        reading <- list(
          year = parts[[positions[["year"]]]],
          month = as.integer(parts[[positions[["month"]]]]),
          day = as.integer(parts[[positions[["day"]]]])
        )
        return(reading)
      }
    )
  }

  # Day, month name, year: "8-Oct-4292", "08 October 4292"
  day_month_name <- stringr::str_match(value, "^([0-9]{1,2})[-/ ]([A-Za-z]{3,9})\\.?[-/ ,]+([0-9]{2,4})$")

  if (!is.na(day_month_name[1, 1])) {
    readings <- c(readings, list(list(
      year = day_month_name[1, 4],
      month = month_from_name(day_month_name[1, 3]),
      day = as.integer(day_month_name[1, 2])
    )))
  }

  # Month name, day, year: "Oct 8, 4292"
  month_name_day <- stringr::str_match(value, "^([A-Za-z]{3,9})\\.?[-/ ]([0-9]{1,2}),?[-/ ]+([0-9]{2,4})$")

  if (!is.na(month_name_day[1, 1])) {
    readings <- c(readings, list(list(
      year = month_name_day[1, 4],
      month = month_from_name(month_name_day[1, 2]),
      day = as.integer(month_name_day[1, 3])
    )))
  }

  # Keep readings that are calendar-like: a 2- or 4-digit year, month 1-12,
  # day 1-31
  readings <- purrr::keep(
    readings,
    \(reading) {
      is_calendar_like <- nchar(reading$year) %in% c(2L, 4L) &&
        reading$month %in% 1:12 &&
        reading$day %in% 1:31
      return(is_calendar_like)
    }
  )

  if (!length(readings)) {
    return(not_a_date)
  }

  candidates <- purrr::map(
    readings,
    \(reading) date_parts_to_casrn(reading$year, reading$month, reading$day)
  ) |>
    unlist() |>
    unique()

  result <- list(
    is_date = TRUE,
    two_digit_year = any(purrr::map_int(readings, \(reading) nchar(reading$year)) == 2L),
    valid = candidates[is_valid_casrn(candidates)]
  )

  return(result)
}

# Candidate CASRNs from an Excel serial day number. Only a date if that gives
# a valid CASRN; the digits may be a valid CASRN without hyphens too.
casrn_candidates_from_serial <- function(value) {
  not_a_date <- list(is_date = FALSE, two_digit_year = FALSE, valid = character())

  serial_date <- .excel_date_origin + as.integer(as.numeric(value))

  serial_candidates <- date_parts_to_casrn(
    format(serial_date, "%Y"),
    as.integer(format(serial_date, "%m")),
    as.integer(format(serial_date, "%d"))
  )
  serial_valid <- serial_candidates[is_valid_casrn(serial_candidates)]

  if (!length(serial_valid)) {
    return(not_a_date)
  }

  digit_reading <- standardise_casrn(value)
  valid <- unique(c(serial_valid, digit_reading[is_valid_casrn(digit_reading)]))

  return(list(is_date = TRUE, two_digit_year = FALSE, valid = valid))
}

# Candidate CASRNs from one reading of a date. The day must be a single digit
# (the check digit). A four-digit year is the first block; a year from 1950
# to 1999 may also come from a two-digit first block (CASRNs start at
# 50-00-0). A two-digit year is only usable when it is that whole first block.
date_parts_to_casrn <- function(year_text, month, day) {
  if (is.na(month) || is.na(day) || day < 1L || day > 9L || month < 1L || month > 12L) {
    return(character())
  }

  year <- as.integer(year_text)

  first_blocks <- if (nchar(year_text) == 4L) year else integer()

  if (nchar(year_text) == 4L && year >= 1950L && year <= 1999L) {
    first_blocks <- c(first_blocks, year - 1900L)
  }

  if (nchar(year_text) == 2L && year >= 50L) {
    first_blocks <- year
  }

  candidates <- stringr::str_c(first_blocks, "-", sprintf("%02d", month), "-", day)

  return(candidates)
}

# Month number from an English month name or abbreviation ("Oct", "October")
month_from_name <- function(month_name) {
  month_number <- match(tolower(substr(month_name, 1, 3)), tolower(month.abb))

  return(month_number)
}
