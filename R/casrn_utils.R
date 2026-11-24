#' Check CAS Registry Numbers
#'
#' Tests whether each value is a well-formed CASRN (2-7 digits, 2 digits and
#' a check digit, separated by hyphens, no leading zero) whose check digit is
#' correct. The check digit is the sum of the other digits, each multiplied by
#' its position counted from the right, modulo 10.
#'
#' @param casrn Character vector of CASRNs.
#'
#' @returns A logical vector the same length as `casrn`: `TRUE` for valid
#'   CASRNs, `FALSE` otherwise (including `NA`).
#'
#' @export
#'
#' @examples
#' is_valid_casrn(c("71-43-2", "71-43-3", "0071-43-2", "4292-10-8", NA))
is_valid_casrn <- function(casrn) {
  if (!is.atomic(casrn)) {
    cli::cli_abort("{.arg casrn} must be an atomic vector, not {.cls {class(casrn)}}.")
  }

  casrn <- as.character(casrn)

  well_formed <- stringr::str_detect(casrn, "^[1-9][0-9]{1,6}-[0-9]{2}-[0-9]$") %in% TRUE

  # Check digits are only computed for well-formed values
  is_valid <- purrr::map2_lgl(
    casrn,
    well_formed,
    \(cas_value, is_well_formed) {
      if (!is_well_formed) {
        return(FALSE)
      }
      digits <- as.integer(strsplit(stringr::str_remove_all(cas_value, "-"), "")[[1]])
      body <- rev(utils::head(digits, -1))
      return(sum(body * seq_along(body)) %% 10 == utils::tail(digits, 1))
    }
  )

  return(is_valid)
}

#' Put CAS Registry Numbers in standard form
#'
#' Removes spaces and leading zeros and adds hyphens to digit-only values, so
#' that `"0000071-43-2"`, `" 71-43-2 "` and `"71432"` all become `"71-43-2"`.
#' Values that do not look like a CASRN are returned unchanged. Run
#' [convert_date_to_casrn()] first if Excel may have turned CASRNs into dates.
#'
#' @param casrn Character vector of CASRNs.
#'
#' @returns A character vector the same length as `casrn`.
#'
#' @export
#'
#' @examples
#' standardise_casrn(c("0000071-43-2", "71432", " 50-00-0", "not a CASRN", NA))
standardise_casrn <- function(casrn) {
  if (!is.atomic(casrn)) {
    cli::cli_abort("{.arg casrn} must be an atomic vector, not {.cls {class(casrn)}}.")
  }

  casrn <- stringr::str_remove_all(as.character(casrn), "\\s")

  # Digit-only values: the last digit is the check digit, the two before it
  # the middle block
  digits_only <- stringr::str_detect(casrn, "^[0-9]{5,10}$") %in% TRUE

  casrn[digits_only] <- stringr::str_replace(
    casrn[digits_only],
    "^([0-9]+)([0-9]{2})([0-9])$",
    "\\1-\\2-\\3"
  )

  casrn <- stringr::str_replace(casrn, "^0+([1-9][0-9]*-[0-9]{2}-[0-9])$", "\\1")

  return(casrn)
}
