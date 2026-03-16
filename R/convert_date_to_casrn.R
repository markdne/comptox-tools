#' Convert Dates Back to CAS Registry Numbers
#'
#' This function converts date strings that were mistakenly converted from CAS registry numbers back to their original CAS registry number format.
#'
#' @param date_str A character vector or a dataframe column containing combination of CASRNs and date strings.
#'
#' @return A character vector with dates converted back to CASRNs, where applicable.
#'
#' @examples
#' # Example with a vector
#' date_vector <- c("3/1/2038", "100-00-5", "3/7/2680", "10/9/3546", "79-06-1")
#' convert_date_to_casrn(date_vector)
#'
#' # Example with a dataframe
#' df <- data.frame(
#'   id = 1:5,
#'   casrn = c("3/1/2038", "100-00-5", "3/7/2680", "10/9/3546", "79-06-1"),
#'   stringsAsFactors = FALSE
#' )
#' df$cas_numbers <- convert_date_to_casrn(df$cas_numbers)
#'
#' @importFrom lubridate mdy
#' @importFrom stringr str_detect str_remove
#' @import magrittr
#'
#' @export
convert_date_to_casrn <- function(date_str) {
  # Ensure date_str is character
  date_str <- as.character(date_str)

  # Check which strings contain a forward slash - added by Excel when converting to date
  contains_slash <- stringr::str_detect(date_str, "/")

  # Initialize the result vector
  date_converted <- date_str

  # Convert only the strings that contain a forward slash
  if (any(contains_slash)) {
    date_converted[contains_slash] <- date_str[contains_slash] %>%
      lubridate::mdy() %>%
      format("%Y-%m-%d") %>%
      stringr::str_remove("(?<=-\\d{2}-)0")
  }

  return(date_converted)
}
