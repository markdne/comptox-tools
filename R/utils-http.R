# Shared HTTP settings and helpers for the CompTox and PubChem search
# functions. None of these are exported.

# Base URLs for the two web services
.comptox_base_url <- "https://comptox.epa.gov/ctx-api"
.pubchem_base_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

# HTTP status codes worth retrying: rate limiting and temporary server
# errors (PubChem answers 503 "ServerBusy" when it is throttling)
.transient_status <- c(429L, 502L, 503L, 504L)

# Longest time (seconds) a single request keeps retrying before giving up
.max_retry_seconds <- 120

# Build a request with the shared retry and error settings
build_request <- function(url) {
  req <- httr2::request(url) |>
    # Retry rate-limit and temporary server errors with exponential back-off
    httr2::req_retry(
      max_seconds = .max_retry_seconds,
      is_transient = \(resp) httr2::resp_status(resp) %in% .transient_status
    ) |>
    # Return every HTTP response (404 included) rather than an error, so
    # callers can tell "not found" apart from "request failed"
    httr2::req_error(is_error = \(resp) FALSE)

  return(req)
}

# Perform a list of requests concurrently with a global rate limit.
# `rate_limit` is the minimum number of seconds between requests across all
# concurrent connections (httr2 shares one token bucket per host), so the
# aggregate rate never exceeds 1 / rate_limit requests per second.
# Returns a list the same length as `reqs`: httr2 responses, or condition
# objects for requests that failed at the network level.
perform_requests <- function(
    reqs,
    rate_limit = 0,
    max_active = 5L,
    progress   = FALSE
) {
  if (!length(reqs)) {
    return(list())
  }

  if (rate_limit > 0) {
    reqs <- purrr::map(
      reqs,
      \(req) httr2::req_throttle(req, capacity = 1, fill_time_s = rate_limit)
    )
  }

  resps <- httr2::req_perform_parallel(
    reqs,
    on_error   = "continue",
    progress   = progress,
    max_active = max_active
  )

  return(resps)
}

# Outcome of a performed request: "ok" (HTTP 2xx), "not_found" (HTTP 404) or
# "error" (any other status, or a network failure)
resp_outcome <- function(resp) {
  if (!inherits(resp, "httr2_response")) {
    return("error")
  }

  status <- httr2::resp_status(resp)

  outcome <- dplyr::case_when(
    status >= 200L & status < 300L ~ "ok",
    status == 404L ~ "not_found",
    .default = "error"
  )

  return(outcome)
}

# Parse a JSON response body; NULL when the body is empty
resp_json <- function(resp) {
  if (!httr2::resp_has_body(resp)) {
    return(NULL)
  }

  body <- httr2::resp_body_string(resp)

  if (!nzchar(trimws(body))) {
    return(NULL)
  }

  parsed <- jsonlite::fromJSON(body, flatten = TRUE)

  return(parsed)
}

# An empty list in place of NULL (e.g. an empty response body)
null_to_list <- function(x) if (is.null(x)) list() else x

# Split a vector into chunks of at most `chunk_size` elements
chunk_vector <- function(x, chunk_size) {
  chunks <- split(x, ceiling(seq_along(x) / chunk_size))

  return(unname(chunks))
}

# Unique, whitespace-squished, non-missing, non-blank identifiers from a
# character vector or from the selected columns of a data frame. Identifier
# columns are converted to character before stacking so that, e.g., a numeric
# ID column does not break pivot_longer().
extract_chem_ids <- function(input_data, chem_id_cols) {
  if (is.data.frame(input_data)) {
    id_col_names <- names(dplyr::select(input_data, {{ chem_id_cols }}))

    chem_ids <- input_data |>
      dplyr::select(dplyr::all_of(id_col_names)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(
        cols      = dplyr::everything(),
        values_to = "chem_ids"
      ) |>
      dplyr::pull("chem_ids")
  } else {
    chem_ids <- as.character(input_data)
  }

  chem_ids <- stringr::str_squish(chem_ids)
  chem_ids <- unique(chem_ids[!is.na(chem_ids) & nzchar(chem_ids)])

  return(chem_ids)
}

# Validate the shared numeric arguments -----------------------------------

# A CompTox API key: a single non-empty string
check_api_key <- function(api_key, call = rlang::caller_env()) {
  is_valid_key <- is.character(api_key) && length(api_key) == 1L && !is.na(api_key) && nzchar(api_key)

  if (!is_valid_key) {
    cli::cli_abort(
      c(
        "{.arg api_key} must be a non-empty string.",
        "i" = "Pass it directly or set the {.envvar CTX_API_KEY} environment variable."
      ),
      call = call
    )
  }

  return(invisible(api_key))
}

# A single positive whole number, returned as an integer
check_count <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x != round(x)) {
    cli::cli_abort("{.arg {arg}} must be a single positive whole number.", call = call)
  }

  return(as.integer(x))
}

# A single non-negative number of seconds
check_rate_limit <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) {
    cli::cli_abort("{.arg {arg}} must be a single non-negative number of seconds.", call = call)
  }

  return(x)
}
