# Output schema of comptox_chem_search(). Defined once at package level so the
# column names and types live in one place.
.comptox_output_cols <- c(
  "input_term", "dtxsid", "dtxcid", "casrn", "preferredName", "smiles",
  "isMarkush", "hasStructureImage", "searchName", "searchValue", "rank",
  "suggestions", "search_status"
)

# One-row template of typed NAs, used for unmatched identifiers and to fill
# columns missing from an API response
.comptox_na_template <- tibble::tibble(
  input_term        = NA_character_,
  dtxsid            = NA_character_,
  dtxcid            = NA_character_,
  casrn             = NA_character_,
  preferredName     = NA_character_,
  smiles            = NA_character_,
  isMarkush         = NA,
  hasStructureImage = NA,
  searchName        = NA_character_,
  searchValue       = NA_character_,
  rank              = NA_integer_,
  suggestions       = NA_character_,
  search_status     = NA_character_
)

#' Search the EPA CompTox Chemicals Dashboard API for chemical identifiers
#'
#' Retrieves chemical data (DTXSID, SMILES, CASRN, preferred name, etc.) from
#' the CTX Chemical API for one or more identifiers. Inputs can be chemical
#' names, CAS registry numbers, DTXSIDs, InChIKeys or any identifier the API
#' recognises.
#'
#' With `batch = TRUE` (the default) identifiers are sent to the `POST` batch
#' endpoint in chunks of `chunk_size`. Identifiers ending in a hyphen (`"-"`)
#' are sent as individual `GET` requests instead, because the `POST` endpoint
#' strips trailing hyphens before searching. With `retry_no_hits = TRUE`,
#' identifiers missing from the batch response, or returned with no match and
#' no suggestions, are retried with `GET`.
#'
#' Requests are made concurrently (up to `max_active` at a time). Rate-limited
#' (HTTP 429) and temporary server errors (HTTP 502-504) are retried with
#' exponential back-off for up to two minutes.
#'
#' When an identifier cannot be matched the API may return candidate names in
#' the `suggestions` column, which can be passed back to this function:
#'
#' ```r
#' no_hits <- dplyr::filter(results, is.na(dtxsid), !is.na(suggestions))
#' retry <- comptox_chem_search(no_hits$suggestions, api_key = key)
#' ```
#'
#' @param input_data A data frame or a character vector of chemical
#'   identifiers.
#' @param search_type Character. Matching strategy: `"equal_to"` (default),
#'   `"starts_with"` or `"contains"`.
#' @param api_key Character. Your CTX API key. Defaults to the `CTX_API_KEY`
#'   environment variable.
#' @param chem_id_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Column(s) of
#'   `input_data` holding identifiers. Required when `input_data` is a data
#'   frame; ignored for vector input.
#' @param batch Logical. If `TRUE` (default) identifiers are sent in batches
#'   via `POST`; if `FALSE` each identifier is a separate `GET` request.
#' @param chunk_size Positive whole number. Maximum identifiers per `POST`
#'   request. Default is `200`.
#' @param rate_limit Non-negative number. Minimum seconds between requests,
#'   applied globally across all concurrent requests. Default is `0` (no
#'   throttling).
#' @param sorted Logical. If `TRUE` (default) rows are returned in the order of
#'   the (de-duplicated) input identifiers.
#' @param retry_no_hits Logical. Only used when `batch = TRUE`. If `TRUE`
#'   (default) identifiers absent from the `POST` response, or returned with no
#'   match and no suggestions, are retried with `GET`.
#' @param max_active Positive whole number. Maximum concurrent requests.
#'   Default is `5`.
#' @param progress Logical or string. Show a progress bar (a string names the
#'   bar). Default is `interactive()`.
#'
#' @returns A tibble with one or more rows per input identifier and columns
#'   `input_term`, `dtxsid`, `dtxcid`, `casrn`, `preferredName`, `smiles`,
#'   `isMarkush`, `hasStructureImage`, `searchName`, `searchValue`, `rank`,
#'   `suggestions` and `search_status`. `suggestions` holds a `" | "`-separated
#'   string of candidate names for unmatched identifiers. `search_status` is
#'   `"found"`, `"not_found"` (the API answered but found nothing) or
#'   `"error"` (the request failed after retries); identifiers with status
#'   `"error"` should be searched again rather than treated as absent from
#'   CompTox. Every input identifier appears in the output. An empty tibble
#'   with these columns is returned when there are no usable identifiers.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   chem_name = c("1-Chloro-4-nitrobenzene", "4-Nitrobenzenamine"),
#'   casrn = c("100-00-5", "100-01-6")
#' )
#'
#' # Search both identifier columns (API key read from CTX_API_KEY)
#' results <- comptox_chem_search(df, chem_id_cols = c(chem_name, casrn))
#'
#' # Character vector, individual GET requests
#' results <- comptox_chem_search(c("100-00-5", "100-01-6"), batch = FALSE)
#' }
comptox_chem_search <- function(
    input_data,
    search_type   = "equal_to",
    api_key       = Sys.getenv("CTX_API_KEY"),
    chem_id_cols,
    batch         = TRUE,
    chunk_size    = 200L,
    rate_limit    = 0,
    sorted        = TRUE,
    retry_no_hits = TRUE,
    max_active    = 5L,
    progress      = interactive()
) {

  ## Input validation ----

  check_api_key(api_key)

  if (!is.data.frame(input_data) && !is.character(input_data)) {
    cli::cli_abort(
      "{.arg input_data} must be a data frame or a character vector, not {.cls {class(input_data)}}."
    )
  }

  if (is.data.frame(input_data) && missing(chem_id_cols)) {
    cli::cli_abort("{.arg chem_id_cols} is required when {.arg input_data} is a data frame.")
  }

  search_type <- match.arg(
    tolower(search_type),
    choices = c("equal_to", "starts_with", "contains")
  )
  chunk_size <- check_count(chunk_size)
  max_active <- check_count(max_active)
  rate_limit <- check_rate_limit(rate_limit)

  ## Extract and sanitise unique identifiers ----

  # Sanitised here so that `chem_ids` and the API's `searchValue` refer to
  # the same strings
  chem_ids <- extract_chem_ids(input_data, {{ chem_id_cols }})

  if (!length(chem_ids)) {
    return(.comptox_na_template[0, ])
  }

  ## API set-up ----

  search_url <- stringr::str_c(
    .comptox_base_url,
    switch(
      search_type,
      equal_to    = "/chemical/search/equal/",
      starts_with = "/chemical/search/start-with/",
      contains    = "/chemical/search/contain/"
    )
  )

  # Shared settings for both request types; the key is redacted when a
  # request is printed
  comptox_request <- function(url) {
    req <- build_request(url) |>
      httr2::req_headers(
        `x-api-key` = api_key,
        Accept      = "application/json",
        .redact     = "x-api-key"
      )

    return(req)
  }

  ## Search ----

  if (!batch) {
    results <- comptox_get_ids(
      chem_ids,
      search_url      = search_url,
      comptox_request = comptox_request,
      rate_limit      = rate_limit,
      max_active      = max_active,
      progress        = progress
    )
  } else {
    # The POST endpoint strips trailing hyphens before searching, so those
    # identifiers go to individual GET requests
    trailing_hyphen <- stringr::str_ends(chem_ids, "-")
    ids_for_get <- chem_ids[trailing_hyphen]
    ids_for_post <- chem_ids[!trailing_hyphen]

    if (any(trailing_hyphen)) {
      cli::cli_inform(
        "{sum(trailing_hyphen)} identifier{?s} ending in {.val -} will be searched individually via GET."
      )
    }

    post <- comptox_post_ids(
      ids_for_post,
      search_url      = search_url,
      comptox_request = comptox_request,
      chunk_size      = chunk_size,
      rate_limit      = rate_limit,
      max_active      = max_active,
      progress        = progress
    )
    post_results <- post$results

    # Retry identifiers that the batch endpoint dropped, failed on, or
    # answered with no match and no suggestions. Identifiers that already
    # have suggestions are not retried: a GET could overwrite them with a
    # less informative answer.
    if (retry_no_hits && length(ids_for_post)) {
      empty_ids <- post_results |>
        dplyr::filter(
          is.na(.data$dtxsid),
          is.na(.data$smiles),
          is.na(.data$suggestions)
        ) |>
        dplyr::pull("input_term")
      missing_ids <- setdiff(ids_for_post, post_results$input_term)
      retry_ids <- unique(c(empty_ids, missing_ids))

      if (length(retry_ids)) {
        cli::cli_inform(
          "Retrying {length(retry_ids)} identifier{?s} with no batch result via individual GET."
        )
        ids_for_get <- unique(c(ids_for_get, retry_ids))
        post_results <- dplyr::filter(post_results, !.data$input_term %in% retry_ids)
      }
    }

    get_results <- comptox_get_ids(
      ids_for_get,
      search_url      = search_url,
      comptox_request = comptox_request,
      rate_limit      = rate_limit,
      max_active      = max_active,
      progress        = progress
    )

    results <- dplyr::bind_rows(post_results, get_results)

    # Identifiers still absent (only possible with retry_no_hits = FALSE):
    # "error" if their batch failed, otherwise "not_found"
    absent_ids <- setdiff(chem_ids, results$input_term)

    if (length(absent_ids)) {
      absent_rows <- tibble::tibble(input_term = absent_ids) |>
        dplyr::mutate(
          search_status = dplyr::if_else(
            .data$input_term %in% post$failed_ids,
            "error",
            "not_found"
          )
        )
      results <- dplyr::bind_rows(
        results,
        fill_comptox_cols(absent_rows)
      )
    }
  }

  ## Report failed requests ----

  n_error <- sum(results$search_status == "error")

  if (n_error > 0) {
    cli::cli_warn(c(
      "{n_error} identifier{?s} could not be searched because the request failed.",
      "i" = "These rows have {.code search_status == \"error\"}; search them again later."
    ))
  }

  ## Sort output to match input order ----

  if (sorted) {
    results <- dplyr::arrange(results, match(.data$input_term, chem_ids))
  }

  return(results)
}

# Add any missing output columns as typed NAs and put columns in schema order.
# All-null JSON fields arrive as logical NA; these are coerced to the schema
# type so that later bind_rows() calls see consistent types.
fill_comptox_cols <- function(results) {
  missing_cols <- setdiff(.comptox_output_cols, names(results))

  if (length(missing_cols)) {
    results <- dplyr::bind_cols(
      results,
      .comptox_na_template[rep(1L, nrow(results)), missing_cols]
    )
  }

  results <- results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("input_term", "dtxsid", "dtxcid", "casrn", "preferredName", "smiles",
                        "searchName", "searchValue", "suggestions", "search_status")),
        as.character
      ),
      dplyr::across(dplyr::all_of(c("isMarkush", "hasStructureImage")), as.logical),
      rank = as.integer(.data$rank)
    ) |>
    dplyr::select(dplyr::all_of(.comptox_output_cols))

  return(results)
}

# Parse a CompTox search response into the output schema. Suggestions arrive
# as a list-column and are collapsed to one " | "-separated string per row.
parse_comptox_search <- function(resp) {
  parsed <- resp_json(resp)

  if (is.null(parsed) || !length(parsed)) {
    return(.comptox_na_template[0, ])
  }

  results <- tibble::as_tibble(parsed)

  if ("suggestions" %in% names(results) && is.list(results$suggestions)) {
    results <- dplyr::mutate(
      results,
      suggestions = purrr::map_chr(
        .data$suggestions,
        \(candidates) {
          candidates <- unlist(candidates)
          if (!length(candidates) || all(is.na(candidates))) {
            return(NA_character_)
          }
          return(stringr::str_c(candidates, collapse = " | "))
        }
      )
    )
  }

  results <- results |>
    dplyr::mutate(search_status = NA_character_) |>
    fill_comptox_cols() |>
    dplyr::mutate(
      search_status = dplyr::if_else(!is.na(.data$dtxsid), "found", "not_found")
    )

  return(results)
}

# Search identifiers one at a time with GET. Every identifier gets at least
# one row: a parsed result, or an NA row with status "not_found" / "error".
comptox_get_ids <- function(
    ids,
    search_url,
    comptox_request,
    rate_limit,
    max_active,
    progress
) {
  if (!length(ids)) {
    return(.comptox_na_template[0, ])
  }

  reqs <- purrr::map(
    ids,
    \(chem_id) {
      comptox_request(stringr::str_c(search_url, utils::URLencode(chem_id, reserved = TRUE)))
    }
  )

  resps <- perform_requests(
    reqs,
    rate_limit = rate_limit,
    max_active = max_active,
    progress   = if (isTRUE(progress)) "CompTox: GET" else progress
  )

  results <- purrr::map2(
    resps,
    ids,
    \(resp, chem_id) {
      outcome <- resp_outcome(resp)

      if (outcome == "ok") {
        parsed <- parse_comptox_search(resp)
        if (nrow(parsed)) {
          return(dplyr::mutate(parsed, input_term = chem_id))
        }
        outcome <- "not_found"
      }

      na_row <- dplyr::mutate(
        .comptox_na_template,
        input_term = chem_id,
        search_status = outcome
      )

      return(na_row)
    }
  ) |>
    purrr::list_rbind()

  return(results)
}

# Search identifiers in POST batches. Returns a list with the parsed `results`
# and the `failed_ids` whose batch request failed.
comptox_post_ids <- function(
    ids,
    search_url,
    comptox_request,
    chunk_size,
    rate_limit,
    max_active,
    progress
) {
  if (!length(ids)) {
    return(list(results = .comptox_na_template[0, ], failed_ids = character()))
  }

  chunks <- chunk_vector(ids, chunk_size)

  reqs <- purrr::map(
    chunks,
    \(chunk) {
      req <- comptox_request(search_url) |>
        httr2::req_body_raw(stringr::str_c(chunk, collapse = "\n"), type = "application/json")
      return(req)
    }
  )

  resps <- perform_requests(
    reqs,
    rate_limit = rate_limit,
    max_active = max_active,
    progress   = if (isTRUE(progress)) "CompTox: batch POST" else progress
  )

  outcomes <- purrr::map_chr(resps, resp_outcome)
  failed_ids <- unlist(chunks[outcomes != "ok"], use.names = FALSE)

  if (any(outcomes != "ok")) {
    cli::cli_warn(
      "{sum(outcomes != 'ok')} batch POST request{?s} failed ({length(failed_ids)} identifier{?s})."
    )
  }

  results <- resps[outcomes == "ok"] |>
    purrr::map(parse_comptox_search) |>
    purrr::list_rbind()

  if (!nrow(results)) {
    return(list(results = .comptox_na_template[0, ], failed_ids = failed_ids))
  }

  # The API's searchValue says which input each result belongs to. Match it
  # case-insensitively in case the API normalises letter case; results that
  # match no input are dropped (their input is then retried or reported).
  results <- results |>
    dplyr::mutate(
      input_term = ids[match(toupper(.data$searchValue), toupper(ids))]
    ) |>
    dplyr::filter(!is.na(.data$input_term))

  return(list(results = results, failed_ids = failed_ids))
}

#' Collapse duplicate chemical search results into one row per chemical
#'
#' When [comptox_chem_search()] is called with several identifier columns
#' (e.g. chemical name *and* CASRN), different `input_term` values can resolve
#' to the same chemical. This function collapses those rows into one row per
#' chemical, joining their `input_term` values with `collapse_sep`.
#'
#' When `input_data` is supplied, its non-identifier columns are joined back
#' onto the results and de-duplication happens *within* each original input
#' row, so rows from different inputs are never merged. The collapsed
#' `input_term` then follows the order of `chem_id_cols`. Without
#' `input_data`, terms containing a letter (names) come before purely numeric
#' or symbolic terms (CASRNs), alphabetically within each group.
#'
#' Rows whose `by` value is `NA` (no chemical matched) are not collapsed;
#' they are returned unchanged in their input position.
#'
#' @param results A tibble returned by [comptox_chem_search()].
#' @param input_data Optional. The data frame or character vector passed to
#'   [comptox_chem_search()].
#' @param chem_id_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> The column
#'   selection passed to [comptox_chem_search()]. Required when `input_data` is
#'   a data frame.
#' @param by Character. Column used as the chemical key: `"dtxsid"` (default),
#'   `"dtxcid"`, `"casrn"` or `"preferredName"`.
#' @param collapse_sep Character. Separator between collapsed `input_term`
#'   values. Default is `" | "`.
#'
#' @returns A tibble with one row per chemical (per original input row when
#'   `input_data` is a data frame), in input order. CompTox columns keep the
#'   value of the first matching result; non-identifier columns of
#'   `input_data` are appended when it is supplied. Unmatched rows are kept
#'   uncollapsed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   chem_name = c("1-Chloro-4-nitrobenzene", "Benzene"),
#'   casrn = c("100-00-5", "71-43-2"),
#'   conc_mg_l = c(0.5, 1.2)
#' )
#'
#' results <- comptox_chem_search(df, chem_id_cols = c(chem_name, casrn))
#'
#' deduped <- deduplicate_chem_results(
#'   results,
#'   input_data = df,
#'   chem_id_cols = c(chem_name, casrn)
#' )
#' }
deduplicate_chem_results <- function(
    results,
    input_data,
    chem_id_cols,
    by           = "dtxsid",
    collapse_sep = " | "
) {

  ## Input validation ----

  if (!is.data.frame(results)) {
    cli::cli_abort(
      "{.arg results} must be a data frame (the output of {.fn comptox_chem_search})."
    )
  }

  by <- match.arg(by, c("dtxsid", "dtxcid", "casrn", "preferredName"))

  if (!by %in% names(results)) {
    cli::cli_abort(
      "Column {.val {by}} not found in {.arg results}; is it the output of {.fn comptox_chem_search}?"
    )
  }

  if (!nrow(results)) {
    return(results)
  }

  # `.result_row` keeps the original order of `results` for the final sort
  results <- dplyr::mutate(results, .result_row = dplyr::row_number())

  group_vars <- by
  has_col_order <- FALSE

  ## Optionally join non-identifier columns from the original input ----

  if (!missing(input_data) && !is.null(input_data)) {

    if (is.data.frame(input_data)) {

      if (missing(chem_id_cols)) {
        cli::cli_abort("{.arg chem_id_cols} is required when {.arg input_data} is a data frame.")
      }

      # Identifier columns in the order given; this order sets which term
      # comes first in the collapsed input_term string
      id_col_names <- names(dplyr::select(input_data, {{ chem_id_cols }}))

      # One row per (original input row x identifier value). Values are
      # squished exactly as comptox_chem_search() squishes them, otherwise
      # identifiers with stray spaces never join back to their results.
      lookup <- input_data |>
        dplyr::mutate(
          .orig_row = dplyr::row_number(),
          dplyr::across(dplyr::all_of(id_col_names), as.character)
        ) |>
        tidyr::pivot_longer(
          cols      = dplyr::all_of(id_col_names),
          names_to  = ".id_col",
          values_to = "input_term"
        ) |>
        dplyr::mutate(
          input_term = stringr::str_squish(.data$input_term),
          .col_order = match(.data$.id_col, id_col_names)
        ) |>
        dplyr::filter(!is.na(.data$input_term), nzchar(.data$input_term)) |>
        dplyr::select(-".id_col")

      results <- dplyr::left_join(
        results,
        lookup,
        by = dplyr::join_by("input_term"),
        suffix = c("", ".orig"),
        relationship = "many-to-many"
      )

      group_vars <- c(".orig_row", by)
      has_col_order <- TRUE

    } else if (!is.atomic(input_data)) {
      cli::cli_abort("{.arg input_data} must be a data frame or an atomic vector.")
    }
  }

  ## Split on whether the key column is populated ----

  keyed <- dplyr::filter(results, !is.na(.data[[by]]))
  unkeyed <- dplyr::filter(results, is.na(.data[[by]]))

  ## Collapse duplicate rows ----

  # Helper columns that must not be summarised with first()
  helper_cols <- c(".col_order", ".result_row")

  if (nrow(keyed)) {
    keyed <- keyed |>
      dplyr::summarise(
        # Order terms by source column (or letters-first), then drop repeats
        input_term = {
          term_order <- if (has_col_order) {
            order(.data$.col_order)
          } else {
            order(!stringr::str_detect(.data$input_term, "[A-Za-z]"), .data$input_term)
          }
          terms <- .data$input_term[term_order]
          stringr::str_c(unique(terms), collapse = collapse_sep)
        },
        .result_row = min(.data$.result_row),
        dplyr::across(!dplyr::any_of(c("input_term", helper_cols)), dplyr::first),
        .by = dplyr::all_of(group_vars)
      ) |>
      dplyr::relocate("input_term")
  }

  ## Restore input order and drop helper columns ----

  sort_col <- if (has_col_order) ".orig_row" else ".result_row"

  deduped <- dplyr::bind_rows(keyed, unkeyed) |>
    dplyr::arrange(.data[[sort_col]]) |>
    dplyr::select(-dplyr::any_of(c(".orig_row", ".result_row", ".col_order")))

  return(deduped)
}
