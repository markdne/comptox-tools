# Package-level constants for the CompTox search output schema.
# Defined once so they are never reconstructed on every function call and are
# accessible to both exported functions in this file.
.comptox_output_cols         <- c(
  "input_term", "dtxsid", "dtxcid", "casrn", "preferredName", "smiles",
  "isMarkush", "hasStructureImage", "searchName", "searchValue", "rank",
  "suggestions"
)
.comptox_logical_output_cols <- c("isMarkush", "hasStructureImage")
.comptox_char_output_cols    <- setdiff(.comptox_output_cols, .comptox_logical_output_cols)


#' Search the EPA CompTox Chemical Dashboard API for chemical information
#'
#' Retrieves chemical data (DTXSID, SMILES, CAS-RN, preferred name, etc.) from
#' the EPA CompTox Chemical Dashboard API for one or more identifiers. Inputs
#' can be chemical names, CAS registry numbers, DTXSIDs, or any identifier
#' recognised by the API.
#'
#' When \code{batch = TRUE} (the default), identifiers are sent to the
#' \code{POST} batch endpoint in chunks of \code{chunk_size}. Identifiers whose
#' name ends with a hyphen (\code{"-"}) are automatically routed to individual
#' \code{GET} calls instead, because the \code{POST} endpoint strips trailing
#' hyphens before searching, which causes those chemicals to return no results.
#' Any other identifiers that return no results from the batch search can also
#' be retried via individual \code{GET} when \code{retry_no_hits = TRUE}.
#'
#' When an identifier cannot be matched, the API may return candidate names in
#' the \code{suggestions} column of the result. These can be inspected and
#' re-queried directly:
#' \preformatted{
#' no_hits <- dplyr::filter(results, is.na(dtxsid), !is.na(suggestions))
#' retry   <- comptox_chem_search(no_hits$suggestions, api_key = key)
#' }
#'
#' @param input_data A data frame/tibble or a character vector of chemical
#'   identifiers.
#' @param search_type Character. The matching strategy. One of
#'   \code{"equal_to"} (default), \code{"starts_with"}, or \code{"contains"}.
#'   See \url{https://api-ccte.epa.gov/docs/chemical.html} for details.
#' @param api_key Character. Your CompTox Dashboard API key.
#' @param chem_id_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Column(s)
#'   in \code{input_data} that contain chemical identifiers. Required when
#'   \code{input_data} is a data frame; ignored for vector input.
#' @param batch Logical. If \code{TRUE} (default) identifiers are sent in
#'   batches via \code{POST} for efficiency. If \code{FALSE} each identifier
#'   is queried individually via \code{GET}.
#' @param chunk_size Positive integer. Maximum number of identifiers per
#'   \code{POST} batch request. Default is \code{200}.
#' @param rate_limit Non-negative numeric. Seconds to pause between API
#'   requests \emph{per worker}. Default is \code{0}. When parallel execution
#'   is active (via a \code{future} plan with multiple workers), each worker
#'   sleeps independently after its own request; to enforce a strict global
#'   rate, either scale \code{rate_limit} proportionally to the number of
#'   workers or set \code{future::plan(future::sequential)} before calling
#'   this function.
#' @param sorted Logical. If \code{TRUE} (default) output rows are returned in
#'   the same order as the (deduplicated) input identifiers.
#' @param retry_no_hits Logical. Applies only when \code{batch = TRUE}. If
#'   \code{TRUE} (default), any identifier that returns no results from the
#'   batch \code{POST} is automatically retried via an individual \code{GET}
#'   request.
#'
#' @return A [tibble][tibble::tibble] with columns \code{input_term},
#'   \code{dtxsid}, \code{dtxcid}, \code{casrn}, \code{preferredName},
#'   \code{smiles}, \code{isMarkush}, \code{hasStructureImage},
#'   \code{searchName}, \code{searchValue}, \code{rank}, and
#'   \code{suggestions}. The \code{suggestions} column is \code{NA} for
#'   matched chemicals; for unmatched identifiers it contains a
#'   \code{" | "}-separated string of candidate names returned by the API
#'   that can be passed back to \code{comptox_chem_search()} for re-querying.
#'   Returns an empty tibble when \code{input_data} contains no usable
#'   identifiers.
#'
#' @export
#' @importFrom dplyr distinct pick everything pull mutate select filter
#'   bind_rows arrange any_of where
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom purrr list_rbind safely map map_chr map_lgl
#' @importFrom furrr future_map
#' @importFrom progressr progressor
#' @importFrom httr GET POST add_headers content stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @importFrom stringr str_squish str_c
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang abort warn inform
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   chem_name = c("1-Chloro-4-nitrobenzene", "4-Nitrobenzenamine", "4-Nitrophenol"),
#'   casrn     = c("100-00-5", "100-01-6", "100-02-7")
#' )
#'
#' # Single-column search using batch POST (default)
#' results <- comptox_chem_search(
#'   input_data   = df,
#'   search_type  = "equal_to",
#'   api_key      = "your_api_key_here",
#'   chem_id_cols = chem_name
#' )
#'
#' # Multi-column search (all unique IDs across both columns are searched)
#' results <- comptox_chem_search(
#'   input_data   = df,
#'   search_type  = "equal_to",
#'   api_key      = "your_api_key_here",
#'   chem_id_cols = c(chem_name, casrn)
#' )
#'
#' # Character vector input, individual GET requests, with a progress bar
#' results <- progressr::with_progress({
#'   comptox_chem_search(
#'     input_data = c("100-00-5", "100-01-6"),
#'     api_key    = "your_api_key_here",
#'     batch      = FALSE
#'   )
#' })
#' }
comptox_chem_search <- function(
    input_data,
    search_type   = "equal_to",
    api_key,
    chem_id_cols,
    batch         = TRUE,
    chunk_size    = 200L,
    rate_limit    = 0,
    sorted        = TRUE,
    retry_no_hits = TRUE
) {

  ## Input validation ---------------------------------------------------------

  if (missing(api_key)) rlang::abort("Please provide your API key via `api_key`.")

  if (!is.data.frame(input_data) && !is.character(input_data))
    rlang::abort("`input_data` must be a data frame/tibble or a character vector.")

  search_type <- match.arg(
    tolower(search_type),
    choices = c("equal_to", "starts_with", "contains")
  )

  if (!is.numeric(rate_limit) || length(rate_limit) != 1L || rate_limit < 0) {
    rlang::warn("`rate_limit` must be a non-negative number; setting to 0.")
    rate_limit <- 0
  }

  if (!is.numeric(chunk_size) || length(chunk_size) != 1L)
    rlang::abort("`chunk_size` must be a single positive integer.")
  chunk_size <- as.integer(chunk_size)
  if (is.na(chunk_size) || chunk_size < 1L)
    rlang::abort("`chunk_size` must be a positive integer.")

  ## API setup ----------------------------------------------------------------

  base_url <- "https://comptox.epa.gov/ctx-api"

  url_path <- switch(
    search_type,
    equal_to    = "/chemical/search/equal/",
    starts_with = "/chemical/search/start-with/",
    contains    = "/chemical/search/contain/"
  )

  api_headers <- httr::add_headers(
    "x-api-key"    = api_key,
    "Content-Type" = "application/json",
    "Accept"       = "application/json"
  )

  ## Internal helpers ---------------------------------------------------------

  # Parse an httr response body into a tibble with standardised columns.
  # Two sources of type-mismatch can cause dplyr::bind_rows() to fail when
  # combining POST and GET results:
  #   1. A column is entirely absent from the response — added as a typed NA.
  #   2. A column is present in the JSON but all values are null — jsonlite
  #      parses these as logical NA, conflicting with character from other calls.
  # Both cases are handled below.
  parse_resp <- function(resp) {
    out <- httr::content(resp, as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON(flatten = TRUE) |>
      tibble::as_tibble()
    # If the API returned suggestions as a list-column, collapse each row's
    # candidates to a single " | "-separated string for easier downstream use.
    if ("suggestions" %in% names(out) && is.list(out[["suggestions"]])) {
      out <- dplyr::mutate(
        out,
        suggestions = purrr::map_chr(
          suggestions,
          \(x) if (length(x) == 0 || all(is.na(x))) NA_character_
                else stringr::str_c(x, collapse = " | ")
        )
      )
    }
    # Case 1: add typed NAs for columns completely absent from the response.
    missing_cols <- setdiff(.comptox_output_cols, names(out))
    if (length(missing_cols)) {
      for (col in missing_cols) {
        out[[col]] <- if (col %in% .comptox_logical_output_cols) NA else NA_character_
      }
    }
    # Case 2: coerce expected character columns that came back as all-null
    # (logical) in the JSON to character, so bind_rows() types are consistent.
    dplyr::mutate(
      out,
      dplyr::across(
        dplyr::any_of(.comptox_char_output_cols) & where(is.logical),
        as.character
      )
    )
  }

  # GET a single identifier. Returns list(result, error); error is non-NULL
  # on HTTP errors or parsing failures. HTTP status is checked before parsing
  # so that 4xx/5xx responses are caught rather than silently parsed.
  safe_get_one <- purrr::safely(
    \(id) {
      resp <- httr::GET(
        url    = paste0(base_url, url_path, utils::URLencode(id, reserved = TRUE)),
        config = api_headers
      )
      httr::stop_for_status(resp)
      parse_resp(resp) |>
        dplyr::mutate(input_term = id) |>
        dplyr::select(dplyr::any_of(.comptox_output_cols))
    }
  )

  # POST a batch of identifiers. Returns list(result, error); error is
  # non-NULL on HTTP errors or parsing failures.
  # The API's `searchValue` field identifies which result belongs to which
  # input, and is used as `input_term` rather than positional matching.
  safe_post_chunk <- purrr::safely(
    \(ids_vec) {
      resp <- httr::POST(
        url    = paste0(base_url, url_path),
        config = api_headers,
        body   = paste(ids_vec, collapse = "\n"),
        encode = "raw"
      )
      httr::stop_for_status(resp)
      parse_resp(resp) |>
        dplyr::mutate(input_term = .data$searchValue) |>
        dplyr::select(dplyr::any_of(.comptox_output_cols))
    }
  )

  # Helper: run safe_get_one over a vector of IDs, warn on failures, and
  # return the combined tibble of successful results.
  run_get <- function(ids, p_label) {
    p   <- progressr::progressor(along = ids, message = p_label)
    raw <- furrr::future_map(
      ids,
      \(id) { p(); out <- safe_get_one(id); Sys.sleep(rate_limit); out }
    )
    n_err <- sum(purrr::map_lgl(raw, \(x) !is.null(x$error)))
    if (n_err > 0)
      rlang::warn(paste0(n_err, " GET request(s) failed and were skipped."))
    purrr::map(raw, "result") |> purrr::list_rbind()
  }

  ## Extract and sanitize unique identifiers ----------------------------------

  # Sanitisation is done here so that `chem_ids` and `searchValue` from
  # the API always refer to the same strings, enabling reliable matching.
  chem_ids <- if (is.data.frame(input_data)) {
    if (missing(chem_id_cols))
      rlang::abort("`chem_id_cols` is required when `input_data` is a data frame.")
    input_data |>
      dplyr::distinct(dplyr::pick({{ chem_id_cols }})) |>
      tidyr::pivot_longer(cols = dplyr::everything(), values_to = "chem_ids") |>
      tidyr::drop_na(chem_ids) |>
      dplyr::pull(.data$chem_ids)
  } else {
    input_data[!is.na(input_data)]
  }

  chem_ids <- as.character(chem_ids) |>
    stringr::str_squish() |>
    unique()

  if (!length(chem_ids)) return(tibble::tibble())

  ## Search -------------------------------------------------------------------

  results <- if (!batch) {

    # --- Individual GET for every identifier --------------------------------
    run_get(chem_ids, "Retrieving data")

  } else {

    # --- Batch POST with GET fallback ---------------------------------------
    #
    # The CompTox POST endpoint silently strips trailing hyphens from
    # identifiers before searching, so chemicals whose names end with "-"
    # return no results. Pre-route them to individual GET calls.
    trailing_hyphen <- grepl("-$", chem_ids)
    ids_for_get     <- chem_ids[trailing_hyphen]
    ids_for_post    <- chem_ids[!trailing_hyphen]

    if (sum(trailing_hyphen) > 0)
      rlang::inform(
        paste0(sum(trailing_hyphen),
               " identifier(s) ending in \"-\" will be searched individually via GET.")
      )

    # Batch POST
    post_results <- if (length(ids_for_post)) {
      chunks   <- split(ids_for_post, ceiling(seq_along(ids_for_post) / chunk_size))
      p        <- progressr::progressor(along = chunks, message = "Retrieving data (batch POST)")
      raw_post <- furrr::future_map(
        chunks,
        \(chunk) { p(); out <- safe_post_chunk(chunk); Sys.sleep(rate_limit); out }
      )
      n_err <- sum(purrr::map_lgl(raw_post, \(x) !is.null(x$error)))
      if (n_err > 0)
        rlang::warn(paste0(n_err, " POST batch chunk(s) failed and were skipped."))
      purrr::map(raw_post, "result") |> purrr::list_rbind()
    } else {
      tibble::tibble()
    }

    # Identify no-hit IDs from POST: rows with NA results AND IDs absent from
    # the response entirely (in case the API omits rows for unmatched inputs).
    if (retry_no_hits && length(ids_for_post)) {
      na_hit_ids  <- if (nrow(post_results) > 0) {
        post_results |>
          dplyr::filter(is.na(.data$dtxsid) & is.na(.data$smiles)) |>
          dplyr::pull(.data$input_term) |>
          unique()
      } else {
        character(0)
      }
      missing_ids   <- setdiff(ids_for_post, post_results$input_term)
      extra_no_hits <- unique(c(na_hit_ids, missing_ids))

      if (length(extra_no_hits)) {
        rlang::inform(
          paste0("Retrying ", length(extra_no_hits),
                 " identifier(s) with no batch results via individual GET.")
        )
        ids_for_get  <- unique(c(ids_for_get, extra_no_hits))
        post_results <- dplyr::filter(post_results, !.data$input_term %in% extra_no_hits)
      }
    }

    # Individual GET for trailing-hyphen IDs and any retry candidates
    get_results <- if (length(ids_for_get)) {
      run_get(ids_for_get, "Retrieving data (GET)")
    } else {
      tibble::tibble()
    }

    dplyr::bind_rows(post_results, get_results)
  }

  ## Sort output to match input order -----------------------------------------

  if (sorted && nrow(results) > 0) {
    results <- results |>
      dplyr::mutate(.row_order = match(.data$input_term, chem_ids)) |>
      dplyr::arrange(.row_order) |>
      dplyr::select(-.row_order)
  }

  results
}


#' Collapse duplicate chemical search results into a single row per chemical
#'
#' When \code{\link{comptox_chem_search}} is called with multiple identifier
#' columns (e.g. chemical name \emph{and} CAS-RN), different \code{input_term}
#' values can resolve to the same chemical. This function collapses those
#' duplicate rows into a single row per unique chemical, merging their
#' \code{input_term} values into one string separated by \code{collapse_sep}.
#'
#' When \code{input_data} is supplied the function also joins the non-identifier
#' columns from the original data back onto the results, so the returned tibble
#' combines both the CompTox chemical information and the user's own data.
#' Deduplication is then performed \emph{within} each original input row (i.e.
#' only identifier columns from the same row that resolve to the same chemical
#' are collapsed), keeping rows that belong to different input rows separate.
#'
#' The collapsed \code{input_term} string respects the order in which columns
#' were listed in \code{chem_id_cols}: if the first column contains chemical
#' names, the name will always appear first in the collapsed string. When
#' \code{input_data} is not supplied, terms containing at least one letter
#' (e.g. chemical names) are placed before purely numeric or symbolic
#' identifiers (e.g. CAS-RNs), with alphabetical ordering within each group.
#'
#' Rows whose value in the \code{by} column is \code{NA} (i.e. no chemical was
#' matched) cannot be reliably attributed to a unique compound and are returned
#' unchanged at the end of the tibble.
#'
#' @param results A tibble returned by \code{\link{comptox_chem_search}}.
#' @param input_data Optional. The original data frame/tibble or character
#'   vector passed to \code{\link{comptox_chem_search}}. When provided, its
#'   non-identifier columns are joined onto the returned tibble and
#'   deduplication is scoped to each original input row.
#' @param chem_id_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> The same
#'   column selection passed to \code{\link{comptox_chem_search}}. Required
#'   when \code{input_data} is a data frame; ignored otherwise.
#' @param by Character. Column used as the chemical uniqueness key. One of
#'   \code{"dtxsid"} (default), \code{"dtxcid"}, \code{"casrn"}, or
#'   \code{"preferredName"}.
#' @param collapse_sep Character. Separator inserted between \code{input_term}
#'   values when multiple terms resolve to the same chemical.
#'   Default is \code{" | "}.
#'
#' @return A tibble with one row per unique chemical (scoped per original input
#'   row when \code{input_data} is provided), in the same order as the input.
#'   The \code{input_term} column contains all matching search terms joined by
#'   \code{collapse_sep}, ordered to match \code{chem_id_cols} when
#'   \code{input_data} is provided, or with letter-containing terms first and
#'   purely numeric/symbolic identifiers last when it is not. CompTox columns
#'   retain the value from the first matching result; non-identifier columns
#'   from \code{input_data} are appended when \code{input_data} is supplied.
#'   Unmatched rows (NA in \code{by}) are returned as-is at the end of the
#'   tibble.
#'
#' @export
#' @importFrom dplyr group_by across all_of any_of summarise first arrange
#'   relocate bind_rows select left_join mutate row_number
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom stringr str_c str_detect
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   chem_name = c("1-Chloro-4-nitrobenzene", "Benzene"),
#'   casrn     = c("100-00-5", "71-43-2"),
#'   conc_mg_l = c(0.5, 1.2)
#' )
#'
#' results <- comptox_chem_search(
#'   input_data   = df,
#'   api_key      = "your_api_key_here",
#'   chem_id_cols = c(chem_name, casrn)
#' )
#'
#' # Without input_data: collapse globally — one row per unique chemical.
#' # input_term values are ordered alphabetically.
#' deduped <- deduplicate_chem_results(results)
#'
#' # With input_data: join conc_mg_l back, collapse within each input row, and
#' # order input_term values to match chem_id_cols (chemical name first).
#' # Output row order matches the original df.
#' deduped <- deduplicate_chem_results(
#'   results      = results,
#'   input_data   = df,
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

  ## Input validation ---------------------------------------------------------

  if (!is.data.frame(results))
    rlang::abort("`results` must be a data frame/tibble (the output of `comptox_chem_search`).")

  by <- match.arg(by, c("dtxsid", "dtxcid", "casrn", "preferredName"))

  if (!by %in% names(results))
    rlang::abort(
      paste0("`", by, "` column not found in `results`. ",
             "Ensure `results` is the output of `comptox_chem_search`.")
    )

  if (!nrow(results)) return(results)

  # `.result_row` preserves the original row order of `results` so that the
  # final output can always be sorted back to match the input, regardless of
  # whether `input_data` is supplied.
  results <- dplyr::mutate(results, .result_row = dplyr::row_number())

  # Grouping variables and whether .col_order is available for input_term
  # ordering; both are updated below when input_data is a data frame.
  group_vars    <- by
  has_col_order <- FALSE

  ## Optionally join non-identifier columns from the original input data ------

  if (!missing(input_data) && !is.null(input_data)) {

    if (is.data.frame(input_data)) {

      if (missing(chem_id_cols))
        rlang::abort("`chem_id_cols` is required when `input_data` is a data frame.")

      # Capture the names of the identifier columns in the order the user
      # specified them — this order controls which term appears first in the
      # collapsed input_term string.
      id_col_names <- names(dplyr::select(input_data, {{ chem_id_cols }}))

      # Build a long lookup table:
      #   - one row per (original input row × identifier value)
      #   - `.col_order` records the position of each column in chem_id_cols
      #     so the first column listed (typically the chemical name) sorts first
      #   - pivoting the identifier columns away leaves only non-identifier
      #     data columns alongside the three helper columns
      lookup <- input_data |>
        dplyr::mutate(.orig_row = dplyr::row_number()) |>
        tidyr::pivot_longer(
          cols      = dplyr::all_of(id_col_names),
          names_to  = ".id_col",
          values_to = "input_term"
        ) |>
        dplyr::mutate(
          input_term = as.character(.data$input_term),
          .col_order = match(.data$.id_col, id_col_names)
        ) |>
        tidyr::drop_na(input_term) |>
        dplyr::select(-".id_col")

      # Join non-identifier columns and the helper columns onto each result row.
      # suffix = c("", ".orig") disambiguates column names that exist in both
      # `results` and the non-identifier portion of `input_data`.
      results <- dplyr::left_join(
        results,
        lookup,
        by     = "input_term",
        suffix = c("", ".orig")
      )

      # Scope deduplication to each original input row so that rows from
      # different inputs are never collapsed into one another, and enable
      # chem_id_cols-order sorting of the collapsed input_term string.
      group_vars    <- c(".orig_row", by)
      has_col_order <- TRUE

    } else if (!is.atomic(input_data)) {
      rlang::abort("`input_data` must be a data frame/tibble or an atomic vector.")
    }
    # Atomic vector: no extra columns to join; fall through to global dedup
  }

  ## Split on whether the key column is populated -----------------------------

  has_key  <- !is.na(results[[by]])
  keyed    <- results[ has_key, ]
  unkeyed  <- results[!has_key, ]

  ## Collapse duplicate rows --------------------------------------------------

  # Helper columns that must be excluded from the across() call below.
  # .col_order and .result_row are not grouping vars so dplyr will not
  # exclude them automatically; they must be listed explicitly.
  helper_cols <- c(".col_order", ".result_row")

  if (nrow(keyed)) {
    keyed <- keyed |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        # Order input terms by their source column's position in chem_id_cols
        # (so the chemical name column always comes first), then remove any
        # duplicate terms while preserving that order.
        # Falls back to alphabetic-first ordering when .col_order is unavailable:
        # terms containing at least one letter (e.g. chemical names) sort before
        # purely numeric/symbolic identifiers (e.g. CAS-RNs); alphabetical order
        # is used as a tiebreaker within each group.
        input_term  = {
          ord <- if (has_col_order) order(.col_order)
                 else order(!stringr::str_detect(input_term, "[A-Za-z]"), input_term)
          t   <- input_term[ord]
          stringr::str_c(t[!duplicated(t)], collapse = collapse_sep)
        },
        # Carry forward the earliest result row index for sorting later
        .result_row = min(.result_row),
        # All other non-grouping columns take the value from the first
        # occurrence. Grouping variables and the explicit helper cols are
        # excluded; dplyr handles the grouping vars automatically.
        dplyr::across(!dplyr::any_of(c("input_term", helper_cols)), dplyr::first),
        .groups     = "drop"
      ) |>
      dplyr::relocate(input_term)
  }

  ## Restore input order and drop all internal helper columns -----------------

  # `has_col_order` is TRUE if and only if `.orig_row` was added (i.e. when
  # input_data is a data frame), so it is the correct signal for which sort
  # column to use.
  sort_col <- if (has_col_order) ".orig_row" else ".result_row"

  dplyr::bind_rows(keyed, unkeyed) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_col))) |>
    dplyr::select(-dplyr::any_of(c(".orig_row", ".result_row", ".col_order")))
}
