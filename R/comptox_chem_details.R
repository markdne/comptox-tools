# Projections offered by the CTX detail endpoint, by short name
.comptox_projections <- c(
  structure            = "chemicalstructure",
  standard             = "chemicaldetailstandard",
  comptox_chem_details = "ccdchemicaldetails",
  chem_identifiers     = "chemicalidentifier",
  all                  = "chemicaldetailall"
)

# Preferred order of detail columns. Columns are moved into this order when
# present; columns a projection does not return are simply absent, and any
# returned column not listed here is kept after these.
.comptox_detail_col_order <- c(
  "dtxsid", "dtxcid", "casrn", "preferredName",
  "smiles", "qsarReadySmiles", "inchiString", "inchikey",
  "qcLevel", "qcLevelDesc", "qcNotes", "hasStructureImage", "isMarkush", "iupacName",
  "averageMass", "atmosphericHydroxylationRate",
  "bioconcentrationFactorOperaPred", "bioconcentrationFactorTestPred",
  "biodegradationHalfLifeDays",
  "boilingPointDegcOperaPred", "boilingPointDegcTestPred",
  "density", "flashPointDegcTestPred", "henrysLawAtm",
  "meltingPointDegcOperaPred", "meltingPointDegcTestPred",
  "monoisotopicMass", "octanolAirPartitionCoeff", "octanolWaterPartition",
  "operaKmDaysOperaPred", "pkaaOperaPred", "pkabOperaPred",
  "soilAdsorptionCoefficient", "surfaceTension", "thermalConductivity",
  "vaporPressureMmhgOperaPred", "vaporPressureMmhgTestPred",
  "viscosityCpCpTestPred", "waterSolubilityOpera", "waterSolubilityTest",
  "amesMutagenicityTestPred", "devtoxTestPred",
  "hrDiphniaLc50", "hrFatheadMinnow", "oralRatLd50Mol", "tetrahymenaPyriformis",
  "activeAssays", "totalAssays", "toxcastSelect", "percentAssays", "toxvalData",
  "sourcesCount", "nhanes", "cpdataCount", "expocat", "expocatMedianPrediction",
  "pubchemCid", "pubchemCount", "pubmedCount",
  "irisLink", "pprtvLink", "wikipediaArticle",
  "compoundId", "genericSubstanceId", "id", "isotope", "molFormula",
  "msReadySmiles", "multicomponent", "relatedStructureCount",
  "relatedSubstanceCount", "stereo"
)

#' Retrieve chemical details from the EPA CompTox Chemicals Dashboard API
#'
#' Fetches structure, identifier and property data for chemicals by DTXSID or
#' DTXCID, using batched `POST` requests to the CTX Chemical API detail
#' endpoint. `results_type` chooses how much is returned, from identifiers
#' only (`"chem_identifiers"`) to every field (`"all"`).
#'
#' Requests run concurrently (up to `max_active`) under a global rate limit.
#' Rate-limited (HTTP 429) and temporary server errors (HTTP 502-504) are
#' retried with exponential back-off for up to two minutes; requests that still
#' fail are reported per identifier rather than stopping the function.
#'
#' @param input_data A data frame or a character vector of DTXSIDs or DTXCIDs.
#' @param search_type Character. Identifier type: `"dtxsid"` (default) or
#'   `"dtxcid"`.
#' @param results_type Character. How much to return (the API projection):
#'   * `"structure"` (default): structure fields such as SMILES, InChI and
#'     InChIKey (projection `chemicalstructure`).
#'   * `"standard"`: the standard detail set (`chemicaldetailstandard`).
#'   * `"comptox_chem_details"`: the fields shown on the Dashboard chemical
#'     page (`ccdchemicaldetails`).
#'   * `"chem_identifiers"`: identifiers only, including CASRN, preferred name
#'     and InChIKey (`chemicalidentifier`).
#'   * `"all"`: every field (`chemicaldetailall`).
#'
#'   See <https://comptox.epa.gov/ctx-api/docs/chemical.html>.
#' @param api_key Character. Your CTX API key. Defaults to the `CTX_API_KEY`
#'   environment variable.
#' @param chem_id_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The one
#'   column of `input_data` holding the identifiers. Required when `input_data`
#'   is a data frame; ignored for vector input.
#' @param chunk_size Positive whole number, at most 1000 (the API limit).
#'   Identifiers per request. Default is `1000`.
#' @param sorted Logical. If `TRUE` (default) rows follow the order of the
#'   (de-duplicated) input identifiers. If `FALSE`, found chemicals come first
#'   in the order the API returned them, followed by the other identifiers.
#' @param rate_limit Non-negative number. Minimum seconds between requests,
#'   applied globally across concurrent requests. Default is `0`.
#' @param max_active Positive whole number. Maximum concurrent requests.
#'   Default is `5`.
#' @param progress Logical or string. Show a progress bar (a string names the
#'   bar). Default is `interactive()`.
#'
#' @returns A tibble with one row per unique input identifier:
#'   * `input_term`: the identifier searched (spaces trimmed, upper case).
#'   * The fields of the chosen projection, in the preferred column order;
#'     returned fields not in that order follow them. Fields are `NA` for
#'     identifiers that were not found.
#'   * `search_status`: `"found"`, `"not_found"` (the API returned nothing for
#'     it), `"invalid"` (not shaped like a DTXSID / DTXCID, so not sent) or
#'     `"error"` (the request failed after retries; search again later).
#'   * `error`: `NA` when found, otherwise the reason (`"No results returned"`,
#'     `"Not a valid DTXSID"`, `"HTTP 500"`, or the error message).
#'
#'   An empty tibble with `input_term`, the identifier column, `search_status`
#'   and `error` is returned when there are no usable identifiers.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   dtxsid = c("DTXSID5020281", "DTXSID8020961", "DTXSID0021834")
#' )
#'
#' # Structure fields (default)
#' structures <- comptox_chem_details(df, chem_id_col = dtxsid)
#'
#' # Identifiers only, for a character vector
#' ids <- comptox_chem_details(df$dtxsid, results_type = "chem_identifiers")
#'
#' # Every field, searching by DTXCID
#' everything <- comptox_chem_details(
#'   "DTXCID30182",
#'   search_type = "dtxcid",
#'   results_type = "all"
#' )
#' }
comptox_chem_details <- function(
    input_data,
    search_type  = c("dtxsid", "dtxcid"),
    results_type = c("structure", "standard", "comptox_chem_details", "chem_identifiers", "all"),
    api_key      = Sys.getenv("CTX_API_KEY"),
    chem_id_col,
    chunk_size   = 1000L,
    sorted       = TRUE,
    rate_limit   = 0,
    max_active   = 5L,
    progress     = interactive()
) {

  ## Input validation ----

  check_api_key(api_key)

  if (!is.data.frame(input_data) && !is.atomic(input_data)) {
    cli::cli_abort(
      "{.arg input_data} must be a data frame or an atomic vector, not {.cls {class(input_data)}}."
    )
  }

  if (is.data.frame(input_data)) {
    if (missing(chem_id_col)) {
      cli::cli_abort("{.arg chem_id_col} is required when {.arg input_data} is a data frame.")
    }

    n_id_cols <- ncol(dplyr::select(input_data, {{ chem_id_col }}))

    if (n_id_cols != 1L) {
      cli::cli_abort("{.arg chem_id_col} must select exactly one column; {n_id_cols} selected.")
    }
  }

  search_type <- match.arg(search_type)
  results_type <- match.arg(results_type)
  chunk_size <- check_count(chunk_size)
  max_active <- check_count(max_active)
  rate_limit <- check_rate_limit(rate_limit)

  if (chunk_size > 1000L) {
    cli::cli_abort("{.arg chunk_size} must be at most 1000 (the API limit).")
  }

  if (!rlang::is_bool(sorted)) {
    cli::cli_abort("{.arg sorted} must be {.val TRUE} or {.val FALSE}.")
  }

  ## Extract and clean identifiers ----

  # Upper-cased so "dtxsid7020182" is searched (and matched) as "DTXSID7020182"
  chem_ids <- extract_chem_ids(input_data, {{ chem_id_col }}) |>
    toupper() |>
    unique()

  # Name of the identifier column in the API response
  key_col <- search_type

  empty_result <- tibble::tibble(
    input_term    = character(),
    key           = character(),
    search_status = character(),
    error         = character()
  )
  names(empty_result)[[2]] <- key_col

  if (!length(chem_ids)) {
    return(empty_result)
  }

  # Identifiers not shaped like a DTXSID / DTXCID are not sent to the API
  id_label <- toupper(search_type)
  is_valid <- stringr::str_detect(chem_ids, stringr::str_c("^", id_label, "[0-9]+$"))

  invalid_rows <- tibble::tibble(
    input_term    = chem_ids[!is_valid],
    search_status = "invalid",
    error         = stringr::str_c("Not a valid ", id_label)
  )

  if (any(!is_valid)) {
    cli::cli_warn(c(
      "{sum(!is_valid)} value{?s} {?is/are} not a valid {id_label} and {?was/were} not searched:",
      " " = "{.val {chem_ids[!is_valid]}}"
    ))
  }

  ## Request details in batches ----

  chunks <- chunk_vector(chem_ids[is_valid], chunk_size)

  detail_url <- stringr::str_c(.comptox_base_url, "/chemical/detail/search/by-", search_type, "/")

  # The body must always be a JSON array, even for a single identifier
  reqs <- purrr::map(
    chunks,
    \(chunk) {
      req <- build_request(detail_url) |>
        httr2::req_url_query(projection = .comptox_projections[[results_type]]) |>
        httr2::req_headers(
          `x-api-key` = api_key,
          Accept      = "application/json",
          .redact     = "x-api-key"
        ) |>
        httr2::req_body_raw(
          jsonlite::toJSON(chunk, auto_unbox = FALSE),
          type = "application/json"
        )
      return(req)
    }
  )

  resps <- perform_requests(
    reqs,
    rate_limit = rate_limit,
    max_active = max_active,
    progress   = if (isTRUE(progress)) "CompTox: details" else progress
  )

  ## Parse each batch ----

  results <- purrr::map2(
    resps,
    chunks,
    \(resp, chunk) parse_comptox_details(resp, chunk, key_col)
  ) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(invalid_rows)

  n_error <- sum(results$search_status == "error")

  if (n_error > 0) {
    cli::cli_warn(c(
      "{n_error} identifier{?s} could not be retrieved because the request failed.",
      "i" = "These rows have {.code search_status == \"error\"}; search them again later."
    ))
  }

  ## Order columns and rows ----

  # Always provide the identifier column, even if nothing was found
  if (!key_col %in% names(results)) {
    results[[key_col]] <- NA_character_
  }

  results <- results |>
    dplyr::relocate("input_term", dplyr::any_of(.comptox_detail_col_order)) |>
    dplyr::relocate("search_status", "error", .after = dplyr::last_col())

  if (sorted) {
    results <- dplyr::arrange(results, match(.data$input_term, chem_ids))
  }

  return(results)
}

# Parse one detail batch into one row per identifier in the batch: found
# chemicals with their fields, then identifiers the API did not return. A
# failed request gives an "error" row for every identifier in the batch.
parse_comptox_details <- function(resp, ids, key_col) {
  batch_error <- function(message) {
    error_rows <- tibble::tibble(input_term = ids, search_status = "error", error = message)
    return(error_rows)
  }

  if (!inherits(resp, "httr2_response")) {
    return(batch_error(conditionMessage(resp)))
  }

  status <- httr2::resp_status(resp)

  if (status < 200L || status >= 300L) {
    return(batch_error(stringr::str_c("HTTP ", status)))
  }

  parsed <- tryCatch(resp_json(resp), error = \(cnd) cnd)

  if (inherits(parsed, "error")) {
    return(batch_error(stringr::str_c("Could not parse response: ", conditionMessage(parsed))))
  }

  # An empty array, or a response without the identifier field, means
  # nothing in this batch was found
  found <- tibble::tibble(input_term = character())

  if (is.data.frame(parsed) && nrow(parsed) && key_col %in% names(parsed)) {
    found <- tibble::as_tibble(parsed) |>
      dplyr::mutate(input_term = toupper(.data[[key_col]])) |>
      dplyr::filter(.data$input_term %in% ids) |>
      dplyr::distinct(.data$input_term, .keep_all = TRUE) |>
      dplyr::mutate(search_status = "found", error = NA_character_)
  }

  not_found <- tibble::tibble(
    input_term    = setdiff(ids, found$input_term),
    search_status = "not_found",
    error         = "No results returned"
  )

  details <- dplyr::bind_rows(found, not_found)

  return(details)
}
