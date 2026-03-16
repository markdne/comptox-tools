#' Retrieve chemical detail information from the EPA CompTox Chemical Dashboard API
#'
#' Fetches structural and physico-chemical property data for one or more chemicals
#' using their DTXSID or DTXCID identifiers via a batch POST request. Results
#' always include an \code{input_term} column (the identifier that was searched)
#' and an \code{error} column (\code{NA} on success, an error message on failure).
#'
#' @param input_data A data frame/tibble or an atomic vector of chemical
#'   identifiers.
#' @param search_type Character. The identifier type to search by. One of
#'   \code{"dtxsid"} (default) or \code{"dtxcid"}.
#'   See \url{https://api-ccte.epa.gov/docs/chemical.html} for details.
#' @param results_type Character. The projection (set of fields) to return.
#'   One of \code{"structure"} (default), \code{"standard"},
#'   \code{"comptox_chem_details"}, \code{"chem_identifiers"}, or \code{"all"}.
#'   See \url{https://api-ccte.epa.gov/docs/chemical.html} for details.
#' @param api_key Character. Your CompTox Dashboard API key.
#' @param chem_id_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The column
#'   in \code{input_data} that contains chemical identifiers. Required when
#'   \code{input_data} is a data frame; ignored for vector input.
#' @param chunk_size Integer. Maximum number of identifiers per POST request.
#'   Must be between 1 and 1000 (API limit). Default is \code{1000L}.
#' @param sorted Logical. If \code{TRUE} (default) output rows are returned in
#'   the same order as the (deduplicated) input identifiers.
#'
#' @return A [tibble][tibble::tibble] with one row per identifier. Columns
#'   depend on \code{results_type}. \code{input_term} records the searched
#'   identifier and \code{error} captures any failure messages (otherwise
#'   \code{NA}). Identifiers with no match in the API response are returned
#'   with \code{error = "No results returned"}.
#'
#' @export
#' @importFrom dplyr distinct pull mutate select arrange bind_rows any_of
#' @importFrom tidyr drop_na
#' @importFrom purrr list_rbind
#' @importFrom furrr future_map
#' @importFrom progressr progressor
#' @importFrom httr POST add_headers content status_code
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom tibble tibble as_tibble
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   dtxsid = c("DTXSID5020281", "DTXSID8020961", "DTXSID0021834")
#' )
#'
#' # Retrieve structure information using DTXSIDs
#' results <- get_struc_from_dtxsid(
#'   input_data  = df,
#'   search_type = "dtxsid",
#'   api_key     = "your_api_key_here",
#'   chem_id_col = dtxsid
#' )
#'
#' # Retrieve all available fields with a progress bar
#' results <- progressr::with_progress({
#'   get_struc_from_dtxsid(
#'     input_data   = df,
#'     search_type  = "dtxsid",
#'     results_type = "all",
#'     api_key      = "your_api_key_here",
#'     chem_id_col  = dtxsid
#'   )
#' })
#' }
get_struc_from_dtxsid <- function(
    input_data,
    search_type  = c("dtxsid", "dtxcid"),
    results_type = c("structure", "standard", "comptox_chem_details", "chem_identifiers", "all"),
    api_key,
    chem_id_col,
    chunk_size   = 1000L,
    sorted       = TRUE
) {
  
  ## Input validation -----------------------------------------------------------
  
  if (missing(api_key)) stop("Please provide your API key via `api_key`.")
  
  if (!is.data.frame(input_data) && !is.atomic(input_data))
    stop("`input_data` must be a data frame/tibble or an atomic vector.")
  
  search_type  <- match.arg(search_type)
  results_type <- match.arg(results_type)
  
  chunk_size <- as.integer(chunk_size)
  if (is.na(chunk_size) || chunk_size < 1L || chunk_size > 1000L)
    stop("`chunk_size` must be an integer between 1 and 1000.")
  
  ## API setup ------------------------------------------------------------------
  
  base_url <- "https://comptox.epa.gov/ctx-api"
  
  url_search <- switch(
    search_type,
    dtxsid = "/chemical/detail/search/by-dtxsid",
    dtxcid = "/chemical/detail/search/by-dtxcid"
  )
  
  results_params <- switch(
    results_type,
    structure            = "chemicalstructure",
    standard             = "chemicaldetailstandard",
    comptox_chem_details = "ccdchemicaldetails",
    chem_identifiers     = "chemicalidentifier",
    all                  = "chemicaldetailall"
  )
  
  api_headers <- httr::add_headers(
    "x-api-key"    = api_key,
    "Content-Type" = "application/json"
  )
  
  # Column in the response used to match results back to input identifiers
  key_col <- switch(search_type, dtxsid = "dtxsid", dtxcid = "dtxcid")
  
  ## Preferred column order -----------------------------------------------------
  
  col_order <- c(
    "input_term",
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
    "relatedSubstanceCount", "stereo",
    "error"
  )
  
  ## Extract and deduplicate identifiers ----------------------------------------
  
  chem_ids <- if (is.data.frame(input_data)) {
    if (missing(chem_id_col))
      stop("`chem_id_col` is required when `input_data` is a data frame.")
    input_data |>
      dplyr::distinct({{ chem_id_col }}) |>
      tidyr::drop_na({{ chem_id_col }}) |>
      dplyr::pull({{ chem_id_col }})
  } else {
    input_data[!is.na(input_data)]
  }
  
  chem_ids <- as.character(chem_ids) |> unique()
  
  if (!length(chem_ids)) return(tibble::tibble())
  
  ## Split into chunks ----------------------------------------------------------
  
  chunks <- split(chem_ids, ceiling(seq_along(chem_ids) / chunk_size))
  
  ## POST chunk worker ----------------------------------------------------------
  
  # Always returns a tibble with one row per ID in the chunk — failures are
  # captured in the `error` column rather than propagating exceptions.
  post_chunk <- function(ids) {
    tryCatch({
      resp <- httr::POST(
        url    = paste0(base_url, url_search),
        config = api_headers,
        query  = list("projection" = results_params),
        body   = jsonlite::toJSON(ids, auto_unbox = FALSE),
        encode = "raw"
      )
      
      if (httr::status_code(resp) != 200L) {
        return(tibble::tibble(
          input_term = ids,
          error      = paste0("HTTP ", httr::status_code(resp))
        ))
      }
      
      raw_text <- httr::content(resp, as = "text", encoding = "UTF-8")
      parsed   <- jsonlite::fromJSON(raw_text, flatten = TRUE)
      
      # Handle empty or null response
      if (is.null(parsed) || length(parsed) == 0L ||
          (is.data.frame(parsed) && nrow(parsed) == 0L)) {
        return(tibble::tibble(input_term = ids, error = "No results returned"))
      }
      
      result <- tibble::as_tibble(parsed) |>
        dplyr::mutate(
          input_term = if (key_col %in% names(parsed)) .data[[key_col]] else NA_character_,
          error      = NA_character_
        ) |>
        dplyr::select(dplyr::any_of(col_order))
      
      # Append error rows for any IDs absent from the response
      missing_ids <- setdiff(ids, result$input_term)
      if (length(missing_ids) > 0L) {
        result <- dplyr::bind_rows(
          result,
          tibble::tibble(input_term = missing_ids, error = "No results returned")
        )
      }
      
      result
      
    }, error = function(e) {
      tibble::tibble(input_term = ids, error = conditionMessage(e))
    })
  }
  
  ## Fetch in parallel ----------------------------------------------------------
  
  p <- progressr::progressor(along = chunks, message = "Retrieving data")
  
  results <- furrr::future_map(
    chunks,
    \(chunk) {
      p()
      post_chunk(chunk)
    }
  ) |> purrr::list_rbind()
  
  ## Sort output to match input order -------------------------------------------
  
  if (sorted && nrow(results) > 0) {
    results <- results |>
      dplyr::mutate(.row_order = match(input_term, chem_ids)) |>
      dplyr::arrange(.row_order) |>
      dplyr::select(-.row_order)
  }
  
  results
}
