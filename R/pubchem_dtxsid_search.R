#################################################################
#
# Author: Mark Nelms, nelms.mark7@gmail.com
#
# Version: 2.0
#
# Description: Defines pubchem_dtxsid_search(), a standalone
#   function that queries the public PubChem REST API to retrieve
#   DTXSIDs for chemical identifiers. No API key required.
#   Intended as a complement to comptox_chem_search() for
#   identifiers not found in the CompTox database.
#
#   Uses a two-step approach for efficiency:
#     Step 1 — n lightweight CID lookups (one per identifier, parallel)
#     Step 2 — chunked batch POST to retrieve synonyms for all CIDs
#
# Notes: Requires httr, jsonlite, dplyr, tidyr, purrr, furrr,
#   progressr, stringr, tibble, rlang, and utils (all available
#   via pacman in the main script).
#
# Potential Issues: None known
#################################################################


#' Search PubChem for DTXSIDs by chemical identifier
#'
#' Queries the PubChem PUG REST API in two steps: first resolving each
#' identifier to a PubChem CID via lightweight individual GET requests, then
#' retrieving synonyms (including DTXSIDs) for all resolved CIDs in chunked
#' batch POST requests. Returns NA for any identifier not found in PubChem or
#' with no DTXSID synonym. Every input identifier always appears in the output.
#'
#' @param input_data A data frame/tibble or a character vector of chemical
#'   identifiers (names, CAS-RNs, SMILES, InChI, etc.).
#' @param chem_id_cols <[tidy-select][dplyr::dplyr_tidy_select]> Column(s) in
#'   \code{input_data} containing chemical identifiers. Required when
#'   \code{input_data} is a data frame; ignored for vector input.
#' @param chunk_size Positive integer. Number of CIDs per batch POST request in
#'   step 2. Default is \code{100}.
#' @param rate_limit Non-negative numeric. Seconds to pause between step-1 GET
#'   requests \emph{per worker}. PubChem recommends ≤5 requests/second total.
#'   With a parallel \code{future} plan the aggregate rate is
#'   \code{n_workers / rate_limit} req/s, so callers should scale accordingly
#'   (e.g. \code{rate_limit = n_workers / 5} keeps the aggregate at 5 req/s).
#'   Default of \code{0.2} is correct for a single sequential worker.
#' @param sorted Logical. If \code{TRUE} (default) output rows are returned in
#'   the same order as the (deduplicated) input identifiers.
#'
#' @return A [tibble][tibble::tibble] with columns \code{input_term}
#'   (character), \code{pubchem_cid} (integer), and \code{dtxsid} (character).
#'   \code{pubchem_cid} and \code{dtxsid} are \code{NA} when PubChem returns
#'   no match or no DTXSID synonym. Returns an empty tibble when
#'   \code{input_data} contains no usable identifiers.
#'
#' @examples
#' \dontrun{
#' # Character vector
#' pubchem_dtxsid_search(c("benzene", "71-43-2", "unknown_xyz"))
#'
#' # Data frame with tidy-select columns
#' df <- tibble::tibble(name = "benzene", casrn = "71-43-2")
#' pubchem_dtxsid_search(df, chem_id_cols = c(name, casrn))
#' }
#' @export
#' @importFrom dplyr arrange distinct everything left_join mutate pick pull select
#' @importFrom furrr future_map
#' @importFrom glue glue
#' @importFrom httr GET POST add_headers content stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom progressr progressor
#' @importFrom purrr compact list_rbind map map2 map_chr map_lgl pluck safely
#' @importFrom rlang .data "%||%" abort warn
#' @importFrom stringr str_detect str_squish
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na pivot_longer
#' @importFrom utils URLencode
pubchem_dtxsid_search <- function(
    input_data,
    chem_id_cols,
    chunk_size = 100L,
    rate_limit = 0.2,
    sorted     = TRUE
) {

  ## Input validation -----------------------------------------------------------

  if (!is.data.frame(input_data) && !is.character(input_data))
    rlang::abort("`input_data` must be a data frame/tibble or a character vector.")

  if (!is.numeric(rate_limit) || length(rate_limit) != 1L || rate_limit < 0) {
    rlang::warn("`rate_limit` must be a non-negative number; setting to 0.2.")
    rate_limit <- 0.2
  }

  chunk_size <- as.integer(chunk_size)
  if (is.na(chunk_size) || chunk_size < 1L)
    rlang::abort("`chunk_size` must be a positive integer.")

  ## NA row template (returned when an identifier cannot be found) -------------

  na_cid_row <- function(id) tibble::tibble(
    input_term  = id,
    pubchem_cid = NA_integer_
  )

  ## Step 1 helper — identifier → CID (lightweight GET) -----------------------

  # Returns only a CID integer; ~50 bytes per response vs ~10–50 KB for
  # the full synonyms endpoint. purrr::safely() catches 404s and parse errors
  # so each failed lookup returns a NA row rather than halting execution.
  safe_get_cid <- purrr::safely(\(id) {
    resp <- httr::GET(
      url = paste0(
        "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/",
        utils::URLencode(id, reserved = TRUE),
        "/cids/JSON"
      )
    )
    httr::stop_for_status(resp)   # throws on 4xx / 5xx (404 = not found)
    parsed <- httr::content(resp, as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON(flatten = TRUE)
    tibble::tibble(
      input_term  = id,
      pubchem_cid = as.integer(parsed$IdentifierList$CID[1])
    )
  })

  ## Step 2 helper — CIDs → synonyms (batch POST) ------------------------------

  # POST body format: cid=241,1140,2244 (comma-separated, single parameter).
  # Repeated cid= params only use the first value; comma-separated is required.
  # fromJSON() returns Information as a data frame with a Synonym list-column.
  safe_post_synonyms <- purrr::safely(\(cid_chunk) {
    resp <- httr::POST(
      url    = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/synonyms/JSON",
      body   = paste0("cid=", paste(cid_chunk, collapse = ",")),
      encode = "raw",
      httr::add_headers("Content-Type" = "application/x-www-form-urlencoded")
    )
    httr::stop_for_status(resp)
    info_df <- httr::content(resp, as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON(flatten = TRUE) |>
      purrr::pluck("InformationList", "Information")
    tibble::tibble(
      pubchem_cid = as.integer(info_df$CID),
      # Collapse all DTXSID synonyms into a pipe-separated string so that
      # compounds with multiple DTXSIDs (e.g. Fytic acid: DTXSID00861653 and
      # DTXSID40889331) don't silently lose matches when compared against a
      # paired identifier's CompTox result.
      dtxsid      = purrr::map_chr(info_df$Synonym, \(syns) {
        hits <- unique(syns[stringr::str_detect(syns, "^DTXSID")])
        if (length(hits) > 0) paste(hits, collapse = " | ") else NA_character_
      })
    )
  })

  ## Extract and sanitise unique identifiers -----------------------------------

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

  if (!length(chem_ids)) return(
    tibble::tibble(input_term = character(), pubchem_cid = integer(), dtxsid = character())
  )

  ## Step 1 — identifier → CID (n parallel GET calls) -------------------------

  p1 <- progressr::progressor(along = chem_ids, message = "PubChem: CID lookup")

  raw_cids <- furrr::future_map(
    chem_ids,
    \(id) { p1(); out <- safe_get_cid(id); Sys.sleep(rate_limit); out }
  )

  n_cid_err <- sum(purrr::map_lgl(raw_cids, \(x) !is.null(x$error)))
  if (n_cid_err > 0)
    rlang::warn(glue::glue(
      "{n_cid_err} identifier(s) not found in PubChem; ",
      "a blank row will be returned for each."
    ))

  cid_map <- purrr::map2(raw_cids, chem_ids, \(x, id) x$result %||% na_cid_row(id)) |>
    purrr::list_rbind()

  ## Step 2 — CIDs → DTXSIDs (chunked batch POST) -----------------------------

  unique_cids <- unique(cid_map$pubchem_cid[!is.na(cid_map$pubchem_cid)])

  dtxsid_map <- if (length(unique_cids) > 0) {
    chunks <- split(unique_cids, ceiling(seq_along(unique_cids) / chunk_size))

    p2 <- progressr::progressor(along = chunks, message = "PubChem: synonym batch")

    raw_syns <- furrr::future_map(chunks, \(chunk) { p2(); safe_post_synonyms(chunk) })

    n_syn_err <- sum(purrr::map_lgl(raw_syns, \(x) !is.null(x$error)))
    if (n_syn_err > 0)
      rlang::warn(glue::glue(
        "{n_syn_err} synonym batch chunk(s) failed; ",
        "affected CIDs will have NA dtxsid."
      ))

    purrr::map(raw_syns, "result") |>
      purrr::compact() |>
      purrr::list_rbind()
  } else {
    tibble::tibble(pubchem_cid = integer(), dtxsid = character())
  }

  ## Combine CID map and DTXSID map --------------------------------------------

  # Left join: identifiers with NA pubchem_cid get NA dtxsid automatically.
  results <- cid_map |>
    dplyr::left_join(dtxsid_map, by = "pubchem_cid")

  ## Sort output to match input order ------------------------------------------

  if (sorted && nrow(results) > 0) {
    results <- results |>
      dplyr::mutate(.row_order = match(.data$input_term, chem_ids)) |>
      dplyr::arrange(.row_order) |>
      dplyr::select(-.row_order)
  }

  results
}
