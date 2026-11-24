# Output schema of pubchem_dtxsid_search()
.pubchem_na_template <- tibble::tibble(
  input_term     = NA_character_,
  namespace      = NA_character_,
  search_status  = NA_character_,
  pubchem_cid    = NA_integer_,
  pubchem_n_cids = NA_integer_,
  dtxsid         = NA_character_,
  inchikey       = NA_character_,
  smiles         = NA_character_
)

#' Search PubChem for DTXSIDs, InChIKeys and SMILES by chemical identifier
#'
#' Queries the public PubChem PUG REST API (no key needed) in two steps:
#'
#' 1. Each identifier is resolved to a PubChem CID with its own `POST` request
#'    (the identifier travels in the request body, so names containing `/`,
#'    `#` or other URL-reserved characters are safe).
#' 2. Synonyms (which include DTXSIDs deposited by EPA DSSTox) and the
#'    InChIKey and SMILES of all resolved CIDs are retrieved in batched `POST`
#'    requests.
#'
#' Requests run concurrently (up to `max_active`) under a global rate limit.
#' PubChem asks for no more than 5 requests per second; the default
#' `rate_limit = 0.2` keeps to that however many requests are in flight.
#' Busy (HTTP 503) and rate-limited (HTTP 429) responses are retried with
#' exponential back-off.
#'
#' @param input_data A data frame or a character vector of identifiers.
#' @param chem_id_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Column(s) of
#'   `input_data` holding identifiers. Required when `input_data` is a data
#'   frame; ignored for vector input.
#' @param chunk_size Positive whole number. CIDs per batch request in step 2.
#'   Default is `100`.
#' @param rate_limit Non-negative number. Minimum seconds between requests,
#'   applied globally across concurrent requests. Default is `0.2` (5 requests
#'   per second).
#' @param sorted Logical. If `TRUE` (default) rows follow the order of the
#'   (de-duplicated) input identifiers.
#' @param namespace Character. How identifiers are interpreted: `"auto"`
#'   (default) treats strings starting `InChI=` as InChI, strings shaped like
#'   an InChIKey as InChIKeys and everything else (names, CASRNs, synonyms) as
#'   names. Use `"smiles"` for SMILES input, which cannot be told apart from
#'   names reliably. One of `"auto"`, `"name"`, `"smiles"`, `"inchi"` or
#'   `"inchikey"`.
#' @param max_active Positive whole number. Maximum concurrent requests.
#'   Default is `5`.
#' @param progress Logical or string. Show a progress bar. Default is
#'   `interactive()`.
#'
#' @returns A tibble with one row per input identifier and columns
#'   `input_term`, `namespace`, `search_status` (`"found"`, `"not_found"` or
#'   `"error"`), `pubchem_cid` (the first, best-matching CID),
#'   `pubchem_n_cids` (number of CIDs the identifier matched; values above 1
#'   mean the identifier is ambiguous in PubChem), `dtxsid` (DTXSID synonyms of
#'   the CID, `" | "`-separated when there are several), `inchikey` and
#'   `smiles` (PubChem's full SMILES, with stereochemistry and isotopes).
#'   An empty tibble with these columns is returned when there are no usable
#'   identifiers.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pubchem_dtxsid_search(c("benzene", "71-43-2", "unknown_xyz"))
#'
#' df <- tibble::tibble(name = "benzene", casrn = "71-43-2")
#' pubchem_dtxsid_search(df, chem_id_cols = c(name, casrn))
#'
#' pubchem_dtxsid_search("c1ccccc1", namespace = "smiles")
#' }
pubchem_dtxsid_search <- function(
    input_data,
    chem_id_cols,
    chunk_size = 100L,
    rate_limit = 0.2,
    sorted     = TRUE,
    namespace  = c("auto", "name", "smiles", "inchi", "inchikey"),
    max_active = 5L,
    progress   = interactive()
) {

  ## Input validation ----

  if (!is.data.frame(input_data) && !is.character(input_data)) {
    cli::cli_abort(
      "{.arg input_data} must be a data frame or a character vector, not {.cls {class(input_data)}}."
    )
  }

  if (is.data.frame(input_data) && missing(chem_id_cols)) {
    cli::cli_abort("{.arg chem_id_cols} is required when {.arg input_data} is a data frame.")
  }

  namespace <- match.arg(namespace)
  chunk_size <- check_count(chunk_size)
  max_active <- check_count(max_active)
  rate_limit <- check_rate_limit(rate_limit)

  ## Extract and sanitise unique identifiers ----

  chem_ids <- extract_chem_ids(input_data, {{ chem_id_cols }})

  if (!length(chem_ids)) {
    return(.pubchem_na_template[0, ])
  }

  id_namespaces <- if (namespace == "auto") detect_pubchem_namespace(chem_ids) else namespace

  ## Step 1: identifier -> CID ----

  # The identifier goes in the form body, keyed by its namespace
  cid_reqs <- purrr::map2(
    chem_ids,
    rep_len(id_namespaces, length(chem_ids)),
    \(chem_id, id_namespace) {
      req <- build_request(
        stringr::str_c(.pubchem_base_url, "/compound/", id_namespace, "/cids/JSON")
      ) |>
        httr2::req_body_form(!!!rlang::set_names(list(chem_id), id_namespace))
      return(req)
    }
  )

  cid_resps <- perform_requests(
    cid_reqs,
    rate_limit = rate_limit,
    max_active = max_active,
    progress   = if (isTRUE(progress)) "PubChem: CID lookup" else progress
  )

  cid_map <- purrr::pmap(
    list(cid_resps, chem_ids, rep_len(id_namespaces, length(chem_ids))),
    \(resp, chem_id, id_namespace) {
      outcome <- resp_outcome(resp)
      cids <- integer()

      if (outcome == "ok") {
        cids <- as.integer(resp_json(resp)$IdentifierList$CID)
        # PubChem reports CID 0 for some unmatched structure inputs
        cids <- cids[!is.na(cids) & cids > 0L]
        if (!length(cids)) outcome <- "not_found"
      }

      cid_row <- tibble::tibble(
        input_term     = chem_id,
        namespace      = id_namespace,
        search_status  = dplyr::if_else(outcome == "ok", "found", outcome),
        pubchem_cid    = if (length(cids)) cids[[1]] else NA_integer_,
        pubchem_n_cids = length(cids)
      )

      return(cid_row)
    }
  ) |>
    purrr::list_rbind()

  n_error <- sum(cid_map$search_status == "error")

  if (n_error > 0) {
    cli::cli_warn(c(
      "{n_error} PubChem CID lookup{?s} failed after retries.",
      "i" = "These rows have {.code search_status == \"error\"}; search them again later."
    ))
  }

  ## Step 2: CIDs -> DTXSID synonyms, InChIKey and SMILES ----

  unique_cids <- unique(cid_map$pubchem_cid[!is.na(cid_map$pubchem_cid)])

  cid_info <- pubchem_cid_info(
    unique_cids,
    chunk_size = chunk_size,
    rate_limit = rate_limit,
    max_active = max_active,
    progress   = progress
  )

  ## Combine ----

  results <- cid_map |>
    dplyr::left_join(cid_info, by = dplyr::join_by("pubchem_cid")) |>
    dplyr::select(dplyr::all_of(names(.pubchem_na_template)))

  if (sorted) {
    results <- dplyr::arrange(results, match(.data$input_term, chem_ids))
  }

  return(results)
}

# Guess the PubChem input namespace of each identifier
detect_pubchem_namespace <- function(chem_ids) {
  id_namespaces <- dplyr::case_when(
    stringr::str_starts(chem_ids, "InChI=") ~ "inchi",
    stringr::str_detect(chem_ids, "^[A-Z]{14}-[A-Z]{10}-[A-Z]$") ~ "inchikey",
    .default = "name"
  )

  return(id_namespaces)
}

# DTXSID synonyms, InChIKey and SMILES for a set of CIDs, via batched POST
# requests. Failed batches leave NA values for their CIDs (with a warning).
pubchem_cid_info <- function(
    cids,
    chunk_size,
    rate_limit,
    max_active,
    progress
) {
  empty_info <- tibble::tibble(
    pubchem_cid = cids,
    dtxsid      = NA_character_,
    inchikey    = NA_character_,
    smiles      = NA_character_
  )

  if (!length(cids)) {
    return(empty_info)
  }

  chunks <- chunk_vector(cids, chunk_size)

  # POST body is a single comma-separated cid= parameter; repeated cid=
  # parameters would only use the first value
  cid_request <- function(operation, chunk) {
    req <- build_request(stringr::str_c(.pubchem_base_url, "/compound/cid/", operation, "/JSON")) |>
      httr2::req_body_raw(
        stringr::str_c("cid=", stringr::str_c(chunk, collapse = ",")),
        type = "application/x-www-form-urlencoded"
      )

    return(req)
  }

  synonym_reqs <- purrr::map(chunks, \(chunk) cid_request("synonyms", chunk))
  property_reqs <- purrr::map(chunks, \(chunk) cid_request("property/InChIKey,SMILES", chunk))

  resps <- perform_requests(
    c(synonym_reqs, property_reqs),
    rate_limit = rate_limit,
    max_active = max_active,
    progress   = if (isTRUE(progress)) "PubChem: synonyms and properties" else progress
  )

  outcomes <- purrr::map_chr(resps, resp_outcome)

  if (any(outcomes != "ok")) {
    cli::cli_warn(c(
      "{sum(outcomes != 'ok')} PubChem batch request{?s} failed.",
      "i" = "Affected CIDs have {.val NA} DTXSID, InChIKey or SMILES."
    ))
  }

  is_synonym_resp <- seq_along(resps) <= length(synonym_reqs)

  # DTXSID synonyms: all of them, " | "-separated, because some CIDs map to
  # several DSSTox substances (e.g. fytic acid: DTXSID00861653, DTXSID40889331)
  synonyms <- resps[is_synonym_resp & outcomes == "ok"] |>
    purrr::map(parse_pubchem_synonyms) |>
    purrr::list_rbind()

  properties <- resps[!is_synonym_resp & outcomes == "ok"] |>
    purrr::map(parse_pubchem_properties) |>
    purrr::list_rbind()

  cid_info <- tibble::tibble(pubchem_cid = cids) |>
    dplyr::left_join(
      dplyr::bind_rows(synonyms, tibble::tibble(pubchem_cid = integer(), dtxsid = character())),
      by = dplyr::join_by("pubchem_cid")
    ) |>
    dplyr::left_join(
      dplyr::bind_rows(
        properties,
        tibble::tibble(pubchem_cid = integer(), inchikey = character(), smiles = character())
      ),
      by = dplyr::join_by("pubchem_cid")
    )

  return(cid_info)
}

# Parse a synonyms response into one row per CID with its DTXSID synonyms
parse_pubchem_synonyms <- function(resp) {
  info <- resp_json(resp)$InformationList$Information

  if (is.null(info) || !nrow(info)) {
    return(NULL)
  }

  # CIDs without synonyms have no Synonym entry at all
  synonym_lists <- if ("Synonym" %in% names(info)) info$Synonym else vector("list", nrow(info))

  synonyms <- tibble::tibble(
    pubchem_cid = as.integer(info$CID),
    dtxsid = purrr::map_chr(
      synonym_lists,
      \(synonym_list) {
        hits <- unique(stringr::str_subset(unlist(synonym_list), "^DTXSID[0-9]+$"))
        if (!length(hits)) {
          return(NA_character_)
        }
        return(stringr::str_c(hits, collapse = " | "))
      }
    )
  )

  return(synonyms)
}

# Parse a property response. PubChem renamed its SMILES properties in 2025
# (IsomericSMILES -> SMILES); the old name is used if the new one is absent.
parse_pubchem_properties <- function(resp) {
  props <- resp_json(resp)$PropertyTable$Properties

  if (is.null(props) || !nrow(props)) {
    return(NULL)
  }

  smiles_col <- intersect(c("SMILES", "IsomericSMILES"), names(props))

  properties <- tibble::tibble(
    pubchem_cid = as.integer(props$CID),
    inchikey = if ("InChIKey" %in% names(props)) as.character(props$InChIKey) else NA_character_,
    smiles = if (length(smiles_col)) as.character(props[[smiles_col[[1]]]]) else NA_character_
  )

  return(properties)
}
