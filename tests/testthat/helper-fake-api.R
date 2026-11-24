# Fake CompTox and PubChem services for offline tests. Tests replace the
# internal perform_requests() with fake_perform_requests(), which answers
# each httr2 request from the small fixtures below.

# CompTox chemicals (fixture values; DTXSID9999999 and DTXSID8888888 are made
# up to exercise the PubChem paths)
fake_comptox_chems <- tibble::tibble(
  dtxsid = c("DTXSID3039242", "DTXSID7021360", "DTXSID9999999", "DTXSID8888888"),
  dtxcid = c("DTXCID30182", "DTXCID101360", "DTXCID999999", "DTXCID888888"),
  casrn = c("71-43-2", "108-88-3", "999-99-9", "888-88-8"),
  preferredName = c("Benzene", "Toluene", "Mysterol acid", "Ketonol"),
  smiles = c("C1=CC=CC=C1", "CC1=CC=CC=C1", "CCO", "CC(C)=O"),
  inchikey = c(
    "UHOVQNZJYSORNB-UHFFFAOYSA-N", "YXFVVABEGXRONW-UHFFFAOYSA-N",
    "AAAAAAAAAAAAAA-UHFFFAOYSA-N", "BBBBBBBBBBBBBB-UHFFFAOYSA-N"
  )
)

# Search terms CompTox matches exactly (upper case) -> DTXSID
fake_comptox_terms <- c(
  "BENZENE" = "DTXSID3039242",
  "71-43-2" = "DTXSID3039242",
  "TOLUENE" = "DTXSID7021360",
  "108-88-3" = "DTXSID7021360",
  "DTXSID9999999" = "DTXSID9999999",
  "BBBBBBBBBBBBBB-UHFFFAOYSA-N" = "DTXSID8888888",
  "DTXSID8888888" = "DTXSID8888888",
  "888-88-8" = "DTXSID8888888",
  "ETHYL-" = "DTXSID7021360"
)

# Unmatched terms for which CompTox offers suggestions
fake_comptox_suggestions <- list(
  "BENZEN" = c("Benzene"),
  "TOLUNE" = c("Toluene")
)

# PubChem name / InChIKey -> CIDs
fake_pubchem_cids <- list(
  "mysterol" = 5001L,
  "ketonol old name" = 6001L,
  "novelide" = 7001L,
  "benzene" = 241L,
  "glucose" = c(5793L, 64689L)
)

# PubChem CID data
fake_pubchem_cid_data <- tibble::tibble(
  cid = c(5001L, 6001L, 7001L, 241L, 5793L),
  synonyms = list(
    c("Mysterol", "DTXSID9999999"),
    c("Ketonol old name"),
    c("Novelide"),
    c("benzene", "DTXSID3039242"),
    c("glucose")
  ),
  inchikey = c(
    "AAAAAAAAAAAAAA-UHFFFAOYSA-N", "BBBBBBBBBBBBBB-UHFFFAOYSA-N",
    "CCCCCCCCCCCCCC-UHFFFAOYSA-N", "UHOVQNZJYSORNB-UHFFFAOYSA-N",
    "WQZGKKKJIJFFOK-GASJEMHNSA-N"
  ),
  smiles = c("CCO", "CC(C)=O", "CCN", "C1=CC=CC=C1", "C(C1C(C(C(C(O1)O)O)O)O)O")
)

# Identifiers that make the fake services fail with HTTP 500
fake_failing_terms <- c("SERVER ERROR CHEM", "pubchem server error")
fake_failing_dtxsids <- "DTXSID0000500"

# Fields returned by each detail projection (fixture subset, in a different
# order from the preferred column order, plus a field that order lacks)
fake_projection_cols <- list(
  chemicalidentifier = c("newApiField", "inchikey", "preferredName", "casrn", "dtxcid", "dtxsid"),
  chemicalstructure = c("inchikey", "smiles", "dtxcid", "dtxsid"),
  chemicaldetailall = c("newApiField", "smiles", "inchikey", "preferredName", "casrn", "dtxcid", "dtxsid")
)

# Build an httr2 response with a JSON body
fake_json_response <- function(req, value, status = 200L) {
  body <- jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null")

  resp <- httr2::response(
    status_code = status,
    url = req$url,
    method = if (is.null(req$body)) "GET" else "POST",
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(as.character(body))
  )

  return(resp)
}

# Request body as a string (raw bodies) or a named list (form bodies)
fake_request_body <- function(req) {
  if (identical(req$body$type, "form")) {
    return(purrr::map(req$body$data, \(value) utils::URLdecode(as.character(value))))
  }

  return(as.character(req$body$data))
}

# One CompTox search record for a term
fake_comptox_record <- function(term) {
  dtxsid <- unname(fake_comptox_terms[toupper(term)])

  if (!is.na(dtxsid)) {
    chem <- dplyr::filter(fake_comptox_chems, .data$dtxsid == .env$dtxsid)
    record <- list(
      dtxsid = chem$dtxsid, dtxcid = chem$dtxcid, casrn = chem$casrn,
      preferredName = chem$preferredName, smiles = chem$smiles,
      isMarkush = FALSE, hasStructureImage = TRUE,
      searchName = "Approved Name", searchValue = term, rank = 1L,
      suggestions = NULL
    )
    return(record)
  }

  suggestions <- fake_comptox_suggestions[[toupper(term)]]

  record <- list(
    dtxsid = NULL, searchValue = term,
    suggestions = if (is.null(suggestions)) NULL else I(suggestions)
  )

  return(record)
}

# Answer one request
fake_handle_request <- function(req) {
  url <- req$url

  # CompTox search: GET (term in URL) or POST (newline-separated terms)
  if (stringr::str_detect(url, "comptox.epa.gov/ctx-api/chemical/search/equal/")) {

    if (is.null(req$body)) {
      term <- utils::URLdecode(stringr::str_remove(url, ".*/chemical/search/equal/"))

      if (toupper(term) %in% fake_failing_terms) {
        return(fake_json_response(req, list(title = "Internal error"), status = 500L))
      }

      record <- fake_comptox_record(term)
      if (is.null(record$dtxsid) && is.null(record$suggestions)) {
        return(fake_json_response(req, list()))
      }
      return(fake_json_response(req, list(record)))
    }

    terms <- stringr::str_split_1(fake_request_body(req), "\n")
    # Mimic the API: unknown terms without suggestions, and failing terms,
    # are dropped from the batch response
    records <- purrr::map(terms, fake_comptox_record) |>
      purrr::keep(\(record) !is.null(record$dtxsid) || !is.null(record$suggestions))

    if (isTRUE(getOption("fake_comptox_upper_case"))) {
      records <- purrr::map(records, \(record) {
        record$searchValue <- toupper(record$searchValue)
        return(record)
      })
    }

    return(fake_json_response(req, records))
  }

  # CompTox details by DTXSID or DTXCID; the fields returned depend on the
  # projection, and include one field the preferred column order does not list
  if (stringr::str_detect(url, "comptox.epa.gov/ctx-api/chemical/detail/search/by-dtx[sc]id/")) {
    ids <- jsonlite::fromJSON(fake_request_body(req))

    if (any(ids %in% fake_failing_dtxsids)) {
      return(fake_json_response(req, list(title = "Internal error"), status = 500L))
    }

    key_col <- stringr::str_match(url, "by-(dtx[sc]id)/")[, 2]
    projection <- stringr::str_match(url, "projection=([a-z]+)")[, 2]
    projection_cols <- fake_projection_cols[[projection]]

    details <- fake_comptox_chems |>
      dplyr::filter(.data[[key_col]] %in% ids) |>
      dplyr::mutate(newApiField = "extra") |>
      dplyr::select(dplyr::all_of(projection_cols))
    return(fake_json_response(req, details))
  }

  # PubChem identifier -> CIDs
  if (stringr::str_detect(url, "pubchem.*/compound/(name|inchikey|inchi|smiles)/cids/JSON")) {
    term <- fake_request_body(req)[[1]]

    if (term %in% fake_failing_terms) {
      return(fake_json_response(req, list(Fault = "busy"), status = 500L))
    }

    cids <- fake_pubchem_cids[[tolower(term)]]

    if (is.null(cids)) {
      return(fake_json_response(req, list(Fault = list(Code = "PUGREST.NotFound")), status = 404L))
    }
    return(fake_json_response(req, list(IdentifierList = list(CID = I(cids)))))
  }

  # PubChem synonyms / properties for a CID batch
  if (stringr::str_detect(url, "pubchem.*/compound/cid/")) {
    cids <- as.integer(stringr::str_split_1(stringr::str_remove(fake_request_body(req), "^cid="), ","))
    cid_data <- dplyr::filter(fake_pubchem_cid_data, .data$cid %in% cids)

    if (stringr::str_detect(url, "/synonyms/")) {
      info <- purrr::map2(cid_data$cid, cid_data$synonyms, \(cid, synonyms) list(CID = cid, Synonym = I(synonyms)))
      return(fake_json_response(req, list(InformationList = list(Information = info))))
    }

    props <- purrr::pmap(
      list(cid_data$cid, cid_data$inchikey, cid_data$smiles),
      \(cid, inchikey, smiles) list(CID = cid, InChIKey = inchikey, SMILES = smiles)
    )
    return(fake_json_response(req, list(PropertyTable = list(Properties = props))))
  }

  stop("Unexpected request in test: ", url)
}

# Drop-in replacement for perform_requests()
fake_perform_requests <- function(reqs, rate_limit = 0, max_active = 5L, progress = FALSE) {
  return(purrr::map(reqs, fake_handle_request))
}

# Use the fake services for the rest of the calling test
local_fake_api <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(perform_requests = fake_perform_requests, .env = env)
  withr::local_envvar(CTX_API_KEY = "test-key", .local_envir = env)
}
