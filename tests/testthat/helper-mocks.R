# helper-mocks.R — shared mock utilities loaded automatically by testthat
# before every test file in this package.

# Run futures sequentially so that local_mocked_bindings() remain visible
# inside furrr::future_map() calls during tests. (Parallel workers run in
# separate processes where the mocked httr bindings would be invisible.)
future::plan(future::sequential)

# ---------------------------------------------------------------------------
# Minimal httr response object
# ---------------------------------------------------------------------------
# Both httr::stop_for_status() (checks $status_code) and
# httr::content(resp, as = "text") (reads $content as raw bytes) work with
# this structure, so no additional mocking of those helpers is required.
make_mock_response <- function(json_string, status_code = 200L) {
  structure(
    list(
      url         = "https://comptox.epa.gov/ctx-api",
      status_code = as.integer(status_code),
      headers     = structure(
        list(`content-type` = "application/json; charset=utf-8"),
        class = "insensitive"
      ),
      content     = charToRaw(json_string)
    ),
    class = "response"
  )
}

# ---------------------------------------------------------------------------
# JSON fixtures
# ---------------------------------------------------------------------------

# Single matched result: Benzene (used as a GET response body).
json_benzene <- '[{
  "dtxsid": "DTXSID3039242",
  "dtxcid": "DTXCID3039242",
  "casrn": "71-43-2",
  "preferredName": "Benzene",
  "smiles": "c1ccccc1",
  "isMarkush": false,
  "hasStructureImage": true,
  "searchName": "Benzene",
  "searchValue": "Benzene",
  "rank": 1,
  "suggestions": null
}]'

# Single matched result: Toluene (used as a GET response body).
json_toluene <- '[{
  "dtxsid": "DTXSID4040008",
  "dtxcid": "DTXCID4040008",
  "casrn": "108-88-3",
  "preferredName": "Toluene",
  "smiles": "Cc1ccccc1",
  "isMarkush": false,
  "hasStructureImage": true,
  "searchName": "Toluene",
  "searchValue": "Toluene",
  "rank": 1,
  "suggestions": null
}]'

# Two matched results in one POST batch response: Benzene + Toluene.
json_batch_benzene_toluene <- '[
  {
    "dtxsid": "DTXSID3039242",
    "dtxcid": "DTXCID3039242",
    "casrn": "71-43-2",
    "preferredName": "Benzene",
    "smiles": "c1ccccc1",
    "isMarkush": false,
    "hasStructureImage": true,
    "searchName": "Benzene",
    "searchValue": "Benzene",
    "rank": 1,
    "suggestions": null
  },
  {
    "dtxsid": "DTXSID4040008",
    "dtxcid": "DTXCID4040008",
    "casrn": "108-88-3",
    "preferredName": "Toluene",
    "smiles": "Cc1ccccc1",
    "isMarkush": false,
    "hasStructureImage": true,
    "searchName": "Toluene",
    "searchValue": "Toluene",
    "rank": 1,
    "suggestions": null
  }
]'

# Unmatched identifier with a single candidate suggestion.
# The API wraps suggestions in a JSON array; jsonlite::fromJSON produces a
# list-column which parse_resp collapses to a " | "-separated string.
json_no_match_suggestion <- '[{
  "dtxsid": null,
  "dtxcid": null,
  "casrn": null,
  "preferredName": null,
  "smiles": null,
  "isMarkush": null,
  "hasStructureImage": null,
  "searchName": null,
  "searchValue": "unknown-chem",
  "rank": null,
  "suggestions": ["unknown-chemical"]
}]'

# Unmatched identifier with multiple candidate suggestions.
json_no_match_multi_suggestions <- '[{
  "dtxsid": null,
  "dtxcid": null,
  "casrn": null,
  "preferredName": null,
  "smiles": null,
  "isMarkush": null,
  "hasStructureImage": null,
  "searchName": null,
  "searchValue": "chlorotoluene",
  "rank": null,
  "suggestions": ["2-Chlorotoluene", "3-Chlorotoluene", "4-Chlorotoluene"]
}]'

# All expected character fields are JSON null — tests Case 2 type coercion in
# parse_resp (jsonlite parses null as logical NA; we must coerce to character).
json_all_null_chars <- '[{
  "dtxsid": null,
  "dtxcid": null,
  "casrn": null,
  "preferredName": null,
  "smiles": null,
  "isMarkush": null,
  "hasStructureImage": null,
  "searchName": null,
  "searchValue": "no-match",
  "rank": null,
  "suggestions": null
}]'

# Empty result array — API returns nothing for this identifier.
json_empty <- '[]'

# Batch POST response: Benzene matched + "chlorotoluene" unmatched WITH
# suggestions.  Used to verify that retry_no_hits does NOT re-query via GET
# when the POST result already contains suggestions.
json_batch_benzene_suggestion <- '[
  {
    "dtxsid": "DTXSID3039242",
    "dtxcid": "DTXCID3039242",
    "casrn": "71-43-2",
    "preferredName": "Benzene",
    "smiles": "c1ccccc1",
    "isMarkush": false,
    "hasStructureImage": true,
    "searchName": "Benzene",
    "searchValue": "Benzene",
    "rank": 1,
    "suggestions": null
  },
  {
    "dtxsid": null,
    "dtxcid": null,
    "casrn": null,
    "preferredName": null,
    "smiles": null,
    "isMarkush": null,
    "hasStructureImage": null,
    "searchName": null,
    "searchValue": "chlorotoluene",
    "rank": null,
    "suggestions": ["2-Chlorotoluene", "3-Chlorotoluene"]
  }
]'

# Batch POST response: Benzene matched + "Toluene" unmatched with NO
# suggestions.  Used to verify that retry_no_hits DOES re-query via GET when
# the POST no-hit row carries no useful information.
json_batch_benzene_no_suggestion <- '[
  {
    "dtxsid": "DTXSID3039242",
    "dtxcid": "DTXCID3039242",
    "casrn": "71-43-2",
    "preferredName": "Benzene",
    "smiles": "c1ccccc1",
    "isMarkush": false,
    "hasStructureImage": true,
    "searchName": "Benzene",
    "searchValue": "Benzene",
    "rank": 1,
    "suggestions": null
  },
  {
    "dtxsid": null,
    "dtxcid": null,
    "casrn": null,
    "preferredName": null,
    "smiles": null,
    "isMarkush": null,
    "hasStructureImage": null,
    "searchName": null,
    "searchValue": "Toluene",
    "rank": null,
    "suggestions": null
  }
]'
