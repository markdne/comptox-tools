# (next version)

## Breaking changes

* All web requests now use httr2 instead of httr, furrr and progressr.
  Requests run concurrently through `httr2::req_perform_parallel()`, so a
  `future` plan is no longer needed (or used). Add `httr2 (>= 1.1.1)` to
  Imports; drop httr, furrr and progressr if nothing else uses them.
* `rate_limit` is now the minimum number of seconds between requests across
  *all* concurrent requests, not per worker. `pubchem_dtxsid_search()`'s
  default of `0.2` therefore keeps to PubChem's 5 requests per second however
  many requests are in flight.
* `verify_chem_identifiers()`:
  * `"agree"` now needs two or more identifiers that all resolved. New
    classes: `"partial"` (some identifiers did not resolve), `"single_id"`
    (only one identifier supplied) and `"ambiguous"` (an identifier matched
    several chemicals and the row cannot narrow them down).
  * The `rank` column is dropped (it described a search, not a chemical).
  * New columns: `comptox_casrn`, `inchikey`, `pubchem_cid`,
    `pubchem_inchikey`, `pubchem_smiles`, `resolved_via` and
    `resolution_detail`.
* `pubchem_dtxsid_search()` returns new columns `namespace`, `search_status`,
  `pubchem_n_cids`, `inchikey` and `smiles`.
* `comptox_chem_search()` returns a new `search_status` column.

## New features

* `verify_chem_identifiers(pubchem_fallback = TRUE)` searches PubChem for
  identifiers with no direct CompTox match. PubChem DTXSIDs take part in the
  agreement check and are looked up in CompTox; PubChem compounds without a
  DTXSID are searched in CompTox by InChIKey, and otherwise keyed on their
  InChIKey.
* New `comptox_chem_details()` replaces `get_struc_from_id()`: the same
  DTXSID / DTXCID detail search with the same `results_type` choices and
  preferred column order, plus httr2 retries and rate limiting. Remove
  `get_struc_from_id()` (or make it a thin wrapper). Differences from
  `get_struc_from_id()`:
  * Columns are *reordered* rather than selected, so fields missing from the
    preferred order (e.g. ones EPA adds later) are kept after the listed
    columns instead of being silently dropped.
  * New `search_status` column (`"found"`, `"not_found"`, `"invalid"`,
    `"error"`) alongside `error`, which now gives the reason for every
    non-found row.
  * Identifiers are trimmed and upper-cased; blanks are dropped; values not
    shaped like a DTXSID / DTXCID are reported as `"invalid"` rather than
    sent to the API.
  * `api_key` defaults to `CTX_API_KEY`; new `rate_limit`, `max_active` and
    `progress` arguments.
* New `flag_duplicate_chems()` flags rows that refer to the same chemical, by
  DTXSID and then by InChIKey.
* `api_key` defaults to the `CTX_API_KEY` environment variable.
* `pubchem_dtxsid_search()` gains `namespace`, so InChI, InChIKey and SMILES
  input are looked up correctly (previously every identifier was treated as a
  name, so SMILES and InChI input failed).
* New `max_active` and `progress` arguments on the search functions.

## Bug fixes

* Failed requests (HTTP 5xx, time-outs) are now retried with back-off and,
  if they still fail, reported as `search_status = "error"` instead of being
  indistinguishable from "not found".
* `comptox_chem_search(retry_no_hits = FALSE)` no longer silently drops the
  identifiers of a failed batch.
* `comptox_chem_search()` matches batch results to inputs case-insensitively.
* `comptox_chem_search()` and `pubchem_dtxsid_search()` accept data frames
  whose identifier columns have different types (e.g. a numeric ID column).
* `comptox_chem_search()` returns an empty tibble *with* its usual columns
  when there are no identifiers.
* `deduplicate_chem_results()` squishes `input_data` identifiers the same way
  `comptox_chem_search()` does, so values with stray spaces join back to their
  results.
* `pubchem_dtxsid_search()` sends identifiers in the request body, so names
  containing `/` or other URL-reserved characters work.
* Undeclared-global NOTEs from `R CMD check` fixed.
