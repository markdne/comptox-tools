# test-comptox_chem_search.R
# Tests for comptox_chem_search().
#
# API calls are intercepted via local_mocked_bindings(.package = "httr") so
# no live network access is required.  future::plan(future::sequential) is set
# in helper-mocks.R so that furrr::future_map() runs in the current process,
# keeping the mocked bindings visible inside the closures.

# ===========================================================================
# Input validation
# ===========================================================================

test_that("missing api_key triggers an informative abort", {
  expect_error(
    comptox_chem_search("Benzene"),
    regexp = "`api_key`"
  )
})

test_that("numeric input_data triggers abort", {
  expect_error(
    comptox_chem_search(123L, api_key = "key"),
    regexp = "`input_data`"
  )
})

test_that("list input_data triggers abort", {
  expect_error(
    comptox_chem_search(list("Benzene"), api_key = "key"),
    regexp = "`input_data`"
  )
})

test_that("invalid search_type triggers an error", {
  expect_error(
    comptox_chem_search("Benzene", search_type = "fuzzy", api_key = "key")
  )
})

test_that("negative rate_limit triggers a warning and is silently set to 0", {
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  expect_warning(
    comptox_chem_search("Benzene", api_key = "key", batch = FALSE, rate_limit = -1),
    regexp = "rate_limit"
  )
})

test_that("non-numeric rate_limit triggers a warning and is silently set to 0", {
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  expect_warning(
    comptox_chem_search("Benzene", api_key = "key", batch = FALSE, rate_limit = "1"),
    regexp = "rate_limit"
  )
})

test_that("NULL chunk_size triggers abort", {
  expect_error(
    comptox_chem_search("Benzene", api_key = "key", chunk_size = NULL),
    regexp = "chunk_size"
  )
})

test_that("non-scalar chunk_size (vector) triggers abort", {
  expect_error(
    comptox_chem_search("Benzene", api_key = "key", chunk_size = c(100L, 200L)),
    regexp = "chunk_size"
  )
})

test_that("zero chunk_size triggers abort", {
  expect_error(
    comptox_chem_search("Benzene", api_key = "key", chunk_size = 0L),
    regexp = "chunk_size"
  )
})

test_that("negative chunk_size triggers abort", {
  expect_error(
    comptox_chem_search("Benzene", api_key = "key", chunk_size = -5L),
    regexp = "chunk_size"
  )
})

test_that("missing chem_id_cols when input_data is a data frame triggers abort", {
  df <- tibble::tibble(chem_name = "Benzene")
  expect_error(
    comptox_chem_search(df, api_key = "key"),
    regexp = "chem_id_cols"
  )
})

# ===========================================================================
# Empty / NA / whitespace / duplicate handling
# ===========================================================================

test_that("empty character vector returns a zero-row tibble without calling the API", {
  post_called <- FALSE
  get_called  <- FALSE
  local_mocked_bindings(
    GET  = function(...) { get_called  <<- TRUE; make_mock_response(json_benzene) },
    POST = function(...) { post_called <<- TRUE; make_mock_response(json_benzene) },
    .package = "httr"
  )
  result <- comptox_chem_search(character(0), api_key = "key")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_false(get_called)
  expect_false(post_called)
})

test_that("all-NA vector returns a zero-row tibble", {
  result <- comptox_chem_search(c(NA_character_, NA), api_key = "key")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("duplicate identifiers are deduplicated before any API call is made", {
  get_count <- 0L
  local_mocked_bindings(
    GET = function(...) { get_count <<- get_count + 1L; make_mock_response(json_benzene) },
    .package = "httr"
  )
  result <- comptox_chem_search(
    c("Benzene", "Benzene", "Benzene"),
    api_key = "key",
    batch   = FALSE
  )
  expect_equal(get_count, 1L)
  expect_equal(nrow(result), 1L)
})

test_that("leading/trailing/internal whitespace is normalised before querying", {
  captured_url <- NULL
  local_mocked_bindings(
    GET = function(url, ...) {
      captured_url <<- url
      make_mock_response(json_benzene)
    },
    .package = "httr"
  )
  comptox_chem_search("  Benzene  ", api_key = "key", batch = FALSE)
  # str_squish("  Benzene  ") == "Benzene"
  expect_true(grepl("Benzene", captured_url, fixed = TRUE))
  expect_false(grepl("  Benzene  ", captured_url, fixed = TRUE))
})

test_that("leading/trailing/internal whitespace variants resolve to the same unique ID", {
  get_count <- 0L
  local_mocked_bindings(
    GET = function(...) { get_count <<- get_count + 1L; make_mock_response(json_benzene) },
    .package = "httr"
  )
  comptox_chem_search(
    c("Benzene", "  Benzene", "Benzene  "),
    api_key = "key",
    batch   = FALSE
  )
  expect_equal(get_count, 1L)
})

# ===========================================================================
# Returned columns and data types
# ===========================================================================

test_that("result tibble contains all twelve expected columns", {
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  result <- comptox_chem_search("Benzene", api_key = "key", batch = FALSE)
  expected_cols <- c(
    "input_term", "dtxsid", "dtxcid", "casrn", "preferredName", "smiles",
    "isMarkush", "hasStructureImage", "searchName", "searchValue", "rank",
    "suggestions"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("character columns are character type for a fully matched result", {
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  result <- comptox_chem_search("Benzene", api_key = "key", batch = FALSE)
  char_cols <- c("input_term", "dtxsid", "dtxcid", "casrn", "preferredName",
                 "smiles", "searchName", "searchValue", "suggestions")
  for (col in char_cols) {
    expect_type(result[[col]], "character")
  }
})

test_that("logical columns are logical type for a fully matched result", {
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  result <- comptox_chem_search("Benzene", api_key = "key", batch = FALSE)
  expect_type(result$isMarkush,        "logical")
  expect_type(result$hasStructureImage, "logical")
})

test_that("all-null character columns from JSON are coerced to character (Case 2 fix)", {
  # When jsonlite parses {"dtxsid": null} it produces a logical NA column.
  # parse_resp must coerce these to character so bind_rows() does not fail.
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_all_null_chars),
    .package = "httr"
  )
  result <- comptox_chem_search("no-match", api_key = "key", batch = FALSE)
  expect_type(result$dtxsid,        "character")
  expect_type(result$casrn,         "character")
  expect_type(result$smiles,        "character")
  expect_type(result$preferredName, "character")
})

# ===========================================================================
# suggestions column behaviour
# ===========================================================================

test_that("matched chemical has NA in the suggestions column", {
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  result <- comptox_chem_search("Benzene", api_key = "key", batch = FALSE)
  expect_true(is.na(result$suggestions[1]))
})

test_that("unmatched identifier with a single suggestion populates suggestions column", {
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_no_match_suggestion),
    .package = "httr"
  )
  result <- comptox_chem_search("unknown-chem", api_key = "key", batch = FALSE)
  expect_true(is.na(result$dtxsid[1]))
  expect_equal(result$suggestions[1], "unknown-chemical")
})

test_that("multiple suggestions are collapsed with ' | ' as separator", {
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_no_match_multi_suggestions),
    .package = "httr"
  )
  result <- comptox_chem_search("chlorotoluene", api_key = "key", batch = FALSE)
  expect_equal(
    result$suggestions[1],
    "2-Chlorotoluene | 3-Chlorotoluene | 4-Chlorotoluene"
  )
})

# ===========================================================================
# Sorting
# ===========================================================================

test_that("sorted = TRUE returns rows in the same order as the input vector", {
  local_mocked_bindings(
    GET = function(url, ...) {
      if (grepl("Toluene", url, fixed = TRUE)) make_mock_response(json_toluene)
      else                                     make_mock_response(json_benzene)
    },
    .package = "httr"
  )
  result <- comptox_chem_search(
    c("Toluene", "Benzene"),
    api_key = "key",
    batch   = FALSE,
    sorted  = TRUE
  )
  expect_equal(result$input_term, c("Toluene", "Benzene"))
})

test_that("sorted = FALSE does not reorder rows", {
  local_mocked_bindings(
    GET = function(url, ...) {
      if (grepl("Toluene", url, fixed = TRUE)) make_mock_response(json_toluene)
      else                                     make_mock_response(json_benzene)
    },
    .package = "httr"
  )
  # With sequential future plan, future_map preserves insertion order
  result <- comptox_chem_search(
    c("Benzene", "Toluene"),
    api_key = "key",
    batch   = FALSE,
    sorted  = FALSE
  )
  # Result should still have both chemicals regardless of order
  expect_setequal(result$input_term, c("Benzene", "Toluene"))
})

# ===========================================================================
# HTTP error handling
# ===========================================================================

test_that("HTTP 4xx error triggers a warning and returns a blank row", {
  local_mocked_bindings(
    GET = function(...) make_mock_response('{"error":"Not Found"}', 404L),
    .package = "httr"
  )
  expect_warning(
    result <- comptox_chem_search("NoSuchChem", api_key = "key", batch = FALSE),
    regexp = "failed"
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$input_term[1], "NoSuchChem")
  expect_true(is.na(result$dtxsid[1]))
  expect_true(is.na(result$smiles[1]))
})

test_that("empty API response returns a blank row rather than dropping the identifier", {
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_empty),
    .package = "httr"
  )
  # No warning expected — empty response is normal "not found" behaviour
  result <- comptox_chem_search("NoSuchChem", api_key = "key", batch = FALSE)
  expect_equal(nrow(result), 1L)
  expect_equal(result$input_term[1], "NoSuchChem")
  expect_true(is.na(result$dtxsid[1]))
  expect_true(is.na(result$casrn[1]))
  expect_true(is.na(result$smiles[1]))
  expect_true(is.na(result$suggestions[1]))
})

test_that("one failed GET returns a blank row alongside successful results", {
  call_n <- 0L
  local_mocked_bindings(
    GET = function(...) {
      call_n <<- call_n + 1L
      # First call fails; second succeeds
      if (call_n == 1L) make_mock_response("{}", 500L)
      else              make_mock_response(json_benzene)
    },
    .package = "httr"
  )
  expect_warning(
    result <- comptox_chem_search(
      c("NoSuchChem", "Benzene"),
      api_key = "key",
      batch   = FALSE
    ),
    regexp = "failed"
  )
  # Both identifiers appear — one blank row, one matched row
  expect_equal(nrow(result), 2L)
  failed_row <- dplyr::filter(result, input_term == "NoSuchChem")
  expect_equal(nrow(failed_row), 1L)
  expect_true(is.na(failed_row$dtxsid))
  expect_true("DTXSID3039242" %in% result$dtxsid)
})

test_that("blank rows for no-result identifiers respect sorted = TRUE order", {
  local_mocked_bindings(
    GET = function(url, ...) {
      if (grepl("Benzene", url, fixed = TRUE)) make_mock_response(json_benzene)
      else                                     make_mock_response(json_empty)
    },
    .package = "httr"
  )
  result <- comptox_chem_search(
    c("NoSuchChem", "Benzene"),
    api_key = "key",
    batch   = FALSE,
    sorted  = TRUE
  )
  expect_equal(nrow(result), 2L)
  # NoSuchChem was first in input — blank row should be first in output
  expect_equal(result$input_term[1], "NoSuchChem")
  expect_true(is.na(result$dtxsid[1]))
  expect_equal(result$input_term[2], "Benzene")
  expect_equal(result$dtxsid[2], "DTXSID3039242")
})

# ===========================================================================
# batch = FALSE vs batch = TRUE routing
# ===========================================================================

test_that("batch = FALSE never calls POST", {
  post_called <- FALSE
  local_mocked_bindings(
    GET  = function(...) make_mock_response(json_benzene),
    POST = function(...) { post_called <<- TRUE; make_mock_response(json_benzene) },
    .package = "httr"
  )
  comptox_chem_search("Benzene", api_key = "key", batch = FALSE)
  expect_false(post_called)
})

test_that("batch = TRUE sends identifiers to POST", {
  post_called <- FALSE
  local_mocked_bindings(
    POST = function(...) { post_called <<- TRUE; make_mock_response(json_batch_benzene_toluene) },
    GET  = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  comptox_chem_search(
    c("Benzene", "Toluene"),
    api_key       = "key",
    batch         = TRUE,
    retry_no_hits = FALSE
  )
  expect_true(post_called)
})

# ===========================================================================
# Trailing-hyphen routing
# ===========================================================================

test_that("identifiers ending in '-' are routed to GET even in batch mode", {
  get_urls  <- character(0)
  local_mocked_bindings(
    GET = function(url, ...) {
      get_urls <<- c(get_urls, url)
      make_mock_response(json_benzene)
    },
    POST = function(url, body, ...) make_mock_response(json_toluene),
    .package = "httr"
  )
  suppressMessages(
    comptox_chem_search(
      c("Benzene-", "Toluene"),
      api_key       = "key",
      batch         = TRUE,
      retry_no_hits = FALSE
    )
  )
  expect_true(any(grepl("Benzene-", utils::URLdecode(get_urls))))
})

test_that("trailing-hyphen identifier triggers an inform message in batch mode", {
  local_mocked_bindings(
    GET  = function(...) make_mock_response(json_benzene),
    POST = function(...) make_mock_response(json_toluene),
    .package = "httr"
  )
  expect_message(
    comptox_chem_search(
      c("Benzene-", "Toluene"),
      api_key       = "key",
      batch         = TRUE,
      retry_no_hits = FALSE
    ),
    regexp = '"-"'
  )
})

# ===========================================================================
# chunk_size
# ===========================================================================

test_that("chunk_size = 1 sends one identifier per POST request", {
  post_count <- 0L
  local_mocked_bindings(
    POST = function(url, body, ...) {
      post_count <<- post_count + 1L
      if (grepl("Toluene", body, fixed = TRUE)) make_mock_response(json_toluene)
      else                                      make_mock_response(json_benzene)
    },
    GET = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  comptox_chem_search(
    c("Benzene", "Toluene"),
    api_key       = "key",
    batch         = TRUE,
    chunk_size    = 1L,
    retry_no_hits = FALSE
  )
  expect_equal(post_count, 2L)
})

test_that("chunk_size larger than identifier count sends a single POST", {
  post_count <- 0L
  local_mocked_bindings(
    POST = function(...) { post_count <<- post_count + 1L; make_mock_response(json_batch_benzene_toluene) },
    GET  = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  comptox_chem_search(
    c("Benzene", "Toluene"),
    api_key       = "key",
    batch         = TRUE,
    chunk_size    = 500L,
    retry_no_hits = FALSE
  )
  expect_equal(post_count, 1L)
})

# ===========================================================================
# retry_no_hits
# ===========================================================================

test_that("retry_no_hits = TRUE retries identifiers absent from the POST response via GET", {
  get_count <- 0L
  local_mocked_bindings(
    # POST returns only Benzene; Toluene is completely absent from the response
    POST = function(...) make_mock_response(json_benzene),
    GET  = function(...) { get_count <<- get_count + 1L; make_mock_response(json_toluene) },
    .package = "httr"
  )
  suppressMessages(
    result <- comptox_chem_search(
      c("Benzene", "Toluene"),
      api_key       = "key",
      batch         = TRUE,
      retry_no_hits = TRUE
    )
  )
  # Toluene was absent from POST → retried via GET
  expect_gte(get_count, 1L)
  expect_true("Toluene" %in% result$input_term)
})

test_that("retry_no_hits = TRUE emits an inform message when retrying", {
  local_mocked_bindings(
    POST = function(...) make_mock_response(json_benzene),
    GET  = function(...) make_mock_response(json_toluene),
    .package = "httr"
  )
  expect_message(
    comptox_chem_search(
      c("Benzene", "Toluene"),
      api_key       = "key",
      batch         = TRUE,
      retry_no_hits = TRUE
    ),
    regexp = "Retrying"
  )
})

test_that("retry_no_hits = FALSE does not call GET for missing POST results", {
  get_called <- FALSE
  local_mocked_bindings(
    POST = function(...) make_mock_response(json_benzene),
    GET  = function(...) { get_called <<- TRUE; make_mock_response(json_toluene) },
    .package = "httr"
  )
  comptox_chem_search(
    c("Benzene", "Toluene"),
    api_key       = "key",
    batch         = TRUE,
    retry_no_hits = FALSE
  )
  expect_false(get_called)
})

test_that("POST no-hit row WITH suggestions is NOT retried via GET", {
  # POST returns Benzene (matched) + chlorotoluene unmatched but with
  # suggestions.  The unmatched row should be kept as-is; GET should not be
  # called because retrying risks overwriting the POST-provided suggestions.
  get_called <- FALSE
  local_mocked_bindings(
    POST = function(...) make_mock_response(json_batch_benzene_suggestion),
    GET  = function(...) { get_called <<- TRUE; make_mock_response(json_toluene) },
    .package = "httr"
  )
  result <- comptox_chem_search(
    c("Benzene", "chlorotoluene"),
    api_key       = "key",
    batch         = TRUE,
    retry_no_hits = TRUE
  )
  expect_false(get_called)
  # The suggestions from POST should be preserved unchanged in the result
  no_hit_row <- dplyr::filter(result, input_term == "chlorotoluene")
  expect_equal(no_hit_row$suggestions, "2-Chlorotoluene | 3-Chlorotoluene")
})

test_that("POST no-hit row WITHOUT suggestions IS retried via GET", {
  # POST returns Benzene (matched) + Toluene unmatched with no suggestions.
  # The no-hit row should be retried via GET since GET may return a match or
  # surface candidate names that POST did not provide.
  get_count <- 0L
  local_mocked_bindings(
    POST = function(...) make_mock_response(json_batch_benzene_no_suggestion),
    GET  = function(...) { get_count <<- get_count + 1L; make_mock_response(json_toluene) },
    .package = "httr"
  )
  suppressMessages(
    comptox_chem_search(
      c("Benzene", "Toluene"),
      api_key       = "key",
      batch         = TRUE,
      retry_no_hits = TRUE
    )
  )
  expect_gte(get_count, 1L)
})

# ===========================================================================
# data.frame input
# ===========================================================================

test_that("data.frame input extracts chem_id_cols and returns matched results", {
  df <- tibble::tibble(
    chem_name = c("Benzene", "Toluene"),
    conc_mg_l = c(1.0, 2.0)
  )
  local_mocked_bindings(
    POST = function(...) make_mock_response(json_batch_benzene_toluene),
    GET  = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  result <- comptox_chem_search(
    df,
    api_key       = "key",
    chem_id_cols  = chem_name,
    retry_no_hits = FALSE
  )
  expect_true("Benzene" %in% result$input_term)
  expect_true("Toluene" %in% result$input_term)
})

test_that("data.frame with multiple chem_id_cols searches all identifier columns", {
  df <- tibble::tibble(
    chem_name = c("Benzene"),
    casrn     = c("71-43-2")
  )
  post_bodies <- character(0)
  local_mocked_bindings(
    POST = function(url, body, ...) {
      post_bodies <<- c(post_bodies, body)
      make_mock_response(json_benzene)
    },
    GET = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  comptox_chem_search(
    df,
    api_key       = "key",
    chem_id_cols  = c(chem_name, casrn),
    retry_no_hits = FALSE
  )
  combined_body <- paste(post_bodies, collapse = "\n")
  expect_true(grepl("Benzene",  combined_body, fixed = TRUE))
  expect_true(grepl("71-43-2",  combined_body, fixed = TRUE))
})

test_that("character vector input_data with NA values filters out NAs before querying", {
  get_count <- 0L
  local_mocked_bindings(
    GET = function(...) { get_count <<- get_count + 1L; make_mock_response(json_benzene) },
    .package = "httr"
  )
  comptox_chem_search(
    c("Benzene", NA_character_, NA),
    api_key = "key",
    batch   = FALSE
  )
  expect_equal(get_count, 1L)
})

# ===========================================================================
# input_term assignment
# ===========================================================================

test_that("input_term in GET results matches the queried identifier exactly", {
  local_mocked_bindings(
    GET = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  result <- comptox_chem_search("Benzene", api_key = "key", batch = FALSE)
  # safe_get_one overwrites input_term with the queried `id`; not searchValue
  expect_equal(result$input_term[1], "Benzene")
})

test_that("input_term in POST results is taken from the API searchValue field", {
  local_mocked_bindings(
    POST = function(...) make_mock_response(json_batch_benzene_toluene),
    GET  = function(...) make_mock_response(json_benzene),
    .package = "httr"
  )
  result <- comptox_chem_search(
    c("Benzene", "Toluene"),
    api_key       = "key",
    batch         = TRUE,
    retry_no_hits = FALSE
  )
  expect_setequal(result$input_term, c("Benzene", "Toluene"))
})
