#' Verify that multiple chemical identifiers per row resolve to the same chemical
#'
#' For each row of \code{input_data}, searches the CompTox Chemical Dashboard
#' for every identifier in \code{chem_id_cols} and determines whether they all
#' resolve to the same chemical (DTXSID). When all identifiers agree, one row
#' is returned. When identifiers resolve to different chemicals, one row per
#' distinct DTXSID is returned. Rows where no identifier matched are returned
#' once with \code{NA} chemical data.
#'
#' @details
#' ## Suggestion cross-validation
#'
#' When an identifier returns no direct match but the API provides candidate
#' suggestions, those candidates are searched in a second API call. A
#' suggestion-derived DTXSID is accepted for that identifier only if it matches
#' the DTXSID already directly resolved by another identifier in the same row.
#' Suggestions that cannot be cross-validated against at least one direct hit
#' are discarded, and the identifier is treated as unresolved.
#'
#' ## Column name conflicts
#'
#' The function appends the following columns to \code{input_data}:
#' \code{dtxsid}, \code{dtxcid}, \code{preferredName}, \code{smiles},
#' \code{isMarkush}, \code{hasStructureImage}, \code{rank}, \code{agreement}.
#' If \code{input_data} already contains any of these names the function aborts
#' with an informative error. Rename the conflicting column(s) before calling.
#'
#' The CompTox API \code{casrn} field is intentionally excluded from the output
#' to avoid clashing with a user \code{casrn} input column. Retrieve it via
#' \code{\link{get_struc_from_id}()} or \code{\link{comptox_chem_search}()} if
#' needed.
#'
#' @param input_data A data frame or tibble containing chemical identifier
#'   columns.
#' @param api_key Your CompTox Dashboard API key (character scalar). Register
#'   at
#'   \url{https://www.epa.gov/comptox-tools/computational-toxicology-and-exposure-apis}.
#' @param chem_id_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Two or more
#'   columns containing chemical identifiers (e.g. \code{c(chem_name, casrn)}).
#'   All non-missing, non-blank values across the selected columns are searched.
#'   At least two columns are required.
#' @param batch Logical (default \code{TRUE}). Passed to
#'   \code{\link{comptox_chem_search}()}.
#' @param chunk_size Positive integer (default \code{200L}). Passed to
#'   \code{\link{comptox_chem_search}()}.
#' @param rate_limit Non-negative numeric (default \code{0}). Passed to
#'   \code{\link{comptox_chem_search}()}.
#'
#' @return A tibble containing all columns from \code{input_data} followed by:
#'   \describe{
#'     \item{\code{dtxsid}}{DTXSID resolved from the CompTox API (\code{NA}
#'       for \code{"no_match"} rows).}
#'     \item{\code{dtxcid}}{DTXCID corresponding to \code{dtxsid}.}
#'     \item{\code{preferredName}}{Preferred chemical name from the API.}
#'     \item{\code{smiles}}{SMILES structure string.}
#'     \item{\code{isMarkush}}{Logical; whether the structure is a Markush
#'       structure.}
#'     \item{\code{hasStructureImage}}{Logical; whether a structure image is
#'       available.}
#'     \item{\code{rank}}{API search rank.}
#'     \item{\code{agreement}}{One of \code{"agree"}, \code{"disagree"}, or
#'       \code{"no_match"}.
#'       \describe{
#'         \item{\code{"agree"}}{All non-missing identifiers that resolved did
#'           so to the same DTXSID (one output row).}
#'         \item{\code{"disagree"}}{At least two identifiers resolved to
#'           different DTXSIDs; the input row is expanded to one output row per
#'           distinct DTXSID.}
#'         \item{\code{"no_match"}}{No identifier resolved to a DTXSID (one
#'           output row with \code{NA} chemical data).}
#'       }
#'     }
#'   }
#'   Rows are ordered to match \code{input_data}; expanded \code{"disagree"}
#'   rows for a given input row appear consecutively.
#'
#' @seealso \code{\link{comptox_chem_search}()},
#'   \code{\link{deduplicate_chem_results}()}
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   chem_name = c("Benzene", "Toluene",  "Benzene"),
#'   casrn     = c("71-43-2", "71-43-2",  "108-88-3"),
#'   conc      = c(1.0,        2.0,         3.0)
#' )
#'
#' # Row 1: both identifiers resolve to Benzene          -> 1 row ("agree")
#' # Row 2: name=Toluene DTXSID, casrn=Benzene DTXSID   -> 2 rows ("disagree")
#' # Row 3: name=Benzene DTXSID, casrn=Toluene DTXSID   -> 2 rows ("disagree")
#' result <- verify_chem_identifiers(
#'   input_data   = df,
#'   api_key      = Sys.getenv("COMPTOX_API_KEY"),
#'   chem_id_cols = c(chem_name, casrn)
#' )
#' }
#'
#' @export
#' @importFrom dplyr mutate filter select distinct left_join group_by summarise
#'   arrange rename n_distinct bind_rows all_of any_of case_when row_number
#' @importFrom tidyr pivot_longer unnest replace_na drop_na
#' @importFrom purrr map pmap_chr
#' @importFrom stringr str_squish str_split fixed
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform .data
verify_chem_identifiers <- function(
    input_data,
    api_key,
    chem_id_cols,
    batch      = TRUE,
    chunk_size = 200L,
    rate_limit = 0
) {

  ## 0. Input validation -------------------------------------------------------

  if (!is.data.frame(input_data))
    rlang::abort("`input_data` must be a data frame or tibble.")

  if (missing(api_key))
    rlang::abort("Please provide your API key via `api_key`.")
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key))
    rlang::abort("`api_key` must be a non-empty character scalar.")

  if (missing(chem_id_cols))
    rlang::abort("`chem_id_cols` is required.")

  id_col_names <- names(dplyr::select(input_data, {{ chem_id_cols }}))

  if (length(id_col_names) < 2L)
    rlang::abort(paste0(
      "`chem_id_cols` must select at least 2 columns; ",
      length(id_col_names), " column(s) selected."
    ))

  if (!is.numeric(rate_limit) || length(rate_limit) != 1L || rate_limit < 0) {
    rlang::warn("`rate_limit` must be a non-negative number; setting to 0.")
    rate_limit <- 0
  }

  if (!is.numeric(chunk_size) || length(chunk_size) != 1L)
    rlang::abort("`chunk_size` must be a single positive integer.")
  chunk_size <- as.integer(chunk_size)
  if (is.na(chunk_size) || chunk_size < 1L)
    rlang::abort("`chunk_size` must be a positive integer.")

  # Abort early if the user's column names would clash with the CompTox output
  # columns that this function appends to the result.
  comptox_out_cols <- c(
    "dtxsid", "dtxcid", "preferredName", "smiles",
    "isMarkush", "hasStructureImage", "rank", "agreement"
  )
  orig_col_names <- names(input_data)
  clashing       <- intersect(orig_col_names, comptox_out_cols)
  if (length(clashing) > 0L)
    rlang::abort(paste0(
      "`input_data` contains column(s) whose names conflict with CompTox output ",
      "columns: ", paste(clashing, collapse = ", "), ". ",
      "Please rename these column(s) before calling `verify_chem_identifiers()`."
    ))

  ## 0b. Add internal row index ------------------------------------------------

  input_data <- dplyr::mutate(input_data, .orig_row = dplyr::row_number())

  ## 1. Build long identifier table --------------------------------------------

  id_long <- input_data |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(id_col_names),
      names_to  = ".id_col",
      values_to = ".id_value"
    ) |>
    dplyr::mutate(.id_value = stringr::str_squish(as.character(.data$.id_value))) |>
    tidyr::drop_na(.id_value) |>
    dplyr::filter(nzchar(.data$.id_value)) |>
    dplyr::select(.orig_row, .id_col, .id_value)

  all_ids <- unique(id_long$.id_value)

  ## Early return: all identifier values were NA or blank ---------------------

  if (length(all_ids) == 0L) {
    return(
      dplyr::select(input_data, -.orig_row) |>
        dplyr::bind_cols(
          tibble::tibble(
            dtxsid            = NA_character_,
            dtxcid            = NA_character_,
            preferredName     = NA_character_,
            smiles            = NA_character_,
            isMarkush         = NA,
            hasStructureImage = NA,
            rank              = NA_integer_,
            agreement         = "no_match"
          )
        )
    )
  }

  ## 2. Initial search ---------------------------------------------------------

  rlang::inform(
    paste0(
      "Searching ", length(all_ids), " unique identifier(s) across ",
      length(id_col_names), " column(s)..."
    )
  )

  initial_results <- comptox_chem_search(
    input_data    = all_ids,
    api_key       = api_key,
    batch         = batch,
    chunk_size    = chunk_size,
    rate_limit    = rate_limit,
    retry_no_hits = TRUE
  )

  ## 3. Suggestion retry -------------------------------------------------------

  has_sugg <- dplyr::filter(
    initial_results,
    is.na(.data$dtxsid),
    !is.na(.data$suggestions)
  )

  sugg_map <- if (nrow(has_sugg) > 0L) {
    has_sugg |>
      dplyr::select(input_term, suggestions) |>
      dplyr::mutate(
        suggestion_term = stringr::str_split(
          .data$suggestions,
          stringr::fixed(" | ")
        )
      ) |>
      tidyr::unnest(cols = "suggestion_term") |>
      dplyr::mutate(suggestion_term = stringr::str_squish(.data$suggestion_term)) |>
      dplyr::filter(nzchar(.data$suggestion_term))
  } else {
    tibble::tibble(
      input_term      = character(0),
      suggestion_term = character(0)
    )
  }

  unique_candidates <- unique(sugg_map$suggestion_term)

  retry_results <- if (length(unique_candidates) > 0L) {
    rlang::inform(
      paste0("Retrying ", length(unique_candidates), " suggestion candidate(s)...")
    )
    comptox_chem_search(
      input_data = unique_candidates,
      api_key    = api_key,
      batch      = batch,
      chunk_size = chunk_size,
      rate_limit = rate_limit
    )
  } else {
    initial_results[0L, ]
  }

  sugg_lookup <- if (nrow(sugg_map) > 0L && nrow(retry_results) > 0L) {
    sugg_map |>
      dplyr::left_join(
        dplyr::select(
          retry_results,
          suggestion_term = input_term,
          sugg_dtxsid     = dtxsid
        ),
        by = "suggestion_term"
      ) |>
      dplyr::filter(!is.na(.data$sugg_dtxsid)) |>
      dplyr::distinct(.data$input_term, .data$sugg_dtxsid) |>
      dplyr::group_by(.data$input_term) |>
      dplyr::summarise(
        sugg_dtxsids = list(.data$sugg_dtxsid),
        .groups      = "drop"
      )
  } else {
    tibble::tibble(
      input_term   = character(0),
      sugg_dtxsids = vector("list", 0L)
    )
  }

  ## 4. Build lookup tables ----------------------------------------------------

  direct_lookup <- dplyr::filter(initial_results, !is.na(.data$dtxsid)) |>
    dplyr::select(input_term, direct_dtxsid = dtxsid)

  metadata_lookup <- dplyr::bind_rows(initial_results, retry_results) |>
    dplyr::filter(!is.na(.data$dtxsid)) |>
    dplyr::distinct(.data$dtxsid, .keep_all = TRUE) |>
    dplyr::select(
      dtxsid, dtxcid, preferredName, smiles, isMarkush, hasStructureImage, rank
    )

  ## 5. Per-row resolution with suggestion cross-validation --------------------

  id_resolved <- id_long |>
    dplyr::left_join(direct_lookup, by = c(".id_value" = "input_term")) |>
    dplyr::left_join(sugg_lookup,   by = c(".id_value" = "input_term"))

  # Build anchor set: union of directly-resolved DTXSIDs within each input row.
  # A suggestion-derived DTXSID is accepted only if it appears in this set,
  # preventing suggestions from one identifier validating another suggestion.
  row_anchors <- id_resolved |>
    dplyr::filter(!is.na(.data$direct_dtxsid)) |>
    dplyr::distinct(.data$.orig_row, .data$direct_dtxsid) |>
    dplyr::group_by(.data$.orig_row) |>
    dplyr::summarise(
      anchor_dtxsids = list(.data$direct_dtxsid),
      .groups        = "drop"
    )

  id_resolved <- dplyr::left_join(id_resolved, row_anchors, by = ".orig_row") |>
    dplyr::mutate(
      resolved_dtxsid = purrr::pmap_chr(
        list(.data$direct_dtxsid, .data$sugg_dtxsids, .data$anchor_dtxsids),
        function(direct, sugg, anchors) {
          if (!is.na(direct))                    return(direct)
          if (is.null(sugg) || is.null(anchors)) return(NA_character_)
          hit <- intersect(sugg, anchors)
          if (length(hit) > 0L) hit[[1L]] else NA_character_
        }
      )
    )

  ## 6. Agreement classification -----------------------------------------------

  row_summary <- id_resolved |>
    dplyr::group_by(.data$.orig_row) |>
    dplyr::summarise(
      resolved_dtxsids = list(
        unique(.data$resolved_dtxsid[!is.na(.data$resolved_dtxsid)])
      ),
      n_resolved       = dplyr::n_distinct(.data$resolved_dtxsid, na.rm = TRUE),
      .groups          = "drop"
    ) |>
    dplyr::mutate(
      agreement = dplyr::case_when(
        .data$n_resolved == 0L ~ "no_match",
        .data$n_resolved == 1L ~ "agree",
        TRUE                   ~ "disagree"
      ),
      # Replace empty-vector list entries (no_match rows) with list(NA_character_)
      # so that unnest() in Step 7 keeps those rows rather than dropping them.
      resolved_dtxsids = purrr::map(
        .data$resolved_dtxsids,
        ~ if (length(.x) == 0L) NA_character_ else .x
      )
    )

  # Ensure every input row is represented. Rows with all-NA/blank identifiers
  # were dropped in Step 1 and have no entry in row_summary.
  row_summary <- dplyr::left_join(
    tibble::tibble(.orig_row = seq_len(nrow(input_data))),
    row_summary,
    by = ".orig_row"
  ) |>
    dplyr::mutate(
      n_resolved       = tidyr::replace_na(.data$n_resolved, 0L),
      agreement        = tidyr::replace_na(.data$agreement, "no_match"),
      resolved_dtxsids = purrr::map(
        .data$resolved_dtxsids,
        ~ if (is.null(.x)) NA_character_ else .x
      )
    )

  ## 7. Expand and assemble output ---------------------------------------------

  expanded <- row_summary |>
    tidyr::unnest(cols = "resolved_dtxsids") |>
    dplyr::rename(dtxsid = resolved_dtxsids) |>
    dplyr::left_join(metadata_lookup, by = "dtxsid")

  dplyr::left_join(
    dplyr::select(
      expanded,
      .orig_row, agreement,
      dtxsid, dtxcid, preferredName, smiles, isMarkush, hasStructureImage, rank
    ),
    dplyr::select(input_data, .orig_row, dplyr::all_of(orig_col_names)),
    by = ".orig_row"
  ) |>
    dplyr::arrange(.data$.orig_row) |>
    dplyr::select(
      dplyr::all_of(orig_col_names),
      dtxsid, dtxcid, preferredName, smiles,
      isMarkush, hasStructureImage, rank,
      agreement
    )
}
