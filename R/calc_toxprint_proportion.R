# ToxPrint proportion functions -----------------------------------------------


#' Calculate ToxPrint Chemotype Proportions by Dataset
#'
#' @description
#' Calculates the proportion of chemicals in each dataset that contain each
#' ToxPrint chemotype, aggregated to a specified hierarchy level. Chemicals
#' positive for any individual ToxPrint that maps to the same level-name are
#' counted as positive for that level-name.
#'
#' @param toxprint_data A data frame/tibble with a dataset ID column, a
#'   chemical ID column, and binary ToxPrint columns (1 = present, 0 = absent).
#' @param tox_levels_data A data frame/tibble containing the ToxPrint naming
#'   hierarchy. Defaults to the bundled \code{\link{toxprint_levels}} dataset
#'   shipped with the package — users do not need to supply this separately.
#' @param toxprint_level Integer specifying the ToxPrint hierarchy level to
#'   aggregate to (2–5). Defaults to \code{5}.
#' @param dataset_id_col <[`data-masked`][rlang::args_data_masking]> Unquoted
#'   name of the column in \code{toxprint_data} that identifies the dataset.
#'   Defaults to \code{dataset}.
#' @param chem_id_col <[`data-masked`][rlang::args_data_masking]> Unquoted name
#'   of the column in \code{toxprint_data} that identifies each chemical.
#'   Defaults to \code{CASRN}.
#' @param scale_to_dataset Optional character string naming the reference
#'   dataset to scale counts against (i.e. counts for every dataset are
#'   multiplied by \code{ref_size / dataset_size}). If \code{NULL} (default)
#'   no scaling is applied.
#'
#' @return A tibble with one row per unique level-name × dataset combination,
#'   containing:
#'   \describe{
#'     \item{<level_col>}{The aggregated ToxPrint name at the chosen level.}
#'     \item{<dataset_id_col>}{The dataset identifier.}
#'     \item{presence}{1 if the chemotype is present in the dataset, 0 otherwise.}
#'     \item{chem_count}{Number of distinct chemicals positive for the chemotype.}
#'     \item{tot_chem}{Total distinct chemicals in the dataset.}
#'     \item{chem_count_scaled}{Scaled chemical count (equal to \code{chem_count}
#'       when \code{scale_to_dataset = NULL}).}
#'     \item{freq}{Proportion of chemicals positive for the chemotype
#'       (\code{chem_count / tot_chem}), rounded to 4 decimal places.}
#'   }
#'   Rows are sorted in descending order of \code{freq}.
#'
#' @details
#' ## Progress reporting
#' The function uses \pkg{progressr} for optional progress reporting. Wrap the
#' call with \code{\link[progressr:with_progress]{progressr::with_progress()}}
#' to display a progress bar:
#' ```r
#' progressr::with_progress({
#'   result <- calculate_toxprint_proportion(toxprint_data)
#' })
#' ```
#' The progress bar advances through five labelled steps: computing dataset
#' sizes, applying scaling factors, pivoting to long format, joining and
#' counting, and finalising.
#'
#' @seealso [calculate_toxprints_per_chem()] for per-chemical ToxPrint counts.
#'
#' @export
#'
#' @importFrom dplyr summarise n_distinct filter pull mutate left_join join_by
#'   distinct arrange desc if_else select
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom glue glue
#' @importFrom progressr progressor
calculate_toxprint_proportion <- function(
    toxprint_data,
    tox_levels_data = toxprint_levels,
    toxprint_level = 5,
    dataset_id_col = dataset,
    chem_id_col = CASRN,
    scale_to_dataset = NULL) {

  p <- progressr::progressor(steps = 5)

  # Convert level number into column name
  chemotype_level_col <- glue::glue("level_{toxprint_level}_full")

  # Validate that the requested level exists in tox_levels_data
  if (!(chemotype_level_col %in% colnames(tox_levels_data))) {
    stop(glue::glue(
      "The specified ToxPrint level '{toxprint_level}' does not exist in ",
      "tox_levels_data. Valid levels are: ",
      "{paste(2:5, collapse = ', ')}."
    ))
  }

  # Step 1 – Calculate per-dataset chemical counts
  p(message = "Calculating dataset sizes...")
  dataset_sizes <- toxprint_data |>
    dplyr::summarise(
      tot_chem = dplyr::n_distinct({{ chem_id_col }}),
      .by = {{ dataset_id_col }}
    )

  # Step 2 – Compute scaling factors
  p(message = "Applying scaling factors...")
  if (!is.null(scale_to_dataset)) {
    ref_size <- dataset_sizes |>
      dplyr::filter({{ dataset_id_col }} == scale_to_dataset) |>
      dplyr::pull(tot_chem)

    dataset_sizes <- dataset_sizes |>
      dplyr::mutate(scaling_factor = ref_size / tot_chem)
  } else {
    dataset_sizes <- dataset_sizes |>
      dplyr::mutate(scaling_factor = 1)
  }

  # Step 3 – Pivot ToxPrint columns to long format
  p(message = "Pivoting ToxPrint data to long format...")
  toxprint_long <- toxprint_data |>
    tidyr::pivot_longer(
      cols      = -c({{ dataset_id_col }}, {{ chem_id_col }}),
      names_to  = "toxprint",
      values_to = "presence"
    )

  # Step 4 – Join hierarchy levels, aggregate to the chosen level, then join sizes
  p(message = "Joining ToxPrint hierarchy levels and counting...")
  toxprint_counts <- toxprint_long |>
    dplyr::left_join(
      tox_levels_data |>
        dplyr::select(toxprint_chemotype_name_original,
                      tidyselect::all_of(chemotype_level_col)),
      by = dplyr::join_by(toxprint == toxprint_chemotype_name_original)
    ) |>
    dplyr::mutate(
      chem_count = dplyr::n_distinct(
        dplyr::if_else(presence == 1, {{ chem_id_col }}, NA_character_),
        na.rm = TRUE
      ),
      presence = max(presence),
      .by = c({{ dataset_id_col }}, tidyselect::all_of(chemotype_level_col))
    ) |>
    dplyr::left_join(dataset_sizes, by = dplyr::join_by({{ dataset_id_col }})) |>
    dplyr::mutate(
      chem_count_scaled = chem_count * scaling_factor,
      freq              = round(chem_count / tot_chem, digits = 4)
    ) |>
    dplyr::select(
      tidyselect::all_of(chemotype_level_col), {{ dataset_id_col }},
      presence, chem_count, tot_chem, chem_count_scaled, freq
    ) |>
    dplyr::distinct()

  # Step 5 – Sort and return
  p(message = "Finalising results...")
  toxprint_counts |>
    dplyr::arrange(dplyr::desc(freq))
}


# ------------------------------------------------------------------------------


#' Count ToxPrint Chemotypes per Chemical
#'
#' @description
#' For each chemical in each dataset, counts how many distinct ToxPrint
#' chemotypes (at a specified hierarchy level) it is positive for.
#'
#' @param toxprint_data A data frame/tibble with a dataset ID column, a
#'   chemical ID column, and binary ToxPrint columns (1 = present, 0 = absent).
#' @param tox_levels_data A data frame/tibble containing the ToxPrint naming
#'   hierarchy. Defaults to the bundled \code{\link{toxprint_levels}} dataset
#'   shipped with the package — users do not need to supply this separately.
#' @param toxprint_level Integer specifying the ToxPrint hierarchy level to
#'   aggregate to (2–5). Defaults to \code{5}.
#' @param dataset_id_col <[`data-masked`][rlang::args_data_masking]> Unquoted
#'   name of the column in \code{toxprint_data} that identifies the dataset.
#'   Defaults to \code{dataset}.
#' @param chem_id_col <[`data-masked`][rlang::args_data_masking]> Unquoted name
#'   of the column in \code{toxprint_data} that identifies each chemical.
#'   Defaults to \code{CASRN}.
#'
#' @return A tibble with one row per unique dataset × chemical × level-name
#'   combination, containing:
#'   \describe{
#'     \item{<dataset_id_col>}{The dataset identifier.}
#'     \item{<chem_id_col>}{The chemical identifier.}
#'     \item{<level_col>}{The aggregated ToxPrint name at the chosen level.}
#'     \item{num_toxprints}{Number of distinct level-name chemotypes the
#'       chemical is positive for.}
#'   }
#'   Rows are sorted in descending order of \code{num_toxprints}.
#'
#' @details
#' ## Progress reporting
#' The function uses \pkg{progressr} for optional progress reporting. Wrap the
#' call with \code{\link[progressr:with_progress]{progressr::with_progress()}}
#' to display a progress bar:
#' ```r
#' progressr::with_progress({
#'   result <- calculate_toxprints_per_chem(toxprint_data)
#' })
#' ```
#' The progress bar advances through four labelled steps: pivoting to long
#' format, joining the hierarchy, counting per chemical, and finalising.
#'
#' @seealso [calculate_toxprint_proportion()] for dataset-level proportions.
#'
#' @export
#'
#' @importFrom dplyr left_join join_by mutate n_distinct distinct arrange desc
#'   if_else select
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom progressr progressor
calculate_toxprints_per_chem <- function(
    toxprint_data,
    tox_levels_data = toxprint_levels,
    toxprint_level = 5,
    dataset_id_col = dataset,
    chem_id_col = CASRN) {

  p <- progressr::progressor(steps = 4)

  # Convert level number into column name
  chemotype_level_col <- glue::glue("level_{toxprint_level}_full")

  # Validate that the requested level exists in tox_levels_data
  if (!(chemotype_level_col %in% colnames(tox_levels_data))) {
    stop(glue::glue(
      "The specified ToxPrint level '{toxprint_level}' does not exist in ",
      "tox_levels_data. Valid levels are: ",
      "{paste(2:5, collapse = ', ')}."
    ))
  }

  # Step 1 – Pivot ToxPrint columns to long format
  p(message = "Pivoting ToxPrint data to long format...")
  toxprint_long <- toxprint_data |>
    tidyr::pivot_longer(
      cols      = -c({{ dataset_id_col }}, {{ chem_id_col }}),
      names_to  = "toxprint",
      values_to = "presence"
    )

  # Step 2 – Join hierarchy levels
  p(message = "Joining ToxPrint hierarchy levels...")
  toxprint_joined <- toxprint_long |>
    dplyr::left_join(
      tox_levels_data |>
        dplyr::select(toxprint_chemotype_name_original,
                      tidyselect::all_of(chemotype_level_col)),
      by = dplyr::join_by(toxprint == toxprint_chemotype_name_original)
    )

  # Step 3 – Count distinct level-name chemotypes per chemical
  p(message = "Counting chemotypes per chemical...")
  toxprints_per_chem <- toxprint_joined |>
    dplyr::mutate(
      num_toxprints = dplyr::n_distinct(
        dplyr::if_else(presence == 1, .data[[chemotype_level_col]], NA_character_),
        na.rm = TRUE
      ),
      .by = c({{ dataset_id_col }}, {{ chem_id_col }})
    ) |>
    dplyr::select(
      {{ dataset_id_col }}, {{ chem_id_col }},
      tidyselect::all_of(chemotype_level_col), num_toxprints
    ) |>
    dplyr::distinct()

  # Step 4 – Sort and return
  p(message = "Finalising results...")
  toxprints_per_chem |>
    dplyr::arrange(dplyr::desc(num_toxprints))
}
