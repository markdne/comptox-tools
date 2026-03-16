#' @title Chemotype Enrichment
#'
#' @description
#' Function to generate chemotype enrichments.
#' This function can be used to generate a summary table of enrichment statistics for each chemotype.
#' Here, a chemotype is a set of binary features (e.g., chemical structure) that are used to predict a binary endpoint (e.g., assay hit call).
#'
#' Progress can be reported by wrapping the call in `progressr::with_progress()`.
#' Parallelisation is enabled via the `workers` argument; alternatively, set a `future::plan()` before calling.
#'
#' @param x Data frame/tibble containing a binary endpoint (e.g., assay hit call) as the first column and a set of features (chemotypes) (e.g., ToxPrints or other features) as the remaining columns.
#' @param endpoint_of_interest The endpoint of interest (e.g., assay) to be used in the enrichment analysis. This should be a bare variable name (i.e., not in quotes).
#' @param alt_hypothesis Alternative hypothesis for Fisher's exact test. Options include "greater", "less", "two.sided". Default is "greater".
#' @param adjust_method Method for adjusting p-values for multiple comparisons. Options include "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". Default is "BH" (Benjamini-Hochberg).
#' @param combo_sizes Integer vector specifying which combination sizes to compute enrichment for.
#'   Use `1` for individual chemotypes (default), `2` for pairwise combinations, `3` for triplets.
#'   Multiple values can be combined (e.g., `c(1, 2)` computes both individual and pairwise enrichments).
#'   Combination columns are 1 when ALL chemotypes in the combination are 1 (AND logic).
#'   Note: large numbers of chemotypes combined with `combo_sizes > 1` can be computationally expensive.
#' @param workers Number of parallel workers to use. Default is `1` (sequential). Set to a value
#'   greater than 1 to enable parallel processing via `future::multisession`. Alternatively,
#'   set a `future::plan()` before calling this function for more control over the parallel backend.
#'
#' @export
#'
#' @importFrom broom tidy
#' @importFrom dplyr across all_of arrange bind_cols bind_rows case_when desc group_by mutate rename select
#' @importFrom furrr future_map furrr_options
#' @importFrom future multisession plan
#' @importFrom janitor remove_constant
#' @importFrom stats fisher.test
#' @importFrom progressr progressor
#' @importFrom purrr list_cbind map walk
#' @importFrom stats p.adjust p.adjust.methods
#' @importFrom stringr str_count
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na nest pivot_longer pivot_wider
#' @importFrom yardstick conf_mat
#'

chemotype_enrichment <- function(x, endpoint_of_interest,
                                 alt_hypothesis = "greater",
                                 adjust_method = "BH",
                                 combo_sizes = 1L,
                                 workers = 1L) {
  
  # --- Input validation -------------------------------------------------------
  
  if (!alt_hypothesis %in% c("greater", "less", "two.sided")) {
    stop(sprintf(
      "Invalid 'alt_hypothesis': '%s'. Must be one of: greater, less, two.sided.\nSee `stats::fisher.test` for more information.",
      alt_hypothesis
    ))
  }
  
  if (!adjust_method %in% stats::p.adjust.methods) {
    stop(sprintf(
      "Invalid 'adjust_method': '%s'. Must be one of: %s.\nSee `stats::p.adjust` for more information.",
      adjust_method, paste(stats::p.adjust.methods, collapse = ", ")
    ))
  }
  
  combo_sizes <- sort(unique(as.integer(combo_sizes)))
  if (!all(combo_sizes %in% 1:3)) {
    stop("'combo_sizes' must contain integers between 1 and 3.")
  }
  
  
  # --- Parallelisation setup --------------------------------------------------
  
  if (workers > 1L) {
    old_plan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)
  }
  
  
  # --- Data preparation -------------------------------------------------------
  # Rename endpoint column, drop NA endpoints, and remove constant feature columns
  
  data_prep <- x |>
    dplyr::rename(endpoint = {{ endpoint_of_interest }}) |>
    tidyr::drop_na(endpoint) |>
    janitor::remove_constant(na.rm = TRUE)
  
  chemotype_cols <- setdiff(names(data_prep), "endpoint")
  
  
  # --- Combination column generation ------------------------------------------
  # Build AND-logic combination columns (value = 1 only when ALL chemotypes in the combo are 1)
  
  if (any(combo_sizes > 1L)) {
    
    purrr::walk(combo_sizes[combo_sizes > 1L], function(k) {
      n_combos <- choose(length(chemotype_cols), k)
      if (n_combos > 5000L) {
        warning(sprintf(
          paste0(
            "%d combinations of size %d will be computed. This may be slow. ",
            "Consider pre-filtering chemotypes (e.g. remove low-prevalence features) to reduce computation time."),
          n_combos, k
        ))
      }
    })
    
    combo_dfs <- purrr::map(
      combo_sizes[combo_sizes > 1L],
      function(k) {
        combos <- combn(chemotype_cols, k, simplify = FALSE)
        purrr::map(combos, function(cols) {
          tibble::tibble(
            !!paste(cols, collapse = " & ") := as.integer(
              rowSums(dplyr::select(data_prep, dplyr::all_of(cols))) == k
            )
          )
        }) |>
          purrr::list_cbind()
      }
    )
    
    data_prep <- if (1L %in% combo_sizes) {
      dplyr::bind_cols(data_prep, combo_dfs)
    } else {
      # Return only combination columns (no individual chemotypes)
      dplyr::bind_cols(dplyr::select(data_prep, endpoint), combo_dfs)
    }
  }
  
  # --- Nest data by chemotype -------------------------------------------------
  # Remove any all-zero combination columns, then pivot to long → nest by chemotype
  
  nested_data <- data_prep |>
    janitor::remove_constant(na.rm = TRUE) |>
    tidyr::pivot_longer(
      cols      = -endpoint,
      names_to  = "chemotype",
      values_to = "CT_count"
    ) |>
    # Factor with 1 as first level: required for correct conf_mat / yardstick orientation
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ factor(.x, levels = c(1, 0)))
    ) |>
    dplyr::group_by(chemotype) |>
    tidyr::nest()
  
  # --- Per-chemotype enrichment (parallelised) --------------------------------
  
  p <- progressr::progressor(steps = nrow(nested_data))
  
  per_ct_results <- furrr::future_map(
    nested_data$data,
    function(chem_data) {
      cm <- yardstick::conf_mat(
        chem_data,
        truth    = endpoint,
        estimate = CT_count,
        dnn      = c("chemotype", "endpoint")
      )
      
      # Flatten confusion matrix cells to named columns (TP, FN, FP, TN)
      tidy_cm <- broom::tidy(cm) |>
        dplyr::mutate(
          name = dplyr::case_when(
            name == "cell_1_1" ~ "TP",
            name == "cell_2_1" ~ "FN",
            name == "cell_1_2" ~ "FP",
            name == "cell_2_2" ~ "TN"
          )
        ) |>
        tidyr::pivot_wider(names_from = name, values_from = value)
      
      # Yardstick summary metrics (sensitivity, specificity, MCC, etc.)
      summary_tbl <- summary(cm) |>
        dplyr::select(-.estimator) |>
        tidyr::pivot_wider(names_from = .metric, values_from = .estimate)
      
      odds <- stats::fisher.test(cm$table, alternative = alt_hypothesis) |>
        broom::tidy()
      
      p()  # advance progress bar
      
      dplyr::bind_cols(tidy_cm, summary_tbl, odds)
    },
    .options = furrr::furrr_options(seed = FALSE)
  )
  
  
  # --- Assemble and finalise output -------------------------------------------
  
  enrichments <- dplyr::bind_cols(
    dplyr::select(nested_data, chemotype),
    dplyr::bind_rows(per_ct_results)
  ) |>
    dplyr::rename(OR = estimate) |>
    dplyr::mutate(CT_total = TP + FP) |>
    dplyr::mutate(p.value_adj = stats::p.adjust(p.value, method = adjust_method)) |>
    dplyr::select(
      chemotype, CT_total, TP, FN, FP, TN, OR, p.value, p.value_adj,
      mcc, sens, spec, bal_accuracy, precision, recall,
      f_meas, ppv, npv, conf.low, conf.high
    ) |>
    dplyr::arrange(dplyr::desc(OR), p.value)
  
  # Add n_chemotypes column when combination enrichments are included in results
  if (max(combo_sizes) > 1L) {
    enrichments <- enrichments |>
      dplyr::mutate(
        n_chemotypes = stringr::str_count(chemotype, " & ") + 1L,
        .after        = chemotype
      )
  }
  
  enrichments
}


# ------------------------------------------------------------------------------


#' @title Multi-Assay Chemotype Enrichment
#'
#' @description
#' Wrapper around `chemotype_enrichment()` that iterates over multiple assays.
#' Assays with no tested chemicals, or where all tested chemicals are negative, are silently skipped.
#'
#' Progress can be reported by wrapping the call in `progressr::with_progress()`.
#' Note: `workers` here parallelises across assays. Each assay's internal chemotype processing
#' always runs sequentially to avoid nested parallelism.
#'
#' @param hitcalls Tibble/dataframe with a chemical ID column and one column per assay containing hit calls (0/1).
#' @param assays Character vector of assay column names to calculate enrichment for.
#' @param toxprints Tibble/dataframe containing a chemical ID column and binary ToxPrint columns.
#' @param id_col_hitcalls <[`data-masked`][rlang::args_data_masking]> Unquoted name of the
#'   chemical ID column in `hitcalls`.
#' @param id_col_toxprints <[`data-masked`][rlang::args_data_masking]> Unquoted name of the
#'   chemical ID column in `toxprints`. Defaults to `id_col_hitcalls`.
#' @param endpoint_col_name Name of the column added to output identifying the assay. Default is "assay".
#' @param workers Number of parallel workers to use for assay-level parallelism. Default is `1` (sequential).
#'   Set to a value greater than 1 to enable parallel processing via `future::multisession`. Alternatively,
#'   set a `future::plan()` before calling this function for more control over the parallel backend.
#' @param ... Additional arguments passed to `chemotype_enrichment()` (e.g. `alt_hypothesis`, `adjust_method`, `combo_sizes`).
#'   Note: `workers` in `...` is ignored to prevent nested parallelism; use the `workers` argument directly.
#'
#' @export
#'
#' @importFrom dplyr all_of bind_rows inner_join join_by mutate select
#' @importFrom furrr future_map furrr_options
#' @importFrom future multisession plan
#' @importFrom progressr progressor
#' @importFrom purrr compact
#' @importFrom rlang enquo
#'

multi_assay_enrichment <- function(hitcalls,
                                   assays,
                                   toxprints,
                                   id_col_hitcalls,
                                   id_col_toxprints = id_col_hitcalls,
                                   endpoint_col_name = "assay",
                                   workers = 1L,
                                   ...) {
  
  if (missing(id_col_hitcalls)) {
    stop("'id_col_hitcalls' is required. Please supply the name of the chemical ID column in 'hitcalls'.")
  }

  # Pre-capture quosures — {{ }} is unreliable inside furrr parallel lambdas
  # because the promise chain may not survive serialisation to worker processes.
  hc_quo <- rlang::enquo(id_col_hitcalls)
  tp_quo <- if (missing(id_col_toxprints)) hc_quo else rlang::enquo(id_col_toxprints)

  # --- Parallelisation setup --------------------------------------------------
  
  if (workers > 1L) {
    old_plan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)
  }
  
  # --- Iterate over assays (parallelised) -------------------------------------
  
  # Set up progressor: one tick per assay (including skipped ones)
  p <- progressr::progressor(steps = length(assays))
  
  furrr::future_map(
    assays,
    function(assay_i) {
      
      hits <- hitcalls[[assay_i]]
      
      # Skip assays with no data or no positive hit calls
      if (all(is.na(hits)) || sum(hits, na.rm = TRUE) == 0L) {
        p(message = sprintf("Skipped (no hits): %s", assay_i))
        return(NULL)
      }
      
      result <- hitcalls |>
        dplyr::select(!!hc_quo, dplyr::all_of(assay_i)) |>
        dplyr::inner_join(
          toxprints,
          by = dplyr::join_by(!!hc_quo == !!tp_quo)
        ) |>
        dplyr::select(-!!hc_quo) |>
        chemotype_enrichment(
          endpoint_of_interest = dplyr::all_of(assay_i),
          workers = 1L,  # prevent nested parallelism inside each worker
          ...
        ) |>
        dplyr::mutate("{endpoint_col_name}" := assay_i, .before = 1)
      
      p(message = sprintf("Done: %s", assay_i))
      result
    },
    .options = furrr::furrr_options(seed = FALSE)
  ) |>
    purrr::compact() |>
    dplyr::bind_rows()
}


# ------------------------------------------------------------------------------


#' @title Condensed Chemotype Enrichment
#'
#' @description
#' Function to generate chemotype enrichments using higher level ToxPrint names.
#' Depending on the level and ToxPrint, this will combine several columns into one (e.g., using the level 3 name
#' will combine 8 columns for "bond:C#N" features into 2 columns (i.e., "bond:C#N_cyano" and "bond:C#N_nitrile")).
#' This function can be used to generate a summary table of enrichment statistics for each condensed chemotype.
#' Here, a chemotype is a set of binary features (e.g., chemical structure) that are used to predict a (binary)
#' endpoint (e.g., assay hit call or presence in a data set).
#'
#' Progress can be reported by wrapping the call in `progressr::with_progress()`. The progressor in this
#' function tracks high-level stages; the internal call to `chemotype_enrichment()` will also report
#' per-chemotype progress when wrapped in `with_progress()`.
#'
#' @param toxprints_data Data frame/tibble containing a binary endpoint (e.g., assay hit call or presence in a
#'   data set) alongside a set of ToxPrint feature columns and one or more chemical ID columns.
#' @param tox_levels_data Data frame/tibble containing the ToxPrint naming hierarchy.
#' @param toxprint_level Integer specifying the ToxPrint hierarchy level to be used for enrichment.
#'   This is used to construct the column name in `tox_levels_data` (e.g., level 5 → `"level_5_full"`). Default is `5`.
#' @param id_cols Character vector specifying the column names in `toxprints_data` that contain unique
#'   chemical identifiers (e.g., DTXSID, CASRN, data source, etc.).
#' @param enrichment_target_col Character string specifying the column name in `toxprints_data` that contains
#'   the (binary) endpoint on which to calculate enrichment (e.g., assay hit call or presence in a data set).
#' @param enrichment_target_value Character string specifying the value in `enrichment_target_col` to treat as
#'   the positive class. Required when `enrichment_target_col` is a character column rather than binary (0/1).
#' @param ... Additional arguments passed to `chemotype_enrichment()` (e.g. `alt_hypothesis`, `adjust_method`,
#'   `combo_sizes`, `workers`).
#'
#' @export
#'
#' @importFrom dplyr all_of distinct if_else join_by left_join mutate n_distinct pick pull select
#' @importFrom glue glue
#' @importFrom progressr progressor
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect all_of
#'

condensed_toxprint_enrichment <- function(toxprints_data,
                                          tox_levels_data,
                                          toxprint_level = 5,
                                          id_cols        = c("id", "CASRN"),
                                          enrichment_target_col,
                                          enrichment_target_value = NULL,
                                          ...) {
  
  # --- Input validation -------------------------------------------------------
  
  chemotype_level_col <- glue::glue("level_{toxprint_level}_full")
  
  if (!(chemotype_level_col %in% colnames(tox_levels_data))) {
    stop(glue::glue(
      "The specified ToxPrint level '{toxprint_level}' does not exist in tox_levels_data. ",
      "Expected column: '{chemotype_level_col}'."
    ))
  }
  
  if (missing(enrichment_target_col)) {
    stop("Argument `enrichment_target_col` is missing. Please provide a column name from toxprints_data.")
  } else if (!(enrichment_target_col %in% colnames(toxprints_data))) {
    stop(glue::glue(
      "Invalid enrichment_target_col '{enrichment_target_col}'. ",
      "Please provide a valid column name from toxprints_data."
    ))
  }
  
  # --- Pull and validate the enrichment source column -------------------------
  
  enrichment_source <- dplyr::pull(toxprints_data, dplyr::all_of(enrichment_target_col))
  enrichment_source_non_na <- enrichment_source[!is.na(enrichment_source)]
  
  is_binary   <- is.logical(enrichment_source) || all(enrichment_source_non_na %in% c(0, 1))
  is_char_bi  <- is.character(enrichment_source) && dplyr::n_distinct(enrichment_source_non_na) <= 2L
  
  if (!is_binary && !is_char_bi) {
    stop(glue::glue(
      "Column '{enrichment_target_col}' must be binary (0/1/logical) or a character column ",
      "with at most 2 unique non-NA values. ",
      "Found {dplyr::n_distinct(enrichment_source_non_na)} unique value(s)."
    ))
  }
  
  if (is_char_bi && is.null(enrichment_target_value)) {
    stop(glue::glue(
      "`enrichment_target_value` must be provided when `enrichment_target_col` is a character column. ",
      "Unique values in '{enrichment_target_col}': ",
      "{paste(sort(unique(enrichment_source_non_na)), collapse = ', ')}."
    ))
  }
  
  if (is_char_bi && !enrichment_target_value %in% enrichment_source_non_na) {
    stop(glue::glue(
      "`enrichment_target_value` '{enrichment_target_value}' was not found in column '{enrichment_target_col}'. ",
      "Unique values present: {paste(sort(unique(enrichment_source_non_na)), collapse = ', ')}."
    ))
  }
  
  # --- Set up progress bar ---------------------------------------------------
  
  p <- progressr::progressor(steps = 2L)
  
  # --- Stage 1: Reduce ToxPrints to requested hierarchy level ----------------
  # OR logic: a compound is flagged for a condensed chemotype if ANY sub-feature is 1.
  # The endpoint column is kept separate and rejoined after the pivot.
  
  p(message = "Reducing ToxPrints to hierarchy level")
  
  endpoint_data <- dplyr::select(toxprints_data, dplyr::all_of(c(id_cols, enrichment_target_col)))
  
  reduced_toxprints <- toxprints_data |>
    dplyr::select(-dplyr::all_of(enrichment_target_col)) |>  # exclude endpoint before pivoting
    tidyr::pivot_longer(
      cols      = -tidyselect::all_of(id_cols),
      names_to  = "toxprint",
      values_to = "presence"
    ) |>
    dplyr::left_join(
      dplyr::select(tox_levels_data, toxprint_chemotype_name_original, dplyr::all_of(chemotype_level_col)),
      by = dplyr::join_by("toxprint" == "toxprint_chemotype_name_original")
    ) |>
    dplyr::mutate(
      tp_presence = max(presence),
      .by = tidyselect::all_of(c(id_cols, chemotype_level_col))
    ) |>
    dplyr::distinct(
      dplyr::pick(tidyselect::all_of(c(id_cols, chemotype_level_col))),
      tp_presence
    ) |>
    tidyr::pivot_wider(
      id_cols     = tidyselect::all_of(id_cols),
      names_from  = dplyr::all_of(chemotype_level_col),
      values_from = tp_presence
    ) |>
    dplyr::left_join(endpoint_data, by = id_cols)  # rejoin endpoint column
  
  # --- Stage 2: Calculate chemotype enrichment --------------------------------
  
  p(message = "Calculating chemotype enrichment")
  
  if (is_binary) {
    enriched_toxprints <- reduced_toxprints |>
      dplyr::select(-dplyr::all_of(id_cols)) |>
      chemotype_enrichment(
        endpoint_of_interest = dplyr::all_of(enrichment_target_col),
        ...
      )
  } else {
    # Character column: convert to binary (1 = target value, 0 = otherwise)
    enriched_toxprints <- reduced_toxprints |>
      dplyr::mutate(
        col_for_enrichment = dplyr::if_else(
          .data[[enrichment_target_col]] == enrichment_target_value, 1L, 0L
        )
      ) |>
      dplyr::select(-dplyr::all_of(c(id_cols, enrichment_target_col))) |>
      chemotype_enrichment(
        endpoint_of_interest = col_for_enrichment,
        ...
      )
  }
  
  enriched_toxprints |>
    dplyr::mutate(
      toxprint_level = stringr::str_remove(chemotype_level_col, "_full"),
      .before = chemotype
    )
}
