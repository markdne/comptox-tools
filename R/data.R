#' ToxPrint Chemotype Naming Hierarchy
#'
#' A dataset containing the naming hierarchy for the 729 ToxPrint chemotypes
#' (version 2.0, release 711) across five levels of specificity. Used by
#' \code{\link{calculate_toxprint_proportion}},
#' \code{\link{calculate_toxprints_per_chem}}, and
#' \code{\link{condensed_toxprint_enrichment}} to map individual ToxPrint
#' column names to higher-level groupings.
#'
#' @format A tibble with 729 rows and 6 columns:
#' \describe{
#'   \item{toxprint_chemotype_name_original}{The original ToxPrint column name
#'     (e.g. \code{"bond:C(=O)O_carboxylic_acid"}).}
#'   \item{level_2_full}{Hierarchy level 2 name (broadest grouping).}
#'   \item{level_3_full}{Hierarchy level 3 name.}
#'   \item{level_4_full}{Hierarchy level 4 name.}
#'   \item{level_5_full}{Hierarchy level 5 name (most specific; matches the
#'     original ToxPrint name for leaf-level chemotypes).}
#' }
#'
#' @source \url{https://comptox.epa.gov/dashboard/chemical-lists/TOXPRINT}
"toxprint_levels"
