#' ToxPrint Chemotype Hierarchy Levels (v2.0, r711)
#'
#' @description
#' A lookup table mapping each individual ToxPrint chemotype fingerprint to its
#' position in the five-level ToxPrint naming hierarchy. This dataset is bundled
#' with the package so that users do not need to download or locate the source
#' CSV separately. Used by \code{\link{calculate_toxprint_proportion}},
#' \code{\link{calculate_toxprints_per_chem}}, and
#' \code{\link{condensed_toxprint_enrichment}} to map individual ToxPrint
#' column names to higher-level groupings.
#'
#' Source: ToxPrint v2.0, release 711
#' (\url{https://toxprint.org}).
#'
#' @format A tibble with 729 rows and 11 variables:
#' \describe{
#'   \item{toxprint_id}{Integer. Unique ToxPrint fingerprint identifier.}
#'   \item{toxprint_chemotype_name_original}{Character. The original,
#'     fully-qualified ToxPrint chemotype name (used as the column header in
#'     ToxPrint output files).}
#'   \item{level_1}{Character. Highest-level (broadest) hierarchy grouping.}
#'   \item{level_2}{Character. Second-level hierarchy grouping.}
#'   \item{level_3}{Character. Third-level hierarchy grouping.}
#'   \item{level_4}{Character. Fourth-level hierarchy grouping.}
#'   \item{level_5}{Character. Lowest-level (most specific) hierarchy
#'     grouping.}
#'   \item{level_2_full}{Character. Full path label at level 2
#'     (concatenation of levels 1–2).}
#'   \item{level_3_full}{Character. Full path label at level 3
#'     (concatenation of levels 1–3).}
#'   \item{level_4_full}{Character. Full path label at level 4
#'     (concatenation of levels 1–4).}
#'   \item{level_5_full}{Character. Full path label at level 5
#'     (concatenation of levels 1–5). This is the default level used by
#'     \code{\link{calculate_toxprint_proportion}} and
#'     \code{\link{calculate_toxprints_per_chem}}.}
#' }
#'
#' @source ToxPrint v2.0, r711. Processed from
#'   \code{data-raw/toxprint_V2.0_r711_5Levels.csv} using
#'   \code{data-raw/toxprint_levels.R}.
"toxprint_levels"
