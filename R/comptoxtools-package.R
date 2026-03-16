#' comptoxtools: Tools for the EPA CompTox Chemical Dashboard API
#'
#' Provides functions to search and retrieve chemical data from the EPA CompTox
#' Chemical Dashboard API, along with tools for ToxPrint chemotype analysis,
#' UMAP dimensionality reduction and visualisation, and chemical data utilities.
#'
#' @section CompTox API functions:
#' \describe{
#'   \item{[comptox_chem_search()]}{Search the API by chemical name, CAS-RN,
#'     DTXSID, or any other identifier. Supports batch POST and individual GET
#'     requests.}
#'   \item{[deduplicate_chem_results()]}{Collapse duplicate rows when multiple
#'     identifier columns resolve to the same chemical.}
#'   \item{[get_struc_from_dtxsid()]}{Retrieve structural and physico-chemical
#'     property data using DTXSID or DTXCID identifiers.}
#'   \item{[get_structure_image()]}{Fetch PNG or SVG structure images by
#'     DTXSID, DTXCID, or GSID.}
#' }
#'
#' @section ToxPrint chemotype functions:
#' \describe{
#'   \item{[calculate_toxprint_proportion()]}{Calculate the proportion of
#'     chemicals in each dataset positive for each ToxPrint chemotype,
#'     aggregated to a chosen hierarchy level.}
#'   \item{[calculate_toxprints_per_chem()]}{Count distinct ToxPrint
#'     chemotypes per chemical at a chosen hierarchy level.}
#'   \item{[chemotype_enrichment()]}{Enrichment analysis (Fisher's exact test,
#'     confusion matrix metrics) for individual or combined chemotypes against
#'     a binary endpoint.}
#'   \item{[multi_assay_enrichment()]}{Wrapper around
#'     [chemotype_enrichment()] that iterates over multiple assays.}
#'   \item{[condensed_toxprint_enrichment()]}{Enrichment using
#'     higher-level ToxPrint names, reducing the feature space before testing.}
#' }
#'
#' @section UMAP functions:
#' \describe{
#'   \item{[run_umap()]}{Project high-dimensional data to 2 dimensions using
#'     UMAP (supports the Python umap-learn backend via reticulate).}
#'   \item{[umap_layout()]}{Convert a UMAP result to a tidy tibble with
#'     optional data-source labels.}
#'   \item{[umap_plot()]}{Create a ggplot2 scatter plot from a
#'     [umap_layout()] tibble.}
#' }
#'
#' @section Utility functions:
#' \describe{
#'   \item{[convert_date_to_casrn()]}{Recover CAS registry numbers that Excel
#'     has reformatted as dates.}
#' }
#'
#' @section Bundled data:
#' \describe{
#'   \item{[toxprint_levels]}{ToxPrint v2.0 naming hierarchy across five
#'     levels of specificity. Used as the default \code{tox_levels_data}
#'     argument in the ToxPrint functions.}
#' }
#'
#' @section API key:
#' CompTox API functions require a Dashboard API key. Register for one at
#' \url{https://comptox.epa.gov/dashboard/}.
#'
#' @section Parallel execution and progress bars:
#' Functions use [furrr::future_map()] for parallel requests. Call
#' [future::plan()] before running to enable parallelism, e.g.:
#' ```r
#' future::plan(future::multisession, workers = 4)
#' ```
#' Wrap any function call in [progressr::with_progress()] to display a
#' progress bar:
#' ```r
#' progressr::with_progress({
#'   results <- comptox_chem_search(...)
#' })
#' ```
#'
#' @keywords internal
"_PACKAGE"
