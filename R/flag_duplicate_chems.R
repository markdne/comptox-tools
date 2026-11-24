# Columns flag_duplicate_chems() appends
.duplicate_flag_cols <- c("dup_key", "dup_group", "dup_n", "dup_first_row", "is_duplicate")

#' Flag rows that refer to the same chemical
#'
#' Groups the rows of a table of resolved chemicals (e.g. the output of
#' [verify_chem_identifiers()]) by chemical identity and flags every row after
#' the first in each group as a duplicate. Remove duplicates with
#' `dplyr::filter(flagged, !is_duplicate)`.
#'
#' The chemical key of each row is:
#'
#' 1. its DTXSID, when present;
#' 2. otherwise, the DTXSID of other rows with the same InChIKey, when that
#'    InChIKey belongs to exactly one DTXSID in the data (so a PubChem-only
#'    hit joins the matching CompTox chemical);
#' 3. otherwise, its full InChIKey.
#'
#' Full InChIKeys keep stereoisomers, isotopologues and different salt or
#' charge forms apart. Rows with neither a DTXSID nor an InChIKey are never
#' flagged as duplicates.
#'
#' @param chem_data A data frame with one row per chemical record.
#' @param dtxsid_col Character. Name of the DTXSID column. Default is
#'   `"dtxsid"`.
#' @param inchikey_col Character or `NULL`. Name of the InChIKey column, or
#'   `NULL` to use DTXSIDs only. Default is `"inchikey"`.
#'
#' @returns `chem_data` with five columns appended:
#'   * `dup_key`: the chemical key (a DTXSID, or `"inchikey:"` + InChIKey);
#'     `NA` when the row has neither.
#'   * `dup_group`: integer group number, in order of first appearance.
#'   * `dup_n`: number of rows in the group.
#'   * `dup_first_row`: row number (in `chem_data`) of the group's first row,
#'     the row that is kept.
#'   * `is_duplicate`: `TRUE` for every row of a group after the first.
#'
#' @export
#'
#' @examples
#' chems <- tibble::tibble(
#'   chem_name = c("Benzene", "benzol", "Toluene", "Mystery"),
#'   dtxsid = c("DTXSID3039242", "DTXSID3039242", "DTXSID7021360", NA),
#'   inchikey = c(
#'     "UHOVQNZJYSORNB-UHFFFAOYSA-N", "UHOVQNZJYSORNB-UHFFFAOYSA-N",
#'     "YXFVVABEGXRONW-UHFFFAOYSA-N", NA
#'   )
#' )
#'
#' flagged <- flag_duplicate_chems(chems)
#' dplyr::filter(flagged, !is_duplicate)
flag_duplicate_chems <- function(
    chem_data,
    dtxsid_col   = "dtxsid",
    inchikey_col = "inchikey"
) {

  ## Input validation ----

  if (!is.data.frame(chem_data)) {
    cli::cli_abort("{.arg chem_data} must be a data frame, not {.cls {class(chem_data)}}.")
  }

  key_cols <- c(dtxsid_col, inchikey_col)
  absent_cols <- setdiff(key_cols, names(chem_data))

  if (length(absent_cols)) {
    cli::cli_abort("Column{?s} {.val {absent_cols}} not found in {.arg chem_data}.")
  }

  clashing <- intersect(names(chem_data), .duplicate_flag_cols)

  if (length(clashing)) {
    cli::cli_abort(
      "{.arg chem_data} already has column{?s} {.val {clashing}}; rename or drop {?it/them} first."
    )
  }

  ## Build one chemical key per row ----

  keys <- tibble::tibble(
    .row = seq_len(nrow(chem_data)),
    dtxsid = as.character(chem_data[[dtxsid_col]]),
    inchikey = if (is.null(inchikey_col)) NA_character_ else as.character(chem_data[[inchikey_col]])
  )

  # InChIKeys that belong to exactly one DTXSID in the data
  inchikey_to_dtxsid <- keys |>
    dplyr::filter(!is.na(.data$dtxsid), !is.na(.data$inchikey)) |>
    dplyr::distinct(.data$inchikey, .data$dtxsid) |>
    dplyr::filter(dplyr::n() == 1L, .by = "inchikey") |>
    dplyr::rename(mapped_dtxsid = "dtxsid")

  keys <- keys |>
    dplyr::left_join(inchikey_to_dtxsid, by = dplyr::join_by("inchikey"), na_matches = "never") |>
    dplyr::mutate(
      dup_key = dplyr::case_when(
        !is.na(.data$dtxsid) ~ .data$dtxsid,
        !is.na(.data$mapped_dtxsid) ~ .data$mapped_dtxsid,
        !is.na(.data$inchikey) ~ stringr::str_c(.inchikey_key_prefix, .data$inchikey),
        .default = NA_character_
      )
    )

  ## Group and flag ----

  flags <- keys |>
    dplyr::mutate(
      dup_n = dplyr::n(),
      dup_first_row = min(.data$.row),
      .by = "dup_key"
    ) |>
    dplyr::mutate(
      has_key = !is.na(.data$dup_key),
      dup_group = dplyr::if_else(
        .data$has_key,
        match(.data$dup_key, unique(.data$dup_key[.data$has_key])),
        NA_integer_
      ),
      dup_n = dplyr::if_else(.data$has_key, .data$dup_n, NA_integer_),
      dup_first_row = dplyr::if_else(.data$has_key, .data$dup_first_row, NA_integer_),
      is_duplicate = .data$has_key & .data$.row != .data$dup_first_row
    ) |>
    dplyr::arrange(.data$.row) |>
    dplyr::select(dplyr::all_of(.duplicate_flag_cols))

  flagged <- dplyr::bind_cols(chem_data, flags)

  return(flagged)
}
