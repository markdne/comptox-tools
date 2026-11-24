# Columns verify_chem_identifiers() appends to the input data
.verify_output_cols <- c(
  "dtxsid", "dtxcid", "preferredName", "comptox_casrn", "smiles", "inchikey",
  "isMarkush", "hasStructureImage", "pubchem_cid", "pubchem_inchikey",
  "pubchem_smiles", "resolved_via", "resolution_detail", "agreement"
)

# Prefix marking a chemical key that is an InChIKey (PubChem hit with no
# DTXSID) rather than a DTXSID
.inchikey_key_prefix <- "inchikey:"

#' Check that the chemical identifiers in each row refer to the same chemical
#'
#' For each row of `input_data`, every identifier in `chem_id_cols` (e.g. a
#' chemical name and a CASRN) is resolved to a chemical, and the row is
#' classified by whether those chemicals agree. Rows whose identifiers point
#' to different chemicals are expanded to one row per candidate chemical so
#' they can be reviewed side by side.
#'
#' @details
#' ## How each identifier is resolved
#'
#' Identifiers are resolved in three tiers; the first tier that gives an
#' answer is used.
#'
#' 1. **CompTox direct match** (`resolved_via = "comptox"`): an exact
#'    [comptox_chem_search()] hit. If an identifier matches several DTXSIDs
#'    and one of them is also the unique direct match of another identifier in
#'    the same row, that one is kept.
#' 2. **CompTox suggestion** (`"comptox_suggestion"`): when there is no direct
#'    match, the API's suggested names are searched. A suggestion is accepted
#'    only if its DTXSID equals the unique direct match of another identifier
#'    in the same row, so suggestions can confirm but never introduce a
#'    chemical.
#' 3. **PubChem** (`"pubchem"`, only with `pubchem_fallback = TRUE`): the
#'    identifier is searched with [pubchem_dtxsid_search()]. A DTXSID synonym
#'    of the PubChem compound is used directly. If there is none, the PubChem
#'    InChIKey is searched in CompTox; if that also fails the chemical is
#'    keyed on the InChIKey alone (`dtxsid` is `NA`, `inchikey` is set).
#'    PubChem DTXSIDs are looked up in CompTox so their rows carry the same
#'    CompTox fields as directly matched rows.
#'
#' ## Agreement classes
#'
#' * `"agree"`: two or more identifiers, all resolved, all to the same
#'   chemical.
#' * `"partial"`: every resolved identifier gives the same chemical, but at
#'   least one identifier did not resolve (worth checking: the unresolved
#'   identifier may be wrong).
#' * `"single_id"`: only one identifier was supplied for the row and it
#'   resolved; nothing could be cross-checked.
#' * `"disagree"`: at least two identifiers resolved to different chemicals.
#'   The row is expanded to one row per chemical.
#' * `"ambiguous"`: no two identifiers contradict each other, but an
#'   identifier matched several chemicals that the row cannot narrow down
#'   (e.g. a name shared by several substances). Expanded like `"disagree"`.
#' * `"no_match"`: no identifier resolved (or all were missing).
#'
#' `resolution_detail` spells out what each identifier resolved to, e.g.
#' `"chem_name 'Toluene' -> DTXSID7021360 (comptox); casrn '71-43-2' ->
#' DTXSID3039242 (comptox)"`.
#'
#' ## Column name conflicts
#'
#' The function appends the columns `dtxsid`, `dtxcid`, `preferredName`,
#' `comptox_casrn`, `smiles`, `inchikey`, `isMarkush`, `hasStructureImage`,
#' `pubchem_cid`, `pubchem_inchikey`, `pubchem_smiles`, `resolved_via`,
#' `resolution_detail` and `agreement`. If `input_data` already has any of
#' these (or `.orig_row`) it stops with an error; rename them first. The
#' CompTox CASRN is returned as `comptox_casrn` so it never clashes with an
#' input `casrn` column.
#'
#' @param input_data A data frame with two or more identifier columns.
#' @param api_key Character. Your CTX API key. Defaults to the `CTX_API_KEY`
#'   environment variable.
#' @param chem_id_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Two or more
#'   identifier columns (e.g. `c(chem_name, casrn)`). All non-missing,
#'   non-blank values are searched.
#' @param batch Logical. Passed to [comptox_chem_search()]. Default is `TRUE`.
#' @param chunk_size Positive whole number. Passed to [comptox_chem_search()].
#'   Default is `200`.
#' @param rate_limit Non-negative number. Minimum seconds between CompTox
#'   requests. Default is `0`.
#' @param pubchem_fallback Logical. If `TRUE`, identifiers with no direct
#'   CompTox match are searched in PubChem. Default is `FALSE`.
#' @param pubchem_rate_limit Non-negative number. Minimum seconds between
#'   PubChem requests. Default is `0.2` (PubChem's limit of 5 per second).
#' @param max_active Positive whole number. Maximum concurrent requests per
#'   service. Default is `5`.
#' @param progress Logical. Show progress bars. Default is `interactive()`.
#'
#' @returns A tibble with all columns of `input_data` followed by:
#'   * `dtxsid`, `dtxcid`, `preferredName`, `comptox_casrn`, `smiles`,
#'     `isMarkush`, `hasStructureImage`: CompTox data for the chemical.
#'   * `inchikey`: InChIKey of the chemical (CompTox, or PubChem when there is
#'     no DTXSID).
#'   * `pubchem_cid`, `pubchem_inchikey`, `pubchem_smiles`: PubChem data, when
#'     an identifier in the row was resolved through PubChem.
#'   * `resolved_via`: tier(s) that gave this chemical.
#'   * `resolution_detail`: what each identifier in the row resolved to.
#'   * `agreement`: see Details.
#'
#'   Rows follow the order of `input_data`; expanded `"disagree"` and
#'   `"ambiguous"` rows for an input row are consecutive.
#'
#' @seealso [comptox_chem_search()], [pubchem_dtxsid_search()],
#'   [flag_duplicate_chems()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   chem_name = c("Benzene", "Toluene", "Benzene"),
#'   casrn = c("71-43-2", "71-43-2", NA),
#'   conc = c(1.0, 2.0, 3.0)
#' )
#'
#' # Row 1: name and CASRN both give benzene          -> 1 row, "agree"
#' # Row 2: name gives toluene, CASRN gives benzene    -> 2 rows, "disagree"
#' # Row 3: only a name supplied                       -> 1 row, "single_id"
#' verified <- verify_chem_identifiers(
#'   df,
#'   chem_id_cols = c(chem_name, casrn),
#'   pubchem_fallback = TRUE
#' )
#' }
verify_chem_identifiers <- function(
    input_data,
    api_key            = Sys.getenv("CTX_API_KEY"),
    chem_id_cols,
    batch              = TRUE,
    chunk_size         = 200L,
    rate_limit         = 0,
    pubchem_fallback   = FALSE,
    pubchem_rate_limit = 0.2,
    max_active         = 5L,
    progress           = interactive()
) {

  ## Input validation ----

  if (!is.data.frame(input_data)) {
    cli::cli_abort("{.arg input_data} must be a data frame, not {.cls {class(input_data)}}.")
  }

  check_api_key(api_key)

  if (missing(chem_id_cols)) {
    cli::cli_abort("{.arg chem_id_cols} is required.")
  }

  id_col_names <- names(dplyr::select(input_data, {{ chem_id_cols }}))

  if (length(id_col_names) < 2L) {
    cli::cli_abort(
      "{.arg chem_id_cols} must select at least 2 columns; {length(id_col_names)} selected."
    )
  }

  chunk_size <- check_count(chunk_size)
  max_active <- check_count(max_active)
  rate_limit <- check_rate_limit(rate_limit)
  pubchem_rate_limit <- check_rate_limit(pubchem_rate_limit)

  if (!rlang::is_bool(pubchem_fallback)) {
    cli::cli_abort("{.arg pubchem_fallback} must be {.val TRUE} or {.val FALSE}.")
  }

  # Stop early if the input already has a column this function appends
  orig_col_names <- names(input_data)
  clashing <- intersect(orig_col_names, c(.verify_output_cols, ".orig_row"))

  if (length(clashing)) {
    cli::cli_abort(c(
      "{.arg input_data} has column{?s} that would clash with the output: {.val {clashing}}.",
      "i" = "Rename {cli::qty(length(clashing))}{?it/them} before calling {.fn verify_chem_identifiers}."
    ))
  }

  input_data <- dplyr::mutate(input_data, .orig_row = dplyr::row_number())

  # CompTox search with this call's settings; empty input gives an empty
  # result with the usual columns
  run_comptox <- function(ids) {
    if (!length(ids)) {
      return(.comptox_na_template[0, ])
    }

    results <- comptox_chem_search(
      ids,
      api_key       = api_key,
      batch         = batch,
      chunk_size    = chunk_size,
      rate_limit    = rate_limit,
      retry_no_hits = TRUE,
      max_active    = max_active,
      progress      = progress
    )

    return(results)
  }

  ## 1. Long identifier table ----

  # One row per (input row x non-missing identifier)
  id_long <- input_data |>
    dplyr::select(".orig_row", dplyr::all_of(id_col_names)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(id_col_names), as.character)) |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(id_col_names),
      names_to  = ".id_col",
      values_to = ".id_value"
    ) |>
    dplyr::mutate(.id_value = stringr::str_squish(.data$.id_value)) |>
    dplyr::filter(!is.na(.data$.id_value), nzchar(.data$.id_value))

  all_ids <- unique(id_long$.id_value)

  ## 2. CompTox direct search ----

  if (length(all_ids)) {
    cli::cli_inform(
      "Searching CompTox for {length(all_ids)} unique identifier{?s} in {length(id_col_names)} columns."
    )
  }

  initial_results <- run_comptox(all_ids)

  direct_hits <- initial_results |>
    dplyr::filter(!is.na(.data$dtxsid)) |>
    dplyr::distinct(input_term = .data$input_term, key = .data$dtxsid) |>
    dplyr::mutate(tier = 1L)

  ## 3. CompTox suggestions ----

  # One row per (unmatched identifier x suggested name)
  sugg_map <- initial_results |>
    dplyr::filter(is.na(.data$dtxsid), !is.na(.data$suggestions)) |>
    dplyr::distinct(.data$input_term, .data$suggestions) |>
    dplyr::mutate(
      suggestion_term = stringr::str_split(.data$suggestions, stringr::fixed(" | "))
    ) |>
    tidyr::unnest(cols = "suggestion_term") |>
    dplyr::mutate(suggestion_term = stringr::str_squish(.data$suggestion_term)) |>
    dplyr::filter(nzchar(.data$suggestion_term)) |>
    dplyr::select("input_term", "suggestion_term")

  sugg_terms <- unique(sugg_map$suggestion_term)

  if (length(sugg_terms)) {
    cli::cli_inform("Searching {length(sugg_terms)} CompTox suggestion{?s}.")
  }

  sugg_results <- run_comptox(sugg_terms)

  sugg_hits <- sugg_map |>
    dplyr::inner_join(
      sugg_results |>
        dplyr::filter(!is.na(.data$dtxsid)) |>
        dplyr::select(suggestion_term = "input_term", key = "dtxsid"),
      by = dplyr::join_by("suggestion_term"),
      relationship = "many-to-many"
    ) |>
    dplyr::distinct(.data$input_term, .data$key) |>
    dplyr::mutate(tier = 2L)

  ## 4. PubChem fallback ----

  unresolved_ids <- setdiff(all_ids, direct_hits$input_term)

  pubchem_results <- .pubchem_na_template[0, ]

  if (pubchem_fallback && length(unresolved_ids)) {
    cli::cli_inform(
      "Searching PubChem for {length(unresolved_ids)} identifier{?s} with no direct CompTox match."
    )
    pubchem_results <- pubchem_dtxsid_search(
      unresolved_ids,
      rate_limit = pubchem_rate_limit,
      max_active = max_active,
      progress   = progress
    )
  }

  pubchem_found <- dplyr::filter(pubchem_results, .data$search_status == "found")

  # PubChem compounds with DTXSID synonyms (possibly several)
  pubchem_dtxsid_hits <- pubchem_found |>
    dplyr::filter(!is.na(.data$dtxsid)) |>
    dplyr::mutate(key = stringr::str_split(.data$dtxsid, stringr::fixed(" | "))) |>
    tidyr::unnest(cols = "key") |>
    dplyr::select("input_term", "key")

  # PubChem compounds without a DTXSID: search CompTox by InChIKey
  pubchem_no_dtxsid <- pubchem_found |>
    dplyr::filter(is.na(.data$dtxsid), !is.na(.data$inchikey))

  inchikey_results <- run_comptox(unique(pubchem_no_dtxsid$inchikey))

  pubchem_inchikey_hits <- pubchem_no_dtxsid |>
    dplyr::select("input_term", "inchikey") |>
    dplyr::inner_join(
      inchikey_results |>
        dplyr::filter(!is.na(.data$dtxsid)) |>
        dplyr::select(inchikey = "input_term", key = "dtxsid"),
      by = dplyr::join_by("inchikey"),
      relationship = "many-to-many"
    ) |>
    dplyr::select("input_term", "key")

  # Still no DTXSID: key the chemical on its InChIKey
  pubchem_inchikey_only <- pubchem_no_dtxsid |>
    dplyr::filter(!.data$input_term %in% pubchem_inchikey_hits$input_term) |>
    dplyr::transmute(
      input_term = .data$input_term,
      key = stringr::str_c(.inchikey_key_prefix, .data$inchikey)
    )

  pubchem_hits <- dplyr::bind_rows(
    pubchem_dtxsid_hits,
    pubchem_inchikey_hits,
    pubchem_inchikey_only
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(tier = 3L)

  ## 5. Resolve each identifier ----

  candidates <- dplyr::bind_rows(
    direct_hits,
    sugg_hits,
    pubchem_hits,
    tibble::tibble(input_term = character(), key = character(), tier = integer())
  )

  # Candidate chemicals of each identifier, by tier
  id_tiers <- id_long |>
    dplyr::left_join(
      candidates,
      by = dplyr::join_by(".id_value" == "input_term"),
      relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      direct = list(unique(.data$key[.data$tier %in% 1L])),
      suggested = list(unique(.data$key[.data$tier %in% 2L])),
      pubchem = list(unique(.data$key[.data$tier %in% 3L])),
      .by = c(".orig_row", ".id_col", ".id_value")
    )

  # Anchors: chemicals that an identifier in the row matched directly and
  # uniquely; used to narrow down or confirm the other identifiers
  row_anchors <- id_tiers |>
    dplyr::filter(lengths(.data$direct) == 1L) |>
    dplyr::summarise(anchors = list(unique(unlist(.data$direct))), .by = ".orig_row")

  # PubChem CID details used in the resolution text
  pubchem_notes <- pubchem_found |>
    dplyr::transmute(
      .id_value = .data$input_term,
      pubchem_note = dplyr::if_else(
        .data$pubchem_n_cids > 1L,
        stringr::str_c("CID ", .data$pubchem_cid, ", 1 of ", .data$pubchem_n_cids, " CIDs"),
        stringr::str_c("CID ", .data$pubchem_cid)
      )
    )

  id_resolved <- id_tiers |>
    dplyr::left_join(row_anchors, by = dplyr::join_by(".orig_row")) |>
    dplyr::mutate(
      resolution = purrr::pmap(
        list(.data$direct, .data$suggested, .data$pubchem, .data$anchors),
        resolve_identifier
      ),
      keys = purrr::map(.data$resolution, "keys"),
      via = purrr::map_chr(.data$resolution, "via")
    ) |>
    dplyr::left_join(pubchem_notes, by = dplyr::join_by(".id_value")) |>
    dplyr::mutate(
      via_note = dplyr::if_else(
        .data$via %in% "pubchem",
        stringr::str_c("pubchem, ", .data$pubchem_note),
        .data$via
      ),
      detail = dplyr::if_else(
        lengths(.data$keys) > 0L,
        stringr::str_c(
          .data$.id_col, " '", .data$.id_value, "' -> ",
          purrr::map_chr(.data$keys, \(keys) stringr::str_c(keys, collapse = " / ")),
          " (", .data$via_note, ")"
        ),
        stringr::str_c(.data$.id_col, " '", .data$.id_value, "' -> unresolved")
      )
    )

  ## 6. Classify agreement per input row ----

  row_summary <- id_resolved |>
    dplyr::summarise(
      n_ids = dplyr::n(),
      n_resolved = sum(lengths(.data$keys) > 0L),
      all_keys = list(unique(unlist(.data$keys))),
      has_conflict = has_disjoint_sets(.data$keys[lengths(.data$keys) > 0L]),
      resolution_detail = stringr::str_c(.data$detail, collapse = "; "),
      .by = ".orig_row"
    )

  # Rows whose identifiers were all missing have no entry yet
  row_summary <- tibble::tibble(.orig_row = input_data$.orig_row) |>
    dplyr::left_join(row_summary, by = dplyr::join_by(".orig_row")) |>
    dplyr::mutate(
      n_ids = dplyr::coalesce(.data$n_ids, 0L),
      n_resolved = dplyr::coalesce(.data$n_resolved, 0L),
      all_keys = purrr::map(.data$all_keys, \(keys) if (is.null(keys)) character() else keys),
      has_conflict = dplyr::coalesce(.data$has_conflict, FALSE),
      resolution_detail = dplyr::coalesce(.data$resolution_detail, "no identifiers supplied"),
      n_keys = lengths(.data$all_keys),
      agreement = dplyr::case_when(
        .data$n_keys == 0L ~ "no_match",
        .data$n_keys == 1L & .data$n_ids == 1L ~ "single_id",
        .data$n_keys == 1L & .data$n_resolved == .data$n_ids ~ "agree",
        .data$n_keys == 1L ~ "partial",
        .data$has_conflict ~ "disagree",
        .default = "ambiguous"
      )
    )

  ## 7. CompTox and PubChem data for each chemical ----

  all_dtxsids <- unique(unlist(row_summary$all_keys))
  all_dtxsids <- all_dtxsids[!stringr::str_starts(all_dtxsids, stringr::fixed(.inchikey_key_prefix))]

  # CompTox search fields for every DTXSID; PubChem-derived DTXSIDs not seen
  # in any CompTox search so far are searched now
  comptox_seen <- dplyr::bind_rows(initial_results, sugg_results, inchikey_results) |>
    dplyr::filter(!is.na(.data$dtxsid))

  comptox_extra <- run_comptox(setdiff(all_dtxsids, comptox_seen$dtxsid))

  metadata_lookup <- dplyr::bind_rows(
    .comptox_na_template[0, ],
    comptox_seen,
    comptox_extra
  ) |>
    dplyr::filter(!is.na(.data$dtxsid)) |>
    dplyr::distinct(.data$dtxsid, .keep_all = TRUE) |>
    dplyr::select(
      "dtxsid", "dtxcid", "preferredName",
      comptox_casrn = "casrn",
      "smiles", "isMarkush", "hasStructureImage"
    )

  # InChIKeys (the search endpoint does not return them)
  inchikey_lookup <- tibble::tibble(dtxsid = character(), comptox_inchikey = character())

  if (length(all_dtxsids)) {
    # Only found rows: the others have NA dtxsid, which would match the NA
    # dtxsid of InChIKey-only chemicals in the join below
    inchikey_lookup <- comptox_chem_details(
      all_dtxsids,
      results_type = "chem_identifiers",
      api_key      = api_key,
      rate_limit   = rate_limit,
      max_active   = max_active,
      progress     = progress
    ) |>
      dplyr::filter(.data$search_status == "found") |>
      dplyr::select(dtxsid = "input_term", comptox_inchikey = dplyr::any_of("inchikey"))

    if (!"comptox_inchikey" %in% names(inchikey_lookup)) {
      inchikey_lookup <- dplyr::mutate(inchikey_lookup, comptox_inchikey = NA_character_)
    }
  }

  # Tier(s) that gave each (input row x chemical)
  via_lookup <- id_resolved |>
    dplyr::select(".orig_row", "keys", "via") |>
    tidyr::unnest(cols = "keys") |>
    dplyr::summarise(
      resolved_via = stringr::str_c(sort(unique(.data$via)), collapse = "; "),
      .by = c(".orig_row", "keys")
    ) |>
    dplyr::rename(key = "keys")

  # PubChem data for chemicals resolved through PubChem
  pubchem_lookup <- id_resolved |>
    dplyr::filter(.data$via %in% "pubchem") |>
    dplyr::select(".orig_row", ".id_value", "keys") |>
    tidyr::unnest(cols = "keys") |>
    dplyr::inner_join(
      pubchem_found |>
        dplyr::select(
          .id_value = "input_term",
          "pubchem_cid",
          pubchem_inchikey = "inchikey",
          pubchem_smiles = "smiles"
        ),
      by = dplyr::join_by(".id_value")
    ) |>
    dplyr::rename(key = "keys") |>
    dplyr::distinct(.data$.orig_row, .data$key, .keep_all = TRUE) |>
    dplyr::select(-".id_value")

  ## 8. Expand to one row per (input row x chemical) and assemble ----

  expanded <- row_summary |>
    dplyr::mutate(
      key = purrr::map(.data$all_keys, \(keys) if (length(keys)) keys else NA_character_)
    ) |>
    tidyr::unnest(cols = "key") |>
    dplyr::left_join(via_lookup, by = dplyr::join_by(".orig_row", "key")) |>
    dplyr::left_join(pubchem_lookup, by = dplyr::join_by(".orig_row", "key")) |>
    dplyr::mutate(
      is_inchikey_key = stringr::str_starts(.data$key, stringr::fixed(.inchikey_key_prefix)) %in% TRUE,
      dtxsid = dplyr::if_else(.data$is_inchikey_key, NA_character_, .data$key)
    ) |>
    dplyr::left_join(metadata_lookup, by = dplyr::join_by("dtxsid")) |>
    dplyr::left_join(inchikey_lookup, by = dplyr::join_by("dtxsid")) |>
    dplyr::mutate(
      inchikey = dplyr::if_else(
        .data$is_inchikey_key,
        stringr::str_remove(.data$key, stringr::fixed(.inchikey_key_prefix)),
        .data$comptox_inchikey
      )
    )

  verified <- expanded |>
    dplyr::select(".orig_row", dplyr::all_of(.verify_output_cols)) |>
    dplyr::left_join(
      dplyr::select(input_data, ".orig_row", dplyr::all_of(orig_col_names)),
      by = dplyr::join_by(".orig_row")
    ) |>
    dplyr::arrange(.data$.orig_row) |>
    dplyr::select(dplyr::all_of(c(orig_col_names, .verify_output_cols)))

  ## Summarise the outcome ----

  agreement_counts <- dplyr::count(row_summary, .data$agreement)
  agreement_text <- stringr::str_c(
    agreement_counts$agreement, ": ", agreement_counts$n,
    collapse = ", "
  )

  cli::cli_inform("Agreement across {nrow(row_summary)} input row{?s}: {agreement_text}.")

  return(verified)
}

# Resolve one identifier from its candidate chemicals, tier by tier.
# `anchors` are the unique direct matches of identifiers in the same row.
resolve_identifier <- function(direct, suggested, pubchem, anchors) {
  if (is.null(anchors)) {
    anchors <- character()
  }

  # Keep only candidates confirmed by an anchor, if any are
  narrow <- function(candidates) {
    confirmed <- intersect(candidates, anchors)
    if (length(confirmed)) {
      return(confirmed)
    }
    return(candidates)
  }

  if (length(direct)) {
    return(list(keys = narrow(direct), via = "comptox"))
  }

  # Suggestions are accepted only when an anchor confirms them
  confirmed_suggestion <- intersect(suggested, anchors)

  if (length(confirmed_suggestion)) {
    return(list(keys = confirmed_suggestion, via = "comptox_suggestion"))
  }

  if (length(pubchem)) {
    return(list(keys = narrow(pubchem), via = "pubchem"))
  }

  return(list(keys = character(), via = NA_character_))
}

# TRUE if any two of the key sets share no chemical, i.e. two identifiers
# point to different chemicals
has_disjoint_sets <- function(key_sets) {
  if (length(key_sets) < 2L) {
    return(FALSE)
  }

  pairs <- utils::combn(length(key_sets), 2L, simplify = FALSE)

  is_disjoint <- purrr::map_lgl(
    pairs,
    \(pair) !length(intersect(key_sets[[pair[[1]]]], key_sets[[pair[[2]]]]))
  )

  return(any(is_disjoint))
}
