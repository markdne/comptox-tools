## Script to process the ToxPrint hierarchy CSV and save it as bundled package data.
##
## Run this script once (or whenever the source CSV is updated) to regenerate
## the internal dataset:
##
##   source("data-raw/toxprint_levels.R")
##
## Requires: readr, janitor, usethis

toxprint_levels <- readr::read_csv(
  "data-raw/toxprint_V2.0_r711_5Levels.csv",
  show_col_types = FALSE
) |>
  janitor::clean_names() |>
  dplyr::rename(
    toxprint_id                     = tox_print_id,
    toxprint_chemotype_name_original = tox_print_chemotype_name_original
  )

usethis::use_data(toxprint_levels, overwrite = TRUE)
