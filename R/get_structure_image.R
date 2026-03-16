#' Retrieve chemical structure images from the EPA CompTox Chemical Dashboard API
#'
#' Fetches structure images (PNG or SVG) for one or more chemicals using their
#' DTXSID, DTXCID, or GSID identifiers.
#'
#' @param input_data A data frame/tibble or an atomic vector of chemical
#'   identifiers.
#' @param search_type Character. The identifier type to search by. One of
#'   \code{"dtxsid"} (default), \code{"dtxcid"}, or \code{"gsid"}.
#'   See \url{https://api-ccte.epa.gov/docs/chemical.html} for details.
#' @param image_type Character. Image format to retrieve. One of \code{"png"}
#'   (default) or \code{"svg"}.
#' @param api_key Character. Your CompTox Dashboard API key.
#' @param chem_id_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The column
#'   in \code{input_data} that contains chemical identifiers. Required when
#'   \code{input_data} is a data frame; ignored for vector input.
#' @param save_images Logical. If \code{TRUE}, images are written to disk in
#'   \code{output_dir}. Default is \code{FALSE}.
#' @param output_dir Character. Directory in which to save images when
#'   \code{save_images = TRUE}. Created automatically if it does not exist.
#'   Default is the current working directory (\code{"."}).
#' @param sorted Logical. If \code{TRUE} (default) output rows are returned in
#'   the same order as the (deduplicated) input identifiers.
#'
#' @return A [tibble][tibble::tibble] with one row per identifier and columns:
#'   \describe{
#'     \item{\code{id}}{The chemical identifier that was searched.}
#'     \item{\code{image}}{A \code{magick-image} object, or \code{NULL} on failure.}
#'     \item{\code{image_path}}{File path if \code{save_images = TRUE}, otherwise \code{NA}.}
#'     \item{\code{error}}{Error message string if the request failed, otherwise \code{NA}.}
#'   }
#'
#' @export
#' @importFrom dplyr distinct pull mutate select arrange
#' @importFrom tidyr drop_na
#' @importFrom purrr list_rbind
#' @importFrom furrr future_map
#' @importFrom progressr progressor
#' @importFrom httr GET add_headers content status_code
#' @importFrom magick image_read image_write
#' @importFrom here here
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   dtxsid = c("DTXSID5020281", "DTXSID8020961", "DTXSID0021834")
#' )
#'
#' # Retrieve PNG structure images using DTXSIDs
#' results <- get_structure_image(
#'   input_data  = df,
#'   search_type = "dtxsid",
#'   api_key     = "your_api_key_here",
#'   chem_id_col = dtxsid
#' )
#'
#' # Save SVG images to disk, with a progress bar
#' results <- progressr::with_progress({
#'   get_structure_image(
#'     input_data  = df,
#'     search_type = "dtxsid",
#'     image_type  = "svg",
#'     api_key     = "your_api_key_here",
#'     chem_id_col = dtxsid,
#'     save_images = TRUE,
#'     output_dir  = "images"
#'   )
#' })
#'
#' # Display the first image
#' print(results$image[[1]])
#' }
get_structure_image <- function(
    input_data,
    search_type = "dtxsid",
    image_type  = "png",
    api_key,
    chem_id_col,
    save_images = FALSE,
    output_dir  = ".",
    sorted      = TRUE
) {
  
  ## Input validation ---------------------------------------------------------
  
  if (missing(api_key)) stop("Please provide your API key via `api_key`.")
  
  if (!is.data.frame(input_data) && !is.atomic(input_data))
    stop("`input_data` must be a data frame/tibble or an atomic vector.")
  
  search_type <- match.arg(search_type, c("dtxsid", "dtxcid", "gsid"))
  image_type  <- match.arg(image_type,  c("png", "svg"))
  
  ## API setup ----------------------------------------------------------------
  
  base_url <- "https://comptox.epa.gov/ctx-api/chemical/file/image/search/"
  
  url_search <- switch(
    search_type,
    dtxsid = "by-dtxsid/",
    dtxcid = "by-dtxcid/",
    gsid   = "by-gsid/"
  )
  
  image_format <- switch(image_type, png = "PNG", svg = "SVG")
  accept_type  <- switch(image_type, png = "image/png", svg = "image/svg+xml")
  
  api_headers <- httr::add_headers(
    "x-api-key" = api_key,
    "Accept"    = accept_type
  )
  
  ## Extract and deduplicate identifiers --------------------------------------
  
  chem_ids <- if (is.data.frame(input_data)) {
    if (missing(chem_id_col))
      stop("`chem_id_col` is required when `input_data` is a data frame.")
    input_data |>
      dplyr::distinct({{ chem_id_col }}) |>
      tidyr::drop_na({{ chem_id_col }}) |>
      dplyr::pull({{ chem_id_col }})
  } else {
    input_data[!is.na(input_data)]
  }
  
  chem_ids <- as.character(chem_ids) |> unique()
  
  if (!length(chem_ids)) return(tibble::tibble())
  
  ## Image fetch worker -------------------------------------------------------
  
  # Always returns a one-row tibble — failures are captured in the `error`
  # column rather than propagating exceptions.
  fetch_image <- function(id) {
    tryCatch({
      resp <- httr::GET(
        url    = paste0(base_url, url_search, id),
        config = api_headers,
        query  = list("Image+Format" = image_format)
      )
      
      if (httr::status_code(resp) != 200L) {
        return(tibble::tibble(
          id         = id,
          image      = list(NULL),
          image_path = NA_character_,
          error      = paste0("HTTP ", httr::status_code(resp))
        ))
      }
      
      img      <- magick::image_read(httr::content(resp, as = "raw"))
      img_path <- NA_character_
      
      if (save_images) {
        if (!dir.exists(here::here(output_dir))) dir.create(here::here(output_dir), recursive = TRUE)
        img_path <- here::here(output_dir, paste0(id, ".", image_type))
        magick::image_write(img, path = img_path, format = image_type)
      }
      
      tibble::tibble(
        id         = id,
        image      = list(img),
        image_path = img_path,
        error      = NA_character_
      )
    }, error = function(e) {
      tibble::tibble(
        id         = id,
        image      = list(NULL),
        image_path = NA_character_,
        error      = conditionMessage(e)
      )
    })
  }
  
  ## Fetch images in parallel -------------------------------------------------
  
  p <- progressr::progressor(along = chem_ids, message = "Retrieving images")
  
  results <- furrr::future_map(
    chem_ids,
    \(id) {
      p()
      fetch_image(id)
    }
  ) |> purrr::list_rbind()
  
  ## Sort output to match input order -----------------------------------------
  
  if (sorted && nrow(results) > 0) {
    results <- results |>
      dplyr::mutate(.row_order = match(id, chem_ids)) |>
      dplyr::arrange(.row_order) |>
      dplyr::select(-.row_order)
  }
  
  results
}
