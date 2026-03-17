#' Create a UMAP projection of high-dimensional data
#'
#' Projects high-dimensional data to 2 dimensions using the UMAP algorithm.
#' By default the function delegates to the Python \pkg{umap-learn} library via
#' \pkg{reticulate}, which supports a wider range of distance metrics (including
#' Jaccard) than the pure-R \code{"naive"} implementation.
#'
#' @param df A numeric matrix containing the features to project.
#' @param method Character. UMAP implementation to use. One of
#'   \code{"umap-learn"} (default) or \code{"naive"}. The \code{"umap-learn"}
#'   option requires the Python \pkg{umap-learn} package to be installed in the
#'   active \pkg{reticulate} environment.
#' @param n_neighbours Positive integer. Number of nearest neighbours
#'   considered during graph construction.
#'   - Low values emphasise local structure.
#'   - High values emphasise global structure.
#'   Default is \code{15}.
#' @param metric Character. Distance metric used to compute pairwise
#'   distances. Common choices are \code{"jaccard"} (default),
#'   \code{"euclidean"}, and \code{"cosine"}. See the \pkg{umap} package
#'   documentation for the full list of supported metrics.
#' @param min_dist Numeric. Minimum distance between embedded points.
#'   - Low values produce tighter clusters.
#'   - High values spread points further apart.
#'   Default is \code{0.1}.
#' @param spread Numeric. Effective scale of the embedded points relative to
#'   \code{min_dist}. Default is \code{1}.
#' @param random_state Integer. Random seed for reproducibility. Default is
#'   \code{2024}.
#' @param init Character or matrix. Initialisation method for the embedding.
#'   Either \code{"spectral"} (default) or \code{"random"}.
#'
#' @return A \code{umap} object as returned by \code{\link[umap]{umap}},
#'   which includes a \code{layout} matrix with one row per observation and
#'   two columns giving the 2-D coordinates.
#'
#' @export
#' @importFrom umap umap
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # Create a binary feature matrix and project with the pure-R method
#' mat <- matrix(rbinom(500, 1, 0.3), nrow = 50, ncol = 10)
#' result <- run_umap(mat, method = "naive", metric = "euclidean")
#' }
run_umap <- function(
    df,
    method       = "umap-learn",
    n_neighbours = 15L,
    metric       = "jaccard",
    min_dist     = 0.1,
    spread       = 1,
    random_state = 2024L,
    init         = "spectral"
) {
  
  if (!is.matrix(df)) stop("`df` must be a numeric matrix.")
  method <- match.arg(method, c("umap-learn", "naive"))
  if (is.character(init)) init <- match.arg(init, c("spectral", "random"))
  
  umap::umap(
    df,
    method          = method,
    metric          = metric,
    n_neighbors     = n_neighbours,
    min_dist        = min_dist,
    spread          = spread,
    random_state    = random_state,
    init            = init,
    umap_learn_args = c(
      "metric", "n_neighbors", "min_dist", "spread", "random_state", "init"
    )
  )
}


# umap_layout() --------------------------------------------------------------------


#' Convert a UMAP result to a tidy tibble
#'
#' Extracts the 2-D layout from a \code{umap} object (or a raw layout matrix)
#' and optionally attaches a data-source label to every row.
#'
#' @param df A \code{umap} object returned by \code{\link{run_umap}}, or a
#'   2-column numeric matrix of layout coordinates.
#' @param n_neighbours Positive integer. The \code{n_neighbours} value used in
#'   \code{\link{run_umap}}. Stored in the returned tibble for reference.
#' @param source_info Data-source label(s) to attach to each row. Accepts:
#'   \describe{
#'     \item{\code{NULL}}{(default) The name of the variable passed to
#'       \code{df} is used as a single label for all rows.}
#'     \item{character or factor vector}{Must be the same length as the number
#'       of rows in \code{df}. Each element labels the corresponding row.}
#'     \item{data frame}{Must contain the columns named by \code{join_by_col}
#'       and \code{source_col}. The label is joined to the layout tibble using
#'       the row-identifier column.}
#'   }
#' @param rowname_name <[`data-masked`][rlang::args_data_masking]> Unquoted
#'   name to give the column that holds the row names of the layout matrix.
#'   Default is \code{identifier}.
#' @param join_by_col <[`data-masked`][rlang::args_data_masking]> Unquoted
#'   name of the column in \code{source_info} to match against the
#'   \code{rowname_name} column of the layout tibble. Required when
#'   \code{source_info} is a data frame.
#' @param source_col <[`data-masked`][rlang::args_data_masking]> Unquoted
#'   name of the column in \code{source_info} whose values become the
#'   \code{dataset} label. Required when \code{source_info} is a data frame.
#'
#' @return A [tibble][tibble::tibble] with columns:
#'   \describe{
#'     \item{\code{<rowname_name>}}{Row identifiers from the layout matrix.}
#'     \item{\code{neighbours}}{Value of \code{n_neighbours}.}
#'     \item{\code{dimension_1}}{First UMAP dimension.}
#'     \item{\code{dimension_2}}{Second UMAP dimension.}
#'     \item{\code{dataset}}{Data-source label (factor).}
#'   }
#'
#' @export
#' @importFrom tibble as_tibble add_column
#' @importFrom dplyr left_join join_by select rename mutate
#' @importFrom rlang enquo as_name as_label quo_is_null
#' @importFrom forcats as_factor
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' mat    <- matrix(rbinom(500, 1, 0.3), nrow = 50, ncol = 10)
#' result <- run_umap(mat, method = "naive", metric = "euclidean")
#' layout <- umap_layout(result, n_neighbours = 15)
#' }
umap_layout <- function(
    df,
    n_neighbours,
    source_info  = NULL,
    rowname_name = identifier,
    join_by_col  = NULL,
    source_col   = NULL
) {
  # Capture the name of `df` before it is reassigned, so it can be used as a
  # fallback label when source_info is NULL
  original_df_name <- rlang::as_name(rlang::enquo(df))

  # rowname_name is used both as a bare column name ({{ }}) and as a character
  # string (for as_tibble / set_names / add_column). Capture the string form once.
  rowname_name_str <- rlang::as_label(rlang::enquo(rowname_name))

  # Extract the 2-D layout matrix
  if (is.list(df) && "layout" %in% names(df)) {
    layout_matrix <- df$layout
  } else if (is.matrix(df)) {
    layout_matrix <- df
  } else {
    stop("`df` must be a umap object with a `layout` element or a numeric matrix.")
  }

  if (ncol(layout_matrix) != 2L) {
    stop(glue::glue(
      "`df` layout must have exactly 2 columns but has {ncol(layout_matrix)}."
    ))
  }

  # Assign column names before as_tibble() to avoid a tibble deprecation
  # warning about non-unique column names on unnamed matrices.
  colnames(layout_matrix) <- c("dimension_1", "dimension_2")
  df <- layout_matrix |>
    tibble::as_tibble(rownames = rowname_name_str) |>
    tibble::add_column(neighbours = n_neighbours, .after = rowname_name_str)

  # Attach the data-source label
  if (is.null(source_info)) {
    df <- tibble::add_column(df, dataset = original_df_name)

  } else if (is.character(source_info) || is.factor(source_info)) {
    if (length(source_info) != nrow(df)) {
      stop(glue::glue(
        "`source_info` as a vector must have the same length as `df` ({nrow(df)} rows), ",
        "but has {length(source_info)} element(s)."
      ))
    }
    df <- tibble::add_column(df, dataset = source_info)

  } else if (is.data.frame(source_info)) {
    if (rlang::quo_is_null(rlang::enquo(source_col)) ||
        rlang::quo_is_null(rlang::enquo(join_by_col))) {
      stop(
        "When `source_info` is a data frame, both `join_by_col` and `source_col` ",
        "must be supplied."
      )
    }
    source_info <- dplyr::select(source_info, {{ join_by_col }}, {{ source_col }})

    df <- df |>
      dplyr::left_join(
        source_info,
        by = dplyr::join_by({{ rowname_name }} == {{ join_by_col }})
      ) |>
      dplyr::rename(dataset = {{ source_col }})

  } else {
    stop("`source_info` must be NULL, a character/factor vector, or a data frame.")
  }

  dplyr::mutate(df, dataset = forcats::as_factor(dataset))
}


# umap_plot() ----------------------------------------------------------------------


#' Create a UMAP scatter plot
#'
#' Generates a \pkg{ggplot2} scatter plot from the tidy tibble produced by
#' \code{\link{umap_layout}}. Data points are coloured and labelled by the
#' data-source column. When \code{bottom_source} is specified, that group's
#' points are drawn first (i.e. underneath all others).
#'
#' @param umap_data A tibble returned by \code{\link{umap_layout}}.
#' @param title Character. Plot title.
#' @param source_col <[`data-masking`][dplyr::dplyr_data_masking]> Unquoted
#'   name of the column in \code{umap_data} that contains the data-source
#'   labels. Default is \code{dataset}.
#' @param bottom_source Character. Value in \code{source_col} whose points
#'   should be rendered first (i.e. appear behind all other points). If
#'   \code{NULL} (default), the default \pkg{ggplot2} draw order is used.
#' @param colours Named or unnamed character vector of colours (names or hex
#'   codes) to associate with each data source.
#'   - If unnamed, colours are assigned alphabetically to the sorted unique
#'     values of \code{source_col}.
#'   - If named, each name must match a value in \code{source_col}.
#'   - If \code{NULL} (default), colours are drawn from
#'     \code{\link[ggthemes]{tableau_color_pal}}.
#' @param point_alpha Numeric vector of alpha (opacity) values. Either length
#'   1 (applied to all groups) or the same length as the number of unique
#'   values in \code{source_col}. Values are assigned in the same alphabetical
#'   order as \code{colours}. Default is \code{1}.
#' @param labels Named or unnamed character vector of legend labels.
#'   Follows the same assignment rules as \code{colours}. If \code{NULL}
#'   (default), the unique values from \code{source_col} are used directly.
#' @param n_breaks Positive integer. Passed to \code{\link[scales]{pretty_breaks}}
#'   to control the number of axis tick marks. Default is \code{5}.
#' @param arrange_by Character vector of column names to sort \code{umap_data}
#'   by before plotting. If \code{NULL} (default), no sorting is applied
#'   (unless \code{arrange_levels} is provided).
#' @param arrange_desc Logical. If \code{TRUE}, the columns specified in
#'   \code{arrange_by} are sorted in descending order. Default is \code{FALSE}.
#' @param arrange_levels Named list. Each element specifies the factor-level
#'   order for a column. The column is temporarily converted to a factor to
#'   impose the given order, then converted back to character. For example,
#'   \code{list(dataset = c("GroupA", "GroupB", "GroupC"))} will sort rows so
#'   that GroupA points are drawn first (bottom), followed by GroupB, then
#'   GroupC (top).
#' @param ... Additional aesthetics passed to \code{\link[ggplot2]{aes}}, e.g.
#'   \code{shape = some_column}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#' @importFrom ggplot2 ggplot aes ggtitle scale_x_continuous scale_y_continuous
#'   scale_colour_manual scale_alpha_manual theme element_blank element_text
#'   element_rect unit geom_point
#' @importFrom ggthemes theme_fivethirtyeight tableau_color_pal
#' @importFrom scales pretty_breaks
#' @importFrom dplyr pull distinct arrange across desc filter mutate all_of
#' @importFrom purrr walk reduce
#' @importFrom rlang set_names
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' mat    <- matrix(rbinom(500, 1, 0.3), nrow = 50, ncol = 10)
#' result <- run_umap(mat, method = "naive", metric = "euclidean")
#' layout <- umap_layout(result, n_neighbours = 15, source_info = rep(c("A", "B"), 25))
#'
#' umap_plot(
#'   umap_data = layout,
#'   title     = "Example UMAP",
#'   colours   = c(A = "#1f77b4", B = "#ff7f0e")
#' )
#' }
umap_plot <- function(
    umap_data,
    title,
    source_col     = dataset,
    bottom_source  = NULL,
    colours        = NULL,
    point_alpha    = 1,
    labels         = NULL,
    n_breaks       = 5,
    arrange_by     = NULL,
    arrange_desc   = FALSE,
    arrange_levels = NULL,
    ...
) {
  # Apply factor-level ordering, arrange by those levels, then revert to character
  if (!is.null(arrange_levels)) {
    invalid_cols <- setdiff(names(arrange_levels), colnames(umap_data))
    if (length(invalid_cols) > 0) {
      purrr::walk(invalid_cols, ~ warning(glue::glue(
        "Column `{.x}` not found in `umap_data`. Ignoring `arrange_levels` for this column."
      )))
    }
    
    valid_cols <- intersect(names(arrange_levels), colnames(umap_data))
    umap_data <- purrr::reduce(
      valid_cols,
      function(data, col) {
        data |>
          dplyr::mutate(
            dplyr::across(dplyr::all_of(col), ~ factor(.x, levels = arrange_levels[[col]]))
          ) |>
          dplyr::arrange(dplyr::across(dplyr::all_of(col))) |>
          dplyr::mutate(
            dplyr::across(dplyr::all_of(col), as.character)
          )
      },
      .init = umap_data
    )
  }
  
  # Sort rows by the columns in arrange_by if specified
  if (!is.null(arrange_by)) {
    if (arrange_desc) {
      umap_data <- dplyr::arrange(umap_data, dplyr::across(dplyr::all_of(arrange_by), dplyr::desc))
    } else {
      umap_data <- dplyr::arrange(umap_data, dplyr::across(dplyr::all_of(arrange_by)))
    }
  }
  
  # Sorted unique source values — establishes a stable alphabetical reference
  # order used to assign colours, labels, and alpha values consistently
  unique_sources <- umap_data |>
    dplyr::arrange({{ source_col }}) |>
    dplyr::distinct({{ source_col }}) |>
    dplyr::pull()
  
  # Validate and name the colour vector
  if (!is.null(colours)) {
    if (is.null(names(colours))) {
      names(colours) <- unique_sources
    } else {
      missing_colours <- setdiff(unique_sources, names(colours))
      extra_colours   <- setdiff(names(colours),  unique_sources)
      
      if (length(missing_colours) > 0) {
        stop(glue::glue(
          "Colours are missing for the following sources: {paste(missing_colours, collapse = ', ')}"
        ))
      }
      if (length(extra_colours) > 0) {
        warning(glue::glue(
          "Extra colours supplied for unused sources: {paste(extra_colours, collapse = ', ')}"
        ))
      }
      colours <- colours[unique_sources]
    }
  } else {
    n_src <- length(unique_sources)
    if (n_src > 20L) {
      stop(glue::glue(
        "The Tableau palette supports at most 20 colours but {n_src} data sources were found. ",
        "Please supply colours manually via the `colours` argument."
      ))
    }
    palette_name <- if (n_src > 10L) "Tableau 20" else "Tableau 10"
    warning(glue::glue("No colours provided. Using the {palette_name} Tableau colour palette."))
    colours <- rlang::set_names(
      ggthemes::tableau_color_pal(palette_name)(n_src),
      unique_sources
    )
  }
  
  # Validate and name the label vector
  if (!is.null(labels)) {
    if (is.null(names(labels))) {
      names(labels) <- unique_sources
    } else {
      missing_labels <- setdiff(unique_sources, names(labels))
      extra_labels   <- setdiff(names(labels),  unique_sources)
      
      if (length(missing_labels) > 0) {
        stop(glue::glue(
          "Labels are missing for the following sources: {paste(missing_labels, collapse = ', ')}"
        ))
      }
      if (length(extra_labels) > 0) {
        warning(glue::glue(
          "Extra labels supplied for unused sources: {paste(extra_labels, collapse = ', ')}"
        ))
      }
      labels <- labels[unique_sources]
    }
  } else {
    warning("No labels provided. Using the unique values from `source_col`.")
    labels <- rlang::set_names(unique_sources, unique_sources)
  }
  
  # Expand a scalar alpha to all groups; validate length
  if (length(point_alpha) == 1L) {
    point_alpha <- rep(point_alpha, length(unique_sources))
  }
  if (length(point_alpha) != length(unique_sources)) {
    stop(
      "`point_alpha` must be length 1 or equal to the number of unique values in `source_col` ",
      glue::glue("({length(unique_sources)}).")
    )
  }
  
  # colours and labels are already named (set in every branch above);
  # only point_alpha needs naming here
  col_vals   <- colours
  alpha_vals <- rlang::set_names(point_alpha, unique_sources)
  label_vals <- labels
  
  p <- ggplot2::ggplot(
    umap_data,
    ggplot2::aes(
      x      = dimension_1,
      y      = dimension_2,
      colour = {{ source_col }},
      alpha  = {{ source_col }},
      ...
    )
  ) +
    ggplot2::ggtitle(
      title,
      subtitle = glue::glue("n_neighbours = {unique(umap_data$neighbours)}")
    ) +
    ggplot2::scale_x_continuous(
      name   = "Dimension 1",
      breaks = scales::pretty_breaks(n = n_breaks)
    ) +
    ggplot2::scale_y_continuous(
      name   = "Dimension 2",
      breaks = scales::pretty_breaks(n = n_breaks)
    ) +
    ggplot2::scale_colour_manual(
      name   = "Data Source",
      values = col_vals,
      labels = label_vals
    ) +
    ggplot2::scale_alpha_manual(
      values = alpha_vals,
      guide  = "none"
    ) +
    ggthemes::theme_fivethirtyeight() +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.key        = ggplot2::element_blank(),
      legend.text       = ggplot2::element_text(size = 14),
      legend.key.size   = ggplot2::unit(0.75, "lines"),
      legend.direction  = "horizontal",
      panel.background  = ggplot2::element_rect(fill = "transparent"),
      plot.background   = ggplot2::element_rect(fill = "transparent"),
      text              = ggplot2::element_text(family = "Inconsolata")
    )
  
  # Draw bottom_source points first so they render beneath all other groups
  if (!is.null(bottom_source) && bottom_source %in% dplyr::pull(umap_data, {{ source_col }})) {
    p <- p + ggplot2::geom_point(data = dplyr::filter(umap_data, {{ source_col }} == bottom_source))
  }
  
  if (!is.null(bottom_source)) {
    p <- p + ggplot2::geom_point(data = dplyr::filter(umap_data, {{ source_col }} != bottom_source))
  } else {
    p <- p + ggplot2::geom_point()
  }
  
  p
}
