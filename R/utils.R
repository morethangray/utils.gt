#' Apply default styling options to a gt table
#'
#' Establishes consistent baseline formatting for `gt` tables using values
#' from a configuration list, including font, padding, and row/column styling.
#'
#' @param gt_table A `gt` table object.
#'
#' @details
#' This function standardizes the visual presentation of a `gt` table by
#' setting default options such as font families, font sizes, row and
#' column spacing, background colors, and text alignment.
#' It also automatically handles missing values and applies special
#' spanner styling if the table contains column spanners.
#'
#' @return A modified `gt` table object with consistent default styling.
#' @examples
#' # gt_table |> set_gt_defaults()
#' @export
set_gt_defaults <- function(gt_table) {
  config <- get("gt_config", envir = asNamespace("utils.gt"))
  text_size_notes <- config$size_base - 4
  has_spanners <- length(gt_table$`_spanners`) > 0
  column_label_weight <- if (has_spanners) "normal" else "bold"
  gt <- gt_table |>
    tab_options(
      table.font.names = config$font_base,
      table.font.size = px(config$size_base),
      table.font.color = config$color_font_base,
      table.font.color.light = config$color_font_light,
      column_labels.font.size = px(config$size_base),
      column_labels.padding = px(config$pad_s),
      column_labels.padding.horizontal = px(config$pad_l),
      row_group.font.size = px(config$size_xl),
      row_group.text_transform = "capitalize",
      row_group.border.right.width = px(0),
      stub.font.size = px(config$size_base),
      data_row.padding = px(config$pad_m),
      data_row.padding.horizontal = px(config$pad_l),
      summary_row.background.color = "gray98",
      summary_row.padding = px(config$pad_s),
      grand_summary_row.background.color = "gray96",
      grand_summary_row.text_transform = "capitalize",
      grand_summary_row.padding = px(config$pad_s),
      source_notes.font.size = px(text_size_notes),
      quarto.disable_processing = TRUE
    ) |>
    tab_style(locations = cells_body(), style = cell_text(v_align = "top"))
  if (has_spanners) {
    gt <- gt |>
      tab_style(
        locations = cells_column_spanners(),
        style = cell_text(
          font = config$font_semibold,
          size = px(config$size_m),
          color = config$color_font_light,
          transform = "uppercase"
        )
      )
  }
  gt <- gt |> sub_missing(columns = everything(), missing_text = "")
  return(gt)
}

#' Set column widths based on grouping variable
#'
#' Adjusts the column width of the `group_id` column depending on the
#' provided group name (e.g., `"year"`, `"season"`, `"location"`, or `"occasion"`).
#'
#' @param gt_table A `gt` table object.
#' @param group_name A string specifying the grouping variable; must be one of
#'   `"year"`, `"season"`, `"location"`, or `"occasion"`.
#'
#' @details
#' Each grouping value applies a fixed width to the `group_id` column.
#' For `"occasion"`, the function additionally applies bold formatting to
#' columns containing `"year"` or `"season"` using
#' [style_gt_column_labels_bold()].
#'
#' @return A modified `gt` table with adjusted column widths.
#' @examples
#' # gt_table |> set_gt_cols_width(group_name = "season")
#' @export
set_gt_cols_width <- function(
    gt_table,
    group_name
    ) {
  if (group_name == "year") {
    gt <- gt_table |> cols_width(group_id ~ px(50))
  }
  if (group_name == "season") {
    gt <- gt_table |> cols_width(group_id ~ px(60))
  }
  if (group_name == "location") {
    gt <- gt_table |> cols_width(group_id ~ px(80))
  }
  if (group_name == "occasion") {
    gt <- gt_table |>
      cols_width(group_id ~ px(80)) |>
      style_gt_column_labels_bold(
        selection_type = "contains",
        columns = c("year", "season")
      )
  }
  return(gt)
}

#' Resolve tidyselect expressions to column names in a gt table
#'
#' Converts a tidyselect-style column specification into a character
#' vector of column names from a `gt` table body.
#'
#' @param gt_table A `gt` table object.
#' @param selection_type A tidyselect function name such as `"contains"`,
#'   `"starts_with"`, or `"ends_with"`, provided as a string.
#' @param columns The selection term passed to the tidyselect function.
#'
#' @details
#' This function extracts the table body and evaluates the tidyselect
#' expression dynamically using [rlang::exec()], returning a list of
#' matching column names.
#'
#' @return A character vector of column names matching the selection.
#' @examples
#' # tidyselect_gt_column_name(gt_table, "contains", "score")
#' @export
tidyselect_gt_column_name <- function(
    gt_table,
    selection_type,
    columns
    ) {
  gt_table |>
    extract_body() |>
    dplyr::select(rlang::exec(selection_type, columns)) |>
    names()
}

#' Apply number formatting to gt table columns
#'
#' Automatically formats numeric columns in a `gt` table based on patterns
#' defined in a configuration list.
#'
#' @param gt_table A `gt` table object.
#' @param labels A list containing
#'   keys for column name patterns such as `format_cols_percent`, `format_cols_number`,
#'   and `format_cols_integer`. Default values are defined in make_gt_labels()
#'
#' @details
#' This function identifies columns containing numeric, integer, and percentage
#' values based on naming conventions and applies corresponding `gt` formatting
#' functions.
#' - Columns containing the pattern in `labels$format_cols_percent` are formatted as percentages.
#' - Columns containing the pattern in `labels$format_cols_number` are formatted as numbers.
#' - Columns containing the pattern in `labels$format_cols_integer` are formatted as integers.
#'
#' @return A modified `gt` table with formatted numeric and percentage columns.
#' @examples
#' # gt_table |> fmt_gt_units()
#' @export
fmt_gt_units <- function(
    gt_table,
    labels = NULL
    ) {

  if(is.null(labels)) labels <- get("gt_labels", envir = asNamespace("utils.gt"))

  gt_extract <- gt_table |> extract_body(incl_stub_cols = FALSE)
  columns_percent <- gt_extract |>
    select(contains(labels$format_cols_percent)) |>
    names()
  columns_number <- gt_extract |>
    select(contains(labels$format_cols_number)) |>
    names()
  columns_integer <- gt_extract |>
    select(contains(labels$format_cols_integer)) |>
    names()
  gt <- gt_table |>
    gt::fmt_percent(columns = columns_percent, decimals = labels$decimals_percent) |>
    gt::fmt_number(columns = columns_number, decimals = labels$decimals_number) |>
    gt::fmt_integer(columns = columns_integer)
  return(gt)
}

#' Update default column labels
#'
#' Automatically formats numeric columns in a `gt` table based on patterns
#' defined in a configuration list.
#'
#' @param new_labels A named list of new labels to apply to gt_labels.
#' @param labels A list containing keys for column name patterns.
#' Default values are defined in make_gt_labels()
#'
#' @details
#' This function revises the existing gt_labels list.
#'
#' @return A modified `gt_labels` list.
#' @export
update_gt_labels <- function(new_labels, labels = NULL) {

  if(is.null(labels)) labels <- get("gt_labels", envir = asNamespace("utils.gt"))

  # Validate input
  if (!is.list(new_labels)) {
    stop("`new_labels` must be a list.")
  }

  # Only update recognized fields
  valid_names <- names(gt_labels)
  unknown <- setdiff(names(new_labels), valid_names)
  if (length(unknown) > 0) {
    warning("Ignoring unrecognized field(s): ", paste(unknown, collapse = ", "))
  }

  # Update only matching names
  for (nm in intersect(names(new_labels), valid_names)) {
    gt_labels[[nm]] <- unique(c(gt_labels[[nm]], new_labels[[nm]]))
  }

  return(gt_labels)
}
