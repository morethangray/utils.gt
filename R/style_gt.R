#' Apply cell text styles to selected columns in a gt table
#'
#' Styles the body cells of a `gt` table using values from a configuration list.
#'
#' @param gt_table A `gt` table object.
#' @param selection_type A string defining how to match columns; passed to
#'   [tidyselect_gt_column_name()]. Defaults to `"contains"`.
#' @param columns A character vector or tidyselect expression identifying columns to style.
#' @param font_transform Optional text transformation (e.g., `"uppercase"` or `"capitalize"`).
#' @param font_name The name of the font key in the configuration list. Defaults to `"font_base"`.
#' @param font_color The name of the color key in the configuration list. Defaults to `"color_font_base"`.
#' @param font_size The name of the size key in the configuration list. Defaults to `"size_s"`.
#'
#' @return A modified `gt` table object with updated cell text styles.
#' @examples
#' # style_gt_cells(gt_table, columns = "value")
#' @export
style_gt_cells <- function(
  gt_table,
  selection_type = "contains",
  columns,
  font_transform = NULL,
  font_name = "font_base",
  font_color = "color_font_base",
  font_size = "size_s"
) {
  config <- get("gt_config", envir = asNamespace("utils.gt"))

  column_names <- tidyselect_gt_column_name(
    gt_table = gt_table,
    selection_type = selection_type,
    columns = columns
  )
  gt_table |>
    tab_style(
      locations = cells_body(columns = column_names),
      style = cell_text(
        font = config[[font_name]],
        color = config[[font_color]],
        size = px(config[[font_size]]),
        transform = font_transform
      )
    )
}

#' Style stub cells in a gt table
#'
#' Applies text and border styles to the stub (row label) column of a `gt` table.
#'
#' @param gt_table A `gt` table object.
#' @param font_transform Optional text transformation (e.g., `"uppercase"`).
#' @param font_name The font key in the configuration list. Defaults to `"font_semibold"`.
#' @param font_color The color key in the configuration list. Defaults to `"color_font_dark"`.
#' @param font_size The font size key in the configuration list. Defaults to `"size_base"`.
#'
#' @return A modified `gt` table with styled stub cells.
#' @examples
#' # style_gt_cells_stub(gt_table)
#' @export
style_gt_cells_stub <- function(
  gt_table,
  font_transform = NULL,
  font_name = "font_semibold",
  font_color = "color_font_dark",
  font_size = "size_base"
) {
  config <- get("gt_config", envir = asNamespace("utils.gt"))

  gt_table |>
    tab_style(
      locations = cells_stub(),
      style = list(
        cell_text(
          font = config[[font_name]],
          color = config[[font_color]],
          size = px(config[[font_size]]),
          transform = font_transform
        ),
        cell_borders(sides = "right", weight = px(0))
      )
    )
}

#' Apply bold column label styles in a gt table
#'
#' Formats column labels in a `gt` table using bold or emphasized styles.
#'
#' @param gt_table A `gt` table object.
#' @param selection_type A string defining how to match columns; passed to
#'   [tidyselect_gt_column_name()]. Defaults to `"contains"`.
#' @param columns A character vector or tidyselect expression identifying columns to style.
#' @param font_transform Text transformation (e.g., `"uppercase"`). Defaults to `"uppercase"`.
#' @param font_name Font key in the configuration list. Defaults to `"font_semibold"`.
#' @param font_color Color key in the configuration list. Defaults to `"color_font_light"`.
#' @param font_size Size key in the configuration list. Defaults to `"size_base"`.
#'
#' @return A `gt` table with formatted column labels.
#' @examples
#' # style_gt_column_labels_bold(gt_table, columns = "score")
#' @export
style_gt_column_labels_bold <- function(
  gt_table,
  selection_type = "contains",
  columns,
  font_transform = "uppercase",
  font_name = "font_semibold",
  font_color = "color_font_light",
  font_size = "size_base"
) {
  config <- get("gt_config", envir = asNamespace("utils.gt"))

  column_names <- tidyselect_gt_column_name(
    gt_table = gt_table,
    selection_type = selection_type,
    columns = columns
  )
  gt_table |>
    tab_style(
      locations = cells_column_labels(columns = column_names),
      style = cell_text(
        font = config[[font_name]],
        color = config[[font_color]],
        size = px(config[[font_size]]),
        transform = font_transform
      )
    )
}

#' Style the grand summary row in a gt table
#'
#' Formats the grand summary row with custom font, fill, and padding settings.
#'
#' @param gt_table A `gt` table object.
#' @param font_transform Text transformation (e.g., `"uppercase"`). Defaults to `"uppercase"`.
#' @param font_name Font key in the configuration list. Defaults to `"font_medium"`.
#' @param font_color Color key in the configuration list. Defaults to `"color_font_medium"`.
#' @param font_size Size key in the configuration list. Defaults to `"size_base"`.
#' @param fill_color Background color key. Defaults to `"color_grand_summary_row"`.
#' @param pad_size Padding key. Defaults to `"pad_s"`.
#' @param border_right_size Optional right border size.
#'
#' @return A `gt` table with styled grand summary rows.
#' @examples
#' # style_gt_grand_summary_row(gt_table)
#' @export
style_gt_grand_summary_row <- function(
  gt_table,
  font_transform = "uppercase",
  font_name = "font_medium",
  font_color = "color_font_medium",
  font_size = "size_base",
  fill_color = "color_grand_summary_row",
  pad_size = "pad_s",
  border_right_size = NULL
) {
  config <- get("gt_config", envir = asNamespace("utils.gt"))

  gt_table |>
    tab_options(grand_summary_row.padding = px(config[[pad_size]])) |>
    tab_style(
      locations = list(cells_stub_grand_summary(), cells_grand_summary()),
      style = list(
        cell_fill(color = config[[fill_color]]),
        cell_text(
          font = config[[font_name]],
          color = config[[font_color]],
          size = px(config[[font_size]]),
          transform = font_transform
        ),
        cell_borders(sides = "right", weight = px(0))
      )
    ) |>
    tab_style(
      locations = cells_stub_grand_summary(),
      style = cell_text(
        font = config[[font_name]],
        color = config[[font_color]],
        size = px(config[[font_size]] - 1),
        transform = font_transform
      )
    )
}

#' Style the row group in a gt table
#'
#' Formats the row group with custom font and font color settings.
#'
#' @param gt_table A `gt` table object.
#' @param font_transform Text transformation (e.g., `"capitalize"`). Defaults to `"capitalize"`.
#' @param font_name Font key in the configuration list. Defaults to `"font_medium"`.
#' @param font_color Color key in the configuration list. Defaults to `"color_font_medium"`.
#' @param font_size Size key in the configuration list. Defaults to `"size_xl"`.
#'
#' @return A `gt` table with styled row group cells.
#' @export
style_gt_row_group <- function(
    gt_table,
    font_transform = "capitalize",
    font_name = "font_medium",
    font_color = "color_font_medium",
    font_size = "size_xl"){

  config <- get("gt_config", envir = asNamespace("utils.gt"))

  gt_table |>
    tab_style(
      locations = cells_row_groups(groups = everything()),
      style = list(
        cell_text(font = config[[font_name]],
                  color = config[[font_color]],
                  size = px(config[[font_size]]),
                  transform = font_transform),
        cell_borders(sides = "right", weight = px(0))
      )
    )
}

#' Style the stubhead in a gt table
#'
#' Formats the stubhead (header above the stub column) with font and color settings.
#'
#' @param gt_table A `gt` table object.
#' @param font_transform Text transformation (e.g., `"uppercase"`). Defaults to `"uppercase"`.
#' @param font_name Font key in the configuration list. Defaults to `"font_semibold"`.
#' @param font_color Color key in the configuration list. Defaults to `"color_font_light"`.
#' @param font_size Size key in the configuration list. Defaults to `"size_base"`.
#'
#' @return A `gt` table with styled stubhead text.
#' @examples
#' # style_gt_stubhead(gt_table)
#' @export
style_gt_stubhead <- function(
  gt_table,
  font_transform = "uppercase",
  font_name = "font_semibold",
  font_color = "color_font_light",
  font_size = "size_base"
) {
  config <- get("gt_config", envir = asNamespace("utils.gt"))

  gt_table |>
    tab_style(
      locations = cells_stubhead(),
      style = cell_text(
        font = config[[font_name]],
        color = config[[font_color]],
        size = px(config[[font_size]]),
        transform = font_transform
      )
    )
}

#' Style column spanners in a gt table
#'
#' Applies font and color styling to column spanners in a `gt` table.
#'
#' @param gt_table A `gt` table object.
#' @param columns Optional character vector to match spanner names (via `starts_with()`).
#' @param font_name Font key in the configuration list. Defaults to `"font_semibold"`.
#' @param font_color Color key in the configuration list. Defaults to `"color_font_light"`.
#' @param font_transform Text transformation (e.g., `"uppercase"`). Defaults to `"uppercase"`.
#' @param font_size Size key in the configuration list. Defaults to `"size_s"`.
#'
#' @return A `gt` table with styled column spanners.
#' @examples
#' # style_gt_spanner(gt_table)
#' @export
style_gt_spanner <- function(
  gt_table,
  columns = NULL,
  font_name = "font_semibold",
  font_color = "color_font_light",
  font_transform = "uppercase",
  font_size = "size_s"
) {
  config <- get("gt_config", envir = asNamespace("utils.gt"))

  if (is.null(columns)) {
    gt_table |>
      tab_style(
        locations = cells_column_spanners(),
        style = cell_text(
          font = config[[font_name]],
          color = config[[font_color]],
          size = px(config[[font_size]]),
          transform = font_transform
        )
      )
  } else {
    gt_table |>
      tab_style(
        locations = cells_column_spanners(spanners = starts_with(columns)),
        style = cell_text(
          font = config[[font_name]],
          color = config[[font_color]],
          size = px(config[[font_size]]),
          transform = font_transform
        )
      )
  }
}

#' Apply standard styling for effort summary gt tables
#'
#' Applies consistent widths, labels, and font styles for effort summary tables.
#'
#' @param gt_table A `gt` table object.
#'
#' @return A `gt` table styled for effort summaries.
#' @examples
#' # style_gt_effort(gt_table)
#' @export
style_gt_effort <- function(gt_table) {
  config <- get("gt_config", envir = asNamespace("utils.gt"))

  columns_muted <- c("count")

  gt <- gt_table |>
    tab_style(
      locations = cells_column_spanners(
        spanners = starts_with(c("Excluded", "Absent"))
      ),
      style = cell_text(
        size = px(config$size_xs),
        color = config$color_spanner_group_light
      )
    ) |>
    tab_style(
      locations = cells_body(columns = starts_with(c("excluded", "absent"))),
      style = cell_text(size = px(config$size_s))
    ) |>
    style_gt_cells(columns = columns_muted, font_color = "color_font_light") |>
    cols_width(
      stub() ~ px(150),
      starts_with("empty") ~ px(25),
      ends_with("percent") ~ px(50),
      ends_with("locations_n") ~ px(80),
      ends_with("count") ~ px(50)
    ) |>
    cols_label(
      starts_with(c("empty", "Empty")) ~ "",
      starts_with("locations_n") ~ "Locations",
      ends_with("percent") ~ "%",
      ends_with("count") ~ "Days"
    )
  return(gt)
}

#' Apply standard styling for diet (carnivore, omnivore, herbivore) gt tables
#'
#' Applies consistent colors and font styles for summary tables with diet type.
#'
#' @param gt_table A `gt` table object.
#'
#' @return A `gt` table styled for diet type.
#' @export
style_gt_diet <- function(gt_table){

  config <- get("gt_config", envir = asNamespace("utils.gt"))

  color_carn <- gt_config$color_carn
  color_herb <- gt_config$color_herb
  color_omni <- gt_config$color_omni
  color_carn_fill <- gt_config$color_carn_fill
  color_herb_fill <- gt_config$color_herb_fill
  color_omni_fill <- gt_config$color_omni_fill

  gt <-   gt_table |>
    # Define styles for row_group labels
    style_gt_row_group() |>
    tab_style(
      locations = cells_row_groups(groups = "Carnivore"),
      style = list(
        cell_text(color = color_carn),
        cell_fill(color = color_carn_fill))) |>
    tab_style(
      locations = cells_row_groups(groups = "Herbivore"),
      style = list(
        cell_text(color = color_herb),
        cell_fill(color = color_herb_fill))) |>
    tab_style(
      locations = cells_row_groups(groups = "Omnivore"),
      style = list(
        cell_text(color = color_omni),
        cell_fill(color = color_omni_fill)))

  return(gt)
}

#' Apply standard styling for image count gt tables
#'
#' Applies consistent font styles and column widths for summary tables with image count.
#'
#' @param gt_table A `gt` table object.
#'
#' @return A `gt` table styled for image count.
#' @export
style_gt_images <- function(gt_table){

  columns_muted <- c("mean", "minimum", "maximum")

  gt <- gt_table |>

    # Emphasize total columns
    utils.gt::style_gt_column_labels_bold(
      columns = "_total",
      font_transform = NULL,
      font_name = "font_semibold",
      font_color = "color_font_dark"
    ) |>
    utils.gt::style_gt_cells(
      columns = "total",
      font_name = "font_semibold",
      font_color = "color_font_dark",
      font_size = "size_base"
    )  |>

    # Lighter color and smaller font for summary stats
    utils.gt::style_gt_cells(
      columns = columns_muted,
      font_color = "color_font_light",
      font_size = "size_s"
    )  |>

    gt::cols_width(
      stub() ~ px(50),
      starts_with("empty") ~ px(25),
      ends_with("percent") ~ px(40),
      ends_with("locations_n") ~ px(80),
      ends_with("total") ~ px(50),
      ends_with(c("mean", "minimum", "maximum")) ~ px(40)
    ) |>
    gt::cols_label(
      starts_with(c("empty", "Empty")) ~ "",
      starts_with("locations_n") ~ "Locations",
      ends_with("percent") ~ "% All",
      ends_with("total") ~ "Total",
      ends_with("mean") ~ "Mean",
      ends_with("minimum") ~ "Min.",
      ends_with("maximum") ~ "Max."
    )

  return(gt)

}

