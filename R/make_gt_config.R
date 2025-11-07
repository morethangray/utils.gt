#' Make a gt configuration list
#'
#' Create a configuration list to control gt table styling (text, font, colors)
#'
#' @return A list containing gt configuration settings.
#' @export
make_gt_config <- function() {

  pad_s <- 1; pad_m <- 2; pad_l <- 3

  text_size_base  <- 12
  text_size_xs <- text_size_base - 2
  text_size_s <- text_size_base - 1
  text_size_m <- text_size_base + 1
  text_size_l <- text_size_base + 2
  text_size_xl <- text_size_base + 3

  font_base <- "Gill Sans Nova Book"
  font_medium <- "Gill Sans Nova Medium"
  font_semibold <- "Gill Sans Nova SemiBold"

  color_carn <- "#D68032"
  color_herb <- "#499894"
  color_omni <- "#79706E"
  color_font_base <- "black"
  color_font_dark <- "gray15"
  color_font_medium <- "gray30"
  color_font_light <- "gray50"
  color_font_lightest <- "gray60"
  color_spanner_group <- "gray30"
  color_spanner_group_light <- "gray50"
  color_border <- "#d3d3d3"
  color_grand_summary_row <- "gray96"
  color_summary_row <- "gray98"

  return(
    list(
      pad_s = pad_s,
      pad_m = pad_m,
      pad_l = pad_l,
      font_base = font_base,
      font_medium = font_medium,
      font_semibold = font_semibold,
      size_base = text_size_base,
      size_xs = text_size_xs,
      size_s = text_size_s,
      size_m = text_size_m,
      size_xl = text_size_xl,
      size_l = text_size_l,
      color_carn = color_carn,
      color_herb = color_herb,
      color_omni = color_omni,
      color_font_base = color_font_base,
      color_font_dark = color_font_dark,
      color_font_medium = color_font_medium,
      color_font_light = color_font_light,
      color_font_lightest = color_font_lightest,
      color_border = color_border,
      color_spanner_group = color_spanner_group,
      color_spanner_group_light = color_spanner_group_light,
      color_summary_row = color_summary_row,
      color_grand_summary_row = color_grand_summary_row
    )
  )
}

