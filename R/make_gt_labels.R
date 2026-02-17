#' Make a gt label list
#'
#' Create a list to control column summarization. Defines names for metric and group. Defines names of columns by format (number, integer, percent), and those to include in summary rows for total and mean. Can also define columns to ignore, which will result in an empty cell (i.e., to replace "---"). Default column names that are accepted are mean, total, count, minimum, maximum, percent, and empty.
#'
#' @param metric_name Character metric root name (e.g., effort, richness, occupancy).
#' @param metric_cols Character metric columns.
#' @param group_name Character name of grouping column (e.g., year, season).
#' @param ignore_cols Character vector of columns to exclude from processing.
#' @param format_cols_number Character vector of columns to format as numeric (e.g., mean).
#' @param format_cols_integer Character vector of columns to format as integer (e.g., minimum, maximum).
#' @param format_cols_percent Character vector of columns to format as percentage (e.g., percent).
#' @param decimals_number Numeric vector of number of decimals to use for numeric values.
#' @param decimals_integer Numeric vector of number of decimals to use for integer values.
#' @param decimals_percent Numeric vector of number of decimals to use for percent values.
#' @param summarize_cols_total Character vector of columns to summarize in total rows.
#' @param summarize_cols_mean Character vector of columns to summarize in mean rows.
#'
#' @return A list containing gt configuration settings.
#' @export
make_gt_labels <- function(
    metric_name = NULL,
    metric_cols = NULL,
    group_name = NULL,
    ignore_cols = NULL,
    format_cols_number = NULL,
    format_cols_integer = NULL,
    format_cols_percent = NULL,
    decimals_number = NULL,
    decimals_integer = NULL,
    decimals_percent = NULL,
    summarize_cols_total = NULL,
    summarize_cols_mean = NULL
) {

  label_group <- stringr::str_to_sentence(group_name)
  label_total <- stringr::str_to_sentence(metric_cols)
  spanner_title_metric <- stringr::str_to_sentence(metric_name)
  spanner_cols_metric <- metric_cols

  format_cols_number <- unique(c("mean", format_cols_number))
  format_cols_integer <- unique(c("total", "count", "minimum", "maximum", format_cols_integer))
  format_cols_percent <- unique(c("percent", format_cols_percent))

  decimals_number <- 0
  decimals_integer <- 1
  decimals_percent <- 0

  ignore_cols <- unique(c("empty", group_name, ignore_cols))

  summarize_cols_total <- unique(c(format_cols_integer, summarize_cols_total))
  summarize_cols_mean <- unique(c(format_cols_number,
                                  format_cols_integer,
                                  format_cols_percent,
                                  summarize_cols_mean))


  return(
    list(
      title_metric = spanner_title_metric,
      cols_metric = spanner_cols_metric,
      ignore_cols = ignore_cols,
      format_cols_number = format_cols_number,
      format_cols_integer = format_cols_integer,
      format_cols_percent = format_cols_percent,
      decimals_number = decimals_number,
      decimals_integer = decimals_integer,
      decimals_percent = decimals_percent,
      summarize_cols_total = summarize_cols_total,
      summarize_cols_mean = summarize_cols_mean,
      label_group = label_group,
      label_total = label_total
    )
  )
}

