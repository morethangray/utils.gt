.onLoad <- function(libname, pkgname) {
  gt_config <<- make_gt_config()
  gt_labels <<- make_gt_labels()
}

utils::globalVariables(c(":="))

# Import functions used throughout utils.gt
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom gt
#'   cell_borders cell_fill cell_text cells_body
#'   cells_column_labels cells_column_spanners
#'   cells_grand_summary cells_row_groups
#'   cells_stub cells_stub_grand_summary
#'   cells_stubhead cols_add cols_label cols_width
#'   extract_body grand_summary_rows px
#'   sub_missing tab_options tab_spanner tab_style
#' @importFrom stats runif
#' @importFrom tidyselect contains starts_with everything
NULL
