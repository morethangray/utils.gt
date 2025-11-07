.onLoad <- function(libname, pkgname) {
  gt_config <<- make_gt_config()
  gt_labels <<- make_gt_labels()
}

# Import functions used throughout utils.gt
#' @importFrom gt
#'   cell_borders cell_fill cell_text cells_body
#'   cells_column_labels cells_column_spanners
#'   cells_grand_summary cells_stub cells_stub_grand_summary
#'   cells_stubhead cols_add cols_label cols_width
#'   extract_body grand_summary_rows px
#'   sub_missing tab_options tab_spanner tab_style
#' @importFrom dplyr select
#' @importFrom tidyselect contains starts_with everything
#' @importFrom stats runif
NULL
