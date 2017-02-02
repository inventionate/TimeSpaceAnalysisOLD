#' @include get-path-coord.R
NULL
#' Concat the categories of a variable
#'
#' @param res_gda_quali GDA result.
#' @param var variable name.
#' @param var_levels categories to concat.
#' @param exclude categories to exclude.
#' @param axes axes to plot.
#' @param linetype linetype of concat path.
#' @param colour colour of concat path.
#' @param size size of concat path.
#'
#' @return ggplo2 path geom.
#' @export
add_path <- function(res_gda_quali, var, var_levels = NULL, exclude = NULL, axes = 1:2, linetype = "dashed", colour = "black", size = 1) {
  geom_path(data = get_path_coord(res_gda_quali, var, var_levels, exclude), aes_string(paste0("Dim.",axes[1]), paste0("Dim.",axes[2])), linetype = linetype, colour = colour, size = size)
}
