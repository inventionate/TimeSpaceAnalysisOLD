#' Extract legends form ggplot2 objects.
#'
#' @param p ggplot2 object (plot) containing legends (guides).
#'
#' @return plottable legend grob.
#' @export
extract_legend <- function(p) {
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  return(tmp$grobs[[leg]])
}
