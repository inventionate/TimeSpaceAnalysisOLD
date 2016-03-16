#' @include utilities.R
NULL
#' Title
#'
#' @param res_gda GDA result.
#' @param level ellipse level (default 0.95\%).
#' @param alpha opacity level (default 0.1).
#' @param colour ellipse fill colour.
#' @param linetype ellipse edge linetype.
#' @param axes the GDA dimensions to plot.
#'
#' @return ggplot2 GDA visualisation with concentration ellipse.
#' @export
#'
#' @examples
fviz_gda_conc_ellipse <- function(res_gda, level = 0.8647, alpha = 0.1, colour = "black", linetype = "dashed", axes = 1:2) {

  # Add Myriad Pro font family
  .add_fonts()

  if(inherits(res_gda, c("MCA", "sMCA"))) p <- fviz_mca_ind(res_gda, label = "none", invisible = "ind", axes.linetype = "solid", axes = axes)
  if(inherits(res_gda, c("MFA", "sMFA"))) p <- fviz_mfa_ind(res_gda, label = "none", invisible = "ind", axes.linetype = "solid", axes = axes)

  p <- p + stat_ellipse(data = .count_distinct_ind(res_gda, axes), aes(x = Dim.1, y = Dim.2),
                        geom ="polygon", level = level, type = "norm",
                        alpha = alpha, colour = colour, linetype = linetype) +
    geom_point(data = .count_distinct_ind(res_gda, axes) %>% distinct(),
               aes(x = Dim.1, y = Dim.2, size = count), inherit.aes = FALSE) +
    scale_size_continuous(range = c(1, max(.count_distinct_ind(res_gda)$count))) +
    add_theme()
  return(p)
}

