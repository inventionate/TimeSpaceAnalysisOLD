#' @include utilities.R
NULL
#' Title
#'
#' @param res_gda GDA result.
#' @param level ellipse level (default 86.47\%).
#' @param alpha opacity level (default 0.1).
#' @param colour ellipse fill colour.
#' @param linetype ellipse edge linetype.
#' @param axes the GDA dimensions to plot.
#' @param myriad use Myriad Pro font (boolean).
#' @param scale_size scale minimal point size.
#'
#' @return ggplot2 GDA visualisation with concentration ellipse.
#' @export
fviz_gda_conc_ellipse <- function(res_gda, level = 0.8647, alpha = 0.1, colour = "black", linetype = "dashed", axes = 1:2, myriad = TRUE, scale_size = 1) {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  if(inherits(res_gda, c("MCA"))) p <- fviz_mca_ind(res_gda, label = "none", invisible = c("ind", "ind.sup"), axes.linetype = "solid", axes = axes)
  if(inherits(res_gda, c("MFA"))) p <- fviz_mfa_ind(res_gda, label = "none", invisible = c("ind", "ind.sup"), axes.linetype = "solid", axes = axes)

  p <- p + stat_ellipse(data = .count_distinct_ind(res_gda, axes), aes(x, y),
                        geom ="polygon", level = level, type = "norm",
                        alpha = alpha, colour = colour, linetype = linetype) +
    geom_point(data = .count_distinct_ind(res_gda, axes) %>% distinct(),
               aes(x, y, size = count), inherit.aes = FALSE) +
    scale_size_continuous(range = c(scale_size, scale_size * max(.count_distinct_ind(res_gda)$count))) +
    add_theme()
  return(p)
}

