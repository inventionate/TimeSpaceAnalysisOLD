#' @include utilities.R
NULL
#' Visualize MCA variable representation square.
#'
#' @param res_gda MCA result.
#' @param axes axes to plot.
#' @param geom whether points or labels to plot.
#' @param labelsize size of labels.
#' @param pointsize size of points.
#' @param invisible hide "passive" or "active" variables.
#' @param labels label points or not (boolean).
#' @param repel repel labels (boolean).
#' @param select selection of variables (names) or eta2 values (all above value).
#' @param title plot title.
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#'
#' @return ggplot2 visualization of variable correlation square (variables representation).
#' @export
fviz_mca_var_corr <- function(res_gda,  axes = c(1,2), geom=c("point", "text"), labelsize = 4, pointsize = 2,
                              invisible = NULL, labels = TRUE, repel = TRUE, select = list(name = NULL, eta2 = NULL),
                              myriad = TRUE, plot_modif_rates = TRUE, title = "MCA - Variable Representation") {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  vars <- get_mca_var_corr(res_gda, axes = axes)

  colnames(vars)[3:4] <-  c("x", "y")

  # Selection
  if(!is.null(select)) vars <- .select(vars, select)

  # Exclude invisible Data
  vars <- vars[which(Hmisc::`%nin%`(vars$type, invisible)), , drop = TRUE] %>% data.frame

  # Plot
  p <- ggplot(data = vars)

  if("point" %in% geom) p <- p + geom_point(aes(x, y, colour = vars$type, shape = vars$type), size = pointsize)

  if(labels & "text" %in% geom)
    if(repel) {
      p <- p + ggrepel::geom_text_repel(mapping = aes(x, y, color = vars$type, label = name), size = labelsize)
    } else {
      p <- p + geom_text(mapping = aes(x, y, color = vars$type, label = name), size = labelsize, nudge_y = -0.015)
    }

  # Set fix dimensions
  p <- p + add_theme() +
    scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
    theme(panel.border = element_rect(linetype = "solid", fill = "transparent"))

  # If there are no passive variables use only one colour
  if( nlevels(vars$type) == 1 ) p <- p + scale_colour_manual(values = c("black"))

  # Update labels
  p <- .gda_plot_labels(res_gda, p, title, axes, plot_modif_rates)

  # Return ggplot
  return(p)
}
