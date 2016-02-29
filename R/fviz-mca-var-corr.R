#' Visualize MCA variable correlation square.
#'
#' @param X MCA result.
#' @param axes axes to plot.
#' @param geom whether points or labels to plot.
#' @param labelsize size of labels.
#' @param pointsize size of points.
#' @param invisible hide "passive" or "active" variables.
#' @param labels label points or not (boolean).
#' @param repel repel labels (boolean).
#' @param select selection of variables (names) or eta2 values (all above value).
#'
#' @return ggplot2 visualization of variable correlation square (variables representation).
#' @export
#'
#' @examples
fviz_mca_var_corr <- function(X,  axes = c(1,2), geom=c("point", "text"), labelsize = 4, pointsize = 2,
                              invisible = NULL, labels = TRUE, repel = TRUE, select = list(name = NULL, eta2 = NULL)) {

  vars <- get_mca_var_corr(X, axes = axes)

  colnames(vars)[3:4] <-  c("x", "y")

  # Selection
  if(!is.null(select)) vars <- .select(vars, select)

  # Exclude invisible Data
  vars <- vars[which(Hmisc::`%nin%`(vars$type, invisible)), , drop = TRUE]

  # Plot
  p <- ggplot()

  vars <- as.data.frame(vars)

  if("point" %in% geom) p <- p + geom_point(data = vars, aes(x, y, colour = type, shape = type), size = pointsize)

  if(labels & "text" %in% geom)
    if(repel) {
      p <- p + ggrepel::geom_text_repel(data = vars, mapping = aes(x, y, color = type, label = name), size = labelsize)
    } else {
      p <- p + geom_text(data = vars, mapping = aes(x, y, color = type, label = name), size = labelsize, nudge_y = -0.015)
    }

  # Set fix dimensions
  p <- p + scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
    theme(panel.border = element_rect(linetype = "solid", fill = "transparent"))

  p + labs(title="MCA - Variable Representation")
}
