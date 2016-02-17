#' Visualize specific contributing modalities.
#'
#' @param res_gda GDA result data frame.
#' @param axis dimension to be filtered.
#' @param contrib count of modalities to plot.
#' @param title plot title.
#' @param axes the GDA dimensions to plot.
#'
#' @return ggplot2 visualization containing selected modalities.
#' @export
#'
#' @examples
fviz_gda_var_axis <- function(res_gda, axis = 1, contrib = 20, title = "GDA axis contribution 20", axes = 1:2) {
  # Check GDA algorithm
  if(inherits(res_gda, c("MCA", "sMCA"))) df <- res_gda$var$contrib
  if(inherits(res_gda, c("MFA", "sMFA"))) df <- res_gda$quali.var$contrib

  # Auswahl festlegen
  axis <- df %>%
    data.frame %>% select(Dim = matches(paste0("Dim.",axis))) %>% add_rownames() %>%
    arrange(desc(Dim)) %>% slice(1:contrib) %>%
    select(rowname) %>% data.frame

  if(inherits(res_gda, c("MCA", "sMCA"))) p <- fviz_mca_var(res_gda, col.var = "black", repel = TRUE, select.var = list(name = axis$rowname), axes.linetype = "solid", axes = axes)
  if(inherits(res_gda, c("MFA", "sMFA"))) p <- fviz_mfa_quali_var(res_gda, col.var = "black", repel = TRUE, select.var = list(name = axis$rowname), axes.linetype = "solid", axes = axes)
  p <- p + add_theme() + ggtitle(title)
  return(p)
}
