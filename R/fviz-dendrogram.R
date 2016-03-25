#' Visualize HCPC hclus trees.
#'
#' @param res_hcpc (s)HCPC results.
#' @param palette colour definition per cluster.
#' @param cluster amount of clusters.
#' @param labels plot labels (boolean).
#' @param circle plot circle (boolean).
#' @param hline hline height.
#' @param pointsize leaves pointsize.
#' @param linetype hline linetype.
#'
#' @return ggplot2 dendrogram visualization.
#' @export
fviz_dendrogram <- function(res_hcpc, palette = NULL, cluster = 1, labels = FALSE, circle = FALSE, hline = 0.8, pointsize = 2, linetype = "dashed") {

  # Add Myriad Pro font family
  .add_fonts()

  if(is.null(palette)) palette <- RColorBrewer::brewer.pal(name = "Set1", n = cluster)

  dend <- res_hcpc$call$t$tree %>%
    as.dendrogram %>%
    set("branches_k_color", k = cluster, palette) %>%
    set("branches_lwd", 0.5) %>%
    set("leaves_pch", 20) %>%
    set("leaves_cex", pointsize)
  if(!circle) {
    p <- ggplot(as.ggdend(dend), labels = labels) +
      geom_hline(yintercept = hline, linetype = linetype)
  } else {
    p <- ggplot(as.ggdend(dend), labels = FALSE) +
      scale_y_reverse(expand = c(0.2, 0)) +
      coord_polar(theta = "x")
  }

  p <- p + theme_minimal() + ylab("Level Index") +
    theme(text = element_text(family = "Myriad Pro"),
          title = element_text(face = "bold", size = 17),
          strip.text = element_text(size = 16),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x	= element_blank())
  return(p)
}
