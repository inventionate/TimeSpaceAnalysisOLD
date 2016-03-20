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
#' @return
#' @export
#'
#' @examples
fviz_dendrogram <- function(res_hcpc, palette = NULL, cluster = 1, labels = FALSE, circle = FALSE, hline = 0.8, pointsize = 2, linetype = "dashed") {
  if(is.null(palette)) palette <- RColorBrewer::brewer.pal(name="Set1", n = cluster)

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

  print(p)

}
