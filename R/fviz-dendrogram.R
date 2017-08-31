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
#' @param myriad use Myriad Pro font (boolean).
#' @param cut_height cut dendrogram at specific hight.
#' @param title the plot title.
#' @param cut_upper style upper dendrogram.
#' @param colour_upper colour of the upper dendrogram.
#'
#' @return ggplot2 dendrogram visualization.
#' @export
fviz_dendrogram <- function(res_hcpc, palette = NULL, cluster = 1, labels = FALSE, circle = FALSE, hline = 0.8,
                            pointsize = 2, linetype = "dashed", myriad = TRUE, cut_height = NULL, title = NULL,
                            cut_upper = NULL, colour_upper = "#555555", hlabel = NULL) {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  if(is.null(palette)) palette <- RColorBrewer::brewer.pal(name = "Set1", n = cluster)

  dend <- res_hcpc$call$t$tree %>%
    as.dendrogram %>%
    set("branches_k_color", k = cluster, palette) %>%
    set("branches_lwd", 0.5) %>%
    set("branches_lty", 1) %>%
    set("leaves_pch", 20) %>%
    set("leaves_cex", pointsize) %>%
    set("nodes_cex", -1)
  if( !is.null(cut_height) ) {
    dend <- cut(dend, h = cut_height)$upper
    dend <- hang.dendrogram(dend, hang = -1)
  }

  # ggplot2 integration
  data_dend <- as.ggdend(dend)

  # Prepare upper data
  if( !is.null(cut_upper) )
  {
    data_dend_segments <- data_dend$segments %>%
      rownames_to_column(var = "id")

    data_dend_selection <- data_dend_segments %>% filter(y > cut_upper)

    data_dend_segments_mod <- data_dend_segments %>%
      mutate(
        col = if_else( id %in% data_dend_selection$id, colour_upper, col),
        lty = if_else( id %in% data_dend_selection$id, 3, lty)) %>%
      select(-id)

    data_dend$segments <- data_dend_segments_mod
  }

  if( !circle ) {
    p <- ggplot(data_dend, labels = labels) +
      geom_hline(yintercept = hline, linetype = linetype)
    if( !is.null(hlabel) ) p <- p + annotate(geom = "text", x = 0, y = hline + 0.01,  label = hlabel)
  } else {
    p <- ggplot(data_dend, labels = labels) +
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

  if(!is.null(title)) p <- p + ggtitle(title)

  return(p)
}
