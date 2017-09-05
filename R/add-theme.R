#' Optimise ggplot2 plot.
#'
#' @param plot ggplot2 object.
#' @param font_family plot overall font family.
#'
#' @return ggplot2 geoms.
#' @export
add_theme <- function(plot, font_family = "Myriad Pro") {
  plot +
    coord_fixed() +
    theme_minimal() +
    theme(text = element_text(family = font_family),
          title = element_text(size = 14),
          strip.text = element_text(size = 13),
          axis.text = element_text(size = 9),
          axis.title = element_text(size = 12),
          axis.ticks = element_line(size = 0.5, colour = "gray70"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = "transparent", colour = "gray70", size = 1, linetype = "solid"),
          legend.position = "none")
}
