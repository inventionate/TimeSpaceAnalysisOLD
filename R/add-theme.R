#' Optimise ggplot2 plot.
#'
#' @param font_family plot overall font family.
#'
#' @return ggplot2 geoms.
#' @export
add_theme <- function(font_family = "Myriad Pro") {
  theme_minimal() +
    theme(text = element_text(family = font_family),
          title = element_text(face = "bold", size = 17),
          strip.text = element_text(size = 16),
          # panel.grid.minor=element_blank(),
          # panel.grid.major=element_blank(),
          legend.position = "none")
}
