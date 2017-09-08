#' Designate axes parts.
#'
#' @param x coordinate of the axes designation. The value will automatically mirrored.
#' @param y coordinate of the axes designation. The value will automatically mirrored.
#' @param designation character vector with the two designation titles.
#' @param rotate designate x (FALSE) or y axis (TRUE).
#' @param size size of the designation titles.
#' @param fontface fontface.
#' @param alpha font alpha.
#'
#' @return ggplot2 annotations.
#' @export
designate_axes <- function (x = NULL, y = NULL, designation = c("left/bottom", "right/top"), rotate = FALSE, size = 5,
                            fontface = "bold.italic", alpha = 1) {

  if(length(x) == 1) x <- c(-x[1], x[1])
  if(length(y) == 1) y <- c(-y[1], y[1])

  if(!rotate) {
    list(
      annotate("text", x = x[1], y = -y[1], label = designation[1], size = size, family = "Myriad Pro", fontface = fontface, alpha = alpha),
      annotate("text", x = x[2], y = -y[1], label = designation[2], size = size, family = "Myriad Pro", fontface = fontface, alpha = alpha)
    )
  } else {
    list(
      annotate("text", x = -x[1], y = y[1], label = designation[1], size = size, family = "Myriad Pro", angle = 90, fontface = fontface, alpha = alpha),
      annotate("text", x = -x[1], y = y[2], label = designation[2], size = size, family = "Myriad Pro", angle = 90, fontface = fontface, alpha = alpha)
    )
  }
}
