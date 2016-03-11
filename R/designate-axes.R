#' Designate axes parts.
#'
#' @param x coordinate of the axes designation. The value will automatically mirrored.
#' @param y coordinate of the axes designation. The value will automatically mirrored.
#' @param designation character vector with the two designation titles.
#' @param rotate designate x (FALSE) or y axis (TRUE).
#' @param size size of the designation titles.
#'
#' @return ggplot2 annotations.
#' @export
#'
#' @examples
designate_axes <- function (x = NULL, y = NULL, designation = c("left", "right"), rotate = FALSE, size = 5) {
  if(!rotate) {
    list(
      annotate("text", x = -x, y = y, label = designation[1], size = 5, fontface = "bold.italic", family = "Myriad Pro"),
      annotate("text", x = x, y = y, label = designation[2], size = 5, fontface = "bold.italic", family = "Myriad Pro")
    )
  } else {
    list(
      annotate("text", x = x, y = -y, label = designation[1], size = size, fontface = "bold.italic", family = "Myriad Pro", angle = 90),
      annotate("text", x = x, y = y, label = designation[1], size = size, fontface = "bold.italic", family = "Myriad Pro", angle = 90)
    )
  }
}
