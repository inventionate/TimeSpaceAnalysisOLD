#' @include get-time-pattern-profile.R
NULL
#' Plot average time pattern profiles.
#'
#' @param data_tp data frame containing questionnaire_id, kml3d results and time pattern data.
#' @param id time pattern to plot.
#' @param ncol facet columns.
#' @param fluid should be static bars or fluid lines visualized (boolean).
#'
#' @return ggplot2 avgerage time pattern profile plot.
#' @export
#'
#' @examples
plot_time_pattern_profile <- function(data_tp, id = "all", ncol = 2, fluid = FALSE)
{

  data_tsp <- get_time_pattern_profile(data_tp, id)

  colours <-  RColorBrewer::brewer.pal(name="Spectral", n = nlevels(data_tsp$activity))

  p <- ggplot(data_tsp, aes(x = day, y = prop_avg_duration))
  if(fluid) p <- p + geom_area(aes(fill = activity), position = "fill") + geom_vline(xintercept = c(1:7), linetype = "dotted", colour = "white")
  else p <- p + geom_bar(aes(fill = activity), position = "fill", stat = "identity", width = 1) + geom_vline(xintercept = c(1.5:6.5), linetype = "solid", colour = "white", size = 0.75)
  p <- p + scale_x_continuous(breaks = c(1:7), labels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%")) +
    scale_fill_manual(name = "Tätigkeiten", values = colours) +
    theme_bw() + theme(axis.title = element_blank(), plot.title = element_text(face = "bold", size = 23), text = element_text(family = "Myriad Pro"),
                       legend.text = element_text(size = 15), legend.title = element_text(face = "bold", size = 17, family = "Myriad Pro"))
  # Mehrere Gafiken parallel erzeugen
  p <- p + facet_wrap(~zeitmuster, ncol = ncol)

  return(p)
}
