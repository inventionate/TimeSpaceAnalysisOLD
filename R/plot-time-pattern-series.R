#' @include get-time-pattern-series.R
NULL
#' Plot time pattern series data.
#'
#' @param data_tp data frame including questionnaire_id, kml3d results and time pattern data.
#' @param alpha opacity of the time pattern lines.
#' @param palette colour of average time pattern lines.
#'
#' @return ggplot2 time pattern series plot.
#' @export
#'
#' @examples
plot_time_pattern_series <- function(data_tp, alpha = 0.3, palette = "Set1")
{
  data_ts <- get_time_pattern_series(data_tp)

  # Zeitserien plotten
  p <- ggplot(data_ts[[1]], aes(x = day, y = duration, group = questionnaire_id)) +
    geom_line(alpha = alpha) + facet_wrap(~activity) +
    geom_line(data = data_ts[[2]], aes(x = day, y = avg_duration, group = zeitmuster, colour = zeitmuster),
              inherit.aes = FALSE, size = 2) +
    scale_colour_brewer(palette = palette, name = "Zeitmuster", labels = data_ts[[3]]) +
    theme_minimal() + scale_x_discrete(name="Wochentage") +
    scale_y_continuous(name="Dauer (in Stunden)") +
    theme(plot.title = element_text(face = "bold", size = 17), text = element_text(family = "Myriad Pro"),
            legend.title = element_text(face = "bold"), strip.text = element_text(face = "bold", size = 10)) +
    ggtitle("Time pattern profiles (kml3d results)")

  return(p)
}
