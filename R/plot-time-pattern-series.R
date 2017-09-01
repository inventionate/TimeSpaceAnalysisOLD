#' @include utilities.R
#' @include get-time-pattern-series.R
NULL
#' Plot time pattern series data.
#'
#' @param data_tp data frame including questionnaire_id, kml3d results and time pattern data.
#' @param alpha opacity of the time pattern lines.
#' @param palette colour of average time pattern lines.
#' @param myriad use Myriad Pro font (boolean).
#' @param title plot title.
#' @param hour_scale y axis breaks (hours).
#' @param hour_limits y axis limits (hours).
#'
#' @return ggplot2 time pattern series plot.
#' @export
plot_time_pattern_series <- function(data_tp, alpha = 0.3, palette = "Set1", myriad = TRUE, title = "Time pattern profiles (kml3d results)",
                                     hour_limits = c(0,24), hour_scale = c(0, 4, 8, 12, 16, 20, 24)) {
  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  data_ts <- get_time_pattern_series(data_tp)

  # Zeitserien plotten
  p <- ggplot(data_ts$data_series, aes(x = day, y = duration, group = questionnaire_id)) +
    geom_line(alpha = alpha) + facet_wrap(~activity) +
    geom_line(data = data_ts$data_series_average, aes(x = day, y = avg_duration, group = as.factor(zeitmuster), colour = as.factor(zeitmuster)),
              inherit.aes = FALSE, size = 2) +
    scale_colour_brewer(palette = palette, name = "Zeitmuster", labels = data_ts$data_series_profile_prop_label) +
    scale_x_discrete(name="Wochentage") +
    scale_y_continuous(limits = hour_limits, breaks = hour_scale, name="Dauer (in Stunden)") +
    ggtitle(title)

  p <- add_theme(p) + coord_cartesian()

  return(p)
}
