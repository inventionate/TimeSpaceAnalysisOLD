#' @include get-time-pattern-series.R
NULL
#' Plot time pattern series data.
#'
#' @param data_tp data frame including questionnaire_id, kml3d results and time pattern data.
#'
#' @return ggplot2 time pattern series plot.
#' @export
#'
#' @examples
plot_time_pattern_series <- function(data_tp)
{
  data_ts <- get_time_pattern_series(data_tp)

  # Zeitserien plotten
  p <- ggplot(data_ts[[1]], aes(x = day, y = duration, group = questionnaire_id)) +
    geom_line() + facet_wrap(~activity) +
    geom_line(data = data_ts[[2]], aes(x = day, y = avg_duration,
                                       group = zeitmuster, colour = zeitmuster),
              inherit.aes = FALSE, size = 2) + theme_minimal()
  return(p)
}
