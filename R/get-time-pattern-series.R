#' Reshape time pattern series data.
#'
#' @param data_tp data frame including questionnaire_id, kml3d results and time pattern data.
#'
#' @return Reshaped data frame.
#' @export
#'
#' @examples
get_time_pattern_series <- function(data_tp)
{
  # Daten für die Zeitserien aufbereiten
  data_series <- data_tp %>% gather("day_activity", "duration", 3:51) %>%
    separate(day_activity, c("day", "activity"), "_") %>%
    mutate(day = mapvalues(day, c("mo", "di", "mi", "do", "fr", "sa", "so"),
                           c("Montag", "Dienstag", "Mittwoch", "Donnerstag",
                             "Freitag", "Samstag", "Sonntag"))) %>%
    mutate(day = factor(day, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag",
                                        "Freitag", "Samstag", "Sonntag")))

  # Durchschnittsprofile berechnen
  data_series_average <- data_series %>% select(-questionnaire_id) %>%
    group_by(zeitmuster, day, activity) %>%
    summarise(avg_duration = mean(duration, na.rm = TRUE)) %>%
    na.omit() %>% ungroup()

  return(list(data_series, data_series_average))
}
