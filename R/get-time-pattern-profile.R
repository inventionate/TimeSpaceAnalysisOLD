#' @include get-time-pattern-series.R
NULL
#' Reshape time pattern profile data frame.
#'
#' @param data_tp data frame containing questionnaire_id, kml3d results and time pattern data.
#' @param id time pattern number.
#'
#' @return Reshaped data frame.
#' @export
#'
#' @examples
get_time_pattern_profile <- function(data_tp, id = "all")
{
  na_exist <- nrow(data_tp) > nrow(na.omit(data_tp))
  if( na_exist ) warning("There are NAs. They will be omitted!")

  data_ts <- get_time_pattern_series(data_tp)

  # Filter ID
  if (id[[1]] != "all") {data_ts[[2]] <- filter(na.omit(data_ts[[2]]), zeitmuster %in% id) } else {data_ts[[2]] <- na.omit(data_ts[[2]])}

  # Prozentuale Durchschnittswerte berechnen
  data_series_average_prop <- data_ts[[2]] %>%
    mutate(day = ifelse(day == "Mo", 1, ifelse(day == "Di", 2, ifelse(day == "Mi", 3, ifelse(day == "Do", 4, ifelse(day == "Fr", 5, ifelse(day == "Sa", 6, 7))))))) %>%
    mutate(activity = mapvalues(activity,
                                c("Veranstaltungen", "Zwischenzeit", "Selbststudium", "Arbeitszeit", "Fahrzeit", "Freizeit", "Schlafen"),
                                c("Lehrveranstaltung", "Zwischenzeit", "Selbststudium", "Arbeitszeit", "Fahrzeit", "Freizeit", "Schlafen"))) %>%
    mutate(activity = factor(activity, levels = c("Lehrveranstaltung", "Zwischenzeit", "Selbststudium", "Arbeitszeit", "Fahrzeit", "Freizeit", "Schlafen"))) %>%
    group_by(zeitmuster, day) %>%
    mutate(prop_avg_duration = avg_duration / sum(avg_duration)) %>%
    arrange(zeitmuster, day, desc(activity)) %>%
    ungroup() %>%
    mutate(zeitmuster = mapvalues(zeitmuster, 1:length(data_ts[[3]]), data_ts[[3]]))

  return(data_series_average_prop)
}
