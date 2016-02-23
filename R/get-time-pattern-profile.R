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

  # Filter ID
  if (id[[1]] != "all") {data_tp <- filter(na.omit(data_tp), zeitmuster %in% id) } else {data_tp <- na.omit(data_tp)}

  data_ts <- get_time_pattern_series(data_tp)

  # Prozentuale Durchschnittswerte berechnen
  data_series_average_prop <- data_ts[[2]] %>%
    mutate(day = ifelse(day == "Montag", 1, ifelse(day == "Dienstag", 2, ifelse(day == "Mittwoch", 3, ifelse(day == "Donnerstag", 4, ifelse(day == "Freitag", 5, ifelse(day == "Samstag", 6, 7))))))) %>%
    mutate(activity = factor(activity, levels = c("veranstaltungen", "zwischenzeit", "selbststudium", "fahrzeit", "arbeitszeit", "freizeit", "schlafen"))) %>%
    group_by(zeitmuster, day) %>%
    mutate(prop_avg_duration = avg_duration / sum(avg_duration)) %>%
    arrange(zeitmuster, day, desc(activity)) %>%
    ungroup()

  return(data_series_average_prop)
}
