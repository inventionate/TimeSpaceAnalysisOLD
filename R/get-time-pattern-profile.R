#' @include get-time-pattern-series.R
NULL
#' Reshape time pattern profile data frame.
#'
#' @param data_tp data frame containing questionnaire_id, kml3d results and time pattern data.
#' @param id time pattern number.
#'
#' @return Reshaped data frame.
#' @export
get_time_pattern_profile <- function(data_tp, id = "all")
{
  na_exist <- nrow(data_tp) > nrow(na.omit(data_tp))
  if( na_exist ) warning("There are NAs. They will be omitted!")

  data_ts <- get_time_pattern_series(data_tp)

  # Filter ID
  if (id[[1]] != "all") {data_ts[[2]] <- filter(na.omit(data_ts[[2]]), zeitmuster %in% id) } else {data_ts[[2]] <- na.omit(data_ts[[2]])}

  # Prozentuale Durchschnittswerte berechnen
  data_series_average_prop <- data_ts[[2]] %>%
    mutate(day = if_else(day == "Mo", 1, if_else(day == "Di", 2, if_else(day == "Mi", 3, if_else(day == "Do", 4, if_else(day == "Fr", 5, if_else(day == "Sa", 6, 7))))))) %>%
    mutate(activity = fct_recode(
      activity,
      "Private Zeit" = "Freizeit")) %>%
    mutate(activity = fct_relevel(activity, "Lehrveranstaltungen", "Zwischenzeit", "Selbststudium", "Arbeitszeit", "Fahrzeit", "Private Zeit", "Schlafen")) %>%
    group_by(zeitmuster, day) %>%
    mutate(prop_avg_duration = avg_duration / sum(avg_duration)) %>%
    arrange(zeitmuster, day, desc(activity)) %>%
    ungroup() %>%
    right_join(., data_ts$data_series_profile_prop_label, by = "zeitmuster") %>%
    unite(zeitmuster, zeitmuster, prop, sep = " ")

  return(data_series_average_prop)
}
