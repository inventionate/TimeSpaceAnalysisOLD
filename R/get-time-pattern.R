#' Reshape time pattern data.
#'
#' @param data data frame which contains time pattern data.
#' @param id vector which contains questionnaire ids.
#' @param reshape_data whether reshape data or not.
#' Use this option if your data is column wise concentration (e. g. "mo_seminar")
#'
#' @return reshaped data frame for further visualization.
#' @export
#'
#' @examples
get_time_pattern <- function(data, id = "all", reshape_data = TRUE) {

  # Check NA
  na_exist <- nrow(data) > nrow(na.omit(data))
  if( na_exist ) warning("There are NAs. They will be omitted!")

  # Filter ID
  if (id[[1]] != "all") data_tp <- filter(na.omit(data), questionnaire_id %in% id)
  else data_tp <- na.omit(data)

  if(reshape_data) {
    data_time_pattern <- data_tp %>%
      gather(activity, duration, mo_veranstaltungen:so_schlafen, -questionnaire_id) %>%
      separate(activity, c("day", "activity"), sep ="_") %>%
      mutate(day = ifelse(day == "mo", 1, ifelse(day == "di", 2, ifelse(day == "mi", 3, ifelse(day == "do", 4, ifelse(day == "fr", 5, ifelse(day == "sa", 6, 7))))))) %>%
      mutate(activity = mapvalues(activity,
                                  c("veranstaltungen", "zwischenzeit", "selbststudium", "arbeitszeit", "fahrzeit", "freizeit", "schlafen"),
                                  c("Lehrveranstaltung", "Zwischenzeit", "Selbststudium", "Arbeitszeit", "Fahrzeit", "Freizeit", "Schlafen"))) %>%
      mutate(activity = factor(activity, levels = c("Lehrveranstaltung", "Zwischenzeit", "Selbststudium", "Arbeitszeit", "Fahrzeit", "Freizeit", "Schlafen"))) %>%  
      group_by(questionnaire_id, day) %>%
      mutate(prop_duration = duration / sum(duration)) %>%
      arrange(questionnaire_id, day, desc(activity)) %>%
      ungroup()
  } else {
    data_time_pattern <- data_tp %>%
      ungroup() %>%
      mutate(day = ifelse(day == "Montag", 1, ifelse(day == "Dienstag", 2, ifelse(day == "Mittwoch", 3, ifelse(day == "Donnerstag", 4, ifelse(day == "Freitag", 5, ifelse(day == "Samstag", 6, 7)))))))
  }
  
  return(data_time_pattern)
}
