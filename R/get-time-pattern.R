#' Reshape time pattern data.
#'
#' @param data
#'
#' @return reshaped data frame for further visualization.
#' @export
#'
#' @examples
get_time_pattern <- function(data) {
  # Check NA
  na_exist <- nrow(data) > nrow(na.omit(data))
  if( na_exist ) warning("There are NAs. They will be omitted!")
  data_time_pattern <- na.omit(data) %>%
    gather(activity, duration, mo_veranstaltungen:so_schlafen, -questionnaire_id) %>%
    separate(activity, c("day", "activity"), sep ="_") %>%
    mutate(day = ifelse(day == "mo", 1, ifelse(day == "di", 2, ifelse(day == "mi", 3, ifelse(day == "do", 4, ifelse(day == "fr", 5, ifelse(day == "sa", 6, 7))))))) %>%
    mutate(activity = factor(activity, levels = c("veranstaltungen", "zwischenzeit", "selbststudium", "fahrzeit", "arbeitszeit", "freizeit", "schlafen"))) %>%
    group_by(questionnaire_id, day) %>%
    mutate(prop_duration = duration / sum(duration)) %>%
    arrange(questionnaire_id, day, desc(activity))
  return(data_time_pattern)
}
