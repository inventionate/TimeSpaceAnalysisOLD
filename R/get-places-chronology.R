#' Reshape place chronology data.
#'
#' @param data a data frame, which contains place chronology data.
#' @param id vector, which contains questionnaiere ids.
#' @param weekday vector, which contains a day selection.
#' @param title specify plot title.
#' @param shape_path number, which specifies the path shape intensity.
#'
#' @return reshaped data frame for further visualization.
#' @export
#'
#' @examples
get_places_chronology <- function(data, id = "all", weekday = "all", title, shape_path) {
  
  # Datensatz aufbereiten
  data_places_chronology <- data %>%
    as.data.frame() %>%
    mutate(start_time = hours(start_time)) %>%
    filter(address != "") %>%
    #  Dauer an einem Ort berechnen
    group_by(questionnaire_id, day, place) %>%
    mutate(place_duration = sum(duration))

  # Anzahl der zu plottenden Fragebögen IDs.
  if (id[[1]] != "all") {
    data_places_chronology <- filter(data_places_chronology, questionnaire_id %in% id)
    # Titel anpassen.
    title <- paste(title, as.character(id))
  }


  # Anzahl der zu plottenden Wochentage.
  if (weekday[[1]] != "all") {
    data_places_chronology <- filter(data_places_chronology, day %in% weekday)
    # Titel anpassen.
    title <- paste(title, as.character(weekday))
  }

  # Datensatz mit besuchten Orten und deren Häufigfkeit erstellen.
  data_unique_places_count <- data_places_chronology %>%
    count(place) %>%
    select(n)

  # Datensatz mit besuchten Orten erstellen.
  data_unique_places <- data_places_chronology %>%
    ungroup() %>%
    select(questionnaire_id, place, lon, lat, place_duration) %>%
    distinct()

  # Datensatz mit besuchten Orten erstellen (ohne Berücksichtugung der Dauer).
  data_unique_places_overall <- data_unique_places %>%
    group_by(questionnaire_id, place) %>%
    mutate(place_duration = sum(place_duration)) %>%
    distinct()

  # Pfade hübscher machen, indem eine Kurve berechnet wird.
  plot.new()
  coord_curved_path <- as.data.frame(xspline(data_places_chronology$lon, data_places_chronology$lat, shape = shape_path, draw = FALSE))

  # Daten zurückgeben
  return(list(data_places_chronology = data_places_chronology,
              data_unique_places_count = data_unique_places_count,
              data_unique_places = data_unique_places,
              coord_curved_path = coord_curved_path,
              data_unique_places_overall = data_unique_places_overall,
              title = title))
}
