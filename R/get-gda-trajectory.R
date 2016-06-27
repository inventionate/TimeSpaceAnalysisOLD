#' @include utilities.R
#' @include add-theme.R
NULL
#' Extract trajectory data.
#'
#' @param res_gda GDA result.
#' @param time_point_names name of the separated time points.
#'
#' @return list containing time point separated ind coord and time point names.
#' @export
get_gda_trajectory <- function(res_gda, time_point_names = NULL) {

  # Anzahl Zeitpunkte bestimmen
  time_points <- res_gda$ind.sup$coord %>% data.frame %>% tibble::rownames_to_column() %>%
    separate(rowname, c("id", "time")) %>% select(time) %>% distinct %>% .$time %>% length

  if(length(time_points) == 0) stop("There are no different time points!")

  if(is.null(time_point_names)) time_point_names <- paste(rep("Zeitpunkt", 1 + time_points), 1:2)

  # Basisdatensatz konstruieren

  # Hauptkoordinaten (Zeitpunkt 1)
  coord_main <- res_gda$ind$coord %>% data.frame %>% tibble::rownames_to_column("id") %>% mutate(time = time_point_names[1])

  # Zusätzliche Koordinaten (Zeitpunkte n)
  coord_all <- bind_rows(coord_main, res_gda$ind.sup$coord %>% data.frame %>% tibble::rownames_to_column() %>%
                           separate(rowname, c("id", "time")) %>%
                           mutate(time = mapvalues(time,
                                                   1:(length(time_point_names) - 1),
                                                   time_point_names[2:length(time_point_names)]))) %>%
    mutate(time = factor(time, levels = time_point_names))

  # # Mittelpunkte
  # coord_mean <- coord_all %>% select(-id) %>% group_by(time) %>% summarise_each(funs(mean))
  #
  # # Massen der Individuenmittelpunkte
  # coord_mass <- coord_all %>% select(-id) %>% count(time) %>% rename(mass = n)
  #
  # # Mittelpunkte und Massen zusammenführen
  # coord_mean_mass <- full_join(coord_mean, coord_mass)

  # Zusammenstellung der Ergebnisse
  res <- list(coord_all = coord_all, time_point_names = time_point_names)

  # Ausgabe der Ergebnisse
  return(res)

}
