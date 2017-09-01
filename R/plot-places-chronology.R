#' @include utilities.R
#' @include get-places-chronology.R
NULL
#' Plot single or multiple place chronologies in different scales.
#'
#' @param data a data frame (columns: ID, day, duration, place, address, lon, lat, prop_duration).
#' @param id vector, which contains questionnaire ids. Choosa "all" to compute all ids.
#' @param map a ggmap map object.
#' @param weekday vector, which contains the weekday to plot.
#' @param size_range specify the size for visualizatipn of duration.
#' @param colour_path sepcify the path line colour.
#' @param size_path specify the path line size.
#' @param alpha_path specify the path line alpha value [0:1].
#' @param linetype_path specify the linetype of the path line.
#' @param force_repel specify how heavy the repel algorithmn should be.
#' @param legend show or hide legends (boolean).
#' @param structure show or hide map background (boolean).
#' @param map_extent how much of the plot should the map take up? "normal", "device", or "panel" (default).
#' @param xlim specify plot x limits.
#' @param ylim specify plot y limits.
#' @param title title of the plot.
#' @param axis_label show or hide axis labels (boolean).
#' @param graph whether to plot or not to plot the praph (boolean).
#' @param ncol number of cols, if there are multiple plots (facets).
#' @param unique_places plot only unique places. Useful to visualize place related overall structure (boolean).
#' @param activity_duration_overall wheter plot activities per week or for all weeks at once (boolean).
#' @param print_place_duration print place overall duration (hours).
#' @param facet_scales should Scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y").
#' @param point_padding Amount of padding around labeled point. Defaults to unit(0, "lines").
#' @param myriad use Myriad Pro font (boolean).
#' @param exclude_sleep exclude sleep duration (boolean).
#' @param shape if not NULL the path will be curved.
#' @param labels facet labels.
#'
#' @return ggplot2 visualization of place chronology data.
#' @export
plot_places_chronology <- function(data, id = "all", weekday = "all", map = NULL, size_range = c(3,15), colour_path = "darkred", size_path = 2,
                                   alpha_path = 0.75, linetype_path = "solid", force_repel = 3, legend = TRUE, structure = TRUE, map_extent = "panel",
                                   title = NULL, axis_label = TRUE, xlim = NULL, ylim = NULL, graph = TRUE, ncol = 3, unique_places = TRUE,
                                   print_place_duration = TRUE, activity_duration_overall = TRUE, facet_scales = "fixed", point_padding = unit(1, "lines"),
                                   myriad = TRUE, exclude_sleep = TRUE, shape = 0.2, labels = NULL) {
  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Datensatz aufbereiten.
  data_pc <- get_places_chronology(data, id, weekday, title, exclude_sleep)

  if(print_place_duration) {
    data_pc$data_unique_places_overall %>%
      select(questionnaire_id, place, place_duration) %>%
      mutate(place_duration =  round(place_duration, 2)) %>%
      arrange(questionnaire_id) %>%
      group_by(questionnaire_id, place) %>%
      spread(questionnaire_id, place_duration) %>%
      print(n = nrow(.))
  }

  # Labeller
  oc_labeller <- function(variable, value) {

    return(labels[value])

  }

  # Prüfen, ob Struktur oder Karte geplottet werden soll
  if(structure) {
    # Strukturmuster berechnen.
    plot_pc <- ggplot(data_pc$data_places_chronology, aes(x = lon, y = lat))
    # Dichte in Form der Größe des Orts abbilden (einfacher zu verstehen und genauer).
    # geom_density_2d(aes(alpha = 0.2), colour = "black", bins = 15) +
    # stat_density_2d(aes(alpha = ..level..), bins = 15, geom = "polygon")
  } else {
    # ggmap Berechnungen durchführen.
    if(is.null(map)) stop("Please provide an object of class ggmap (from function get_map) as map.")
    plot_pc <- ggmap(map, extent = map_extent) %+%
      data_pc$data_places_chronology + aes(x = lon, y = lat)
  }

  # Grafische Elemente hinzufügen
  if(is.null(shape))
  {
    plot_pc <- plot_pc + geom_path(colour = colour_path, size = size_path, alpha = alpha_path, linetype = linetype_path)
  } else
  {
    plot.new()
    curved_data <- NULL
    for(i in seq_along(id)) {
      tmp_data <- data_pc$data_places_chronology %>% filter(questionnaire_id == id[i])

      if( nrow(tmp_data) == 0) stop("Mindestens eine ID wurde nicht gefunden")

      spline_data <- data.frame(xspline(tmp_data$lon, tmp_data$lat, shape=shape, open = FALSE, draw = FALSE))

      questionnaire_id <- rep(id[i], nrow(spline_data))

      spline_data <- data.frame(questionnaire_id, spline_data) %>% mutate(questionnaire_id = as.character(questionnaire_id))

      curved_data <- bind_rows(curved_data, spline_data)
    }
    invisible(dev.off())

    plot_pc <- plot_pc + geom_path(data=curved_data, inherit.aes = FALSE, aes(x, y), colour = colour_path, size = size_path, alpha = alpha_path, linetype = linetype_path)
  }

  if(unique_places) {
    plot_pc <- plot_pc +
      geom_point(data = data_pc$data_unique_places_overall, size = 5, colour = colour_path, alpha = 0.75) +
      geom_label_repel(data = data_pc$data_unique_places_overall, aes(label = place, size = place_duration), colour = "white", fontface = "bold", fill = "black", show.legend = TRUE, segment.colour = "black")

    # Verdopplung der breaks nötig, weil zwei Wochen beobachtet wurden.
    plot_pc <- plot_pc + scale_size(range = size_range, name = "Dauer", labels = function(x) paste0(x, "h"))

  } else {
    plot_pc <- plot_pc +
      geom_label(data = data_pc$data_unique_places, aes(label = place, size = place_duration), colour = "white", fontface = "bold", fill = "black", show.legend = TRUE)

    if(!activity_duration_overall) {
      plot_pc <- plot_pc +
        geom_label_repel(aes(fill = start_time, size = duration), colour = "white", fontface = "bold", show.legend = FALSE, alpha = 0.85, force = force_repel, point.padding = point_padding, segment.colour = "black")
    } else {
      plot_pc <- plot_pc +
        geom_label_repel(data = data_pc$data_unique_activities, aes(label = activity, fill = start_time_average, size = activity_duration_overall), colour = "white", fontface = "bold", show.legend = FALSE, alpha = 0.85, force = force_repel, point.padding = point_padding)
    }

    plot_pc <- plot_pc +
      scale_size(breaks = c(0, 8, 16, 24, 32, 40, 48), labels = c("0h", "8h", "16h", "24h", "32h", "40h", "48h"), range = size_range, name = "Dauer") +
      scale_fill_gradient2(low = "#7f7f7f", mid = "#ffd932", high = "#d13131", name = "Startzeit", midpoint = 12,
                           breaks = c(0, 6, 12, 18, 24), labels = c("0 Uhr", "6 Uhr", "12 Uhr", "18 Uhr", "24 Uhr"))

  }

  plot_pc <- plot_pc + scale_alpha_continuous(range = c(0.2, 0.3), guide = FALSE) + ggtitle(data_pc$title)

  # Theme
  plot_pc <- add_theme(plot_pc) + xlab("Längengrad") + ylab("Breitengrad")

  if(!axis_label) plot_pc <- plot_pc + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

  if(length(id) > 1 | id[[1]] == "all") plot_pc <- plot_pc + facet_wrap(~questionnaire_id, ncol = ncol, scales = facet_scales, labeller = as_labeller(labels))

  if(!is.null(xlim)) plot_pc <- plot_pc + scale_x_continuous(limits = xlim)

  if(!is.null(ylim)) plot_pc <- plot_pc + scale_y_continuous(limits = ylim)

  if(legend) plot_pc <- plot_pc + theme(legend.position = "right", legend.title = element_blank())

  # Plotten
  if(graph) print(plot_pc)

  return(plot_pc)
}
