#' @include get-places-chronology.R
NULL
#' Plot single or multiple place chronologies in different scales.
#'
#' @param data a data frame (columns: ID, day, duration, place, address, lon, lat, prop_duration).
#' @param id vector, which contains questionnaire ids. Choosa "all" to compute all ids.
#' @param map a ggmap map object.
#' @param weekday vactor, which contains the weekday to plot.
#' @param size_range specify the size for visualizatipn of duration.
#' @param shape_path specify the shape of the path line. Higher values result in smoother paths.
#' @param colour_path sepcify the path line colour.
#' @param size_path specify the path line size.
#' @param alpha_path specify the path line alpha value [0:1].
#' @param linetype_path specify the linetype of the path line.
#' @param force_repel specify how heavy the repel algorithmn should be.
#' @param legend show or hide legends (boolean).
#' @param structure show or hide map background (boolean).
#' @param map_extent how much of the plot should the map take up? "normal", "device", or "panel" (default).
#' @param xlim specify plot x limits.
#' @param ylim spcify plot y limits.
#' @param title title of the plot.
#' @param axis_label show or hide axis labels (boolean).
#' @param graph whether to plot or not to plot the praph (boolean).
#' @param ncol number of cols, if there are multiple plots (facets).
#' @param unique_places plot only unique places. Useful to visualize place related overall structure.
#'
#' @return ggplot2 visualization of place chronology data.
#' @export
#'
#' @examples
plot_places_chronology <- function(data, id = "all", weekday = "all", map = NULL, size_range = c(3,15), shape_path = 1, colour_path = "black", size_path = 2,
                                   alpha_path = 0.75, linetype_path = "solid", force_repel = 1, legend = FALSE, structure = FALSE, map_extent = "panel",
                                   title = "Orte Chronologie", axis_label = FALSE, xlim = NULL, ylim = NULL, graph = TRUE, ncol = 3, unique_places = FALSE) {

  # Datensatz aufbereiten.
  data_pc <- get_places_chronology(data, id, weekday, title, shape_path)

  # Prüfen, ob Struktur oder Karte geplottet werden soll
  if (structure) {
    # Strukturmuster berechnen.
    plot_pc <- ggplot2::ggplot(data_pc$data_places_chronology, aes(x = lon, y = lat, label = activity)) #+
    # Dichte in Form der Größe des Orts abbilden (einfacher zu verstehen und genauer).
    # geom_density_2d(aes(alpha = 0.2), colour = "black", bins = 15) +
    # stat_density_2d(aes(alpha = ..level..), bins = 15, geom = "polygon")
  } else {
    # ggmap Berechnungen durchführen.
    if(is.null(map)) stop("Please provide an object of class ggmap (from function get_map) as map.")
    plot_pc <- ggmap::ggmap(map, extent = map_extent) %+%
      data_pc$data_places_chronology + aes(x = lon, y = lat, label = activity)
  }

  # Grafische Elemente hinzufügen
  if(id[[1]] == "all") {
    plot_pc <- plot_pc +
      geom_path(data = data_pc$data_places_chronology, aes(label = NULL), colour = colour_path, size = size_path, alpha = alpha_path, linetype = linetype_path)
  } else {
    plot_pc <- plot_pc +
      geom_path(data = data_pc$coord_curved_path, aes(x = x, y = y, label = NULL), colour = colour_path, size = size_path, alpha = alpha_path, linetype = linetype_path)
  }
  if(unique_places) {
    plot_pc <- plot_pc +
      geom_point(data = data_pc$data_unique_places_overall, aes(label = NULL), size = 5, colour = colour_path, alpha = 0.75) +
      ggrepel::geom_label_repel(data = data_pc$data_unique_places_overall, aes(label = place, size = place_duration), colour = "white", fontface = "bold", fill = "black", show.legend = TRUE)
  } else {
    plot_pc <- plot_pc +
      geom_label(data = data_pc$data_unique_places, aes(label = place, size = place_duration), colour = "white", fontface = "bold", fill = "black", show.legend = TRUE)
  }
  if(!unique_places) {
    plot_pc <- plot_pc +
      ggrepel::geom_label_repel(aes(fill = start_time, size = duration), colour = "white", fontface = "bold", show.legend = FALSE, alpha = 0.85, force = force_repel)
  }
  # Verdopplung der breaks nötig, weil zwei Wochen beobachtet wurden.
  if(unique_places) {
    plot_pc <- plot_pc + scale_size(range = size_range, name = "Dauer", labels = function(x) paste0(x, "h"))
  }
  else {
    plot_pc <- plot_pc + scale_size(breaks = c(0, 8, 16, 24, 32, 40, 48), labels = c("0h", "8h", "16h", "24h", "32h", "40h", "48h"), range = size_range, name = "Dauer") +
      scale_fill_gradient2(low = "#7f7f7f", mid = "#ffd932", high = "#d13131", name = "Startzeit", midpoint = 12,
                           breaks = c(0, 6, 12, 18, 24), labels = c("0 Uhr", "6 Uhr", "12 Uhr", "18 Uhr", "24 Uhr"))
  }
  plot_pc <- plot_pc + theme_bw() +
    theme(plot.title = element_text(lineheight = 1, face = "bold", size = 25), legend.position = "bottom", text = element_text(family = "Myriad Pro"),
          legend.text = element_text(size = 10), legend.title = element_text(face = "bold", size = 12, family = "Myriad Pro"), legend.box = "horizontal") +
    scale_alpha_continuous(range = c(0.2, 0.3), guide = FALSE) +
    ggtitle(data_pc$plot_title)

  if(!legend) plot_pc <- plot_pc + theme(legend.position = "none")

  if(!axis_label) plot_pc <- plot_pc + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

  if(!is.null(xlim)) plot_pc <- plot_pc + scale_x_continuous(limits = xlim)

  if(!is.null(ylim)) plot_pc <- plot_pc + scale_y_continuous(limits = ylim)

  if(length(id) > 1 | id[[1]] == "all") plot_pc <- plot_pc + facet_wrap(~questionnaire_id, ncol = ncol, scales = "free")

  # Plotten
  if(graph) print(plot_pc)

  return(plot_pc)
}
