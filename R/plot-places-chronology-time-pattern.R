#' @include utilities.R
#' @include get-places-chronology-time-pattern.R
NULL
#' Plot place chronology time pattern data.
#'
#' @param data data frame, which contains place chronology time pattern data.
#' @param id vector, which contains questionnaire ids. Use "all" if you want to plot all ids.
#' @param weekday weekday vector, which contains days to plot.
#' @param graph whether to plot or not to plot the praph (boolean)
#' @param print_prop_duration whether to print or not to print prop duration data.
#' @param legend show or hide legends (boolean).
#' @param bar_width specify the width of the bars.
#' @param ncol number of cols, if there are multiple plots (facets).
#' @param myriad use Myriad Pro font (boolean).
#'
#' @return ggplot2 visualization of place chronology time pattern data.
#' @export
plot_places_chronology_time_pattern <- function(data, id = "all", weekday = "all", graph = TRUE, print_prop_duration = TRUE, legend = TRUE,
                                                bar_width = 1, ncol = 3, myriad = TRUE) {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

    # Datensatz Zeitmuster
    data_pc_zm <- get_places_chronology_time_pattern(data, id, weekday)

    # Tagesauswahl definieren
    if (weekday[[1]] != "all") {
      # Farbpalette festlegen
      colours <- RColorBrewer::brewer.pal(name="Spectral", n = nlevels(data_pc_zm$activity))
      # Die Farbe für "Lerngruppe" ändern, damit es sich von "Zwischenzeit" unterscheidet.
      colours[6] <- "#FD5A9D"

      # Prozentuale Verteilung der Aktivitäten
      if(print_prop_duration) {
        data_pc_zm %>%
          select(questionnaire_id, activity, prop_duration) %>%
          mutate(prop_duration =  round(prop_duration * 100, 2)) %>%
          arrange(questionnaire_id) %>%
          group_by(questionnaire_id, activity) %>%
          spread(questionnaire_id, prop_duration) %>%
          as_tibble() %>%
          print(n = nrow(.))
      }

      # Plotten der Zeitmuster
      plot_pc_zm <- ggplot2::ggplot(data_pc_zm, aes(x = day, y = prop_duration)) +
        geom_bar(aes(fill = activity), stat = "identity", position = "stack", width = bar_width) +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%")) +
        #scale_fill_brewer(name = "Tätigkeiten:",
        #                 labels = c("Veranstaltungen", "Zwischenzeit", "Selbststudium", "Fahrzeit", "Arbeitszeit", "Freizeit", "Schlafen"),
        #                palette = "Spectral") +
        scale_fill_manual(name = "Tätigkeiten", values = colours, guide = guide_legend(reverse=TRUE))

      if(!legend) plot_pc_zm <- plot_pc_zm + theme(legend.position = "none")

      if(length(id) > 1 | id[[1]] == "all") plot_pc_zm <- plot_pc_zm + facet_wrap(~questionnaire_id, ncol = ncol)

    } else {
      # Allgemeines Zeitmuster plotten.
      plot_pc_zm <- plot_time_pattern(data_pc_zm, id = id, reshape_data = FALSE, ncol = ncol, print_prop_duration = print_prop_duration)
    }

    # Theme
    plot_pc_zm <- add_theme(plot_pc_zm)

  if(graph) print(plot_pc_zm)

  return(plot_pc_zm)
}
