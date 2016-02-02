#' @include get-time-pattern.R
NULL
#' Plot single or multiple time pattern.
#'
#' @param data data frame which contains time pattern data.
#' @param ncol number of cols, if there are multiple plots (facets).
#' @param id vector which contains questionnaire ids.
#' @param reshape_data whether reshape data or not.
#'
#' @return ggplot2 visualization of time pattern data.
#' @export
#'
#' @examples
plot_time_pattern <- function(data, id = "all", ncol = 3, reshape_data = TRUE) {

    data <- get_time_pattern(data, id, reshape_data)

    if(reshape_data) colours <-  RColorBrewer::brewer.pal(name="Spectral", n = nlevels(data$activity))
    else colours <-  rev(RColorBrewer::brewer.pal(name="Spectral", n = nlevels(data$activity)))

    p <- ggplot(data, aes(x = day, y = prop_duration)) +
      geom_area(aes(fill = activity), position = "fill") +
      geom_vline(xintercept = c(1:7), linetype = "dotted", colour = "white") +
      scale_x_continuous(breaks = c(1:7), labels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")) +
      scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%")) +
      # scale_fill_brewer(name = "Tätigkeiten", labels = c("Veranstaltungen", "Zwischenzeit", "Selbststudium", "Fahrzeit",
      #                                                     "Arbeitszeit", "Freizeit", "Schlafen"), palette = "Spectral") +
      scale_fill_manual(name = "Tätigkeiten", values = colours) +
      theme_bw() +
    # ggtitle(paste('Zeitmuster
    # ID', test$questionnaire_id))
      theme(axis.title = element_blank(), plot.title = element_text(face = "bold", size = 23), text = element_text(family = "Myriad Pro"),
            legend.text = element_text(size = 15), legend.title = element_text(face = "bold", size = 17, family = "Myriad Pro"))

    # Mehrere Gafiken parallel erzeugen
    if(length(id) > 1 | id[[1]] == "all") p <- p + facet_wrap(~questionnaire_id, ncol = ncol)

    return(p)
}
