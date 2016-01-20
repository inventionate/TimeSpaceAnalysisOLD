#' Plot ingle or multiple time pattern.
#'
#' @param data
#' @param ncol
#'
#' @return
#' @export
#'
#' @examples
plot_time_pattern <- function(data, ncol = 3) {

    data <- get_time_pattern(data)

    p <- ggplot(data, aes(x = day, y = prop_duration)) +
      geom_area(aes(fill = activity), position = "fill") +
    scale_x_continuous(breaks = c(1:7), labels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")) +
    geom_vline(xintercept = c(1:7), linetype = "dotted", colour = "white") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%")) +
    scale_fill_brewer(name = "Tätigkeiten", labels = c("Veranstaltungen", "Zwischenzeit", "Selbststudium", "Fahrzeit",
                                                       "Arbeitszeit", "Freizeit", "Schlafen"), palette = "Spectral") +
    theme_bw() +
    # ggtitle(paste('Zeitmuster
    # ID', test$questionnaire_id))
    theme(axis.title = element_blank(), plot.title = element_text(face = "bold", size = 23), text = element_text(family = "Myriad Pro")) +
    facet_wrap(~questionnaire_id, ncol = ncol)
    return(p)
}
