#' @include utilities.R
#' @include get-time-pattern.R
NULL
#' Plot single or multiple time pattern.
#'
#' @param data data frame which contains time pattern data.
#' @param ncol number of cols, if there are multiple plots (facets).
#' @param id vector which contains questionnaire ids.
#' @param reshape_data whether reshape data or not.
#' @param print_prop_duration whether to print or not to print prop duration data (boolean).
#' @param fluid should be static bars or fluid lines visualized (boolean).
#' @param myriad use Myriad Pro font (boolean).
#' @param labels facet labels.
#' @param legend show legend (boolean).
#' @param facet plot facets (boolean).
#'
#' @return ggplot2 visualization of time pattern data.
#' @export
plot_time_pattern <- function(data, id = "all", ncol = 3, reshape_data = TRUE, print_prop_duration = TRUE,
                              fluid = FALSE, myriad = TRUE, labels = NULL, legend = TRUE, facet = TRUE) {
  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  data <- get_time_pattern(data, id, reshape_data)

    if(reshape_data) {
      # Flexible Kategorienanzahl
      # colours <-  RColorBrewer::brewer.pal(name="Spectral", n = nlevels(data$activity))
      # Fixe sieben Kategorien
      colours <- c("#f15b60", "#ce7058", "#faa75b", "#9e67ab", "#5a9bd4", "#7ac36a", "#737373")
    } else {
      # Flexible Kategorienanzahl
      # colours <-  RColorBrewer::brewer.pal(name="Spectral", n = nlevels(data$activity))
      # Fixe sieben Kategorien
      colours <- c("#f15b60", "#ce7058", "#faa75b", "#9e67ab", "#5a9bd4", "#7ac36a", "#737373")
      # Die Farbe für "Lerngruppen" ändern, da es sich deutlich von "Zwischenzeit" unterscheiden sollte.
      colours[2] <- "#d77fb4"
    }

    if(print_prop_duration) {
      data %>%
        mutate(day = fct_recode(as.factor(day),
                                "Montag" = "1",
                                "Dienstag" = "2",
                                "Mittwoch" = "3",
                                "Donnerstag" = "4",
                                "Freitag" = "5",
                                "Samstag" = "6",
                                "Sonntag" = "7")) %>%
        select(questionnaire_id, day, activity, prop_duration) %>%
        mutate(prop_duration =  round(prop_duration * 100, 2)) %>%
        arrange(questionnaire_id) %>%
        group_by(questionnaire_id, day, activity) %>%
        spread(questionnaire_id, prop_duration)  %>%
        print(n = nrow(.))
    }

    p <- ggplot(data, aes(x = day, y = prop_duration))

    if(fluid) p <- p + geom_area(aes(fill = activity), position = "fill") + geom_vline(xintercept = c(1:7), linetype = "dotted", colour = "white")
    else p <- p + geom_bar(aes(fill = activity), position = "fill", stat = "identity", width = 1) + geom_vline(xintercept = c(1.5:6.5), linetype = "solid", colour = "white", size = 0.75)

    p <- p + scale_x_continuous(breaks = c(1:7), labels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"), name = "Wochentag") +
      scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%"), name = "Zeitanteil in Prozent")
      # scale_fill_brewer(name = "Tätigkeiten", labels = c("Veranstaltungen", "Zwischenzeit", "Selbststudium", "Fahrzeit",
      #                                                     "Arbeitszeit", "Freizeit", "Schlafen"), palette = "Spectral") +

    if(reshape_data) p <- p + scale_fill_manual(name = "Tätigkeiten", values = colours, guide = guide_legend(reverse=FALSE))
    else p <- p + scale_fill_manual(name = "Tätigkeiten", values = colours)

    # Mehrere Gafiken parallel erzeugen
    if( length(id) > 1 | id[[1]] == "all" | is_true(facet) ) p <- p + facet_wrap(~questionnaire_id, ncol = ncol, labeller = as_labeller(labels))

    # Theme
    p <- add_theme(p) + coord_cartesian()

    # Legende
    if( is_true(legend) ) p <- p + theme(legend.title = element_blank(), legend.position = "right")

    return(p)
}
