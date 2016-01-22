#' @include get-places-chronology.R
NULL
#' Plot single or multiple places chronologies in different scales.
#'
#' @param data
#' @param id
#' @param map
#' @param weekday
#' @param timepattern
#' @param distinct_places
#' @param timepattern_graph
#' @param print_prop_duration
#' @param legends
#' @param structure
#'
#' @return
#' @export
#'
#' @examples
plot_places_chronology <- function(data, id = "all", map, weekday = "all", timepattern = TRUE, distinct_places = FALSE,
    # timepattern_graph = FALSE,
    print_prop_duration = FALSE, legends = FALSE, structure = FALSE, title = "Orte Chronologie") {
    plot_title <- title

    # Datensatz Orte Chronologien
    # aufbereiten
    data_pc <- data %>% mutate(lat = jitter(lat,
        amount = 0.0017)) %>% mutate(lon = jitter(lon)) %>%
        mutate(start_time = hours(start_time)) %>%
        filter(place != "")

    # Falls nur distinkte Orte geplottet
    # werden sollen.  Das ist dann der Fall,
    # wenn man keine Tätigkeitswechsel an
    # einem Ort abbilden will.
    if (distinct_places) {
        data_pc <- data_pc %>% distinct(place)
    }

    # Anzahl der zu plottenden Fragebögen
    # IDs
    if (id != "all") {
        data_pc <- filter(data_pc, questionnaire_id ==
            id)
        plot_title <- paste(plot_title,
            as.character(id))
    }

    # Anzahl der zu plottenden Wochentage
    if (weekday != "all") {
        data_pc <- filter(data_pc, day ==
            weekday)
        plot_title <- paste(plot_title,
            as.character(weekday))
    }

    # ggmap Berechnungen durchführen
    map_pc <- ggmap(map, extent = "device") %+%
        data_pc + aes(x = lon, y = lat)

    if (structure) {
        # Strukturmuster plotten
        plot_pc <- ggplot(data_pc, aes(x = lon,
            y = lat))
    } else {
        # ggmap Karte gestalten
        plot_pc <- map_pc + geom_density2d(aes(alpha = 0.2),
            colour = "black", size = 1,
            bins = 7, h = 0.028) + stat_density2d(aes(fill = ..level..,
            alpha = ..level..), bins = 7,
            geom = "polygon", h = 0.028)
    }

    # Grafische Elemente hinzufügen
    plot_pc <- plot_pc + geom_path(size = 4,
        alpha = 0.5, colour = "darkred") +
        geom_point(aes(colour = start_time),
            alpha = 0.75, size = 10) + scale_colour_gradient(low = "yellow",
        high = "red", name = "Startzeit (Uhrzeit):",
        labels = c("0", "5", "10", "15",
            "20")) + theme(plot.title = element_text(lineheight = 1,
        face = "bold", size = 25), legend.position = "bottom",
        legend.text = element_text(size = 15),
        legend.title = element_text(face = "bold",
            size = 17, family = "Myriad Pro"),
        panel.margin = unit(c(0, 0, -0,
            0), "cm"), plot.margin = unit(c(0,
            0, -0, 0), "cm"), text = element_text(family = "Myriad Pro")) +
        scale_fill_gradient(low = "#333333",
            high = "#222222", guide = FALSE) +
        scale_alpha_continuous(range = c(0.2,
            0.3), guide = FALSE) + ggtitle(plot_title)

    # Legende der Karte extrahieren
    if (legends) {
        print("Achtung! Sie extrahieren die Liste der Legenden. Es wird kein Plot erzeugt!")
        tmp <- ggplot_gtable(ggplot_build(plot_pc))
        leg <- which(sapply(tmp$grobs, function(x) x$name) ==
            "guide-box")
        plot_pc_legend <- tmp$grobs[[leg]]
    }

    # Legende löschen
    plot_pc <- plot_pc + theme(legend.position = "none")

    # Zeitmuster zu den geografischen Orte
    # Chronologien hinzufügen
    if (timepattern) {
        # Dimension der ggmap Karte berechnen
        xmin <- attr(map, "bb")$ll.lon
        xmax <- attr(map, "bb")$ur.lon
        ymin <- attr(map, "bb")$ll.lat
        ymax <- attr(map, "bb")$ur.lat

        # Datensatz Zeitmuster
        data_pc_zm <- data %>% # Berechnung der Dauer für eine
        # durchschnittliche Woche (nicht pro
        # Woche)
        group_by(questionnaire_id, day) %>%
            mutate(prop_duration = duration/sum(duration)) %>%
            ungroup() %>% group_by(day,
            activity) %>% mutate(prop_duration = sum(prop_duration)) %>%
            distinct() %>% arrange(questionnaire_id,
            date, desc(activity))

        # Tagesauswahl definieren
        if (weekday != "all") {
            data_pc_zm <- filter(data_pc_zm,
                day == weekday)

            # Prozentuale Verteilung der Aktivitäten
            if (print_prop_duration)
                print(data_pc_zm[c("activity",
                  "prop_duration")])

            # Plotten der Teitmuster
            plot_pc_zm <- ggplot(data_pc_zm,
                aes(x = day, y = prop_duration)) +
                geom_bar(aes(fill = activity),
                  stat = "identity", position = "stack",
                  guide = FALSE, width = 0.2) +
                scale_y_continuous(breaks = c(0,
                  0.25, 0.5, 0.75, 1), labels = c("0%",
                  "25%", "50%", "75%", "100%")) +
                coord_cartesian(xlim = c(1,
                  1.1), ylim = c(0, 1)) +
                scale_fill_brewer(name = "Tätigkeiten:",
                  labels = c("Veranstaltungen",
                    "Zwischenzeit", "Selbststudium",
                    "Fahrzeit", "Arbeitszeit",
                    "Freizeit", "Schlafen"),
                  palette = "Spectral") +
                theme_bw() + theme(axis.title = element_blank(),
                plot.title = element_text(face = "bold",
                  size = 23, family = "Myriad Pro"),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(size = 15),
                legend.title = element_text(face = "bold",
                  size = 17), panel.margin = unit(c(0,
                  0, -0, 0), "cm"), plot.margin = unit(c(0,
                  0, -0, 0), "cm"), text = element_text(family = "Myriad Pro"))

            # Legende der Zeitmuster extrahieren
            if (legends) {
                tmp <- ggplot_gtable(ggplot_build(plot_pc_zm))
                leg <- which(sapply(tmp$grobs,
                  function(x) x$name) ==
                  "guide-box")
                plot_pc_zm_legend <- tmp$grobs[[leg]]
            }

            # Legende löschen
            plot_pc_zm <- plot_pc_zm + theme(legend.position = "none")

            # TableGrob für die Zeitmuster erzeugen
            grob_pc_zm = ggplotGrob(plot_pc_zm)

            # Zeitmuster zu ggmap Karte hinzufügen
            plot_pc <- plot_pc + inset(grob = grob_pc_zm,
                xmin = 8.32, xmax = 8.349,
                ymin = ymin - 0.002, ymax = ymax -
                  0.002)
        } else {
            print("Bitte plotten Sie ein alle Tage umfassendes Zeituster separat, indem Sie die Option timepattern_graph = TRUE benutzen.")
        }
    }

    # # Allgemeine Übersicht über die
    # # Zeitmuster
    # if (timepattern_graph) {
    #     # Datensatz für den eines allgemeinen
    #     # Zeitprofils (identisch zu den
    #     # Fragebogenmustern) aufebreiten.  Es
    #     # wird ein Datensatz mit day (num),
    #     # activity (fac) und prop_duration (num)
    #     # benötigt.
    #     data_pc_zm_sum <- data_pc_zm[c("questionnaire_id",
    #         "date", "prop_duration", "activity")] %>%
    #         # Tagesformat anpassen
    #     mutate(day = as.numeric(mapvalues(date,
    #         c("Montag", "Dienstag", "Mittwoch",
    #             "Donnerstag", "Freitag",
    #             "Samstag", "Sonnatg"), c(1,
    #             2, 3, 4, 5, 6, 7), warn_missing = FALSE))) %>%
    #         arrange(questionnaire_id, day,
    #             desc(activity))
    #
    #     # todo Die momentane Lösung ermöglicht
    #     # eine schnelle Übersicht über die
    #     # Zeitprofile, ähnlich der facet_wrap
    #     # Variante für alle chorischen
    #     # Wegeprofile. Eventuell kann das
    #     # ausgebaut werden.
    #     plot_pc <- ggtimepattern(data_pc_zm_sum)
    # }

    # Legenden Zurückgeben
    if (legends)
        return(list(plot_pc_legend, plot_pc_zm_legend))

    # plotten
    return(plot_pc)

    # Funktion ausgeben return(list(plot_pc,
    # legend))

    # PDF export print(plot_pc dev.off()
}
