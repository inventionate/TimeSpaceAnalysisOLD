#' @include add-theme.R
NULL
#' Visualization of trajectories (connected MFA active/passive individual points).
#'
#' @param res_gda MFA result (rownames have to be questionnaire IDs including time number, e.g. 87654_1).
#' @param select vector of names or within_inertia of individuals selection (within_inertia: vector containing the number of high variation and low variationindividuals).
#' @param ellipse_level the ellipse level. Default: 86.47\%.
#' @param ellipse_alpha opacity value.
#' @param axes axes to plot.
#' @param myriad use Myriad Pro font (boolean).
#' @param ellipses plot concentration ellipses (boolean).
#' @param facet plot ellipses/ individuals per time (boolean).
#' @param mean_path plot mean path (boolean). If yes, no ellipses and no facets are plotted.
#' @param clust HCPC result of primary MFA.
#' @param time_points vector containing names of time points (not yet implemented!).
#'
#' @return HMFA trajectory ggplot2 visualization.
#' @export
fviz_gda_mfa_trajectory <- function(res_gda, clust, select = list(name = NULL, within_inertia = NULL), ellipse_level = 0.8647,
                                    ellipse_alpha = 0.1, axes = 1:2, myriad = TRUE, time_points = NULL, ellipses = FALSE,
                                    facet = FALSE, mean_path = FALSE) {

  # @todo: Umgang mit teilweise kompletten Fällen!!!

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Fälle zusammenstellen pro Semester
  ws1516 <- res_gda$ind$coord %>% data.frame %>% add_rownames %>% separate(rowname, c("id", "time")) %>%
    mutate(time = "Wintersemester 15/16")
  ss16 <- res_gda$ind.sup$coord %>% data.frame %>% add_rownames %>% separate(rowname, c("id", "time")) %>%
    filter(time == 2) %>% mutate(time = "Sommersemester 16")
  ws1617 <- res_gda$ind.sup$coord %>% data.frame %>% add_rownames %>% separate(rowname, c("id", "time")) %>%
    filter(time == 3) %>% mutate(time = "Wintersemester 16/17")

  # Koordinaten der Individuen pro Semester
  # @todo: Hier ist das Zuordnungsproblem! Es muss so programmiert werden, dass es ausreicht eine Zuweiseung für das 1. WS zu machen,
  # die dann auf die anderen Semester übertragen wird.
  ws_coord_ind_1516 <- data.frame(ws1516, clust = clust$data.clust$clust)
  ss16_id <- data.frame(ss16)$id
  ss_coord_ind_16 <- data.frame(ss16, clust = data.frame(clust$data.clust %>% add_rownames %>% filter(rowname %in% ss16_id))$clust)
  ws1617_id <- data.frame(ws1617)$id
  ws_coord_ind_1617 <- data.frame(ws1617, clust = data.frame(clust$data.clust %>% add_rownames %>% filter(rowname %in% ws1617_id))$clust)

  # Koordinaten der Ellipsenmittelpunkte pro Semester und Cluster

  ws_coord_quali_1516 <- ws_coord_ind_1516 %>% select(-id) %>%
    unite(clust_time, clust, time) %>% group_by(clust_time) %>%
    summarise_each(funs(mean))
  ss_coord_quali_16 <- ss_coord_ind_16 %>% select(-id) %>%
    unite(clust_time, clust, time) %>% group_by(clust_time) %>%
    summarise_each(funs(mean))
  ws_coord_quali_1617 <- ws_coord_ind_1617 %>% select(-id) %>%
    unite(clust_time, clust, time) %>% group_by(clust_time) %>%
    summarise_each(funs(mean))

  # Selection (es wird select_ind definiert)
  selected_ind <- res_gda$ind$coord %>% data.frame %>% add_rownames %>% separate(rowname, c("id", "time"))

  if(!is.null(select$name))
  {
    selected_ind <- res_gda$ind %>% data.frame %>% add_rownames %>% separate(rowname, c("id", "time")) %>%
      filter(id %in% select$name)
  }
  if(!is.null(select$within_inertia))
  {
    # Mittelwerte aller Individuen berechnen
    ind_mean_coord <- rbind(ws1516 %>% filter(id %in% ss16_id & id %in% ws1617_id),
                            ss16 %>% filter(id %in% ws1617_id), ws1617) %>%
      select(-time) %>% group_by(id) %>% summarise_each(funs(mean))
    ind_mean_coord_id <- data.frame(ind_mean_coord)$id

    # "within inertia" berechnen (adaptiert von FactoMineR)
    tmp <- array(0, dim = c(dim(ind_mean_coord %>% select(-id)), 3))
    tmp[,,1] <- (ws1516 %>% filter(id %in% ind_mean_coord_id) %>% select(-id, -time) - ind_mean_coord %>% select(-id))^2 / 3
    tmp[,,2] <- (ss16 %>% filter(id %in% ind_mean_coord_id) %>% select(-id, -time) - ind_mean_coord %>% select(-id))^2 / 3
    tmp[,,3] <- (ws1617 %>% filter(id %in% ind_mean_coord_id) %>% select(-id, -time) - ind_mean_coord %>% select(-id))^2 / 3
    variab.auxil <- apply(tmp,2,sum)
    tmp <- sweep(tmp,2,variab.auxil,FUN="/") * 100
    inertie.intra.ind <- apply(tmp,c(1,2),sum)
    rownames(inertie.intra.ind) <- ind_mean_coord_id
    colnames(inertie.intra.ind) <- colnames(ws1516)[-c(1:2)]
    ind_within_inertia <- inertie.intra.ind

    if(select$within_inertia[[1]] == 0) {
      selected_ind_high <- NULL
    } else {
      selected_ind_high <- ind_within_inertia %>% data.frame %>% select(Dim.1 = matches(paste0("Dim.", axes[1], "$")), Dim.2 = matches(paste0("Dim.", axes[2], "$"))) %>%
        add_rownames %>% rename(id = rowname) %>% mutate(within_inertia = Dim.1 + Dim.2) %>% arrange(desc(within_inertia)) %>% slice(1:select$within_inertia[[1]])
    }
    if(select$within_inertia[[2]] == 0) {
      selected_ind_low <- NULL
    } else {
      selected_ind_low <- ind_within_inertia %>% data.frame %>% select(Dim.1 = matches(paste0("Dim.", axes[1], "$")), Dim.2 = matches(paste0("Dim.", axes[2], "$"))) %>%
        add_rownames %>% rename(id = rowname) %>% mutate(within_inertia = Dim.1 + Dim.2) %>% arrange(within_inertia) %>% slice(1:select$within_inertia[[2]])
    }
    selected_ind <- rbind(selected_ind_high, selected_ind_low)
  }

  # Filterung vornehmen
  ws_coord_ind_1516 <- ws_coord_ind_1516 %>% data.frame %>%
    filter(id %in% selected_ind$id)
  ss_coord_ind_16 <- ss_coord_ind_16 %>% data.frame %>%
    filter(id %in% selected_ind$id)
  ws_coord_ind_1617 <- ws_coord_ind_1617 %>% data.frame %>%
    filter(id %in% selected_ind$id)

  # Daten final zusammenstellen
  coord_mean_timeseries <- bind_rows(ws_coord_quali_1516, ss_coord_quali_16, ws_coord_quali_1617)

  coord_ind_timeseries <- bind_rows(ws_coord_ind_1516, ss_coord_ind_16, ws_coord_ind_1617) %>%
    mutate(time = factor(time, levels = c("Wintersemester 15/16", "Sommersemester 16", "Wintersemester 16/17")))

  # Plot data

  p <- factoextra::fviz_mfa_ind(res_gda, label = "none", invisible = "ind", pointsize = -1, axes.linetype = "solid", axes = axes)
  # Individuen mit Zeitpfeil
  if(!mean_path & !ellipses) {
    p <- p +
      scale_colour_brewer(palette = "YlGnBu", direction = -1) +
      geom_point(data = coord_ind_timeseries, aes(Dim.1, Dim.2, group = clust), colour = "black", size = 7) +
      geom_point(data = coord_ind_timeseries, aes(Dim.1, Dim.2, group = clust, colour = time), size = 5) +
      geom_path(data = coord_ind_timeseries, aes(Dim.1, Dim.2, group = id), size = 1,
                arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      ggtitle("Fiktiver Vergleich der ersten drei Studiensemester") +
      xlab(paste0("Achse 1 (", round(res_gda$eig$`percentage of variance`[1], 1), "%)")) +
      ylab(paste0("Achse 2 (", round(res_gda$eig$`percentage of variance`[2], 1), "%)"))
    if(facet) p <- p + facet_wrap(~clust)
  }
  # Mittelpunkte
  if(mean_path) {
    p <- p +
      geom_point(data = coord_mean_timeseries, aes(Dim.1, Dim.2), colour = "black", shape = 18, size = 5) +
      geom_point(data = coord_mean_timeseries %>% separate(clust_time, c("clust", "time"), sep = "_"),
                 aes(Dim.1, Dim.2, colour = time), shape = 18, size = 4) +
      geom_path(data = coord_mean_timeseries %>% separate(clust_time, c("clust", "time"), sep = "_"),
                aes(Dim.1, Dim.2, group = clust), size = 1,
                arrow = arrow(length = unit(0.2, "cm"), type = "closed"))
  }
  # Ellipsen der unterschiedlichen Zeitgruppen einzeichnen
  if(ellipses) {
    p <- p +
      stat_ellipse(data = coord_ind_timeseries %>% unite(clust_time, clust, time),
                   aes(Dim.1, Dim.2, fill = clust_time, colour = clust_time), geom ="polygon",  type = "norm",
                   alpha = 0.15, segments = 100, level = 0.8647, linetype = "solid") +
      geom_point(data = coord_mean_timeseries, aes(Dim.1, Dim.2), colour = "black", shape = 18, size = 5) +
      geom_point(data = coord_mean_timeseries, aes(Dim.1, Dim.2, colour = clust_time), shape = 18, size = 4)
    if(facet) p <- p + facet_wrap(~clust_time)
  }

  # Theme adaptieren
  p <- p + add_theme()

  # Ausgabe des Plots
  return(p)
}
