#' @include add-theme.R
NULL
#' Visualization of trajectories (connected HMFA partial points).
#'
#' @param res_gda HMFA result (rownames have to be questionnaire IDs).
#' @param select contribution, cos2, vector of names or within_inertia of individuals selection (within_inertia: vector containing the number of high variation and low variationindividuals).
#' @param ellipse_level the ellipse level. Default: 86.47\%.
#' @param ellipse_alpha opacity value.
#' @param axes axes to plot.
#' @param myriad use Myriad Pro font (boolean).
#' @param ellipses plot concentration ellipses (boolean).
#' @param facet plot ellipses/ individuals per year (boolean).
#' @param mean_path plot mean path (boolean). If yes, no ellipses and no facets are plotted.
#' @param clust HCPC result of primary MFA.
#' @param facet_labels Rename facet labels (vector).
#'
#' @return HMFA trajectory ggplot2 visualization.
#' @export
fviz_gda_hmfa_trajectory <- function(res_gda, clust, select = list(name = NULL, contrib = NULL, cos2 = NULL, within_inertia = NULL),
                                ellipse_level = 0.8647, ellipse_alpha = 0.1, axes = 1:2, myriad = TRUE, facet_labels = NULL,
                                ellipses = FALSE, facet = FALSE, mean_path = FALSE) {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Selection (es wird select_ind definiert)
  selected_ind <- res_gda$ind$coord %>% data.frame %>% add_rownames

  if(!is.null(select$contrib))
  {
    selected_ind <- res_gda$ind$contrib %>% data.frame %>% select(Dim.1 = matches(paste0("Dim.", axes[1])), Dim.2 = matches(paste0("Dim.", axes[2]))) %>% add_rownames %>%
      mutate(ctr = Dim.1*res_gda$eig$eigenvalue[1] + Dim.2*res_gda$eig$eigenvalue[2]) %>%
      arrange(desc(ctr)) %>% slice(1:select$contrib) %>% data.frame
  }
  if(!is.null(select$cos2))
  {
    selected_ind <- res_gda$ind$cos2 %>% data.frame %>% select(Dim.1 = matches(paste0("Dim.", axes[1])), Dim.2 = matches(paste0("Dim.", axes[2]))) %>% add_rownames %>%
      mutate(ctr = Dim.1*res_gda$eig$eigenvalue[1] + Dim.2*res_gda$eig$eigenvalue[2]) %>%
      arrange(desc(ctr)) %>% slice(1:select$cos2) %>% data.frame
  }
  if(!is.null(select$name))
  {
    selected_ind <- res_gda$ind %>% data.frame %>% add_rownames %>%
      filter(rowname %in% select$name)
  }
  if(!is.null(select$within_inertia))
  {
    # Within inertia (adaptiert von FactoMineR)
    # @todo Momentan für drei Semester optimiert. Es müsste für allgemeine Operationen angepasst werden.
    tmp <- array(0,dim=c(dim(res_gda$ind$coord),3))
    tmp[,,1] <- (res_gda$partial[[2]][,,1]-res_gda$ind$coord)^2 / 3
    tmp[,,2] <- (res_gda$partial[[2]][,,2]-res_gda$ind$coord)^2 / 3
    tmp[,,3] <- (res_gda$partial[[2]][,,3]-res_gda$ind$coord)^2 / 3
    variab.auxil <- apply(tmp,2,sum)
    tmp <- sweep(tmp,2,variab.auxil,FUN="/") * 100
    inertie.intra.ind <- apply(tmp,c(1,2),sum)
    rownames(inertie.intra.ind) <- rownames(res_gda$ind$coord)
    colnames(inertie.intra.ind) <- colnames(res_gda$ind$coord)
    hmfa_within_inertia <- inertie.intra.ind

    # Auswahl treffen
    if(select$within_inertia[[1]] == 0) {
      selected_ind_high <- NULL
    } else {
      selected_ind_high <- hmfa_within_inertia %>% data.frame %>% select(Dim.1 = matches(paste0("Dim.", axes[1], "$")), Dim.2 = matches(paste0("Dim.", axes[2], "$"))) %>%
        add_rownames %>% mutate(within_inertia = Dim.1 + Dim.2) %>% arrange(desc(within_inertia)) %>% slice(1:select$within_inertia[[1]])
    }
    if(select$within_inertia[[2]] == 0) {
      selected_ind_low <- NULL
    } else {
      selected_ind_low <- hmfa_within_inertia %>% data.frame %>% select(Dim.1 = matches(paste0("Dim.", axes[1], "$")), Dim.2 = matches(paste0("Dim.", axes[2], "$"))) %>%
        add_rownames %>% mutate(within_inertia = Dim.1 + Dim.2) %>% arrange(within_inertia) %>% slice(1:select$within_inertia[[2]])
    }
    selected_ind <- rbind(selected_ind_low, selected_ind_high)
  }

  # Prepare data
  ws1516 <- res_gda$partial[[2]][,axes,1]
  ss16 <- res_gda$partial[[2]][,axes,2]
  ws1617 <- res_gda$partial[[2]][,axes,3]

  # Koordinaten der Individuen pro Semester
  ws_coord_ind_1516 <- data.frame(ws1516, clust = clust$data.clust$clust) %>%
    add_rownames %>% mutate(year = "Wintersemester 15/16")
  ss_coord_ind_16 <- data.frame(ss16, clust = clust$data.clust$clust) %>%
    add_rownames %>% mutate(year = "Sommersemester 16")
  ws_coord_ind_1617 <- data.frame(ws1617, clust = clust$data.clust$clust) %>%
    add_rownames %>% mutate(year = "Wintersemester 16/17")

  # Koordinaten der Ellipsenmittelpunkte pro Semester und Cluster
  ws_coord_quali_1516 <- ws_coord_ind_1516 %>% select(-rowname) %>%
    unite(clust_year, clust, year) %>% group_by(clust_year) %>%
    summarise_each(funs(mean))
  ss_coord_quali_16 <- ss_coord_ind_16 %>% select(-rowname) %>%
    unite(clust_year, clust, year) %>% group_by(clust_year) %>%
    summarise_each(funs(mean))
  ws_coord_quali_1617 <- ws_coord_ind_1617 %>% select(-rowname) %>%
    unite(clust_year, clust, year) %>% group_by(clust_year) %>%
    summarise_each(funs(mean))

  # Filterung vornehmen
  ws_coord_ind_1516 <- ws_coord_ind_1516 %>% data.frame %>%
    filter(rowname %in% selected_ind$rowname)
  ss_coord_ind_16 <- ss_coord_ind_16 %>% data.frame %>%
    filter(rowname %in% selected_ind$rowname)
  ws_coord_ind_1617 <- ws_coord_ind_1617 %>% data.frame %>%
    filter(rowname %in% selected_ind$rowname)

  # Daten final zusammenstellen
  # @todo Auch hier müsste generalisiert werden.
  coord_mean_timeseries <- bind_rows(ws_coord_quali_1516, ss_coord_quali_16, ws_coord_quali_1617)

  coord_ind_timeseries <- bind_rows(ws_coord_ind_1516, ss_coord_ind_16, ws_coord_ind_1617) %>%
    mutate(year = factor(year, levels = c("Wintersemester 15/16", "Sommersemester 16", "Wintersemester 16/17")))

  # Plot data

  p <- factoextra::fviz_hmfa_ind_starplot(res_gda, node.level = 2, label = "none", invisible = "ind", axes = axes, axes.linetype = "solid")
  # Individuen mit Zeitpfeil
  if(!mean_path & !ellipses) {
    p <- p +
      scale_colour_brewer(palette = "YlGnBu", direction = -1) +
      geom_point(data = coord_ind_timeseries, aes(Dim1, Dim2, group = clust), colour = "black", size = 7) +
      geom_point(data = coord_ind_timeseries, aes(Dim1, Dim2, group = clust, colour = year), size = 5) +
      geom_path(data = coord_ind_timeseries, aes(Dim1, Dim2, group = rowname), size = 1,
                arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      ggtitle("Fiktiver Vergleich der ersten drei Studiensemester") +
      xlab(paste0("Achse 1 (", round(res_gda$eig$`percentage of variance`[1], 1), "%)")) +
      ylab(paste0("Achse 2 (", round(res_gda$eig$`percentage of variance`[2], 1), "%)"))
    if(facet) {
      if(is.null(facet_labels)) p <- p + facet_wrap(~clust, labeller = label_both)
      else p <- p + facet_wrap(~clust, labeller = labeller(clust = facet_labels))
    }
  }
  # Mittelpunkte
  if(mean_path) {
    p <- p +
      geom_point(data = coord_mean_timeseries, aes(Dim1, Dim2), colour = "black", shape = 18, size = 5) +
      geom_point(data = coord_mean_timeseries %>% separate(clust_year, c("clust", "year"), sep = "_"),
                 aes(Dim1, Dim2, colour = year), shape = 18, size = 4) +
      geom_path(data = coord_mean_timeseries %>% separate(clust_year, c("clust", "year"), sep = "_"),
                aes(Dim1, Dim2, group = clust), size = 1,
                arrow = arrow(length = unit(0.2, "cm"), type = "closed"))
  }
  # Ellipsen der unterschiedlichen Zeitgruppen einzeichnen
  if(ellipses) {
    p <- p +
      stat_ellipse(data = coord_ind_timeseries %>% unite(clust_year, clust, year),
                   aes(Dim1, Dim2, fill = clust_year, colour = clust_year), geom ="polygon",  type = "norm",
                   alpha = 0.15, segments = 100, level = 0.8647, linetype = "solid") +
      geom_point(data = coord_mean_timeseries, aes(Dim1, Dim2), colour = "black", shape = 18, size = 5) +
      geom_point(data = coord_mean_timeseries, aes(Dim1, Dim2, colour = clust_year), shape = 18, size = 4)
    if(facet) {
      if(is.null(facet_labels)) p <- p + facet_wrap(~clust_time, labeller = label_both)
      else p <- p + facet_wrap(~clust_time, labeller = labeller(clust_time = facet_labels))
    }
  }

  # Theme adaptieren
  p <- p + add_theme()

  # Beschreibung der Punkte
  p <- p + theme(legend.position = "bottom", legend.title = element_blank())

  # Ausgabe des Plots
  return(p)
}
