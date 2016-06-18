#' @include utilities.R
#' @include add-theme.R
#' @include get-gda-trajectory.R
NULL
#' Visualization of the separated concentration ellipses per time point.
#'
#' @param res_gda MCA result (rownames have to be questionnaire IDs including time number, e.g. 87654_1).
#' @param time_point_names vector containing the name of the time points.
#' @param myriad use Myriad Pro font (boolean).
#' @param axes the axes to plot.
#' @param ind_points show individuals (boolean).
#' @param title title of the plot
#'
#' @return ggplot2 visualization.
#' @export
fviz_gda_trajectory_conc <- function(res_gda, time_point_names = NULL, myriad = TRUE, axes = 1:2,
                                     ind_points = TRUE, title = "Trajectory plot compare concentration ellipses") {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Trajektoriedaten zusammenstellen
  coord_trajectory <- get_gda_trajectory(res_gda, time_point_names)
  coord_all <-  coord_trajectory$coord_all
  coord_mean_mass <-  coord_trajectory$coord_mean_mass

  # Masse hinzufügen
  coord_all <- coord_all %>% select_(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), "time") %>%
    group_by(Dim.1, Dim.2, time) %>% mutate(mass = n()) %>% ungroup()

  # @todo HILFE + Dimensionen variabel machen!!!!!!!!


  # Plot der Daten
  if(inherits(res_gda, c("MCA"))) p <- factoextra::fviz_mca_ind(res_gda, label = "none", invisible = c("ind", "ind.sup"), pointsize = -1, axes.linetype = "solid", axes = axes)
  else stop("Only MCA plots are currently supported!")

  p <- p + stat_ellipse(data = coord_all, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), fill = "time", colour = "time"), geom ="polygon",  type = "norm",
                        alpha = 0.15, segments = 100, level = 0.8647, linetype = "solid")

  if(ind_points) p <- p + geom_point(data = coord_all, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), colour = "time", size = "mass"), show.legend = FALSE)

  p <- p + geom_point(data = coord_mean_mass, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), size = paste0("mass", "* 1.75")), colour = "black", shape = 18, show.legend = FALSE) +
    geom_point(data = coord_mean_mass, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), size = "mass", colour = "time"), shape = 18, show.legend = FALSE) +
    geom_path(data = coord_mean_mass, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2])), size = 1, arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    ggtitle(title) +
    xlab(paste0("Achse", axes[1], "(", round(res_gda$eig$`percentage of variance`[axes[1]], 1), "%)")) +
    ylab(paste0("Achse", axes[2], "(", round(res_gda$eig$`percentage of variance`[axes[2]], 1), "%)"))

  # Theme adaptieren
  p <- p + add_theme()

  # Beschreibung der Punkte
  p <- p + theme(legend.position = "bottom", legend.title = element_blank())

  # Ausgabe des Plots
  return(p)

}
