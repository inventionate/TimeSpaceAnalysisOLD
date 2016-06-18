#' @include utilities.R
#' @include add-theme.R
NULL
#' Title
#'
#' @return
#' @export
#'
#' @examples
fviz_gda_trajectory_conc <- function(res_gda, time_point_names = NULL, myriad = TRUE, axes = 1:2, ind_points = TRUE) {

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

  p <- p + stat_ellipse(data = coord_all, aes(Dim.1, Dim.2, fill = time, colour = time), geom ="polygon",  type = "norm",
                        alpha = 0.15, segments = 100, level = 0.8647, linetype = "solid")

  if(ind_points) p <- p + geom_point(data = coord_all, aes(Dim.1, Dim.2, colour = time, size = mass), show.legend = FALSE)

  p <- p + geom_point(data = coord_mean_mass, aes(Dim.1, Dim.2, size = mass * 1.5), colour = "black", shape = 18, show.legend = FALSE) +
    geom_point(data = coord_mean_mass, aes(Dim.1, Dim.2, size = mass, colour = time), shape = 18, show.legend = FALSE) +
    geom_path(data = coord_mean_mass, aes(Dim.1, Dim.2), size = 1, arrow = arrow(length = unit(0.2, "cm"), type = "closed"))

  # Theme adaptieren
  p <- p + add_theme()

  # Beschreibung der Punkte
  p <- p + theme(legend.position = "bottom", legend.title = element_blank())

  # Ausgabe des Plots
  return(p)

}
