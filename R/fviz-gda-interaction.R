#' @include supvar-stats.R
NULL
#' Visualize interaction cloud.
#'
#' @param res_gda GDA result.
#' @param df_var_quali crossed variable data frame.
#' @param title plot title.
#' @param mod_level_order order of the crossed modalities.
#' @param mean.alpha alpha of the mean point.
#' @param path.linetype linetype of concat path.
#' @param path.size size of concat path.
#' @param path.colour colour of concat path.
#' @param scale_mean_points scale mean points (boolean).
#' @param axes axes to plot.
#' @param palette used colour brewer palette.
#' @param path.alpha opacity of the path.
#' @param myriad use myriad font or not (boolean).
#' @param impute use imputation to handle missing data.
#'
#' @return ggplot2 interaction cloud visualizsation.
#' @export
fviz_gda_interaction <- function(res_gda, df_var_quali, title = "MCA quali interaction effects", mean.alpha = 0.75,
                               path.linetype = "solid", path.size = 1, path.colour = NULL, scale_mean_points = TRUE, axes = 1:2,
                               palette = "Set1", mod_level_order = NULL, path.alpha = 0.7, myriad = TRUE, impute = TRUE)
  {

  # Dev message.
  print("Interaktionswolke: Die Funktion befindet sich im Alpha-Stadium und ist noch nicht einsatzfähig.")

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Sofern es fehlende Werte gibt imputieren.

  # Berechnung passive Variable durchführen
  res_quali <- supvar_stats(res_gda, var_quali)

  # Koordinaten bestimmen
  res_quali$coord

  # Skalierung der Koordinaten im Fall von MCA Ergebnissen (* 1/wurzel(eigenwert))
  # Für MCA dann die Koordinaten anpassen (liefert Kategorien, benötigt werden Individuen)

  # Gewicht bestimmen
  res_gda$weight

  # Plot
  if(inherits(res_gda, c("MCA"))) p <- fviz_mca_ind(res_gda, label = "none", invisible = "ind", axes.linetype = "solid", axes = axes)
  if(inherits(res_gda, c("MFA"))) p <- fviz_mfa_ind(res_gda, label = "none", invisible = "ind", axes.linetype = "solid", axes = axes)

  p <- p + scale_size_continuous(range = c(1, 7))

  # Hier müssen die Daten so gefiltert werden, dass sie das entsprechende Level enthalten
  if(!is.null(mod_names))
  {
    for (i in 1:length(mod_names)) {
      mean_mod_coord <- coord_mean_quali %>% filter(grepl(mod_names[i], var_quali)) %>% mutate(var_quali = factor(var_quali, levels = c(mod_level_order[[i]]))) %>% arrange(var_quali)
      # Punkte plotten
      if(scale_mean_points) p <- p + geom_point(data = mean_mod_coord, aes(x = Dim.1, y = Dim.2, size = size), shape = 18, alpha = mean.alpha, inherit.aes = FALSE, colour = path.colour[i])
      else  p <- p + geom_point(data = mean_mod_coord, aes(x = Dim.1, y = Dim.2), shape = 18, size = 4, alpha = mean.alpha, inherit.aes = FALSE, colour = path.colour[i])
      # Punkte beschriften
      p <- p + ggrepel::geom_text_repel(data = mean_mod_coord, aes(x = Dim.1, y = Dim.2, label = var_quali), size = 4, alpha = mean.alpha, inherit.aes = FALSE, colour = path.colour[i], family = "Myriad Pro")
      # Pfad plotten
      p <- p + geom_path(data = mean_mod_coord, aes(x = Dim.1, y = Dim.2), arrow = arrow(), linetype = path.linetype, size = path.size, colour = path.colour[i], alpha = path.alpha)
    }
  }

  p <- p + add_theme() + ggtitle(title)
  return(p)
}
