#' @include supvar-stats.R
NULL
#' Visualize interaction cloud.
#'
#' @param res_gda GDA result.
#' @param df_var_quali crossed variable data frame.
#' @param var_quali name of crossed supplementary variable.
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
fviz_gda_interaction <- function(res_gda, df_var_quali, var_quali, title = "MCA quali interaction effects", mean.alpha = 0.75,
                               path.linetype = "solid", path.size = 1, path.colour = NULL, scale_mean_points = TRUE, axes = 1:2,
                               palette = "Set1", path.alpha = 0.7, myriad = TRUE, impute = TRUE, variable = 1)
  {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Sofern es fehlende Werte gibt imputieren.

  # Berechnung passive Variable durchführen
  res_quali <- supvar_stats(res_gda, df_var_quali, var_quali, impute)

  # Datensatz konstruieren
  df_coord <- data.frame(res_quali$coord, weight = res_quali$weight) %>%
    add_rownames %>%
    separate(rowname, c("var_1", "var_2"), sep = "_", remove = FALSE) %>%
    gather(key = variable, value = category, var_1, var_2)

  # Skalierung der Koordinaten im Fall von MCA Ergebnissen (* 1/wurzel(eigenwert))
  # Für MCA dann die Koordinaten anpassen (liefert Kategorien, benötigt werden Individuen)

  if(variable == 1) df_coord <- df_coord %>% filter(variable == "var_1")
  if(variable == 2) df_coord <- df_coord %>% filter(variable == "var_2")

  # Plot
  if(inherits(res_gda, c("MCA"))) p <- factoextra::fviz_mca_ind(res_gda, label = "none", invisible = "ind", axes.linetype = "solid", axes = axes)
  if(inherits(res_gda, c("MFA"))) p <- factoextra::fviz_mfa_ind(res_gda, label = "none", invisible = "ind", axes.linetype = "solid", axes = axes)

  p <- p + scale_size_continuous(range = c(1, 7))

  # Punkte plotten
  if(scale_mean_points) p <- p + geom_point(data = df_coord , aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), size = "weight"), shape = 18, alpha = mean.alpha, inherit.aes = FALSE)
  else  p <- p + geom_point(data = df_coord , aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2])), size = 4, shape = 18, alpha = mean.alpha, inherit.aes = FALSE)
  # Punkte beschriften
  p <- p + ggrepel::geom_text_repel(data = df_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), label = "rowname"), size = 4, alpha = mean.alpha, inherit.aes = FALSE, family = "Myriad Pro")
  # Pfad plotten
  p <- p + geom_path(data = df_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), group = "category"), arrow = arrow(), linetype = path.linetype, size = path.size, alpha = path.alpha)
  # Beide Möglichkeiten abbilden
  if(variable == "b") p <- p + facet_wrap(~variable)

  p <- p + add_theme() + ggtitle(title)

  return(p)
}
