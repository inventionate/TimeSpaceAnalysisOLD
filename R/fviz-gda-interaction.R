#' @include supvar-stats.R
NULL
#' Visualize interaction cloud.
#'
#' @param res_gda MCA result.
#' @param df_var_quali crossed variable data frame.
#' @param var_quali name of crossed supplementary variable.
#' @param title plot title.
#' @param mean_alpha alpha of the mean point.
#' @param path_linetype linetype of concat path.
#' @param path_size size of concat path.
#' @param path_colour colour of concat path.
#' @param scale_mean_points scale mean points (boolean).
#' @param axes axes to plot.
#' @param palette used colour brewer palette.
#' @param path_alpha opacity of the path.
#' @param myriad use myriad font or not (boolean).
#' @param impute use imputation to handle missing data.
#' @param variable which diagram to plot (vector containing 1, 2 or "both").
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#'
#' @return ggplot2 interaction cloud visualizsation.
#' @export
fviz_gda_interaction <- function(res_gda, df_var_quali, var_quali, title = "MCA quali interaction effects", mean_alpha = 0.75,
                               path_linetype = "solid", path_size = 1, path_colour = "black", scale_mean_points = TRUE, axes = 1:2,
                               palette = "Set1", path_alpha = 1, myriad = TRUE, impute = TRUE, variable = "both", plot_modif_rates = TRUE) {

  # Check GDA result
  if(!inherits(res_gda, c("MCA"))) stop("GDA result have to be MCA results.")

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Berechnung passive Variable durchführen
  res_quali <- supvar_stats(res_gda, df_var_quali, var_quali, impute)

  # Datensatz konstruieren
  df_coord <- data.frame(res_quali$coord, weight = res_quali$weight) %>%
    tibble::rownames_to_column() %>%
    separate(rowname, c("var_1", "var_2"), sep = "_", remove = FALSE) %>%
    gather(key = variable, value = category, var_1, var_2)

  # Filtern der Variablen
  if(variable == 1) df_coord <- df_coord %>% filter(variable == "var_1")
  if(variable == 2) df_coord <- df_coord %>% filter(variable == "var_2")

  # Plot
  if(inherits(res_gda, c("MCA"))) p <- .create_plot()
  else stop("Only MCA plots are currently supported!")

  # Skalierungsdimension festlegen
  p <- p + scale_size_continuous(range = c(1, 7))
  # Punkte plotten
  if(scale_mean_points) p <- p + geom_point(data = df_coord , aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), size = "weight"), shape = 18, alpha = mean_alpha, inherit.aes = FALSE)
  else  p <- p + geom_point(data = df_coord , aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2])), size = 4, shape = 18, alpha = mean_alpha, inherit.aes = FALSE)
  # Punkte beschriften
  p <- p + ggrepel::geom_text_repel(data = df_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), label = "rowname"), size = 4, alpha = mean_alpha, inherit.aes = FALSE, family = "Myriad Pro")
  # Pfad plotten
  p <- p + geom_path(data = df_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), group = "category"), arrow = arrow(length = unit(0.3, "cm")), linetype = path_linetype, size = path_size, alpha = path_alpha, colour = path_colour)
  # Beide Möglichkeiten abbilden
  if(variable == "both") p <- p + facet_wrap(~variable)
  # Designanpassungen
  p <- add_theme(p) + ggtitle(title)

  # Beschriftung anpassen
  p <- .gda_plot_labels(res_gda, p, title, axes, plot_modif_rates)

  # Plotten
  return(p)
}
