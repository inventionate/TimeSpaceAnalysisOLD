#' @include utilities.R
#' @include supvar-stats.R
NULL
#' Visualize supplementary variables.
#'
#' @param res_gda GDA result.
#' @param df_var_quali crossed variable data frame.
#' @param var_quali crossed variable name.
#' @param title plot title.
#' @param path plot path (boolean).
#' @param linetype specify path linetype.
#' @param axes which axes should be plotted.
#' @param palette RColorBrewer palette.
#' @param scale_point scale points by weight (boolean).
#' @param scale_text scale text by weight (boolean).
#' @param size_point define point size.
#' @param size_text define text size.
#' @param myriad use Myriad Pro font family (boolean).
#' @param impute impute missing data (boolean).
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param impute_ncp number of dimensions to predict missing values.
#' @param relevel character vector containing new level order.
#'
#' @return ggplot2 visualization of supplementary variables.
#' @export
fviz_gda_quali_supvar <- function(res_gda, df_var_quali, var_quali, title = "MCA quali var structure",
                               path = FALSE, linetype = "solid", axes = 1:2, scale_point = TRUE, size_point = 3,
                               scale_text = FALSE, size_text = 3, palette = "Set1", myriad = TRUE, impute = TRUE,
                               plot_modif_rates = TRUE, impute_ncp = 2, relevel = NULL) {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  var <- df_var_quali %>% select_(var_quali) %>% data.frame %>% mutate_all(funs(as.character))

  # Berechnungen der passiven Variable durchführen
  supvar_stats <- supvar_stats(res_gda, df_var_quali, var_quali, impute, impute_ncp)

  # Achsen auswählen
  supvar <- data.frame(weight = supvar_stats$weight, coord = supvar_stats$coord) %>%
    tibble::rownames_to_column() %>% select(rowname, weight, matches(paste0("Dim.",axes[1], "$|Dim.", axes[2], "$")))

  # Spaltennamen anpassen
  colnames(supvar) <- c("rowname", "weight", "Dim.1", "Dim.2")

  # Reihenfolge der Zeilen an die Faktorenlevels anpassen
  order_levels <- df_var_quali %>% select(matches(var_quali)) %>% data.frame

  order_levels <- levels(factor(order_levels[,1]))

  if( !is.null(relevel) ) order_levels <- relevel

  if( length(which(is.na(var))) != 0 & !impute ) order_levels <- c(order_levels, "Fehlender Wert")

  supvar <- supvar %>% slice(match(order_levels, rowname))

  # Plot
  if(inherits(res_gda, c("MCA"))) p <- fviz_mca_var(res_gda, label = "none", invisible = "var", axes.linetype = "solid", axes = axes)
  if(inherits(res_gda, c("MFA"))) p <- fviz_mfa_quali_var(res_gda, label = "none", invisible = "var", axes.linetype = "solid", axes = axes)

  # Skalierungsgrenzen festlegen
  p <- p + scale_size_continuous(range = c(1, 7))

  # Punkte verbinden
  if(path) p <- p + geom_path(data = supvar, aes(x = Dim.1, y = Dim.2), linetype = linetype)

  # Punkte plotten
  if(scale_point) p <- p + geom_point(data = supvar, aes(x = Dim.1, y = Dim.2, size = weight, colour = rowname), shape = 17, inherit.aes = FALSE)
  else p <- p + geom_point(data = supvar, aes(x = Dim.1, y = Dim.2, colour = rowname), size = size_point, shape = 17, inherit.aes = FALSE)

  # Beschriftung hinzufügen
  if(scale_text) p <- p + ggrepel::geom_text_repel(data = supvar, aes(x = Dim.1, y = Dim.2, size = weight, label = rowname), point.padding = unit(0.5, "lines"))
  else p <- p + ggrepel::geom_text_repel(data = supvar, aes(x = Dim.1, y = Dim.2, label = rowname), size = size_text, point.padding = unit(0.5, "lines"))

  # Farbpalette wählen
  if(palette != FALSE) p <- p + scale_colour_brewer(palette = palette) + scale_fill_brewer(palette = palette)

  # Standardthema hinzufügen
  p <- add_theme(p) + ggtitle(title)

  # Beschriftung anpassen
  p <- .gda_plot_labels(res_gda, p, title, axes, plot_modif_rates)

  # Plotten
  return(p)
}
