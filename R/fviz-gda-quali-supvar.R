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
#' @param plot_eta2 plot eta2 value per axis (boolean).
#' @param axis_lab_name name of axis label.
#'
#' @return ggplot2 visualization of supplementary variables.
#' @export
fviz_gda_quali_supvar <- function(res_gda, df_var_quali, var_quali, title = "MCA quali var structure",
                               path = FALSE, linetype = "solid", axes = 1:2, scale_point = TRUE, size_point = 3,
                               scale_text = FALSE, size_text = 3, palette = "Set1", myriad = TRUE, impute = TRUE,
                               plot_modif_rates = TRUE, impute_ncp = 2, relevel = NULL, plot_eta2 = TRUE,
                               axis_lab_name = "Achse") {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  var <- df_var_quali %>% select(!! var_quali) %>% data.frame %>% mutate_all(funs(as.character))

  # Berechnungen der passiven Variable durchführen
  supvar_stats <- supvar_stats(res_gda, df_var_quali, var_quali, impute, impute_ncp)

  # Achsen auswählen
  supvar <- bind_cols(rowname = rownames(supvar_stats$coord), weight = supvar_stats$weight, supvar_stats$coord) %>%
    select(rowname, weight, matches(glue("Dim.{axes[1]}$|Dim.{axes[2]}$")))

  #eta2 extrahieren
  if ( plot_eta2 ) {
    supvar_eta2 <- bind_cols(rowname = rownames(supvar_stats$var), supvar_stats$var) %>%
      filter(rowname == "eta2") %>% select(-rowname) %>% mutate_all(funs(round(., 3)))
  } else {
    supvar_eta2 <- NULL
  }


  # Spaltennamen anpassen
  colnames(supvar) <- c("rowname", "weight", "Dim.1", "Dim.2")

  # Reihenfolge der Zeilen an die Faktorenlevels anpassen
  order_levels <- df_var_quali %>% select(matches(var_quali)) %>% data.frame

  order_levels <- levels(factor(order_levels[,1]))

  if( !is.null(relevel) ) order_levels <- relevel

  if( length(which(is.na(var))) != 0 & !impute ) order_levels <- c(order_levels, "Fehlender Wert")

  supvar <- supvar %>% slice(match(order_levels, rowname))

  # Plot
  if(inherits(res_gda, c("MCA"))) p <- .create_plot()
  else stop("Only MCA plots are currently supported!")

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
  p <- .gda_plot_labels(res_gda, p, title, axes, plot_modif_rates, supvar_eta2, axis_lab_name = axis_lab_name)

  # Plotten
  return(p)
}
