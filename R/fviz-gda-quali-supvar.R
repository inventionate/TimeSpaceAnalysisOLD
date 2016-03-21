#' @include utilities.R
#' @include supvar-stats.R
NULL
#' Visualize supplementary variables.
#'
#' @param res_gda GDA result.
#' @param df_var_quali crossed variable data frame.
#' @param var_quali_name crossed variable name.
#' @param title plot title.
#' @param path plot path (boolean).
#' @param linetype specify path linetype.
#' @param axes which axes should be plotted.
#' @param palette RColorBrewer palette.
#' @param scale_point scale points by weight (boolean).
#' @param scale_text scale text by weight (boolean).
#' @param na_exclude handle missing data (boolean).
#' @param size_point define point size.
#' @param size_text define text size.
#'
#' @return ggplot2 visualization of supplementary variables.
#' @export
#'
#' @examples
fviz_gda_quali_supvar <- function(res_gda, df_var_quali, var_quali_name, title = "MCA quali var structure",
                               path = FALSE, linetype = "solid", axes = 1:2, scale_point = TRUE, size_point = 3,
                               scale_text = FALSE, size_text = 3, palette = "Set1", na_exclude = TRUE)
{

  # Add Myriad Pro font family
  .add_fonts()

  # Berechnungen der passiven Variable durchführen
  supvar <- supvar_stats(res_gda, df_var_quali, var_quali_name)

  # Fehlende Werte ausschließen
  if(na_exclude) {
    supvar <- data.frame(weight = supvar$weight, coord = supvar$coord) %>%
      add_rownames %>% filter(!grepl("Fehlender Wert$", rowname))
  } else {
    supvar <- data.frame(weight = supvar$weight, coord = supvar$coord) %>%
      add_rownames
  }

  # Achsen auswählen
  supvar <- supvar %>% select(rowname, weight, matches(paste0("Dim.",axes[1], "$|Dim.", axes[2], "$")))

  # Spaltennamen anpassen
  colnames(supvar) <- c("rowname", "weight", "Dim.1", "Dim.2")

  # Plot
  if(inherits(res_gda, c("MCA", "sMCA"))) p <- fviz_mca_var(res_gda, label = "none", invisible = "var", axes.linetype = "solid", axes = axes)
  if(inherits(res_gda, c("MFA", "sMFA"))) p <- fviz_mfa_quali_var(res_gda, label = "none", invisible = "var", axes.linetype = "solid", axes = axes)

  # Skalierungsgrenzen festlegen
  p <- p + scale_size_continuous(range = c(1, 7))

  # Punkte verbinden
  if(path) p <- p + geom_path(data = supvar, aes(x = Dim.1, y = Dim.2), linetype = linetype)

  # Punkte plotten
  if(scale_point) p <- p + geom_point(data = supvar, aes(x = Dim.1, y = Dim.2, size = weight, colour = rowname), shape = 17, inherit.aes = FALSE)
  else p <- p + geom_point(data = supvar, aes(x = Dim.1, y = Dim.2, colour = rowname), size = size_point, shape = 17, inherit.aes = FALSE)

  # Beschriftung hinzufügen
  if(scale_text) p <- p + ggrepel::geom_text_repel(data = supvar, aes(x = Dim.1, y = Dim.2, size = weight, label = rowname))
  else p <- p + ggrepel::geom_text_repel(data = supvar, aes(x = Dim.1, y = Dim.2, label = rowname), size = size_text)

  # Farbpalette wählen
  if(palette != FALSE) p <- p + scale_colour_brewer(palette = palette) + scale_fill_brewer(palette = palette)

  # Standardthema hinzufügen
  p <- p + add_theme() + ggtitle(title)

  # Plotten
  return(p)
}
