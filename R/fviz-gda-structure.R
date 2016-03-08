#' Concat crossed variables.
#'
#' @param res_gda GDA result.
#' @param df_var_quali crossed variable data frame.
#' @param var_quali_name crossed variable name.
#' @param title plot title.
#' @param mod_names names of the crossed categories.
#' @param mod_level_order order of the crossed modalities.
#' @param mean.alpha alpha of the mean point.
#' @param path.linetype linetype of concat path.
#' @param path.size size of concat path.
#' @param path.colour colour of concat path.
#' @param scale_mean_points scale mean points (boolean).
#' @param axes axes to plot.
#' @param palette used colour brewer palette.
#'
#' @return
#' @export
#'
#' @examples
fviz_gda_structure <- function(res_gda, df_var_quali, var_quali_name, title = "MCA quali structure effects", mean.alpha = 0.75,
                               path.linetype = "solid", path.size = 1, path.colour = NULL, scale_mean_points = TRUE, axes = 1:2,
                               palette = "Set1", mod_names = NULL, mod_level_order = NULL) {
  # @todo die Ellipsenfunktion darun ergänzen, keinen allgemeinen Pfad zu zeichnen, sondern spezifisch je nach Variablenkreuzung.
  # Man müsste dann spezifizieren "connect = age" oder "connect = gender", wobei standard NULL ist. D. h., der Crossmode wird
  # erst aktiviert, sobald eine spezielle Connection kommt. Dazu benötigt man dann auch die Option "arrow" inkl. Richtung.
  # Inhaltlich muss dass über mehrere Pfad geoms gemacht werden, die in Abhängigkeit von der connect Option generiert werden
  # (eventuelle mit Schleife). Im Fall age X gender wären es einemal ein path_geom und das zweite mal vier.
  df_source <- data.frame(allgemeine_angaben, df_var_quali)
  var_quali <- df_source %>%
    select(which(names(df_source) %in% c("questionnaire_id", var_quali_name))) %>%
    filter(questionnaire_id %in% rownames(res_gda$call$X))

  # Auf fehlende Werte prüfen
  exclude_na <- which(is.na(var_quali[,2]))
  # Datensätze zusammenstellen
  if(length(exclude_na) == 0) df_source_na <- data.frame(Dim.1 = res_gda$ind$coord[, axes[1]], Dim.2 = res_gda$ind$coord[, axes[2]], var_quali = var_quali[,2])
  else df_source_na <- data.frame(Dim.1 = res_gda$ind$coord[, axes[1]], Dim.2 = res_gda$ind$coord[, axes[2]], var_quali = var_quali[,2])[-exclude_na,]

  coord_ind_quali <- df_source_na %>%
    group_by(Dim.1, Dim.2, var_quali) %>%
    mutate(count = n()) %>%
    ungroup()
  coord_mean_quali <- coord_ind_quali %>% group_by(var_quali) %>% summarise_each(funs(mean))
  size_mean_quali <- coord_ind_quali %>% group_by(var_quali) %>% summarise_each(funs(length)) %>% ungroup() %>% mutate(size = count) %>% select(size) %>% data.frame
  coord_mean_quali <- data.frame(coord_mean_quali, size = size_mean_quali)


  # Plot
  if(inherits(res_gda, c("MCA", "sMCA"))) p <- fviz_mca_ind(res_gda, label = "none", invisible = "ind", axes.linetype = "solid", axes = axes)
  if(inherits(res_gda, c("MFA", "sMFA"))) p <- fviz_mfa_ind(res_gda, label = "none", invisible = "ind", axes.linetype = "solid", axes = axes)

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
      p <- p + geom_text(data = mean_mod_coord, aes(x = Dim.1, y = Dim.2, label = var_quali), size = 4, alpha = mean.alpha, inherit.aes = FALSE, colour = path.colour[i])
      # Pfad plotten
      p <- p + geom_path(data = mean_mod_coord, aes(x = Dim.1, y = Dim.2), arrow = arrow(), linetype = path.linetype, size = path.size, colour = path.colour[i])
    }
  }

  p <- p + add_theme() + ggtitle(title)
  return(p)
}
