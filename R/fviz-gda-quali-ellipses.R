#' @include utilities.R
NULL
#' Title
#'
#' @param res_gda GDA result.
#' @param df_var_quali data frame of one quali variable.
#' @param var_quali_name name if quali variable.
#' @param title plot title.
#' @param facet whether facet ellipses or not (boolean).
#' @param path_mean plot mean point path (boolean). Only possible if facet is FALSE.
#' @param alpha.point opacity of mean points.
#' @param path.linetype linetype of mean point path.
#' @param scale_mean_points scale mean point size in respect of the group size (boolean).
#' @param axes the GDA dimensions to plot.
#' @param palette Colour brewer scale.
#' @param hcpc add ellipses to HCPC results (boolean).
#'
#' @return ggplo2 visualization with concentration and quali var ellipses.
#' @export
#'
#' @examples
fviz_gda_quali_ellipses <- function(res_gda, df_var_quali, var_quali_name, title = "MCA quali var ellipses",
                                    facet = TRUE, path_mean = FALSE, alpha.point = 0.75, path.linetype = "solid",
                                    scale_mean_points = TRUE, hcpc = FALSE, axes = 1:2, palette = "Set1") {
  # Add Myriad Pro font family
  .add_fonts()

  # Variable bestimmen
  if(hcpc) {
    var_quali <- data.frame(hcpc_studienalltag$data.clust) %>%
      add_rownames() %>%
      select(questionnaire_id = rowname, var_quali = clust)

  } else {
    var_names <- rownames(res_gda$call$X)
    df_source <- data.frame(allgemeine_angaben, df_var_quali)
    var_quali <- df_source %>%
      select(which(names(df_source) %in% c("questionnaire_id", var_quali_name))) %>%
      filter(questionnaire_id %in% var_names)
  }
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

  # ALlgemeine Konzentrationsellipse hinzufügen (level = 86,47% nach Le Roux/Rouanet 2010: 69, da es sich um eine 2-dimesnionale Konzentrationsellipse handelt)
  if(!path_mean) {
    p <- p + stat_ellipse(data = .count_distinct_ind(res_gda), aes(x = Dim.1, y = Dim.2), geom ="polygon", level = 0.8647, type = "norm", alpha = 0.1, colour = "black") +
      stat_ellipse(data = .count_distinct_ind(res_gda), aes(x = Dim.1, y = Dim.2), geom ="path", type = "norm", alpha = 1, level = 0.8647, colour = "black", linetype = "dashed", segments = 100)
  }
  # Konzentrationsellipsen für die passiven Variablengruppen (i. d. F. "Geschlecht")
  if(facet) p <- p + geom_point(data = coord_ind_quali %>% distinct(), aes(x = Dim.1, y = Dim.2, colour = var_quali, size = count), inherit.aes = FALSE, alpha = alpha.point)
  p <- p + scale_size_continuous(range = c(1, 7))
  if(scale_mean_points) p <- p + geom_point(data = coord_mean_quali, aes(x = Dim.1, y = Dim.2, colour = var_quali, size = size), shape = 18, inherit.aes = FALSE)
  else  p <- p + geom_point(data = coord_mean_quali, aes(x = Dim.1, y = Dim.2, colour = var_quali), shape = 18, size = 7, inherit.aes = FALSE)
  if(path_mean & !facet) {
    p <- p + geom_path(data = coord_mean_quali, aes(x = Dim.1, y = Dim.2), linetype = path.linetype)
  } else {
    p <- p + stat_ellipse(data = coord_ind_quali, aes(x = Dim.1, y = Dim.2, fill = var_quali), geom ="polygon",  type = "norm", alpha = 0.15, segments = 100, level = 0.8647, inherit.aes = FALSE) +
      stat_ellipse(data = coord_ind_quali, aes(x = Dim.1, y = Dim.2, colour = var_quali), geom ="path", type = "norm", alpha = 1, linetype = "solid", segments = 100, level = 0.8647, inherit.aes = FALSE)
  }
  if(palette != FALSE) p <- p + scale_colour_brewer(palette = palette) + scale_fill_brewer(palette = palette)
  if(facet) p <- p + facet_wrap(~var_quali)
  p <- p + add_theme() + ggtitle(title)
  return(p)
}
