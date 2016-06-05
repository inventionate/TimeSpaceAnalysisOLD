#' @include utilities.R
NULL
#' Title
#'
#' @param res_gda GDA (MCA, MFA) result (rownames have to be individual questionnaire IDs).
#' @param df_var_quali data frame of one quali variable.
#' @param var_quali name if quali variable.
#' @param title plot title.
#' @param facet whether facet ellipses or not (boolean).
#' @param scale_mean_points scale mean point size in respect of the group size (boolean).
#' @param axes the GDA dimensions to plot.
#' @param palette Colour brewer scale.
#' @param alpha_point opacity of individual points.
#' @param conc_linetype linetype of concentration ellipse.
#' @param conf_linetype linetype of confidence ellipse.
#' @param myriad use Myriad Pro font family (boolean).
#' @param concentration_ellipses plot concentration ellipse (boolean).
#' @param confidence_ellipses plot confidence ellipses (boolean).
#' @param conf_colour colour confidence ellipses (boolean).
#' @param impute impute missing data (boolean).
#'
#' @return ggplo2 visualization with concentration and quali var ellipses.
#' @export
fviz_gda_quali_ellipses <- function(res_gda, df_var_quali, var_quali, title = "MCA quali var ellipses",
                                    facet = TRUE, alpha_point = 0.75, conc_linetype = "solid", conf_linetype = "solid",
                                    scale_mean_points = TRUE, axes = 1:2, palette = "Set1", myriad = TRUE, impute = TRUE,
                                    concentration_ellipses = TRUE, confidence_ellipses = TRUE, conf_colour = FALSE) {

  # @todo: Reihenfolge der facets an die Levels anpassen!

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Datensatz auslesen
  var <- df_var_quali %>% select_(var_quali) %>% data.frame %>% mutate_each(funs(as.character))
  var_levels <- df_var_quali %>% select_(var_quali) %>% data.frame %>% .[,1] %>% levels

  # Auf Fehlende Werte prüfen.
  exclude_na <- which(is.na(var))

  if( length(exclude_na) != 0 & impute ) {

    message("Info: Missing data will be imputed!")

    var <- var %>% mutate_each(funs(as.factor))

    if(inherits(res_gda, c("MCA"))) {

      var_impute <- missMDA::imputeMCA(data.frame(res_gda$call$X, var))

    }

    if(inherits(res_gda, c("MFA"))) {

      var_impute <- missMDA::imputeMFA(data.frame(res_gda$call$X, var),
                                       c(res_gda$call$group, 1),
                                       res_gda$call$ncp,
                                       c(res_gda$call$type, "n"))

    }

    var <- var_impute$completeObs[var_quali]
  } else {
    # Fehlende Werte durch Kategorie ersetzen (falls nicht imputiert wurde).
    var[is.na(var)] <- "Fehlender Wert"
    var_levels <- c(var_levels, "Fehlender Wert")
  }

  # Spalte in Vektor umwandeln
  var <- var[,1]

  # Datensatz zusammenstellen (Koordinaten mit passiver Variable zusammenführen)
  df_source <- data_frame(Dim.1 = res_gda$ind$coord[, axes[1]], Dim.2 = res_gda$ind$coord[, axes[2]], var_quali = factor(var)) %>%
    mutate(var_quali = factor(var_quali, levels = var_levels))

  coord_ind_quali <- df_source %>%
    group_by(Dim.1, Dim.2, var_quali) %>%
    mutate(count = n()) %>%
    ungroup()
  coord_mean_quali <- coord_ind_quali %>% group_by(var_quali) %>% summarise_each(funs(mean))
  size_mean_quali <- coord_ind_quali %>% group_by(var_quali) %>% summarise_each(funs(length)) %>% ungroup() %>% mutate(size = count) %>% select(size) %>% data.frame
  coord_mean_quali <- data.frame(coord_mean_quali, size = size_mean_quali)

  # Levels berücksichtigen


  # Plot
  if(inherits(res_gda, c("MCA"))) p <- fviz_mca_ind(res_gda, label = "none", invisible = "ind", axes.linetype = "solid", axes = axes)
  if(inherits(res_gda, c("MFA"))) p <- fviz_mfa_ind(res_gda, label = "none", invisible = "ind", axes.linetype = "solid", axes = axes)

  # ALlgemeine Konzentrationsellipse hinzufügen (level = 86,47% nach Le Roux/Rouanet 2010: 69, da es sich um eine 2-dimesnionale Konzentrationsellipse handelt)
  p <- p + stat_ellipse(data = .count_distinct_ind(res_gda), aes(x = Dim.1, y = Dim.2), geom ="polygon", level = 0.8647, type = "norm", alpha = 0.1, colour = "black", linetype = "dashed", segments = 100)
  # Konzentrationsellipsen für die passiven Variablengruppen (i. d. F. "Geschlecht")
  if(facet) p <- p + geom_point(data = coord_ind_quali, aes(x = Dim.1, y = Dim.2, colour = var_quali, size = count), inherit.aes = FALSE, alpha = alpha_point)
  p <- p + scale_size_continuous(range = c(1, 7))
  if(scale_mean_points) p <- p + geom_point(data = coord_mean_quali, aes(x = Dim.1, y = Dim.2, fill = var_quali, size = size), colour = "black", shape = 23, inherit.aes = FALSE)
  else p <- p + geom_point(data = coord_mean_quali, aes(x = Dim.1, y = Dim.2, fill = var_quali), colour = "black", shape = 23, size = 10, inherit.aes = FALSE)
  if(concentration_ellipses) p <- p + stat_ellipse(data = coord_ind_quali, aes(x = Dim.1, y = Dim.2, fill = var_quali, colour = var_quali), geom ="polygon",  type = "norm", alpha = 0.15, linetype = conc_linetype, segments = 100, level = 0.8647, inherit.aes = FALSE)
  if(confidence_ellipses) {
    conf_ellipses_coord <- FactoMineR::coord.ellipse(data.frame(coord_ind_quali[c(3,1,2)]), bary = TRUE)$res
    if(conf_colour) p <- p + geom_path(data = conf_ellipses_coord, aes(Dim.1, Dim.2, colour = var_quali), show.legend = FALSE, linetype = conf_linetype, size = 0.75)
    else p <- p + geom_path(data = conf_ellipses_coord, aes(Dim.1, Dim.2, group = var_quali), show.legend = FALSE, linetype = conf_linetype, size = 0.75)
  }
  if(palette != FALSE) p <- p + scale_colour_brewer(palette = palette) + scale_fill_brewer(palette = palette)
  if(facet) p <- p + facet_wrap(~var_quali)
  p <- p + add_theme() + ggtitle(title)
  return(p)
}
