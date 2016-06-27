#' @include utilities.R
#' @include supvar-stats.R
#' @include supvar-crossing-stats.R
NULL
#' Visualize additive cloud.
#'
#' @param res_gda MCA result.
#' @param df_var_quali crossed variable data frame.
#' @param var_quali name of quali variable.
#' @param title plot title.
#' @param scale_mean_points scale points (boolean).
#' @param axes which axis to plot.
#' @param palette colour palette (boolean).
#' @param impute impute missing data (boolean).
#' @param cloud which cloud should be plotted (string: real, fitted, both, deviation)
#' @param myriad use Myriad Pro font (boolean).
#'
#' @return ggplot2 visualization of additive cloud.
#' @export
fviz_gda_structure <- function(res_gda, df_var_quali, var_quali, title = "MCA quali structure effects",
                               scale_mean_points = TRUE, axes = 1:2, palette = "Set1", impute = TRUE,
                               myriad = TRUE, cloud = "both") {

  # Check GDA result
  if(!inherits(res_gda, c("MCA"))) stop("GDA result have to be MCA results.")

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Berechnung der passiven Variable durchführen
  res_quali <- supvar_stats(res_gda, df_var_quali, var_quali, impute)

  # Eigenwerte extrahieren, um Variablen skalieren zu können
  eigenvalues <- .get_eigenvalues(res_gda)

  # Punkte ohne Interaktion berechnen
  # Kreuzsstatistiken berechnen
  res_crossing <- supvar_crossing_stats(res_gda, df_var_quali, var_quali, impute, axes)

  # Anzahl der Modalitäten (Levels) pro Variable
  var_mod <- res_quali$coord %>% tibble::rownames_to_column() %>% separate(rowname, c("var1_mod", "var2_mod"), sep = "_") %>%
    select(var1_mod, var2_mod) %>% summarise_each(funs(n_distinct))

  var_mod_names <- res_quali$coord %>% tibble::rownames_to_column() %>%
    separate(rowname, c("var1_mod", "var2_mod"), sep = "_", remove = FALSE) %>%
    arrange(var1_mod, desc(var2_mod)) %>% .$rowname

  # Axis 1
  ref1 <- res_crossing$reg$Axis.1$coefficients[1, 1]
  df_coord1 <- c(ref1, ref1 + res_crossing$reg$Axis.1$coefficients[-c(1:var_mod$var1_mod), 1])
  for(i in 2:var_mod$var1_mod) {
    ref1 <- res_crossing$reg$Axis.1$coefficients[1, 1] + res_crossing$reg$Axis.1$coefficients[i, 1]
    df_coord1 <- c(df_coord1, ref1, ref1 + res_crossing$reg$Axis.1$coefficients[-c(1:var_mod$var1_mod), 1])
  }
  # Bezeichnungen anpassen
  df_coord1 <- data.frame(df_coord1, row.names = var_mod_names)
  colnames(df_coord1) <- paste0("Dim.", axes[1])

  # Axis 2
  ref2 <- res_crossing$reg$Axis.2$coefficients[1, 1]
  df_coord2 <- c(ref2, ref2 + res_crossing$reg$Axis.2$coefficients[-c(1:var_mod$var1_mod), 1])
  for(i in 2:var_mod$var1_mod) {
    ref2 <- res_crossing$reg$Axis.2$coefficients[1, 1] + res_crossing$reg$Axis.2$coefficients[i, 1]
    df_coord2 <- c(df_coord2, ref2, ref2 + res_crossing$reg$Axis.2$coefficients[-c(1:var_mod$var1_mod), 1])
  }
  # Bezeichnungen anpassen
  df_coord2 <- data.frame(df_coord2, row.names = var_mod_names)
  colnames(df_coord2) <- paste0("Dim.", axes[2])
  fitted_coord <- data.frame(df_coord1, df_coord2)

  # Datensatz zusammenstellen
  df_real <- data.frame(res_quali$coord, weight = res_quali$weight) %>% tibble::rownames_to_column() %>%
    select_("rowname", paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), "weight") %>%
    separate(rowname, c("var_1", "var_2"), sep = "_", remove = FALSE) %>%
    mutate(variable = "real")

  # Fitted cloud berechnen. Es können nur  Indikatortabellen verarbeitet werden, daher wird hier reskaliert!
  # Die errechneten Koordianten müssen noch an die Wolke der Kategorien angepasst werden.
  df_fitted <- data.frame(fitted_coord, weight = res_quali$weight) %>% tibble::rownames_to_column() %>%
    separate(rowname, c("var_1", "var_2"), sep = "_", remove = FALSE) %>%
    mutate(variable = "fitted") %>% mutate_each(funs(. * 1/sqrt(eigenvalues$.)), matches("Dim"))

  df_ges <- bind_rows(df_real, df_fitted)

  # Filterung je nach Auswahl
  if(cloud == "real") df_ges <- df_ges %>% filter(variable == "real")
  if(cloud == "fitted") df_ges <- df_ges %>% filter(variable == "fitted")

  # Plot
  if(inherits(res_gda, c("MCA"))) p <- factoextra::fviz_mca_ind(res_gda, label = "none", invisible = c("ind", "ind.sup"), axes.linetype = "solid", axes = axes)
  else stop("Only MCA plots are currently supported!")

  # Skalierungsdimension festlegen
  p <- p + scale_size_continuous(range = c(1, 7))

  # Pfad plotten
  if(cloud == "deviation") {

    # Real cloud (solid)
    # Punkte
    if(scale_mean_points) p <- p + geom_point(data = df_real , aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), size = "weight"), shape = 18, inherit.aes = FALSE, alpha = 0.5)
    else  p <- p + geom_point(data = df_real , aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2])), size = 4, shape = 18, inherit.aes = FALSE, alpha = 0.5)
    # Pfade
    p <- p + geom_path(data = df_real, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), group = "var_1"), alpha = 0.5)
    p <- p + geom_path(data = df_real, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), group = "var_2"), alpha = 0.5)

    # Fitted cloud (dashed)
    # Punkte
    if(scale_mean_points) p <- p + geom_point(data = df_fitted , aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), size = "weight"), shape = 18, inherit.aes = FALSE, alpha = 0.5)
    else  p <- p + geom_point(data = df_fitted , aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2])), size = 4, shape = 18, inherit.aes = FALSE, alpha = 0.5)
    # Pfade
    p <- p + geom_path(data = df_fitted, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), group = "var_1"), alpha = 0.5, linetype = "dashed")
    p <- p + geom_path(data = df_fitted, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), group = "var_2"), alpha = 0.5, linetype = "dashed")

    # Punkte beschriften
    # p <- p + ggrepel::geom_text_repel(data = df_ges, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), label = "rowname"), size = 4, inherit.aes = FALSE, family = "Myriad Pro")

    # Deviation vectors (Pfeilrichtung: von der fitted zu real)
    p <- p + geom_path(data = df_ges, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), group = "rowname"), colour = "red", arrow = arrow(length = unit(0.3, "cm")), size = 1)

  } else {
    if(scale_mean_points) p <- p + geom_point(data = df_ges , aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), size = "weight"), shape = 18, inherit.aes = FALSE)
    else  p <- p + geom_point(data = df_ges , aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2])), size = 4, shape = 18, inherit.aes = FALSE)
    # Punkte beschriften
    p <- p + ggrepel::geom_text_repel(data = df_ges, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), label = "rowname"), size = 4, inherit.aes = FALSE, family = "Myriad Pro")
    # Punkte verbinden
    p <- p + geom_path(data = df_ges, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), group = "var_1"))
    p <- p + geom_path(data = df_ges, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), group = "var_2"), linetype = "dashed")
  }
  # Beide Möglichkeiten abbilden
  if(cloud == "both") p <- p + facet_wrap(~variable)

  # Designanpassungen
  p <- p + add_theme() + ggtitle(title)

  return(p)
}
