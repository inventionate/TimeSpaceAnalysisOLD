#' Title
#'
#' @return
#' @export
#'
#' @examples
fviz_gda_trajectory_ellipses <- function(res_gda, df_var_quali, var_quali, axes = 1:2, myriad = TRUE,
                                         time_point_names = NULL, ind_points = TRUE, conc = TRUE) {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Trajektoriedaten zusammenstellen
  coord_trajectory <- get_gda_trajectory(res_gda, time_point_names)
  coord_all = coord_trajectory$coord_all
  coord_mean_mass = coord_trajectory$coord_mean_mass
  time_point_names <- coord_trajectory$time_point_names

  # Datensatz für zusätzliche Variable konstruieren
  df_quali <- df_var_quali %>% data.frame %>% add_rownames(var = "id") %>% select_("id", var_quali = var_quali)
  df_base <- res_gda$call$X %>% data.frame %>% add_rownames %>% separate(rowname, c("id", "time"), sep = "_")
  df_full <- full_join(df_base, df_quali) %>% mutate_each(funs(as.factor)) %>% select(-id, -time) %>% data.frame

  # Imputation
  message("Info: Missing data will be imputed!")
  df_full_imp <- imputeMCA(df_full)

  # Datensatz um qualitative Variable ergänzen, um Gruppierungen vorzunehmen.
  coord_var_quali <- bind_cols(coord_all, data_frame(var_quali = df_full_imp$completeObs$var_quali)) %>%
    select_(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), "var_quali", "time") %>%
    group_by(Dim.1, Dim.2, var_quali, time) %>% mutate(mass = n()) %>% ungroup()

  # Mittelwerte und Gruppengewicht berechnen
  coord_mean_var_quali <- coord_var_quali %>% select(-mass) %>% group_by(time, var_quali) %>% summarise_each(funs(mean))
  coord_mass_var_quali <- coord_var_quali %>% count(var_quali, time) %>% rename(mass = n)
  coord_mean_mass_var_quali <- full_join(coord_mean_var_quali, coord_mass_var_quali)

  # Plot der Daten
  if(inherits(res_gda, c("MCA"))) p <- factoextra::fviz_mca_ind(res_gda, label = "none", invisible = c("ind", "ind.sup"), pointsize = -1, axes.linetype = "solid", axes = axes)
  else stop("Only MCA plots are currently supported!")

  # @todo die Dimensionen anpassen!!!!

  # Concentartion ellipse
  if(conc) p <- p + stat_ellipse(data = .count_distinct_ind(res_gda), aes(x = Dim.1, y = Dim.2), geom ="polygon", level = 0.8647, type = "norm", alpha = 0.1, colour = "black", linetype = "dashed", segments = 100)

  # Quali ellipses
  p <- p + stat_ellipse(data = coord_var_quali, aes(Dim.1, Dim.2, fill = time, colour = time), geom ="polygon",  type = "norm",
                        alpha = 0.15, segments = 100, level = 0.8647, linetype = "solid")

  if(ind_points) p <- p + geom_point(data = coord_var_quali, aes(Dim.1, Dim.2, colour = time, size = mass), show.legend = FALSE)

  p <- p + geom_point(data = coord_mean_mass_var_quali, aes(Dim.1, Dim.2, size = mass*1.5), colour = "black", shape = 18, show.legend = FALSE) +
    geom_point(data = coord_mean_mass_var_quali, aes(Dim.1, Dim.2, size = mass, colour = time), shape = 18, show.legend = FALSE) +
    geom_path(data = coord_mean_mass_var_quali, aes(Dim.1, Dim.2), size = 1,
              arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    facet_wrap(~var_quali)

  # Theme adaptieren
  p <- p + add_theme()

  # Beschreibung der Punkte
  p <- p + theme(legend.position = "bottom", legend.title = element_blank())

  # Ausgabe des Plots
  return(p)

}
