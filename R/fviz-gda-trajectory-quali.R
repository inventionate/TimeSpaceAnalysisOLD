#' @include utilities.R
#' @include add-theme.R
#' @include get-gda-trajectory.R
NULL
#' Visualize trajectories and structuring factors.
#'
#' @param res_gda MCA result (rownames have to be questionnaire IDs including time number, e.g. 87654_1).
#' @param df_var_quali data frame containing one qualitative variable (with IDs as rownames).
#' @param var_quali name of the structuring variable.
#' @param axes the axes to plot.
#' @param myriad use Myriad Pro font (boolean).
#' @param labels plot labels (boolean).
#' @param select select vector of names, within_inertia of individuals selection (within_inertia: vector containing the number of high variation and low variationindividuals) or case (vector containing NULL, complete, or incomplete).
#' @param title the plot title.
#' @param time_point_names vector containing the name of the time points.
#'
#' @return ggplot2 visualization.
#' @export
fviz_gda_trajectory_quali <- function(res_gda, df_var_quali, var_quali, axes = 1:2, myriad = TRUE, labels = FALSE,
                                      title = "Trajectory individuals structuring factors plot", time_point_names = NULL,
                                      select = list(name = NULL, within_inertia = NULL, case = NULL)) {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Trajektoriedaten zusammenstellen
  coord_trajectory <- get_gda_trajectory(res_gda, time_point_names)
  coord_all = coord_trajectory$coord_all
  time_point_names <- coord_trajectory$time_point_names

  # Datensatz für zusätzliche Variable konstruieren
  df_quali <- df_var_quali %>% data.frame %>% add_rownames(var = "id") %>% select_("id", var_quali = var_quali)
  df_base <- res_gda$call$X %>% data.frame %>% add_rownames %>% separate(rowname, c("id", "time"), sep = "_", fill = "right")
  df_full <- full_join(df_base, df_quali) %>% mutate_each(funs(as.factor)) %>% select(-id, -time) %>% data.frame

  # Imputation
  message("Info: Missing data will be imputed!")
  df_full_imp <- imputeMCA(df_full)

  # Datensatz um qualitative Variable ergänzen, um Gruppierungen vorzunehmen.
  coord_var_quali <- bind_cols(coord_all, data_frame(var_quali = df_full_imp$completeObs$var_quali))

  # Auswahl vornehmen
  selected_ind <- .select_trajectory(coord_all, select, time_point_names, axes)

  # Filterung vornehmen
  coord_ind_timeseries <-  coord_var_quali %>% filter(id %in% selected_ind$id)

  # Plot der Daten
  if(inherits(res_gda, c("MCA"))) p <- factoextra::fviz_mca_ind(res_gda, label = "none", invisible = c("ind", "ind.sup"), pointsize = -1, axes.linetype = "solid", axes = axes)
  else stop("Only MCA plots are currently supported!")

  p <- p + scale_colour_brewer(palette = "YlGnBu", direction = -1) +
    geom_point(data = coord_ind_timeseries, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2])), colour = "black", size = 4) +
    geom_point(data = coord_ind_timeseries, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), colour = "time"), size = 2.5) +
    geom_path(data = coord_ind_timeseries, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), group = "id"), size = 1,
              arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    ggtitle(title) +
    xlab(paste0("Achse ", axes[1], "(", round(res_gda$eig$`percentage of variance`[axes[1]], 1), "%)")) +
    ylab(paste0("Achse ", axes[2], "(", round(res_gda$eig$`percentage of variance`[axes[2]], 1), "%)"))

  # Labeln
  if(labels) {
    p <- p + ggrepel::geom_label_repel(data = coord_ind_timeseries %>% filter(time == time_point_names[1]),
                                       aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), colour = "time", label = "id"))
  }

  # Aufteilen
  p <- p + facet_wrap(~var_quali)

  # Theme adaptieren
  p <- p + add_theme()

  # Beschreibung der Punkte
  p <- p + theme(legend.position = "bottom", legend.title = element_blank())

  # Ausgabe des Plots
  return(p)

}
