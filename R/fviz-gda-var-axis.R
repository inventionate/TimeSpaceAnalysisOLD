#' @include utilities.R
#' @include get-index-mod.R
#' @include get-mfa-mod-group-id.R
NULL
#' Visualize specific contributing modalities.
#'
#' @param res_gda GDA result data frame.
#' @param axis dimension to be filtered.
#' @param contrib "auto" calculates the optimal modalities to show (based on the basic criterion). Otherwise define an amount of modalities to plot.
#' @param title plot title.
#' @param axes the GDA dimensions to plot.
#' @param myriad use Myriad Pro font (boolean).
#' @param group vector containing group definition.
#' @param group_names names of the groups.
#' @param group_style style to plot (vector containing "shape", "colour" or "both).
#' @param textsize size of the text.
#' @param colour_palette name of the used colour palette.
#' @param individuals show individual points/ biplot (boolean).
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param individuals_size set individual point size manual or "auto".
#' @param individuals_alpha set alpha value.
#' @param individuals_names plot individual names (boolean).
#' @param axis_lab_name name of axis label.
#' @param group_lab_name name of variable groups.
#'
#' @return ggplot2 visualization containing selected modalities.
#' @export
fviz_gda_var_axis <- function(res_gda, axis = 1, contrib = "auto", title = "GDA axis high contribution modalities", axes = 1:2,
                              group = NULL, group_names = NULL, group_style = "both", textsize = 4, colour_palette = "Set1",
                              individuals = FALSE, individuals_size = "auto", individuals_alpha = 0.5, individuals_names = FALSE,
                              myriad = TRUE, plot_modif_rates = TRUE, axis_lab_name = "Achse", group_lab_name = "Themengruppen") {
  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Calculate contribution criterion (Le Roux & Rouanet 2004: 372)
  if(is.null(res_gda$call$excl)) criterion <- 100/(length(GDAtools::getindexcat(res_gda$call$X)))
  else criterion <- 100/(length(GDAtools::getindexcat(res_gda$call$X)[-res_gda$call$excl]))

  # Check GDA algorithm
  if(inherits(res_gda, c("MCA"))) df <- res_gda$var$contrib
  else stop("Only MCA plots are currently supported!")

  # Auswahl festlegen
  modalities <- df %>% data.frame %>% select(ctr = matches(paste0("^Dim.", axis, "$"))) %>% tibble::rownames_to_column() %>%
    arrange(desc(ctr))

  if(contrib == "auto") modalities <- modalities %>% filter(ctr > criterion) %>% select(rowname) %>% data.frame
  else modalities <- modalities %>% slice(1:contrib) %>% select(rowname) %>% data.frame

  if(inherits(res_gda, c("MCA"))) {

    if(!is.null(group)) {

      # Gruppennamen festlegen, falls keine angegeben
      if(is.null(group_names)) group_names <- paste0("Group_", seq_along(group))

      # Checken, on Definition korrekt war
      if(length(group) != length(group_names)) stop("Wrongt group and group name definition!")

      # Anzahl der Kategorien zählen
      df_group_names <- .get_group_names(res_gda, group, group_names)

      # Zeilennamen hinzufügen
      if(is.null(res_gda$call$excl)) col_weight <- res_gda$call$marge.col
      else col_weight <- res_gda$call$marge.col[-res_gda$call$excl]

      modalities_coord <- res_gda$var$coord %>% data.frame %>%
        tibble::rownames_to_column() %>% bind_cols(., df_group_names, data.frame(weight = col_weight * res_gda$call$N)) %>%
        filter(rowname %in% modalities$rowname)

      # Plot
      p <- fviz_mca_var(res_gda, label = "none", select.var = list(name = modalities$rowname), axes.linetype = "blank", axes = axes,  pointsize = 0) +
        geom_hline(yintercept = 0, colour = "gray70", linetype = "solid") + geom_vline(xintercept = 0, colour = "gray70", linetype = "solid")

      if(individuals) {
        if(individuals_size == "auto") {
          p <- p + geom_point(data = .count_distinct_ind(res_gda, axes, modalities_coord$weight) %>% distinct(), aes(x, y, size = count), inherit.aes = FALSE, alpha = individuals_alpha)
        } else {
          p <- p + geom_point(data = .count_distinct_ind(res_gda, axes) %>% distinct(), aes(x, y), size = individuals_size, inherit.aes = FALSE, alpha = individuals_alpha)
        }
      }
      if(individuals_names) p <- p + ggrepel::geom_label_repel(data = .count_distinct_ind(res_gda, axes), aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), label = rownames(.count_distinct_ind(res_gda, axes))), colour = "black", size = individuals_size, alpha = individuals_alpha)
      if(group_style == "both") p <- p + geom_point(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), colour = "group", shape = "group", size = "weight"))
      if(group_style == "colour") p <- p + geom_point(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), colour = "group", size = "weight"), shape = 17)
      if(group_style == "shape") p <- p + geom_point(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), shape = "group", size = "weight"), colour = "black")
      if(group_style %in% c("colour", "both")) {
        p <- p + ggrepel::geom_text_repel(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]),
                                                                              colour = "group", label = factor(modalities_coord$rowname)),
                                          size = textsize, show.legend = FALSE)
      }
      if(group_style == "shape") {
        p <- p + ggrepel::geom_text_repel(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), label = factor(modalities_coord$rowname)),
                                          size = textsize, show.legend = FALSE)
      }

    } else {
      # Plot group specific modalities
      p <- fviz_mca_var(res_gda, col.var = "black", repel = TRUE, select.var = list(name = modalities$rowname), axes.linetype = "blank", axes = axes) +
        geom_hline(yintercept = 0, colour = "gray70", linetype = "solid") + geom_vline(xintercept = 0, colour = "gray70", linetype = "solid")
    }
  }

  p <- add_theme(p) + ggtitle(title)

  # Legende für Größen ausblenden
  p <- p + scale_size(guide = FALSE)

  if(!is.null(group_style) & !is.null(group)) {
    if(group_style %in% c("colour", "both")) p <- p + scale_colour_brewer(name = glue("{group_lab_name}"), palette = colour_palette, labels = modalities_coord %>% select(group) %>% distinct, type = "qualitative")
    if(group_style %in% c("shape", "both")) p <- p + scale_shape(name =glue("{group_lab_name}"), labels = modalities_coord %>% select(group) %>% distinct, solid = TRUE)
    p <- p + theme(legend.position = "bottom")
  }

  # Beschriftung anpassen
  p <- .gda_plot_labels(res_gda, p, title, axes, plot_modif_rates, axis_lab_name = axis_lab_name)

  # Plotten
  return(p)
}
