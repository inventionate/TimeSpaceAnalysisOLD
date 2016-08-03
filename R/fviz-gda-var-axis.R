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
#'
#' @return ggplot2 visualization containing selected modalities.
#' @export
fviz_gda_var_axis <- function(res_gda, axis = 1, contrib = "auto", title = "GDA axis high contribution modalities", axes = 1:2,
                              group = NULL, group_names = NULL, group_style = "both", textsize = 4, colour_palette = "Dark2",
                              myriad = TRUE) {
  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Calculate contribution criterion (Le Roux & Rouanet 2004: 372)
  if(is.null(res_gda$call$excl)) criterion <- 100/(length(GDAtools::getindexcat(res_gda$call$X)))
  else criterion <- 100/(length(GDAtools::getindexcat(res_gda$call$X)[-res_gda$call$excl]))

  # Check GDA algorithm
  if(inherits(res_gda, c("MCA"))) df <- res_gda$var$contrib
  if(inherits(res_gda, c("MFA"))) df <- res_gda$quali.var$contrib

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
      p <- fviz_mca_var(res_gda, label = "none", select.var = list(name = modalities$rowname), axes.linetype = "solid", axes = axes,  pointsize = 0)
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
      p <- fviz_mca_var(res_gda, col.var = "black", repel = TRUE, select.var = list(name = modalities$rowname), axes.linetype = "solid", axes = axes)
    }
  }
  if(inherits(res_gda, c("MFA"))) {
    group_names <- res_gda$call$name.group

    if(!is.null(group_style)) {
      # Identify groups and modality coordinates.
      group_id <- get_mfa_mod_group_id(res_gda)
      modalities_coord <- res_gda$quali.var$coord %>% data.frame %>% tibble::rownames_to_column() %>% filter(rowname %in% modalities$rowname)
      # Add group ids
      modalities_coord <- left_join(modalities_coord, group_id, by = c("rowname" = "mod")) %>% data.frame

      # Plot group specific modalities
      p <- fviz_mfa_quali_var(res_gda, label = "none", select.var = list(name = modalities$rowname), axes.linetype = "solid", axes = axes, pointsize = 0)
      if(group_style == "both") p <- p + geom_point(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), colour = "group_id", shape = "group_id", size = "weight"))
      if(group_style == "colour") p <- p + geom_point(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), colour = "group_id", size = "weight"), shape = 17)
      if(group_style == "shape") p <- p + geom_point(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), shape = "group_id", size = "weight"), colour = "black")
      if(group_style %in% c("colour", "both")) {
        p <- p + ggrepel::geom_text_repel(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]),
                                                                              colour = "group_id", label = factor(modalities_coord$rowname)),
                                          size = textsize, show.legend = FALSE)
      }
      if(group_style == "shape") {
      p <- p + ggrepel::geom_text_repel(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), label = factor(modalities_coord$rowname)),
                                        size = textsize, show.legend = FALSE)
      }
    }
    else {
      p <- fviz_mfa_quali_var(res_gda, col.var = "black", repel = TRUE, select.var = list(name = modalities$rowname), axes.linetype = "solid", axes = axes)
    }
  }
  p <- p + add_theme() + ggtitle(title)

  # Legende für Größen ausblenden
  p <- p + scale_size(guide = FALSE)

  if(!is.null(group_style) & !is.null(group)) {
    if(group_style %in% c("colour", "both")) p <- p + scale_colour_brewer(name = "Gruppen", palette = colour_palette, labels = modalities_coord %>% select(group) %>% distinct, type = "qualitative")
    if(group_style %in% c("shape", "both")) p <- p + scale_shape_discrete(name = "Gruppen", labels = modalities_coord %>% select(group) %>% distinct, solid = TRUE)
    p <- p + theme(legend.position = "bottom")
  }

  return(p)
}
