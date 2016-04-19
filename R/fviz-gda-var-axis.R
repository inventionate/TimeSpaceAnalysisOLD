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
#' @param groups MFA group specific colours ("c"), shapes ("s") or both ("b").
#' @param textsize size of the labels.
#' @param colour_palette Specify colour brewer palette.
#'
#' @return ggplot2 visualization containing selected modalities.
#' @export
fviz_gda_var_axis <- function(res_gda, axis = 1, contrib = "auto", title = "GDA axis high contribution modalities", axes = 1:2, groups = NULL,
                              textsize = 4, colour_palette = "Dark2", myriad = TRUE) {
  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Calculate contribution criterion (Le Roux & Rouanet 2004: 372)
  criterion <- 100/(length(GDAtools::getindexcat(res_gda$call$X)) - length(get_index_mod(res_gda$call$X)))

  # Check GDA algorithm
  if(inherits(res_gda, c("MCA"))) df <- res_gda$var$contrib
  if(inherits(res_gda, c("MFA"))) df <- res_gda$quali.var$contrib

  # Auswahl festlegen
  modalities <- df %>% data.frame %>% select(ctr = matches(paste0("^Dim.", axis, "$"))) %>% add_rownames() %>%
    arrange(desc(ctr))

  if(contrib == "auto") modalities <- modalities %>% filter(ctr > criterion) %>% select(rowname) %>% data.frame
  else modalities <- modalities %>% slice(1:contrib) %>% select(rowname) %>% data.frame

  if(inherits(res_gda, c("MCA"))) p <- fviz_mca_var(res_gda, col.var = "black", repel = TRUE, select.var = list(name = modalities$rowname), axes.linetype = "solid", axes = axes)
  if(inherits(res_gda, c("MFA"))) {
    if(!is.null(groups)) {
      # Identify groups and modality coordinates.
      group_id <- get_mfa_mod_group_id(res_gda)
      modalities_coord <- res_gda$quali.var$coord %>% data.frame %>% add_rownames %>% filter(rowname %in% modalities$rowname)
      # Add group ids
      modalities_coord <- left_join(modalities_coord, group_id, by = c("rowname" = "mod")) %>% data.frame

      # Plot group specific modalities
      p <- fviz_mfa_quali_var(res_gda, label = "none", select.var = list(name = modalities$rowname), axes.linetype = "solid", axes = axes, pointsize = 0)
      if(groups == "b") p <- p + geom_point(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), colour = "group_id", shape = "group_id"), size = 3)
      if(groups == "c") p <- p + geom_point(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), colour = "group_id"), shape = 17, size = 3)
      if(groups == "s") p <- p + geom_point(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), shape = "group_id"), colour = "black", size = 3)
      if(groups %in% c("c", "b")) {
        p <- p + ggrepel::geom_text_repel(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]),
                                                                              colour = "group_id", label = factor(modalities_coord$rowname)),
                                          size = textsize, show.legend = FALSE)
      }
      if(groups == "s") {
      p <- p + ggrepel::geom_text_repel(data = modalities_coord, aes_string(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), label = factor(modalities_coord$rowname)),
                                        size = textsize, show.legend = FALSE)
      }
    }
    else {
      p <- fviz_mfa_quali_var(res_gda, col.var = "black", repel = TRUE, select.var = list(name = modalities$rowname), axes.linetype = "solid", axes = axes)
    }
  }
  p <- p + add_theme() + ggtitle(title)

  if(!is.null(groups)) {
    if(groups %in% c("c", "b")) p <- p + scale_colour_brewer(name = "Gruppen", palette = colour_palette, labels = res_gda$call$name.group, type = "qualitative")
    if(groups %in% c("s", "b")) p <- p + scale_shape_discrete(name = "Gruppen", labels = res_gda$call$name.group, solid = TRUE)
    p <- p + theme(legend.position = "bottom")
  }

  return(p)
}
