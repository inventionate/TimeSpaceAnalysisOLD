#' @include fviz-gda-conc-ellipse.R
NULL
#' Add supplementary individuals.
#'
#' @param res_gda GDA results.
#' @param excl Exclude modalities (list).
#' @param sup_ind supplementary individual profiles (data.frame).
#' @param colour colour of point and labels.
#' @param label label names.
#' @param size label size.
#'
#' @return ggplot2 visalization of supplementary individuals.
#' @export
fviz_add_sup_ind <- function(res_gda, excl = NULL, sup_ind = NULL, colour = "red",
                             label = NULL, size = 10, myriad = TRUE) {

  # Datensatz vorbereiten
  colnames(sup_ind) <- colnames(res_gda$call$X)
  res_gda_added <- rbind(res_gda$call$X, sup_ind)

  # MFA mit passiven Individuen berechnen
  res_sup_ind <- MFA(res_gda_added, group = res_gda$call$group, type = res_gda$call$type,
                     name.group = res_gda$call$name.group, excl = excl, res_gda$call$ncp,
                     graph = FALSE, ind.sup = (nrow(res_gda$call$X) + 1):nrow(res_gda_added))

  # Koordinaten extrahieren
  res_sup_ind_coord <- data.frame(res_sup_ind$ind.sup$coord)

  # Repräsentation extrahieren
  # res_sup_ind_coord <- data.frame(res_sup_ind$ind.sup$cos2)

  # Label bezeichnen
  if(is.null(label)) label_names = rownames(res_sup_ind_coord)
  else label_names = label

  # Plotten
  p <- fviz_gda_conc_ellipse(res_gda, myriad = myriad) +
    # geom_point(data = res_sup_ind_coord, aes(Dim.1, Dim.2), size = point_size,
    #            shape = shape, inherit.aes = FALSE, colour = "red") +
    geom_label(data = res_sup_ind_coord, aes(Dim.1, Dim.2), inherit.aes = FALSE,
               size = size, colour = "red", label = label_names)

  return(p)
}
