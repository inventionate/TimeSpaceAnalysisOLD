#' Calculate axis contributions.
#'
#' @param res_gda MCA result.
#' @param axis which axis to calculate.
#' @param contrib "auto" calculates the optimal modalities to show (based on the basic criterion). Otherwise define an amount of modalities to plot.
#'
#' @return list containing axis contribution results.
#' @export
gda_describe_axis <- function(res_gda, axis = 1, contrib = "auto") {

  # Check GDA result
  if(!inherits(res_gda, c("MCA"))) stop("GDA result have to be MCA results.")

  # Calculate contribution criterion (Le Roux & Rouanet 2004: 372)
  if(is.null(res_gda$call$excl)) criterion <- 100/(length(GDAtools::getindexcat(res_gda$call$X)))
  else criterion <- 100/(length(GDAtools::getindexcat(res_gda$call$X)[-res_gda$call$excl]))

  # Alle Contributions abfragen
  ctr <- res_gda$var$contrib %>% data.frame %>% tibble::rownames_to_column()

  # Contrib exportieren und sortieren
  ctr_axis <- ctr %>% select(rowname, ctr = matches(paste0("^Dim.", axis, "$"))) %>%
    arrange(desc(ctr))

  # Auswahl der contrib vornehmen
  if(contrib == "auto") ctr_axis <- ctr_axis %>% filter(ctr > criterion)
  else ctr_axis <- ctr_axis %>% slice(1:contrib)

  # Die entsprechenden Koordinaten extrahieren
  coord_axis <- res_gda$var$coord %>% data.frame %>% tibble::rownames_to_column() %>%
    filter(rowname %in% ctr_axis$rowname) %>% select(rowname, coord = matches(paste0("^Dim.", axis, "$")))

  # Datensatz Kategorien ctr
  df_category <- left_join(ctr_axis, coord_axis, by = "rowname") %>% rename(category = rowname) %>% arrange(desc(coord))

  # Datensatz Dimensionen ctr
  df_dims <- df_category %>% mutate(coord = ifelse(coord < 0, "negative", "positive")) %>%
    group_by(coord) %>% summarise_at(vars(matches("ctr")), funs(sum))

  # Datensatz angezeigte Kategorien gesamt ctr
  df_total <- df_dims %>% ungroup %>% summarise(ctr = sum(ctr))

  # Ausgabe
  res <- list(category = df_category, dims = df_dims, total = df_total)
  return(res)
}
