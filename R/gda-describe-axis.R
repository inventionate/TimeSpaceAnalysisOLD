#' Calculate group and dimension contributions.
#'
#' @param res_gda MCA result.
#' @param axis which axis to calculate.
#' @param contrib "auto" calculates the optimal modalities to show (based on the basic criterion). Otherwise define an amount of modalities to plot.
#' @param group vector containing group definition.
#' @param group_names names of the groups.
#' @param group_style style to plot (vector containing "shape", "colour" or "both).
#'
#' @return list containing group and axis results.
#' @export
gda_describe_axis <- function(res_gda, axis = 1, contrib = "auto", group = NULL, group_names = NULL, group_style = "both") {

  # Check GDA result
  if(!inherits(res_gda, c("MCA"))) stop("GDA result have to be MCA results.")

  # Calculate contribution criterion (Le Roux & Rouanet 2004: 372)
  criterion <- 100/(length(GDAtools::getindexcat(res_gda$call$X)) - length(get_index_mod(res_gda$call$X)))

  # Alle Contributions abfragen
  ctr <- res_gda$var$contrib %>% data.frame %>% add_rownames

  # Gruppenweise Berechnung durchführen
  ctr_group_overall <- NULL
  if(!is.null(group)) {

    # Gruppennamen festlegen, falls keine angegeben
    if(is.null(group_names)) group_names <- paste0("Group_", 1:length(group))

    # Checken, on Definition korrekt war
    if(length(group) != length(group_names)) stop("Wrongt group and group name definition!")

    # Anzahl der Kategorien zählen
    n_mod <- res_gda$call$X %>% mutate_each(funs(n_distinct)) %>% distinct

    n_mod_group <- NULL
    start <- 0
    for(i in 1:length(group)) {
      n_mod_group <- c(n_mod_group, sum( n_mod[(start + 1):(start + group[i])] ) )
      start <- sum( group[1:i] )
    }

    # Gruppenzuordnung der Modalitäten
    df_group_names <- data.frame(group = rep(group_names, n_mod_group))

    # Gruppennamen hinzufügen
    ctr <- ctr %>% bind_cols(., df_group_names)

    # Gruppen contrib berechnen
    ctr_group_overall <- ctr %>% group_by(group) %>%
      summarise_each(funs(sum), matches("Dim"))
  }

  # Achsenweise Berechnungen

  # Contrib exportieren und sortieren
  ctr_axis <- ctr %>% select(rowname, ctr = matches(paste0("^Dim.", axis, "$"))) %>%
    arrange(desc(ctr))

  # Auswahl der contrib vornehmen
  if(contrib == "auto") ctr_axis <- ctr_axis %>% filter(ctr > criterion)
  else ctr_axis <- ctr_axis %>% slice(1:contrib)

  # Die entsprechenden Koordinaten extrahieren
  coord_axis <- res_gda$var$coord %>% data.frame %>% add_rownames %>%
    filter(rowname %in% ctr_axis$rowname) %>% select(rowname, coord = matches(paste0("^Dim.", axis, "$")))

  # Datensatz Kategorien ctr
  df_category <- left_join(ctr_axis, coord_axis, by = "rowname") %>% rename(category = rowname) %>% arrange(desc(coord))

  # Datensatz Dimensionen ctr
  df_dims <- df_category %>% mutate(coord = ifelse(coord < 0, "negative", "positive")) %>%
    group_by(coord) %>% summarise_each(funs(sum), matches("ctr"))

  # Datensatz angezeigte Kategorien gesamt ctr
  df_total <- df_dims %>% ungroup %>% summarise(ctr = sum(ctr))

  # Ausgabe
  res <- list(group = ctr_group_overall, axis = list(category = df_category, dims = df_dims, total = df_total))
  return(res)
}
