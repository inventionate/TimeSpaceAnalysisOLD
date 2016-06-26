#' Calculate group contributions.
#'
#' @param res_gda MCA result.
#' @param group vector containing group definition.
#' @param group_names names of the groups.
#'
#' @return list containing group results.
#' @export
gda_describe_group <- function(res_gda, group = NULL, group_names = NULL) {

  # Check GDA result
  if(!inherits(res_gda, c("MCA"))) stop("GDA result have to be MCA results.")

  # Alle Contributions abfragen
  ctr <- res_gda$var$contrib %>% data.frame %>% tibble::rownames_to_column()

  # Gruppenweise Berechnung durchführen
  ctr_group_overall <- NULL

  # Gruppennamen festlegen, falls keine angegeben
  if(is.null(group_names)) group_names <- paste0("Group_", 1:length(group))

  # Checken, on Definition korrekt war
  if(length(group) != length(group_names)) stop("Wrongt group and group name definition!")

  # Anzahl der Kategorien zählen
  df_group_names <- .get_group_names(res_gda, group, group_names)

  # Gruppennamen hinzufügen
  ctr <- ctr %>% bind_cols(., df_group_names)

  # Gruppen contrib berechnen
  ctr_group_overall <- ctr %>% group_by(group) %>%
    summarise_each(funs(sum), matches("Dim"))

  # Ausgabe
  return(ctr_group_overall)
}
