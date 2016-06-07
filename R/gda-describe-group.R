#' Calculate group contributions.
#'
#' @param res_gda MCA result.
#' @param group vector containing group definition.
#' @param group_names names of the groups.
#' @param group_style style to plot (vector containing "shape", "colour" or "both).
#'
#' @return list containing group results.
#' @export
gda_describe_group <- function(res_gda, group = NULL, group_names = NULL, group_style = "both") {

  # Check GDA result
  if(!inherits(res_gda, c("MCA"))) stop("GDA result have to be MCA results.")

  # Alle Contributions abfragen
  ctr <- res_gda$var$contrib %>% data.frame %>% add_rownames

  # Gruppenweise Berechnung durchführen
  ctr_group_overall <- NULL

  # Gruppennamen festlegen, falls keine angegeben
  if(is.null(group_names)) group_names <- paste0("Group_", 1:length(group))

  # Checken, on Definition korrekt war
  if(length(group) != length(group_names)) stop("Wrongt group and group name definition!")

  # Anzahl der Kategorien zählen
  if(is.null(res_gda$call$excl)) var_num <- getindexcat(res_gda$call$X)
  else var_num <- getindexcat(res_gda$call$X)[-res_gda$call$excl]
  var_num <- var_num %>%
    data_frame(var.cat = .) %>% separate(var.cat, c("var", "cat"), sep = "[.]") %>%
    select(var) %>% count(var)
  var <- data_frame(var = colnames(res_gda$call$X))
  n_mod <- left_join(var, var_num) %>% .$n
  # n_mod <- res_gda$call$X %>% mutate_each(funs(n_distinct)) %>% distinct

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

  # Ausgabe
  return(ctr_group_overall)
}
