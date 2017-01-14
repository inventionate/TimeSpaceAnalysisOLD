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
  ctr_group_axes <- NULL

  # Gruppennamen festlegen, falls keine angegeben
  if(is.null(group_names)) group_names <- paste0("Group_", seq_along(group))

  # Checken, on Definition korrekt war
  if(length(group) != length(group_names)) stop("Wrongt group and group name definition!")

  # Anzahl der Kategorien zählen
  df_group_names <- .get_group_names(res_gda, group, group_names)

  # Gruppennamen hinzufügen
  ctr <- ctr %>% bind_cols(., df_group_names)

  # Gruppen contrib berechnen
  ctr_group_axes <- ctr %>% group_by(group) %>%
    summarise_each(funs(sum), matches("Dim"))

  # Gesamtvarianz und Beitrag der einzelnen Gruppen berechnen

  # Anzahl der Kategorien
  cat <- nrow(res_gda$var$coord)

  # Anzahl der Variablen
  var <- ncol(res_gda$call$X)

  # Beitrag pro Variable berechnen
  var_car_n <- res_gda$call$X %>% mutate_all(funs(nlevels)) %>% distinct

  ctr_var_overall <- var_cat_n %>% mutate_all(funs( (.-1)/(cat-var)*100 )) %>% gather("cat", "ctr") %>% as_tibble

  # Beitrag pro Gruppe berechnen
  ctr_group_overall <- add_column(ctr_var_overall, group = rep(group_names, group)) %>% group_by(group) %>% summarise_at(vars(ctr), sum)

  # Round values
  ctr_var_overall <- ctr_var_overall %>% mutate_at(vars(ctr), funs(round(., digits = 2)))

  ctr_group_overall <- ctr_group_overall %>% mutate_at(vars(ctr), funs(round(., digits = 2)))

  ctr_group_axes <- ctr_group_axes %>% mutate_at(vars(matches("Dim.")), funs(round(., digits = 2)))

  # Ausgabe
  return(list(overall_variance_group_ctr = ctr_group_overall, overall_variance_var_ctr = ctr_var_overall, dim_variance_group_ctr = ctr_group_axes))
}
