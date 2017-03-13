#' Calculate group contributions.
#'
#' @param res_gda MCA result.
#' @param group vector containing group definition.
#' @param group_names names of the groups.
#' @param excl vector indicating excluded modalities.
#'
#' @return list containing group results.
#' @export
gda_describe_group <- function(res_gda, group = NULL, group_names = NULL, excl = NULL) {

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
  ctr_group_axes <- ctr %>% group_by(group) %>% summarise_at(vars(matches("Dim")), funs(sum))

  # Gesamtvarianz und Beitrag der einzelnen Gruppen berechnen

  # Anzahl der Kategorien
  cat <- nrow(res_gda$var$coord)

  # Anzahl der Variablen
  var <- ncol(res_gda$call$X)

  # Different Kategorien und Variablen
  diff_cat_var <- cat - var

  # Gruppe der Variable
  group_var <- as_tibble(list(var = rownames(res_gda$var$eta2))) %>% mutate_all(funs( sub("\\.", "_", .) )) %>% add_column(group = rep(group_names, group)) %>% mutate_all(funs(as.factor))

  # Beitrag pro Variable berechnen
   #res_gda$call$X %>% mutate_all(funs(nlevels)) %>% distinct

  data <- res_gda$call$X %>% mutate_all(funs( sub("\\.", "_", .) ))
  colnames(data) <- colnames(data) %>% sub("\\.", "_", .)

  if( is.null(excl) ) var_cat_mod <- GDAtools::getindexcat(data)
  else var_cat_mod <- GDAtools::getindexcat(data)[-excl]

  var_cat_n <- var_cat_mod %>% as_tibble %>% separate(value, c("var", "mod"), sep="\\.") %>% mutate_all(funs(as.factor)) %>%
    count(var) %>% full_join(group_var, by="var")

  # Beitrag pro Variable berechnen
  ctr_var_overall <- var_cat_n %>% mutate( ctr = (n-1)/(diff_cat_var)*100 ) %>% rename(mod_n = n)

  # Beitrag pro Gruppe berechnen
  ctr_group_overall <- ctr_var_overall %>% group_by(group) %>% summarise_at(vars(ctr), sum)

  # Round values
  ctr_var_overall <- ctr_var_overall %>% mutate_at(vars(ctr), funs(round(., digits = 2)))

  ctr_group_overall <- ctr_group_overall %>% mutate_at(vars(ctr), funs(round(., digits = 2)))

  ctr_group_axes <- ctr_group_axes %>% mutate_at(vars(matches("Dim.")), funs(round(., digits = 2)))

  # Ausgabe
  return(list(overall_variance_group_ctr = ctr_group_overall, overall_variance_var_ctr = ctr_var_overall, dim_variance_group_ctr = ctr_group_axes))
}
