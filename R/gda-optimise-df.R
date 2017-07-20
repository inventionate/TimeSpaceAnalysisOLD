#' Optimise data frame for Geometric Data Analysis
#'
#' @param df_name name of the data frame to optimise.
#' @param mod_excl specify, which modalities should excluded.
#' @param prop_na_excl overall level to exclude specified modalities.
#'
#' @return optimised data frame.
#' @export
gda_optimise_df <- function(df_name, mod_excl = NA, prop_na_excl = 0.2) {
  # rename_na <- function(values) {
  #   values[is.na(values)] <- "Fehlender Wert"
  #   return(values)
  #   }
  # Personen mit zu vielen fehlenden Werten ausschließen (mehr als 5% der Gesamtfragenanzahl)
  df_name_na_id <- df_name %>%
    gather(variable, value, 2:ncol(df_name)) %>%
    count(questionnaire_id, value) %>%
    filter((value %in% mod_excl) & (n > (ncol(df_name)-1) * prop_na_excl))
  df_name <- df_name %>%
    filter(questionnaire_id %nin% df_name_na_id$questionnaire_id) %>%
    # mutate_each(funs(as.character)) %>%
    # mutate_each(funs(rename_na), -questionnaire_id) %>%
    mutate_all(funs(as.factor)) %>%
    # Alle NAs in Faktorlevels umwandeln. Der Gebrauch von NA ist problematisch, da dann
    # die Indixierung nicht mehr funktioniert. FactoMineR ändert nämlich das Level.
    # lapply(., function(x) addNA(x, ifany = TRUE)) %>%
    as.data.frame()
  # Fehlende Werte mit Variablennamen idnetifizieren, damit FactoMineR keine Umbenennung vornimmt.
  for (j in 1:ncol(df_name)) df_name[,j] <- as.factor(replace(as.character(df_name[,j]),is.na(df_name[,j]),paste(attributes(df_name)$names[j],"Fehlender Wert",sep="_")))
  # Questionnaire ID als Zeilenname hinzufügen und Spalte löschen
  rownames(df_name) <- df_name$questionnaire_id
  df_name <- df_name[,-1]
  return(df_name)
}
