#' Optimise data frame for Geometric Data Analysis
#'
#' @param df_name name of the data frame to optimise.
#' @param mod_excl specify, which modalities should excluded.
#' @param prop_na_excl overall level to exclude specified modalities.
#' @param rename_na rename NA with label
#'
#' @return optimised data frame.
#' @export
gda_optimise_df <- function(df_name, mod_excl = NA, prop_na_excl = 0.2, rename_na = FALSE) {

  # Identify cases (more than 5% NA over all variables)
  df_name_na_id <- df_name %>%
    rownames_to_column("id") %>%
    gather(variable, value, !! 2:ncol(df_name)) %>%
    count(id, value) %>%
    filter((value %in% mod_excl) & (n > (ncol(df_name)-1) * prop_na_excl))

  # Remove cases
  df_name <- df_name %>%
    rownames_to_column("id") %>%
    filter(id %nin% df_name_na_id$id) %>%
    mutate_all(funs(as.factor)) %>%
    as_tibble()

  # FactoMineR compatibility (rename missing values)
  if( rename_na ) for (j in 1:ncol(df_name)) df_name[,j] <- as.factor(replace(as.character(df_name[,j]),is.na(df_name[,j]),paste(attributes(df_name)$names[j],"Fehlender Wert",sep="_")))

  return(df_name)
}
