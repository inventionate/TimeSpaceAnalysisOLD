#' Extract coords of categories to concat.
#'
#' @param res_gda_quali GDA result.
#' @param var variable names.
#' @param var_levels variable categories to concat.
#' @param exclude categories to exclude.
#'
#' @return data frame with path coords.
#' @export
get_path_coord <- function(res_gda_quali, var, var_levels = NULL, exclude = NULL) {
  df_var <- data.frame(res_gda_quali$quali.var$coord) %>% add_rownames() %>% filter(grepl(var, rowname))
  if(!is.null(exclude)) df_var <- df_var %>% filter(!grepl(exclude, rowname))
  if(!is.null(var_levels))df_var <- df_var %>% mutate(rowname = factor(rowname, levels = var_levels)) %>% arrange(rowname)
  return(df_var)
}
