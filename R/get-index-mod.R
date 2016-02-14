#' Exctract index of specific modalities.
#'
#' @param df_gda GDA optimised data frame.
#' @param pattern search pattern (regular expression).
#'
#' @return indices of modalities.
#' @export
#'
#' @examples
get_index_mod <- function(df_gda, pattern = "Fehlender Wert") {
  sort(as.numeric(c(which(grepl(pattern, colnames(dichotom(df_gda)))))))
}
