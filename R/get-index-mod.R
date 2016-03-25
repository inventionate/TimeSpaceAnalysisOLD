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
  res <- sort(as.numeric(c(which(grepl(pattern, colnames(GDAtools::dichotom(df_gda)))))))
  if(nrow(data.frame(res)) == 0) res <- NULL
  return(res)
}
