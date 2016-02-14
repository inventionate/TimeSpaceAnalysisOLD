#' @include get-index-mod.R
NULL
#' Extract list of MFA group indices to exclude (for sMFA).
#'
#' @param df_mfa MFA optmised data frame.
#' @param group_mfa MFA group definition.
#' @param pattern search pattern (regular expression).
#'
#' @return list with MFA group specific exclude indices.
#' @export
#'
#' @examples
excl_mfa_group <- function(df_mfa, group_mfa, pattern) {
  excl <- list()
  for(i in 1:length(group_studienalltag)) {
    if(i == 1) tmp <- get_index_mod(df_mfa[1:cumsum(group_mfa)[i]], pattern = pattern)
    else tmp <- get_index_mod(df_mfa[(cumsum(group_mfa)+1)[i-1]:cumsum(group_mfa)[i]], pattern = pattern)
    excl[[i]] <- tmp
  }
  return(excl)
}
