#' Calculate modified rates
#'
#' @param mca_res FactoMineR MCA object.
#'
#' @return Modified rates as tibble.
#' @export
#'
#' @examples
modified_rates <- function(mca_res) {
  Q <- ncol(mca_res$call$X)
  seuil <- 1/Q
  e <- data.frame(mca_res$eig)[[1]][data.frame(mca_res$eig)[[1]] >= seuil]
  pseudo <- (Q/(Q - 1) * (e - seuil))^2
  mrate <- round(pseudo/sum(pseudo) * 100, 2)
  cum_mrate <- cumsum(mrate)
  tibble(mod_rates = mrate, cum_mod_rates = cum_mrate)
}
