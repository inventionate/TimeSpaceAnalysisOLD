#' Title
#'
#' @param res_mca MCA result.
#' @param axes axes selection.
#'
#' @return data frame with MCA varoable names, types, coords and eta2 values.
#' @export
get_mca_var_corr <- function(res_mca, axes = 1:2)
{

  coo <- name <- type <- NULL
  coord_actif <- res_mca$var$eta2[, axes,drop=FALSE]

  if (!is.null(res_mca$quali.sup$eta2)) coord_illu <- res_mca$quali.sup$eta2[,axes,drop=FALSE]
  if (!is.null(res_mca$quanti.sup$coord)) coord_illuq <- res_mca$quanti.sup$coord[,axes,drop=FALSE]^2

  coo <- rbind(coo,coord_actif)
  name <- c(name,rownames(coord_actif))
  type <- rep("active", length(rownames(coord_actif)))

  if (!is.null(res_mca$quali.sup$eta2)) {
    coo <- rbind(coo,coord_illu)
    name <- c(name,rownames(coord_illu))
    type <- c(type, rep("passive", length(rownames(coord_illu))))
  }
  if (!is.null(res_mca$quanti.sup$coord)) {
    coo <- rbind(coo,coord_illuq)
    name <- c(name,rownames(coord_illuq))
    type <- c(type, rep("passive", length(rownames(coord_illuq))))
  }

  eta2 <- apply(coo, 1, sum)

  return(data.frame(name, type, coo, eta2))

}
