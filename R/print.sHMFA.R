#' Print sHMFA summary.
#'
#' @param x
#' @param file
#' @param sep
#' @param ...
#'
#' @return List of sHMFA results.
#' @export
#'
#' @examples
print.spHMFA <- function (x, file = NULL, sep = ";", ...){
  res.shmfa <- x
  if (!inherits(res.shmfa, "spHMFA")) stop("non convenient data")
  cat("**Results of the Specific Hierarchical Multiple Factor Analysis (spHMFA)**\n\n")
  cat("There are", nrow(res.shmfa$ind$coord), "individuals\n\n")
  cat("*Results are available in the following objects :\n\n")
  res <- array("", c(22, 2), list(1:22, c("name", "description")))
  res[1, ] <- c("$eig", "eigenvalues")
  res[2, ] <- c("$group", "results for all the groups")
  res[3, ] <- c("$ind", "results for the individuals")
  res[4, ] <- c("$partial", "partial coordinates for the individuals")
  indice <- 4
  if (!is.null(res.shmfa["quanti.var"]$quanti.var)){
    indice <- indice + 1
    res[indice, ] <- c("$quanti.var", "results for the quantitative variables")
  }
  if (!is.null(res.shmfa["quali.var"]$quali.var)){
    indice <- indice + 1
    res[indice, ] <- c("$quali.var", "results for the categorical variables")
  }
  print(res[1:indice,])
  if (!is.null(file)) {
    write.infile(res.shmfa,file = file, sep=sep)
    print(paste("All the results are in the file",file))
  }
}
