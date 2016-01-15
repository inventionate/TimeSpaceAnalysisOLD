#' Print method for an object of class factoextra
#' 
#' @description
#' Print method for an object of class factoextra
#' @param x an object of class factoextra
#' @param ... further arguments to be passed to print method
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @author Fabian Mundt \email{f.mundt@@inventionate.de}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#'  data(iris)
#'  res.pca <- princomp(iris[, -5],  cor = TRUE)
#'  ind <- get_mfa_ind(res.pca, data = iris[, -5])
#'  print(ind)
#'  }
#'  
#' @export
print.factoextra<-function(x, ...){
  if(!inherits(x, "factoextra"))
    stop("Can't handle data of class ", class(x))
  
  # v-test hinzufügen
  
  if(inherits(x, "mfa_ind")){
    cat("Multiple Factor Analysis Results for individuals\n",
        "===================================================\n")
    res <- array(data="", dim=c(3,2), dimnames=list(1:3, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for the individuals")
    res[2, ] <- c("$cos2", "Cos2 for the individuals")
    res[3, ] <- c("$contrib", "contributions of the individuals")
    print(res[1:3,], ...)
  }
  else if(inherits(x, "mfa_quali_var")){
    cat("Multiple Factor Analysis Results for qualitative variables\n",
        "===================================================\n")
    res <- array(data="", dim=c(3,2), dimnames=list(1:3, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for categories")
    res[2, ] <- c("$cos2", "Cos2 for categories")
    res[3, ] <- c("$contrib", "contributions of categories")
    print(res[1:3,])
  }
  else if(inherits(x, "mfa_quanti_var")){
    cat("Multiple Factor Analysis Results for quantitative variables\n",
        "===================================================\n")
    res <- array(data="", dim=c(3,2), dimnames=list(1:3, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for the individuals")
    res[2, ] <- c("$cos2", "Cos2 for the individuals")
    res[3, ] <- c("$contrib", "contributions of the individuals")
    print(res[1:3,], ...)
  }
  
  # Correlation Koeffizienten hinzufügen
  else if(inherits(x, "mfa_group")){
    cat("Multiple Factor Analysis Results for groups\n",
        "===================================================\n")
    res <- array(data="", dim=c(4,2), dimnames=list(1:4, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for categories")
    res[2, ] <- c("$cos2", "Cos2 for categories")
    res[3, ] <- c("$contrib", "contributions of categories")
    res[4, ] <- c("$correlation", "correlation between each group and each factor")
    print(res[1:4,])
  }
  else if(inherits(x, "mfa_partial_axes")){
    cat("Multiple Factor Analysis Results for partial axes\n",
        "===================================================\n")
    res <- array(data="", dim=c(3,2), dimnames=list(1:3, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for categories")
    res[2, ] <- c("$cos2", "Cos2 for categories")
    res[3, ] <- c("$contrib", "contributions of categories")
    print(res[1:3,])
  }
  
}
