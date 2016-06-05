#' Calculate results for supplementary variables.
#'
#' @param res_gda GDA result.
#' @param var_quali_df the supplementary data frame.
#' @param var_quali supplementary variable name (string).
#' @param impute impute missing data (boolean).
#'
#' @return Returns a list:
#' \item{weight}{numeric vector of categories weights}
#' \item{cord}{data frame of categories coordinates}
#' \item{cos2}{data frame of categories square cosine}
#' \item{var}{data frame of categories within variances, variance between and within categories and variable square correlation ratio (eta2)}
#' \item{v.test}{data frame of categories test-values}
#' \item{supvar}{vector of the supplementary variable categories}
#' @export
supvar_stats <- function(res_gda, var_quali_df, var_quali, impute = TRUE) {

  # Datensatz auslesen
  var <- var_quali_df %>% select_(var_quali) %>% data.frame %>% mutate_each(funs(as.character))

  # Check, ob es fehlende Werte gibt und ggf. imputieren
  if( length(which(is.na(var))) != 0 & impute ) {

    message("Info: Missing data will be imputed!")

    var <- var %>% mutate_each(funs(as.factor))

    if(inherits(res_gda, c("MCA"))) {

      var_impute <- missMDA::imputeMCA(data.frame(res_gda$call$X, var))

    }

    if(inherits(res_gda, c("MFA"))) {

      var_impute <- missMDA::imputeMFA(data.frame(res_gda$call$X, var),
                       c(res_gda$call$group, 1),
                       res_gda$call$ncp,
                       c(res_gda$call$type, "n"))

    }

    var <- var_impute$completeObs[var_quali]
  }

  # Spalte in Vektor umwandeln
  var <- var[,1]

  # Fehlende Werte durch Kategorie ersetzen (falls nicht imputiert wurde).
  var[is.na(var)] <- "Fehlender Wert"

  # Adaptiert von GDAtools.
  n <- sum(res_gda$call$row.w)
  FK <- colSums(res_gda$call$row.w*(dichotom(as.data.frame(factor(var)),out='numeric')))/n
  v <- factor(var)
  wt <- res_gda$call$row.w
  # Hier direkt alle Individuen aus dem GDA Ergebnis
  ind <- data.frame(res_gda$ind$coord[,1:res_gda$call$ncp])
  coord <- aggregate(wt*ind,list(v),sum)[,-1]/n/FK
  vrc <- aggregate(wt*ind*ind,list(v),sum)[,-1]/n/FK-coord*coord
  if(inherits(res_gda, c("MCA"))) for(i in 1:res_gda$call$ncp) coord[,i] <- coord[,i]/res_gda$svd$vs[i]
  if(inherits(res_gda, c("MFA"))) for(i in 1:res_gda$call$ncp) coord[,i] <- coord[,i]
  cos2 <- coord*coord/((1/FK)-1)
  weight=n*FK
  names(weight) <- levels(v)# v au lieu de var
  rownames(coord) <- levels(v)#[as.numeric(table(v))>0]
  rownames(cos2) <- levels(v)#[as.numeric(table(v))>0]
  wi <- apply(vrc,2,weighted.mean,w=weight) # Die within variance entspricht dem gewichteten Mittelwert der Unterpunktwolken (vgl. Le Roux/Rouanet 2004: 103)
  be <- res_gda$eig[[1]][1:res_gda$call$ncp]-wi
  eta2 <- be/res_gda$eig[[1]][1:res_gda$call$ncp]
  vrc <- rbind(vrc,wi,be,res_gda$eig[[1]][1:res_gda$call$ncp],eta2)
  vrc <- round(vrc,6)
  rownames(vrc) <- c(levels(v),'within','between','total','eta2')
  coord <- round(coord,6)
  v.test <- sqrt(cos2)*sqrt(length(v)-1)
  v.test <- (((abs(coord)+coord)/coord)-1)*v.test
  list(supvar=var,weight=round(weight,1),coord=coord,cos2=round(cos2,6),var=round(vrc,6),v.test=round(v.test,6))
}
