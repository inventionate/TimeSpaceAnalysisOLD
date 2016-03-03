#' Calculate results for supplementary variables.
#'
#' @param res_gda GDA result.
#' @param df_var_quali data frame of one quali variable.
#' @param var_quali_name name if quali variable.
#'
#' @return Returns a list:
#' \item{weight}{numeric vector of categories weights}
#' \item{cord}{data frame of categories coordinates}
#' \item{cos2}{data frame of categories square cosine}
#' \item{var}{data frame of categories within variances, variance between and within categories and variable square correlation ratio (eta2)}
#' \item{v.test}{data frame of categories test-values}
#' @export
#'
#' @examples
supvar_stats <- function(res_gda, df_var_quali, var_quali_name) {


  # Hier umdenken und eher ganze MCA oder MFA rechnen (ausgehend vom call, um an die entsprechenden Werte zu kommen. Vielleicht macht das dann
  # aber wenig Sinn, als Zusatzfunktion?)

  var_names <- rownames(res_gda$call$X)
  df_source <- data.frame(allgemeine_angaben, df_var_quali)
  var_quali <- df_source %>%
    select(which(names(df_source) %in% c("questionnaire_id", var_quali_name))) %>%
    filter(questionnaire_id %in% var_names)

  # Auf fehlende Werte prüfen
  # exclude_na <- which(is.na(var_quali[,2]))
  # Datensätze zusammenstellen
  #if(length(exclude_na) == 0) df_source_na <- data.frame(res_gda$ind$coord, var_quali = var_quali[,2])
  #else df_source_na <- data.frame(res_gda$ind$coord, var_quali = var_quali[,2])[-exclude_na,]

  df_source_na <- data.frame(res_gda$ind$coord, var_quali = as.factor(replace(as.character(var_quali[,2]),is.na(var_quali[,2]),paste(attributes(var_quali)$names[2],"Fehlender Wert",sep="_"))))

  var <- df_source_na$var_quali

  n <- sum(res_gda$call$row.w)
  FK <- colSums(res_gda$call$row.w*(dichotom(as.data.frame(factor(var)),out='numeric')))/n
  v <- factor(var)
  wt <- res_gda$call$row.w

  ind <- df_source_na[,1:res_gda$call$ncp]

  coord <- aggregate(wt*ind,list(v),sum)[,-1]/n/FK
  vrc <- aggregate(wt*ind*ind,list(v),sum)[,-1]/n/FK-coord*coord
  if(inherits(res_gda, c("MCA", "sMCA"))) for(i in 1:res_gda$call$ncp) coord[,i] <- coord[,i]/res_gda$svd$vs[i]
  if(inherits(res_gda, c("MFA", "sMFA"))) for(i in 1:res_gda$call$ncp) coord[,i] <- coord[,i]
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
  list(weight=round(weight,1),coord=coord,cos2=round(cos2,6),var=round(vrc,6),v.test=round(v.test,6))
}
