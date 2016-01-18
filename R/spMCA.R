#' Performs a Specific Multiple Correspondance Analysis.
#'
#' @param X
#' @param ncp
#' @param excl
#' @param ind.sup
#' @param quanti.sup
#' @param quali.sup
#' @param graph
#' @param level.ventil
#' @param axes
#' @param row.w
#' @param method
#' @param na.method
#' @param tab.disj
#'
#' @return Returns a list of spMCA results.
#' @export
#'
#' @examples
spMCA <- function (X, ncp = 5, excl = NULL, ind.sup = NULL, quanti.sup = NULL, quali.sup = NULL,
                 graph = TRUE, level.ventil = 0, axes = c(1, 2), row.w = NULL,
                 method="Indicator",na.method="NA",tab.disj=NULL){

  ############
  ventil.tab <- function (tab, level.ventil=0.05,row.w=NULL,ind.sup=NULL,quali.sup=NULL,quanti.sup=NULL) {
    if (is.null(row.w)) row.w <- rep(1,nrow(tab)-length(ind.sup))
    col.var <- 1:ncol(tab)
    if (!is.null(c(quali.sup,quanti.sup))) col.var = col.var[-c(quali.sup,quanti.sup)]
    for (i in col.var) {
      if (is.factor(tab[,i])){
        tab[,i] <- ventilation(tab[,i],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
      }
      if (is.ordered(tab[,i])){
        tab[,i] <- ventilation.ordonnee(tab[,i],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
      }
    }
    return(tab)
  }

  ventilation <- function(Xqual,level.ventil=0.05,row.w=NULL,ind.sup=NULL) {
    if (!is.factor(Xqual)) stop("Xqual should be a factor \n")
    modalites <- levels(Xqual)
    if (length(modalites)<=1) stop("not enough levels \n")
    if (is.null(ind.sup)) {
      ind.act <- (1:length(Xqual))
    } else {ind.act <- (1:length(Xqual))[-ind.sup]}
    tabl <- table(Xqual[ind.act])
    if (!is.null(row.w)){
      for (j in 1:nlevels(Xqual)) tabl[j] <- sum((Xqual[ind.act]==levels(Xqual)[j])*row.w,na.rm=TRUE)
    }
    selecti <- (tabl/sum(tabl,na.rm=TRUE))< level.ventil
    if (sum(selecti)==length(modalites)) return(Xqual)

    if (!any(selecti)) return(Xqual) else {
      lesquels <- modalites[!selecti]
      #  if (length(lesquels)==1) return(NULL) else {
      if (length(lesquels)==1) return(Xqual) else {
        prov <- factor(Xqual[(Xqual%in%lesquels)],levels=lesquels)
        prov <- table(prov)
        proba <- prov/sum(prov)

        for (j in modalites[selecti]) {
          Xqual[which(Xqual==j)] <- sample(lesquels,sum(Xqual==j,na.rm=TRUE), replace=TRUE,prob=proba)
        }
        Xqualvent <- factor(as.character(Xqual))
      }
    }
    return(Xqualvent)
  }

  ventilation.ordonnee <- function(Xqual,level.ventil=0.05,ind.sup=NULL,row.w=NULL) {
    if (!is.ordered(Xqual)) stop("Xqual must be ordered \n")
    mod <- levels(Xqual)
    if (length(mod)<=1) stop("not enough levels \n")
    if (is.null(ind.sup)) {
      ind.act <- (1:length(Xqual))
    } else {ind.act <- (1:length(Xqual))[-ind.sup]}
    tabl <- table(Xqual[ind.act])
    if (!is.null(row.w)){
      for (j in 1:nlevels(Xqual)) tabl[j] <- sum((Xqual[ind.act]==levels(Xqual)[j])*row.w,na.rm=TRUE)
    }
    selecti <- (tabl/sum(tabl))<level.ventil
    if (!any(selecti)) return(Xqual) else {
      numero <- which(selecti)
      while(any((tabl/sum(tabl))<level.ventil)) {
        j <- which(((tabl/sum(tabl))<level.ventil))[1]
        K <- length(mod)
        if (j<K) {
          if ((j>1)&(j<K-1)) levels(Xqual) <- c(mod[1:(j-1)],paste(mod[j],mod[j+1],sep="."),paste(mod[j],mod[j+1],sep="."),mod[j+2:K])
          if (j==1) levels(Xqual) <- c(paste(mod[j],mod[j+1],sep="."),paste(mod[j],mod[j+1],sep="."),mod[j+2:K])
          if (j==(K-1)) levels(Xqual) <- c(mod[1:(j-1)],paste(mod[j],mod[j+1],sep="."),paste(mod[j],mod[j+1],sep="."))
        } else {
          levels(Xqual) <- c(mod[1:(j-2)],paste(mod[j-1],mod[j],sep="."),paste(mod[j-1],mod[j],sep="."))
        }
      }
    }
    # if (nlevels(Xqual)>1) return(Xqual)
    # else return(NULL)
    return(Xqual)
  }

  # fct.eta2 <- function(vec,x,weights) {
  # res <- summary(lm(x~vec,weights=weights))$r.squared
  # }


  fct.eta2 <- function(vec,x,weights) {   ## pb avec les poids
    VB <- function(xx) {
      return(sum((colSums((tt*xx)*weights)^2)/ni))
    }
    tt <- tab.disjonctif(vec)
    ni <- colSums(tt*weights)
    unlist(lapply(as.data.frame(x),VB))/colSums(x^2*weights)
  }

  # @info GDA Extensions aus Nicolas Robettes GDAtools Paket.
  # Mit dieser Funktion lassen sich modified rates berechnen, die Anhaltspunkte für
  # die Wahl der zu interpretierende Achsen geben
  modif.rate <- function(resmca) {
    Q <- ncol(resmca$call$X)
    seuil <- 1/Q
    e <- resmca$eig[[1]][resmca$eig[[1]]>=seuil]
    pseudo <- (Q/(Q-1)*(e-seuil))^2
    mrate <- round(pseudo/sum(pseudo)*100,2)
    cum.mrate <- cumsum(mrate)
    return(data.frame(mrate,cum.mrate))
  }

  ##################
  ## Main program ##
  ##################

  if (is.null(rownames(X))) rownames(X) <- 1:nrow(X)
  if (is.null(colnames(X))) colnames(X) <- colnames(X, do.NULL = FALSE,prefix="V")
  X <- as.data.frame(X)
  X <- droplevels(X)
  ind.act <- (1:nrow(X))[!(1:nrow(X))%in%ind.sup]

  if (!is.null(which(lapply(X,class)=="logical"))){
    for (k in which(lapply(X,class)=="logical")) X[,k] <- as.factor(X[,k])
  }

  if (level.ventil > 0) X <- ventil.tab(X,level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup,quali.sup=quali.sup,quanti.sup=quanti.sup)

  niveau <- NULL
  for (j in 1:ncol(X)) niveau = c(niveau, levels(X[, j]))
  for (j in 1:ncol(X)) {
    if (sum(niveau %in% levels(X[, j])) != nlevels(X[, j])) levels(X[, j]) = paste(attributes(X)$names[j], levels(X[, j]), sep = "_")
  }

  nonact <- c(quanti.sup,quali.sup)
  if (!is.null(nonact)) act <- (1:ncol(X))[-nonact]
  else act <- (1:ncol(X))
  Z <- tab.disjonctif(X[, act,drop=FALSE])
  if (any(is.na(X[,act]))){
    if (is.null(tab.disj)){
      if (na.method=="Average") {
        tab.disj <- tab.disjonctif.prop(X[ind.act, act],row.w=row.w)
        Z[ind.act,] <- tab.disj
      }
      if (na.method=="NA"){
        warnings('Missing values for one variable are considered as a new category; you can use method="Average" or use the imputeMCA function of the missMDA package')
        for (j in act) X[,j] <- as.factor(replace(as.character(X[,j]),is.na(X[,j]),paste(attributes(X)$names[j],".NA",sep="")))
        Z <- tab.disjonctif(X[, act])
      }
    } else Z[ind.act,] <- tab.disj
  }
  Ztot <- Z

  col.sup <- NULL
  if (!is.null(quali.sup)){
    if (any(is.na(X[,quali.sup,drop=FALSE]))){
      for (j in quali.sup) X[,j] <- as.factor(replace(as.character(X[,j]),is.na(X[,j]),paste(attributes(X)$names[j],".NA",sep="")))
    }
    X[,quali.sup] <- ventil.tab(X[,quali.sup,drop=FALSE],level.ventil=level.ventil,row.w=row.w,ind.sup=ind.sup)
    Zqs <- tab.disjonctif(X[, quali.sup])
    Ztot <- cbind.data.frame(Z, Zqs)
    col.sup <- (ncol(Z) + 1):ncol(Ztot)
  }
  Xact <- X[,act]

  if (!is.null(quanti.sup)){
    if (any(is.na(X[,quanti.sup,drop=FALSE]))){
      for (j in quanti.sup) X[,j] <- replace(X[,j],is.na(X[,j]), mean(X[,j], na.rm=TRUE))
    }
    X.quanti.sup <- as.matrix(X[, quanti.sup])
    if (!is.null(ind.sup)) X.quanti.sup <- X.quanti.sup[ind.act, ,drop=FALSE]
    colnames(X.quanti.sup) = attributes(X)$names[quanti.sup]
  }

  if (is.null(row.w)) row.w = rep(1, nrow(X) - length(ind.sup))
  if (length(row.w) != nrow(X) - length(ind.sup)) stop("length of vector row.w should be the number of active rows")
  if (tolower(method)=="burt") {  ## boucle utile pour calculer la distance au cdg et pour calculer les cos2
    # @info Specific MCA Anpassungen
    res.mca <- spCA(Ztot, excl = excl, ncp = ncol(Z)-length(act), row.sup = ind.sup, col.sup = col.sup, graph = FALSE, row.w = row.w)
    res.mca$col$coord <- t(t(res.mca$col$coord)*sqrt(res.mca$eig[1:ncol(res.mca$col$coord),1]))
    auxil <- rowSums(res.mca$col$coord^2)
    if (!is.null(col.sup)){
      res.mca$col.sup$coord <- t(t(res.mca$col.sup$coord)*sqrt(res.mca$eig[1:ncol(res.mca$col.sup$coord),1]))
      auxil2 <- rowSums(res.mca$col.sup$coord^2)
    }
  }
  #    res.mca <- CA(Ztot, ncp = ncol(Z)-length(act), row.sup = ind.sup, col.sup = col.sup, graph = FALSE, row.w = row.w)
  # @info Specific MCA Anpassungen
  # Die auszuschließenden Antwortmodalitäten werden komplett gelöscht
  res.mca <- spCA(Ztot, excl = excl, ncp = min(ncp,ncol(Z)-length(act)), row.sup = ind.sup, col.sup = col.sup, graph = FALSE, row.w = row.w)
  if (is.null(ncol(res.mca$row$coord))) res.mca$row$coord = matrix(res.mca$row$coord,ncol=1)
  ncp <- ncol(res.mca$row$coord)
  res.mca$call$X <- X
  res.mca$call$ind.sup = ind.sup
  res.mca$call$quali = (1:ncol(X))
  if (!is.null(quali.sup) | !is.null(quanti.sup)) res.mca$call$quali <- res.mca$call$quali[-c(quali.sup, quanti.sup)]
  res.mca$call$quali.sup = quali.sup
  res.mca$call$quanti.sup = quanti.sup
  res.mca$call$row.w = row.w
  if (!is.null(excl)) res.mca$call$excl = excl
  res.mca$call$call <- match.call()
  #	res.mca$call$call <- sys.calls()[[1]]
  if (length(act)>1) res.mca$eig <- res.mca$eig[1:min(length(ind.act)-1,sum(sapply(Xact,nlevels))-length(act)),]
  else res.mca$eig <- res.mca$eig[1:(nlevels(Xact)-1),]
  names(res.mca)[3] <- "ind"
  res.mca$ind <- res.mca$ind[1:3]
  names(res.mca$ind) <- c("coord", "contrib", "cos2")
  names(res.mca)[4] <- "var"
  if (tolower(method)=="burt"){
    res.mca$var$coord <- t(t(res.mca$var$coord)*sqrt(res.mca$eig[1:ncol(res.mca$var$coord),1]))
    res.mca$var$cos2 <- res.mca$var$coord^2/auxil
  }
  res.mca$var <- res.mca$var[1:3]
  names(res.mca$var) <- c("coord", "contrib", "cos2")
  indice <- 6
  if (!is.null(ind.sup)) {
    names(res.mca)[indice] <- "ind.sup"
    names(res.mca$ind.sup) <- c("coord", "cos2")
    indice <- indice + 1
    Xact = X[ind.act,act ,drop=FALSE]
  }
  if (!is.null(quali.sup)) {
    names(res.mca)[indice] <- "quali.sup"
    names(res.mca$quali.sup) <- c("coord", "cos2")
    if (tolower(method)=="burt"){
      res.mca$quali.sup$coord <- t(t(res.mca$quali.sup$coord)*sqrt(res.mca$eig[1:ncol(res.mca$quali.sup$coord),1]))
      res.mca$quali.sup$cos2 <- res.mca$quali.sup$coord^2/auxil2
    }
  }

  if (!is.null(ind.sup)) Z = Z[ind.act, ]
  Nj <- colSums(Z * row.w)
  N <- sum(Nj)/(ncol(X) - length(quali.sup) - length(quanti.sup))
  if (N>1) coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
  else coef <- sqrt(Nj)
  res.mca$var$v.test <- as.matrix(res.mca$var$coord*coef)

  # if (ncp>1) eta2 <- t(sapply(Xact,fct.eta2,res.mca$ind$coord,weights=row.w))
  # else {
  # eta2 <- as.matrix(sapply(Xact,fct.eta2,res.mca$ind$coord,weights=row.w),ncol=ncp)
  # colnames(eta2) = paste("Dim", 1:ncp)
  # rownames(eta2) = colnames(Xact)
  # }

  variable <- rep(attributes(Xact)$names,unlist(lapply(Xact,nlevels)))
  if (length(act)>1){
    CTR <- aggregate(res.mca$var$contrib/100,by=list(factor(variable)),FUN=sum)
    rownames(CTR) <- CTR[,1]
    CTR <- t(t(CTR[,-1,drop=FALSE])*res.mca$eig[1:ncp,1])*ncol(Xact)
    eta2 <- CTR[attributes(Xact)$names,,drop=FALSE]
    res.mca$var$eta2 <- eta2
  }

  if (!is.null(quali.sup)) {
    if (!is.null(ind.sup)) Zqs = Zqs[ind.act, ]
    Nj <- colSums(Zqs * row.w)
    if (N>1) coef <- sqrt(Nj * ((N - 1)/(N - Nj)))
    else coef <- sqrt(Nj)
    res.mca$quali.sup$v.test <- as.matrix(res.mca$quali.sup$coord*coef)

    eta2 = matrix(NA, length(quali.sup), ncp)
    colnames(eta2) = paste("Dim", 1:ncp)
    rownames(eta2) = attributes(X)$names[quali.sup]
    #        for (i in 1:ncp)  eta2[, i] <- unlist(lapply(as.data.frame(X[rownames(Xact), quali.sup]),fct.eta2,res.mca$ind$coord[,i],weights=row.w))
    if (ncp>1) eta2 <- t(sapply(as.data.frame(X[rownames(Xact), quali.sup,drop=FALSE]),fct.eta2,res.mca$ind$coord,weights=row.w))
    else {
      eta2 <- as.matrix(sapply(as.data.frame(X[rownames(Xact), quali.sup,drop=FALSE]),fct.eta2,res.mca$ind$coord,weights=row.w),ncol=ncp)
    }
    # @info Varianzberechnungen in Anlehnung an (M)ANOVA verfahren.
    # Der Algorithmus ist eine Adaption aus Nicolas Robettes GDAtools "varsup" Funktion.
    v_n <- sum(row.w)
    v_var <- X[quali.sup]
    # Varianzen für die einzelnen Variablen berechnen
    v_vrc_g <- NULL
    v_wi_g <- v_be_g <- v_tot <- matrix(nrow = ncol(v_var), ncol = ncp)
    for(i in 1:ncol(v_var)) {
      v_FK <- colSums(row.w*(dichotom(as.data.frame(factor(v_var[[i]])),out='numeric')))/v_n
      v_v <- factor(v_var[[i]])
      v_wt <- row.w
      v_ind <- res.mca$ind$coord
      v_coord <- aggregate(v_wt*v_ind,list(v_v),sum)[,-1]/v_n/v_FK
      v_vrc <- aggregate(v_wt*v_ind*v_ind,list(v_v),sum)[,-1]/v_n/v_FK-v_coord*v_coord
      v_vrc_g <- rbind(v_vrc_g, round(v_vrc, 6))
      for(j in 1:res.mca$call$ncp) v_coord[,j] <- v_coord[,j]/res.mca$svd$vs[j]
      v_cos2 <- v_coord*v_coord/((1/v_FK)-1)
      v_weight = v_n * v_FK
      names(v_weight) <- levels(v_v)
      rownames(v_coord) <- levels(v_v)
      rownames(v_cos2) <- levels(v_v)
      # within variance
      v_wi <- apply(v_vrc,2,weighted.mean,w=v_weight)
      v_wi_g[i,] <- round(v_wi, 6)
      # between variance
      v_be <- res.mca$eig[[1]][1:res.mca$call$ncp] - v_wi
      v_be_g[i,] <- round(v_be, 6)
      # total variance
      v_tot[i,] <- round(res.mca$eig[[1]][1:res.mca$call$ncp], 6)
    }
    mod_names <- NULL
    for (j in 1:ncol(v_var)) {
      mod_names = c(mod_names, paste(colnames(v_var)[j], levels(v_var[, j]), sep = "_"))
    }
    colnames(v_vrc_g) = paste("Dim", 1:ncp)
    rownames(v_vrc_g) = mod_names
    colnames(v_wi_g) = paste("Dim", 1:ncp)
    rownames(v_wi_g) = attributes(X)$names[quali.sup]
    colnames(v_be_g) = paste("Dim", 1:ncp)
    rownames(v_be_g) = attributes(X)$names[quali.sup]
    colnames(v_tot) = paste("Dim", 1:ncp)
    rownames(v_tot) = attributes(X)$names[quali.sup]
    # modality variance
    res.mca$quali.sup$variance <- v_vrc_g
    # within variance
    res.mca$quali.sup$within <- v_wi_g
    # between variance
    res.mca$quali.sup$between <- v_be_g
    # total variance
    res.mca$quali.sup$total <- v_tot
    # eta2
    res.mca$quali.sup$eta2 <- eta2
  }

  if (!is.null(quanti.sup)) {
    U <- res.mca$svd$U
    coord.quanti.sup <- matrix(NA, ncol(X.quanti.sup), ncp)
    coord.quanti.sup <- cov.wt(cbind.data.frame(U,X.quanti.sup),cor=TRUE,wt=row.w,method="ML")$cor[-(1:ncol(U)),1:ncol(U),drop=FALSE]
    #		coord.quanti.sup <- cor(X.quanti.sup,U,method="pearson")
    dimnames(coord.quanti.sup) <- list(colnames(X.quanti.sup), paste("Dim", 1:ncp))
    res.mca$quanti.sup$coord <- coord.quanti.sup
  }

  if (tolower(method)=="burt"){
    res.mca$eig[,1] <- res.mca$eig[,1]^2
    res.mca$eig[,2] <- res.mca$eig[,1]/sum(res.mca$eig[,1]) * 100
    res.mca$eig[,3] <- cumsum(res.mca$eig[,2])
  }

  # @info Specific MCA Anpassungen
  # Die Ergebnisse der ausgeschlossenen Antwortmodalitäten ausschließen
  res.mca$var$coord <- res.mca$var$coord[-excl,]
  res.mca$var$contrib <- res.mca$var$contrib[-excl,]
  res.mca$var$cos2 <- res.mca$var$cos2[-excl,]
  res.mca$var$v.test <- res.mca$var$v.test[-excl,]
  res.mca$var$eta2 <- res.mca$var$eta2[-excl,]

  # @info modif.rates der Ausgabe hinzufügen
  data.mrate <- modif.rate(res.mca)
  mrate <- rep(0, nrow(res.mca$eig))
  mrate[1:nrow(data.mrate)] <- data.mrate$mrate
  mrate <- as.data.frame(mrate)
  colnames(mrate) <- "modified rates"
  cum.mrate <- rep(100, nrow(res.mca$eig))
  cum.mrate[1:nrow(data.mrate)] <- data.mrate$cum.mrate
  cum.mrate <- as.data.frame(cum.mrate)
  colnames(cum.mrate) <- "cumulative modified rates"
  res.mca$eig <- cbind(res.mca$eig, mrate, cum.mrate)

  class(res.mca) <- c("spMCA", "list")
  if (graph & (ncp>1)) {
    plot.MCA(res.mca, choix = "ind", invisible="ind", axes = axes,new.plot=TRUE)
    if (method=="Indicator") plot.MCA(res.mca, choix = "ind", invisible=c("var","quali.sup","quanti.sup"), axes = axes,new.plot=TRUE,cex=0.8)
    plot.MCA(res.mca, choix = "var", axes = axes,new.plot=TRUE)
    if (!is.null(quanti.sup)) plot.MCA(res.mca, choix = "quanti.sup", axes = axes,new.plot=TRUE)
  }
  return(res.mca)
}
