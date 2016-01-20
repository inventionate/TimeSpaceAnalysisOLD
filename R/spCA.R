#' Performs a specific Principal Component Analysis
#'
#' @param X
#' @param scale.unit
#' @param ncp
#' @param ind.sup
#' @param quanti.sup
#' @param structuring.factors
#' @param quali.sup
#' @param row.w
#' @param col.w
#' @param graph
#' @param axes
#'
#' @return Returns a list of sPCA results.
#' @export
#'
#' @examples
sPCA <- function (X, scale.unit = TRUE, ncp = 5, ind.sup = NULL, quanti.sup = NULL, structuring.factors = NULL,
                 quali.sup = NULL, row.w = NULL, col.w = NULL, graph = TRUE, axes = c(1, 2))
{
     # moy.p <- function(V, poids) {
     #     res <- sum(V * poids)/sum(poids)
     # }
     #
  moy.ptab <- function(V, poids) {
    #        res <- colSums(V * (poids/sum(poids)))
    as.vector(crossprod(poids/sum(poids),as.matrix(V)))
  }

  #    ec <- function(V, poids) {
  #        res <- sqrt(sum(V^2 * poids)/sum(poids))
  #    }

  ec.tab <- function(V, poids) {
    #        res <- sqrt(colSums(V^2 * poids)/sum(poids))
    ecart.type <- sqrt(as.vector(crossprod(poids/sum(poids),as.matrix(V^2))))
    ecart.type[ecart.type <= 1e-16] <- 1
    return(ecart.type)
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

  X <- as.data.frame(X)
  X <- droplevels(X)
  if (any(is.na(X))) {
    warning("Missing values are imputed by the mean of the variable: you should use the imputePCA function of the missMDA package")
    if (is.null(quali.sup))
      X[is.na(X)] = matrix(colMeans(X,na.rm=TRUE),ncol=ncol(X),nrow=nrow(X),byrow=TRUE)[is.na(X)]
    else for (j in (1:ncol(X))[-quali.sup]) X[, j] <- replace(X[, j], is.na(X[, j]), mean(X[, j], na.rm = TRUE))
  }
  Xtot <- X
  if (!is.null(quali.sup))
    X <- X[, -quali.sup,drop=FALSE]
  if (any(!sapply(X, is.numeric))) {
    auxi = NULL
    for (j in 1:ncol(X)) if (!is.numeric(X[, j]))
      auxi = c(auxi, colnames(X)[j])
    stop(paste("\nThe following variables are not quantitative: ", auxi))
  }
  todelete <- c(quali.sup, quanti.sup)
  if (!is.null(todelete)) X <- Xtot[, -todelete,drop=FALSE]
  if (!is.null(ind.sup)) {
    X.ind.sup <- X[ind.sup, , drop = FALSE]
    X <- X[-ind.sup, , drop = FALSE]
  }
  ncp <- min(ncp, nrow(X) - 1, ncol(X))
  if (is.null(row.w)) row.w <- rep(1, nrow(X))
  row.w.init <- row.w
  row.w <- row.w/sum(row.w)
  if (is.null(col.w)) col.w <- rep(1, ncol(X))
  centre <- moy.ptab(X,row.w)
  data <- X
  X <- t(t(as.matrix(X))-centre)
  if (is.null(attributes(X)$row.names)) rownames(X) <- rownames(data)
  if (is.null(attributes(X)$names)) colnames(X) <- colnames(data)
  if (scale.unit) {
    ecart.type <- ec.tab(X,row.w)
    X <- t(t(X)/ecart.type)
  }
  else ecart.type <- rep(1, length(centre))
  dist2.ind <- rowSums(X^2*sqrt(col.w))
  dist2.ind <- as.vector(tcrossprod(as.matrix(X^2*sqrt(col.w)),t(rep(1,ncol(X)))))
  dist2.var <- as.vector(crossprod(rep(1,nrow(X)),as.matrix(X^2*row.w)))
  res.call <- list(row.w = (row.w/sum(row.w)), col.w = col.w,
                   scale.unit = scale.unit, ncp = ncp, centre = centre,
                   ecart.type = ecart.type, X = Xtot, row.w.init = row.w.init,call=match.call())
  tmp <- svd.triplet(X, row.w = row.w, col.w = col.w,ncp=ncp)
  # @info sMCA Adaption
  # Das Gewicht der ausgeklammerten Kategorien auf 0 setzen.
  col.w[col.w == 1e-15 ] <- 0
  eig <- tmp$vs^2
  vp <- as.data.frame(matrix(NA, length(eig), 3))
  rownames(vp) <- paste("comp", 1:length(eig))
  colnames(vp) <- c("eigenvalue", "percentage of variance",
                    "cumulative percentage of variance")
  vp[, "eigenvalue"] <- eig
  vp[, "percentage of variance"] <- (eig/sum(eig)) * 100
  vp[, "cumulative percentage of variance"] <- cumsum(vp[, "percentage of variance"])
  V <- tmp$V
  U <- tmp$U
  eig <- eig[1:ncp]
  coord.ind <- t(t(as.matrix(U))*sqrt(eig))
  coord.var <- t(t(as.matrix(V))*sqrt(eig))
  contrib.var <- t(t(coord.var^2)/eig)*col.w
  dist2 <- dist2.var
  cor.var <- coord.var/sqrt(dist2)
  cos2.var <- cor.var^2
  rownames(coord.var) <- rownames(cos2.var) <- rownames(cor.var) <- rownames(contrib.var) <- colnames(X)
  colnames(coord.var) <- colnames(cos2.var) <- colnames(cor.var) <- colnames(contrib.var) <- paste("Dim",
                                                                                                   c(1:ncol(V)), sep = ".")
  res.var <- list(coord = coord.var[, 1:ncp], cor = cor.var[,
                                                            1:ncp], cos2 = cos2.var[, 1:ncp], contrib = contrib.var[,
                                                                                                                    1:ncp] * 100)
  dist2 <- dist2.ind
  cos2.ind <- coord.ind^2/dist2
  contrib.ind <- t(t(coord.ind^2*row.w/sum(row.w))/eig)
  rownames(coord.ind) <- rownames(cos2.ind) <- rownames(contrib.ind) <- names(dist2) <- rownames(X)
  colnames(coord.ind) <- colnames(cos2.ind) <- colnames(contrib.ind) <- paste("Dim",
                                                                              c(1:ncol(U)), sep = ".")
  res.ind <- list(coord = coord.ind[, 1:ncp,drop=FALSE], cos2 = cos2.ind[,
                                                                         1:ncp,drop=FALSE], contrib = contrib.ind[, 1:ncp,drop=FALSE] * 100, dist = sqrt(dist2))
  res <- list(eig = vp, var = res.var, ind = res.ind, svd = tmp)
  if (!is.null(ind.sup)) {
    if (is.null(ecart.type)) ecart.type <- rep(1, length(centre))
    X.ind.sup <- t(t(as.matrix(X.ind.sup))-centre)
    X.ind.sup <- t(t(X.ind.sup)/ecart.type)
    coord.ind.sup <- t(t(X.ind.sup)*col.w)
    coord.ind.sup <- crossprod(t(coord.ind.sup),tmp$V)
    dist2 <- rowSums(t(t(X.ind.sup^2)*col.w))
    cos2.ind.sup <- coord.ind.sup^2/dist2
    coord.ind.sup <- coord.ind.sup[, 1:ncp, drop = F]
    cos2.ind.sup <- cos2.ind.sup[, 1:ncp, drop = F]
    colnames(coord.ind.sup) <- colnames(cos2.ind.sup) <- paste("Dim",  c(1:ncp), sep = ".")
    rownames(coord.ind.sup) <- rownames(cos2.ind.sup) <- names(dist2) <- rownames(X.ind.sup)
    res.ind.sup <- list(coord = coord.ind.sup, cos2 = cos2.ind.sup, dist = sqrt(dist2))
    res$ind.sup = res.ind.sup
    res.call$ind.sup = ind.sup
  }
  if (!is.null(quanti.sup)) {
    X.quanti.sup <- as.data.frame(Xtot[, quanti.sup,drop=FALSE])
    if (!is.null(ind.sup)) X.quanti.sup <- as.data.frame(X.quanti.sup[-ind.sup, ,drop=FALSE])
    colnames(X.quanti.sup) <- colnames(Xtot)[quanti.sup]
    res.call$quanti.sup = X.quanti.sup
    centre.sup <- moy.ptab(X.quanti.sup,row.w)
    X.quanti.sup <- t(t(as.matrix(X.quanti.sup))-centre.sup)
    if (scale.unit) {
      ecart.type.sup <- ec.tab(X.quanti.sup, row.w)
      X.quanti.sup <- t(t(X.quanti.sup)/ecart.type.sup)
    }
    coord.vcs <- t(X.quanti.sup*row.w)
    coord.vcs <- crossprod(t(coord.vcs),tmp$U)
    col.w.vcs <- rep(1, ncol(coord.vcs))
    cor.vcs <- matrix(NA, ncol(X.quanti.sup), ncol(tmp$U))
    dist2 <- as.vector(crossprod(rep(1,nrow(X.quanti.sup)),as.matrix(X.quanti.sup^2*row.w)))
    cor.vcs <- coord.vcs/sqrt(dist2)
    cos2.vcs <- cor.vcs^2
    colnames(coord.vcs) <- colnames(cor.vcs) <- colnames(cos2.vcs) <- paste("Dim", c(1:ncol(cor.vcs)), sep = ".")
    rownames(coord.vcs) <- rownames(cor.vcs) <- rownames(cos2.vcs) <- colnames(Xtot)[quanti.sup]
    res.quanti.sup <- list(coord = coord.vcs[, 1:ncp, drop=FALSE], cor = cor.vcs[, 1:ncp, drop=FALSE], cos2 = cos2.vcs[, 1:ncp, drop=FALSE])
    res$quanti.sup = res.quanti.sup
  }
  if (!is.null(quali.sup)) {
    X.quali.sup <- as.data.frame(Xtot[, quali.sup,drop=FALSE])
    if (!is.null(ind.sup)) X.quali.sup <- as.data.frame(X.quali.sup[-ind.sup,,drop=FALSE])
    colnames(X.quali.sup) <- colnames(Xtot)[quali.sup]
    nombre <- modalite <- NULL

    # eta2 <- matrix(NA, length(quali.sup), ncp)
    # if (ncp>1){
    # for (i in 1:ncp)  eta2[, i] <- unlist(lapply(X.quali.sup,fct.eta2,res$ind$coord[,i,drop=FALSE],weights=row.w))
    # } else eta2 <- unlist(lapply(X.quali.sup,fct.eta2,res$ind$coord,weights=row.w))
    # eta2 <- as.matrix(eta2,ncol=ncp)
    # colnames(eta2) = paste("Dim", 1:ncp)
    # rownames(eta2) = colnames(X.quali.sup)


    # @info Hier umfrangreiche Möglichkeiten einer an (M)ANOVA Verfahren angelehnten Strukturierten Datenanalyse integrieren,
    # wie es auch bei der sMCA Funktion gemacht wurde (vgl. Robette GDAtools und Le Roux/Rouanet 2004: 268).

    if (ncp>1) {
      # @info Strukturierte Datenanalyse, die sich an (M)ANOVA Verfahren anlehnt.
      # Der Algorithmus ist eine Adaption aus Nicolas Robettes GDAtools "varsup" Funktion.
      v_n <- sum(row.w)
      v_var <- Xtot[quali.sup]

      # Varianzen für die einzelnen Variablen berechnen
      v_vrc_g <- NULL
      v_wi_g <- v_be_g <- v_tot <- matrix(nrow = ncol(v_var), ncol = ncp)
      for(i in 1:ncol(v_var)) {
        v_FK <- colSums(row.w*(dichotom(as.data.frame(factor(v_var[[i]])),out='numeric')))/v_n
        v_v <- factor(v_var[[i]])
        v_wt <- row.w
        v_ind <- res$ind$coord
        v_coord <- aggregate(v_wt*v_ind,list(v_v),sum)[,-1]/v_n/v_FK
        v_vrc <- aggregate(v_wt*v_ind*v_ind,list(v_v),sum)[,-1]/v_n/v_FK-v_coord*v_coord
        v_vrc_g <- rbind(v_vrc_g, round(v_vrc, 6))
        for(j in 1:ncp) v_coord[,j] <- v_coord[,j]/res$svd$vs[j]
        v_cos2 <- v_coord*v_coord/((1/v_FK)-1)
        v_weight = v_n * v_FK
        names(v_weight) <- levels(v_v)
        rownames(v_coord) <- levels(v_v)
        rownames(v_cos2) <- levels(v_v)
        # within variance
        v_wi <- apply(v_vrc,2,weighted.mean,w=v_weight)
        v_wi_g[i,] <- round(v_wi, 6)
        # between variance
        v_be <- res$eig[[1]][1:ncp] - v_wi
        v_be_g[i,] <- round(v_be, 6)
        # total variance
        v_tot[i,] <- round(res$eig[[1]][1:ncp], 6)
      }
      # modality variance
      mod_names <- colnames(Xtot[,structuring.factors[[3]]])
      v_vrc_g <- v_vrc_g[structuring.factors[[3]],]
      colnames(v_vrc_g) = paste("Dim", 1:ncp)
      rownames(v_vrc_g) = mod_names
      # within variance
      v_wi_g <- as.data.frame(v_wi_g)[c(structuring.factors[[1]]),]
      colnames(v_wi_g) = paste("Dim", 1:ncp)
      rownames(v_wi_g) = colnames(Xtot[structuring.factors[[2]]])
      # between variance
      v_be_g <- as.data.frame(v_be_g)[c(structuring.factors[[1]]),]
      colnames(v_be_g) = paste("Dim", 1:ncp)
      rownames(v_be_g) = colnames(Xtot[structuring.factors[[2]]])
      # total variance
      v_tot <- as.data.frame(v_tot)[c(structuring.factors[[1]]),]
      colnames(v_tot) = paste("Dim", 1:ncp)
      rownames(v_tot) = colnames(Xtot[structuring.factors[[2]]])
      # eta2
      eta2 <- as.data.frame(t(sapply(X.quali.sup,fct.eta2,res$ind$coord,weights=row.w)))[c(structuring.factors[[1]]),]
      colnames(eta2) = paste("Dim", 1:ncp)
      rownames(eta2) = colnames(Xtot[structuring.factors[[2]]])
    }
    else {
      warning("Bitte mehr als eine Dimension nutzen! Die eindimensionale Varianzberechnung ist noch nicht implementiert.")
      eta2 <- as.matrix(sapply(X.quali.sup,fct.eta2,res$ind$coord,weights=row.w),ncol=ncp)
      colnames(eta2) = paste("Dim", 1:ncp)
      rownames(eta2) = colnames(X.quali.sup)
    }

    for (i in 1:ncol(X.quali.sup)) {
      var <- as.factor(X.quali.sup[, i])
      n.mod <- nlevels(var)
      modalite <- c(modalite, n.mod)
      bary <- matrix(NA, n.mod, ncol(X))
      for (j in 1:n.mod) {
        ind <- levels(var)[j]
        #                bary[j, ] <- apply(data[which(var == ind), ], 2, moy.p, row.w[which(var == ind)])
        bary[j, ] <- moy.ptab(data[which(var == ind), ], row.w[which(var == ind)])
        ### modif Avril 2011
        ##                nombre <- c(nombre, length(var[which(var == ind)]))
        nombre <- c(nombre, sum(row.w.init[which(var == ind)]))
      }
      colnames(bary) <- colnames(X)
      if ((levels(var)[1] %in% (1:nrow(X))) | (levels(var)[1] %in% c("y", "Y", "n", "N"))) row.names(bary) <- paste(colnames(X.quali.sup)[i], as.character(levels(var)))
      else row.names(bary) <- as.character(levels(var))
      if (i == 1)  barycentre <- as.data.frame(bary)
      else barycentre <- rbind(barycentre, as.data.frame(bary))
    }
    bary <- t(t(as.matrix(barycentre))-centre)
    if (!is.null(ecart.type)) bary <- t(t(bary)/ecart.type)
    #        bary <- as.matrix(sweep(as.matrix(barycentre), 2, centre, FUN = "-"))
    #        if (!is.null(ecart.type)) bary <- as.matrix(sweep(as.matrix(bary), 2, ecart.type, FUN = "/"))
    dist2 <- rowSums(t(t(bary^2)*col.w))
    coord.barycentre <- t(t(bary)*col.w)
    coord.barycentre <- crossprod(t(coord.barycentre),tmp$V)
    colnames(coord.barycentre) <- paste("Dim", 1:ncol(coord.barycentre), sep = ".")
    cos2.bary.sup <- coord.barycentre^2/dist2
    vtest <- t(t(coord.barycentre)/sqrt(eig))
    if (sum(row.w.init)>1) vtest <- vtest*sqrt(nombre/((sum(row.w.init) - nombre)/(sum(row.w.init) - 1)))
    else vtest <- vtest*sqrt(nombre)
    cos2.bary.sup <- cos2.bary.sup[, 1:ncp, drop=FALSE]
    coord.barycentre <- coord.barycentre[, 1:ncp, drop=FALSE]
    vtest <- vtest[, 1:ncp, drop=FALSE]
    dimnames(cos2.bary.sup) <- dimnames(vtest) <- dimnames(coord.barycentre)
    names(dist2) <- rownames(coord.barycentre)
    # @info Strukturierte Datenanalyse Anpassungen
    res.quali.sup <- list(coord = coord.barycentre, cos2 = cos2.bary.sup, v.test = vtest, dist = sqrt(dist2), eta2=eta2, variance = v_vrc_g, within = v_wi_g, between = v_be_g, total = v_tot)
    call.quali.sup <- list(quali.sup = X.quali.sup, modalite = modalite, nombre = nombre, barycentre = barycentre, numero = quali.sup)
    res$quali.sup = res.quali.sup
    res.call$quali.sup = call.quali.sup
  }
  res$call = res.call
  class(res) <- c("PCA", "list ")
  if (graph & (ncp>1)) {
    plot.PCA(res, choix = "ind", axes = axes,new.plot=TRUE)
    plot.PCA(res, choix = "var", axes = axes,new.plot=TRUE,shadowtext=TRUE)
  }
  return(res)
}
