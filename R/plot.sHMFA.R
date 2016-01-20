#' Plot Specific Hierarchical Multiple Factor Analysis.
#'
#' @param x
#' @param axes
#' @param num
#' @param choix
#' @param plot.nodes
#' @param select
#' @param unselect
#' @param lim.cos2.var
#' @param xlim
#' @param ylim
#' @param cex
#' @param title
#' @param new.plot
#' @param ...
#'
#' @return Returns standard plots of sHMFA results.
#' @export
#'
#' @examples
plot.sHMFA <- function(x, axes = c(1,2), num=6, choix = "ind", plot.nodes = FALSE, select = NULL, unselect = 0.7,
                      lab.grpe = TRUE, lab.var = TRUE, lab.ind.moy = TRUE, invisible = NULL,
                      lim.cos2.var = 0., xlim = NULL, ylim = NULL, cex = 1, title = NULL, new.plot=FALSE, ...) {

  partial.tab.pour.plot <- function(res.shmfa,coord=c(1,2)) {
    H <- res.shmfa$call$H
    P <- res.shmfa$partial
    ptpp1 <- list()
    nbnivo <- length(H)
    for (h in 2:nbnivo) {
      x <- 1
      ptpp2 <- list()
      nbgroup <- length(H[[h]])
      for (g in 1:nbgroup) {
        nb <- H[[h]][g]
        ptpp2[[g]]<-P[[h-1]][,coord,x]
        if (nb > 1) {
          for (n in 2:nb) {
            ptpp2[[g]] <- rbind(ptpp2[[g]],P[[h-1]][,coord,n+x-1])
          }
        }
        x <- x + nb
      }
      ptpp1[[h-1]] <- ptpp2
    }
    ptpp2 <- list()
    nb <- length(H[[nbnivo]])
    ptpp2[[1]] <- P[[nbnivo]][,coord,1]
    for (n in 2:nb) {
      ptpp2[[1]] <- rbind(ptpp2[[1]],P[[nbnivo]][,coord,n])
    }
    ptpp1[[nbnivo]] <- ptpp2
    return(ptpp1)
  }

  #################################################################
  ##"plot.partial.ind" <- function(res.shmfa, numb = 6, coord=c(1,2), cex = 1) {
  ##
  ##    cex <- 1
  ##    coord.partial <- res.shmfa$partial
  ##    H <- res.shmfa$call$H
  ##    nbnivo <- length(coord.partial)
  ##    nbind <- nrow(coord.partial[[1]])
  ##    if (!is.null(res.shmfa$quali.var$coord)) nbind <- nbind + nrow(res.shmfa$quali.var$coord)
  ##    mult <- nbind / numb
  ##    if ((nbind / numb)!=as.integer(nbind / numb))   mult <- as.integer(mult) + 1
  ##    quali <- FALSE
  ##    for (m in 1:mult) {
  ##        par(mfrow = c(2,(numb/2)))
  ##        for (ind in 1:numb) {
  ##        indd <- (m-1)*numb+ind
  ##        if (indd <= nbind) {
  ##          if (indd > nrow(res.shmfa$ind$coord)) {
  ##            coord.partial <- res.shmfa$quali.var$partial
  ##            indd <- indd - nrow(res.shmfa$ind$coord)
  ##            quali <- TRUE
  ##          }
  ##          x <- c(min(coord.partial[[1]][indd,coord[1],]),max(coord.partial[[1]][indd,coord[1],]))*1.1
  ##          y <- c(min(coord.partial[[1]][indd,coord[2],]),max(coord.partial[[1]][indd,coord[2],]))*1.1
  ##          plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = x, ylim = y, col = "white", asp=1, cex=cex)
  ##          abline(v=0,lty=2, cex=cex)
  ##          abline(h=0,lty=2, cex=cex)
  ##          index.col <- 0
  ##          name <- NULL
  ##          if (!quali){
  ##            points(res.shmfa$ind$coord[indd,coord[1]],res.shmfa$ind$coord[indd,coord[2]],pch=15,cex=cex*0.8)
  ##            text(res.shmfa$ind$coord[indd,coord[1]],res.shmfa$ind$coord[indd,coord[2]],rownames(res.shmfa$ind$coord)[indd], pos = 4,offset = 0.2, cex=0.8*cex)
  ##          }
  ##          else{
  ##            points(res.shmfa$quali.var$coord[indd,coord[1]],res.shmfa$quali.var$coord[indd,coord[2]],pch=15,cex=cex*0.8)
  ##            text(res.shmfa$quali.var$coord[indd,coord[1]],res.shmfa$quali.var$coord[indd,coord[2]],rownames(res.shmfa$quali.var$coord)[indd], pos = 4,offset = 0.2, cex=0.8*cex)
  ##          }
  ##          for (k in 1:(nbnivo-1)) {
  ##            jj <- 1
  ##            for (j in 1:length(H[[k]])) {
  ##              index.col <- index.col +1
  ##              if (j == sum(H[[k+1]][1:jj])+1 ) jj=jj+1
  ##              if(length(H[[k]])>1) {
  ##                points(coord.partial[[k]][indd,coord[1],j],coord.partial[[k]][indd,coord[2],j],col=color[index.col],pch=20,cex=cex*0.8)
  ##                lines(c(coord.partial[[k+1]][indd,coord[1],jj],coord.partial[[k]][indd,coord[1],j]),c(coord.partial[[k+1]][indd,coord[2],jj],coord.partial[[k]][indd,coord[2],j]))
  ##              }
  ##            }
  ##            name <- c(name,rownames(res.shmfa$group[[k]]))
  ##          }
  ##          for (j in 1:length(H[[nbnivo]])) {
  ##            index.col <- index.col+1
  ##            points(coord.partial[[nbnivo]][indd,coord[1],j],coord.partial[[nbnivo]][indd,coord[2],j],col=color[index.col],pch=16,cex=cex*0.8)
  ##            if (!quali) lines(c(res.shmfa$ind$coord[indd,coord[1]],coord.partial[[nbnivo]][indd,coord[1],j]),c(res.shmfa$ind$coord[indd,coord[2]],coord.partial[[nbnivo]][indd,coord[2],j]))
  ##            else lines(c(res.shmfa$quali.var$coord[indd,coord[1]],coord.partial[[nbnivo]][indd,coord[1],j]),c(res.shmfa$quali.var$coord[indd,coord[2]],coord.partial[[nbnivo]][indd,coord[2],j]))
  ##          }
  ##          name <- c(name,rownames(res.shmfa$group[[nbnivo]]))
  ##          legend("topleft",legend= name,text.col= color[1:index.col],cex=0.7*cex,bg="white")
  ##        }
  ##      }
  ##      if (m < mult) dev.new()
  ##    }
  ##}

  ###############################################################
  plot.partial <- function(res.shmfa , coord=c(1,2), invisible = NULL, title = NULL, cex = 1, nivo=length(res.shmfa$partial)){

    test.invisible <- vector(length = 2)
    if (!is.null(invisible)) {
      test.invisible[1] <- match("ind", invisible)
      test.invisible[2] <- match("quali", invisible)
    }
    else test.invisible <- rep(NA, 2)
    coord.partial <- res.shmfa$partial
    H <- res.shmfa$call$H
    nbpart1 <- dim(coord.partial[[nivo]])[3]
    inter <- coord.partial[[nivo]][,coord,1]
    for(i in 2:nbpart1) inter <- rbind(inter,coord.partial[[nivo]][,coord,i])
    xmin <- xmax <- ymin <- ymax <- 0
    if (is.na(test.invisible[1])) {
      xmin <- min(xmin,min(inter[,1]))
      xmax <- max(xmax,max(inter[,1]))
      ymin <- min(ymin,min(inter[,2]))
      ymax <- max(ymax,max(inter[,2]))
      x <- c(xmin,xmax)*1.1
      y <- c(ymin,ymax)*1.1
      nbnivo <- length(coord.partial)

      plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = x, ylim = y, col = "white", asp=1, cex=cex)
      #    title(sub = sub.title, cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)
      abline(v=0,lty=2, cex=cex)
      abline(h=0,lty=2, cex=cex)
      points(res.shmfa$ind$coord[,coord],pch=20,cex=cex)
      if (lab.ind.moy) text(res.shmfa$ind$coord[,coord[1]],res.shmfa$ind$coord[,coord[2]],rownames(res.shmfa$ind$coord), pos = 4,offset = 0.2, cex=0.8*cex)
      index.col <- 0
      name <- NULL
      if (nbnivo>nivo){
        for (k in nivo:(nbnivo-1)) {
          jj <- 1
          for (j in 1:length(H[[k]])) {
            index.col <- index.col +1
            if (j == sum(H[[k+1]][1:jj])+1 ) jj=jj+1
            if(length(H[[k]])>1) {
              points(coord.partial[[k]][,coord,j],col=color[index.col],pch=20,cex=cex*0.8)
              for (i in 1:nrow(coord.partial[[k]])) lines(c(coord.partial[[k+1]][i,coord[1],jj],coord.partial[[k]][i,coord[1],j]),c(coord.partial[[k+1]][i,coord[2],jj],coord.partial[[k]][i,coord[2],j]))
            }
          }
          name <- c(name,rownames(res.shmfa$group$coord[[k]]))
        }
      }
      for (j in 1:length(H[[nbnivo]])) {
        index.col <- index.col+1
        points(coord.partial[[nbnivo]][,coord,j],col=color[index.col],pch=20,cex=cex*0.8)
        for (i in 1:nrow(coord.partial[[nbnivo]])) lines(c(res.shmfa$ind$coord[i,coord[1]],coord.partial[[nbnivo]][i,coord[1],j]),c(res.shmfa$ind$coord[i,coord[2]],coord.partial[[nbnivo]][i,coord[2],j]))
      }
      name <- c(name,rownames(res.shmfa$group$coord[[nbnivo]]))
    }

    if (!is.null(res.shmfa$quali.var)){
      coord.partial <- res.shmfa$quali.var$partial
      nbnivo <- length(coord.partial)
      if (!is.na(test.invisible[1])) {
        inter <- coord.partial[[nivo]][,coord,1]
        for(i in 2:nbpart1) inter <- rbind(inter,coord.partial[[nivo]][,coord,i])
        xmin <- xmax <- ymin <- ymax <- 0
        xmin <- min(xmin,min(inter[,1]))
        xmax <- max(xmax,max(inter[,1]))
        ymin <- min(ymin,min(inter[,2]))
        ymax <- max(ymax,max(inter[,2]))
        x <- c(xmin,xmax)*1.1
        y <- c(ymin,ymax)*1.1
        plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = x, ylim = y, col = "white", asp=1, cex=cex)
        abline(v=0,lty=2, cex=cex)
        abline(h=0,lty=2, cex=cex)
      }

      if (is.na(test.invisible[2])) {
        name <- NULL
        points(res.shmfa$quali.var$coord[,coord],pch=15,cex=cex)
        if (lab.ind.moy) text(res.shmfa$quali.var$coord[,coord[1]],res.shmfa$quali.var$coord[,coord[2]],rownames(res.shmfa$quali.var$coord), pos = 4,offset = 0.2, cex=0.8*cex)
        index.col <- 0
        if (nbnivo > nivo){
          for (k in nivo:(nbnivo-1)) {
            jj <- 1
            for (j in 1:length(H[[k]])) {
              index.col <- index.col +1
              if (j == sum(H[[k+1]][1:jj])+1 ) jj=jj+1
              if(length(H[[k]])>1) {
                points(coord.partial[[k]][,coord,j],col=color[index.col],pch=15,cex=cex*0.8)
                for (i in 1:nrow(coord.partial[[k]])) lines(c(coord.partial[[k+1]][i,coord[1],jj],coord.partial[[k]][i,coord[1],j]),c(coord.partial[[k+1]][i,coord[2],jj],coord.partial[[k]][i,coord[2],j]))
              }
            }
            name <- c(name,rownames(res.shmfa$group$coord[[k]]))
          }
        }
        for (j in 1:length(H[[nbnivo]])) {
          index.col <- index.col+1
          points(coord.partial[[nbnivo]][,coord,j],col=color[index.col],pch=15,cex=cex*0.8)
          for (i in 1:nrow(coord.partial[[nbnivo]])) lines(c(res.shmfa$quali.var$coord[i,coord[1]],coord.partial[[nbnivo]][i,coord[1],j]),c(res.shmfa$quali.var$coord[i,coord[2]],coord.partial[[nbnivo]][i,coord[2],j]))
        }
        name <- c(name,rownames(res.shmfa$group$coord[[nbnivo]]))
      }
    }
    legend("topleft",legend= name,text.col= color[1:index.col],cex=0.8,bg="white")
    if (is.null(title)) title(main = "Superimposed representation of the partial clouds" , cex.sub = 1.2, col.sub = "steelblue4", font.sub=3, adj = 0.5, line = 3)
    else{
      title(main = title , cex.sub = 1.2, col.sub = "steelblue4", font.sub=3, adj = 0.5, line = 3)
      title(sub= "Superimposed representation of the partial clouds" , cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)
    }
  }

  plot.moy <- function(res.shmfa , coord=c(1,2), invisible = NULL, title = NULL, cex = 1, xlim = NULL, ylim = NULL){

    test.invisible <- vector(length = 2)
    if (!is.null(invisible)) {
      test.invisible[1] <- match("ind", invisible)
      test.invisible[2] <- match("quali", invisible)
    }
    else test.invisible <- rep(NA, 2)

    xmin <- xmax <- ymin <- ymax <- 0
    if (is.na(test.invisible[1])) {
      xmin <- min(xmin,min(res.shmfa$ind$coord[,coord[1]]))
      xmax <- max(xmax,max(res.shmfa$ind$coord[,coord[1]]))
      ymin <- min(ymin,min(res.shmfa$ind$coord[,coord[2]]))
      ymax <- max(ymax,max(res.shmfa$ind$coord[,coord[2]]))
      x <- c(xmin,xmax)*1.1
      y <- c(ymin,ymax)*1.1

      # @info Optionserweiterungen
      if(!is.null(xlim)) x <- xlim
      if(!is.null(ylim)) y <- ylim

      plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = x, ylim = y, col = "white", asp=1, cex=cex)
      #    title(sub = sub.title, cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)
      abline(v=0,lty=2, cex=cex)
      abline(h=0,lty=2, cex=cex)
      points(res.shmfa$ind$coord[,coord],pch=20,cex=cex)
      if (lab.ind.moy) text(res.shmfa$ind$coord[,coord[1]],res.shmfa$ind$coord[,coord[2]],rownames(res.shmfa$ind$coord), pos = 4,offset = 0.2, cex=0.8*cex)
    }


    selection <- NULL
    if (!is.null(select)) {
      if (mode(select)=="numeric") selection <- select
      else {
        if (sum(rownames(res.shmfa$ind$coord)%in%select)!=0) selection <- which(rownames(res.shmfa$ind$coord)%in%select)
        else {
          if (grepl("contrib",select)) selection <- (rev(order(res.shmfa$ind$contrib[,axes[1],drop=FALSE]*res.shmfa$eig[axes[1],1]+res.shmfa$ind$contrib[,axes[2],drop=FALSE]*res.shmfa$eig[axes[2],1])))[1:min(nrow(res.shmfa$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          # 		    if (grepl("contrib",select)) selection <- (rev(order(apply(res.spmfa$ind$contrib[,axes],1,sum))))[1:min(nrow(res.spmfa$ind$coord),sum(as.integer(unlist(strsplit(select,"contrib"))),na.rm=T))]
          if (grepl("dist",select)) selection <- (rev(order(res.shmfa$ind$dist)))[1:min(nrow(res.shmfa$ind$coord),sum(as.integer(unlist(strsplit(select,"dist"))),na.rm=T))]
          if (grepl("coord",select)) selection <- (rev(order(apply(res.shmfa$ind$coord[,axes]^2,1,sum))))[1:min(nrow(res.shmfa$ind$coord),sum(as.integer(unlist(strsplit(select,"coord"))),na.rm=T))]
          if (grepl("cos2",select)) {
            if (sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T)>=1) selection <- (rev(order(apply(res.shmfa$ind$cos2[,axes],1,sum))))[1:min(nrow(res.shmfa$ind$coord),sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))]
            else selection <- which(apply(res.shmfa$ind$cos2[,axes],1,sum)>sum(as.numeric(unlist(strsplit(select,"cos2"))),na.rm=T))
          }
          if (is.integer(select)) selection <- select
        }
      }
    }

    coo <- labe <- coll <- ipch <- fonte <- NULL
    coo <- rbind(coo,res.shmfa$ind$coord)
    labe <- rownames(res.shmfa$quali.var$coord) # evtl. ind
    coll <- c(coll, rep("black", nrow(res.shmfa$quali.var$coord)))
    ipch <- c(ipch,rep(20,nrow(res.shmfa$ind$coord)))
    fonte <- c(fonte,rep(1,nrow(res.shmfa$ind$coord)))
    if (!is.null(selection)){
      if (is.numeric(unselect)) coll[!((1:length(coll))%in%selection)] = rgb(t(col2rgb(coll[!((1:length(coll))%in%selection)])),alpha=255*(1-unselect),maxColorValue=255)
      else coll[!((1:length(coll))%in%selection)] = unselect
      labe[!((1:length(coll))%in%selection)] <- ""
    }



    if (!is.null(res.shmfa$quali.var)){
      if (!is.na(test.invisible[1])) {
        xmin <- xmax <- ymin <- ymax <- 0
        xmin <- min(res.shmfa$quali.var$coord[,coord[1]])
        xmax <- max(res.shmfa$quali.var$coord[,coord[1]])
        ymin <- min(res.shmfa$quali.var$coord[,coord[2]])
        ymax <- max(res.shmfa$quali.var$coord[,coord[2]])
        x <- c(xmin,xmax)*1.1
        y <- c(ymin,ymax)*1.1

        # @info Optionserweiterungen
        if(!is.null(xlim)) x <- xlim
        if(!is.null(ylim)) y <- ylim

        plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = x, ylim = y, col = "white", asp=1, cex=cex)
        abline(v=0,lty=2, cex=cex)
        abline(h=0,lty=2, cex=cex)
      }

      if (is.na(test.invisible[2])) {
        points(res.shmfa$quali.var$coord[,coord],pch=15,cex=cex, col = coll)
        if (lab.ind.moy) text(res.shmfa$quali.var$coord[,coord[1]],res.shmfa$quali.var$coord[,coord[2]],labe, pos = 4,offset = 0.2, cex=0.8*cex)
      }
    }
    if (is.null(title)) title(main = "Individuals factor map" , cex.sub = 1.2, col.sub = "steelblue4", font.sub=3, adj = 0.5, line = 3)
    else{
      title(main = title , cex.sub = 1.2, col.sub = "steelblue4", font.sub=3, adj = 0.5, line = 3)
      title(sub= "Individuals factor map" , cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)
    }
  }

  ###############################################################

  res.shmfa <- x
  if (!inherits(res.shmfa, "sHMFA")) stop("non convenient data")
  color = c("black","red","green3","blue",
            "cyan","magenta","darkgray","darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender","yellow","lightgreen","lightgrey",
            "lightblue","darkkhaki", "darkmagenta","darkolivegreen","lightcyan", "darkorange",
            "darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey",
            "darkturquoise","darkviolet", "lightgray","lightsalmon","lightyellow", "maroon")

  sub.title <- NULL
  lab.x <- paste("Dim ", axes[1], " (",signif(res.shmfa$eig[axes[1],2],4)," %)",sep="")
  lab.y <- paste("Dim ", axes[2], " (",signif(res.shmfa$eig[axes[2],2],4)," %)",sep="")


  if (choix == "group") {
    if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
    if (is.null(title)) title <- "Groups representation"
    else sub.title <- "Groups representation"
    for (h in 1:length(res.shmfa$group$coord)){
      coord.actif <- res.shmfa$group$coord[[h]][, axes]
      if (h ==1) plot(coord.actif, xlab = lab.x, ylab = lab.y, xlim = c(0, 1), ylim = c(0, 1), pch = 17, col = color[h], cex = cex, main = title, cex.main = cex, asp = 1)
      else points(coord.actif[,1],coord.actif[,2],col=color[h], pch = 17, cex=cex)
      if (lab.grpe) text(coord.actif[, 1], y = coord.actif[, 2], labels = rownames(coord.actif), pos = 3, col = color[h],cex=cex)
    }
    title(sub = sub.title, cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)

  }

  if (choix == "var") {
    if (is.null(res.shmfa$quanti.var$coord)) stop("No quantitative variables to plot")
    if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
    if (is.null(title)) title <- "Correlation circle"
    else sub.title <- "Correlation circle"
    plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), col = "white", asp=1, cex=cex)
    title(sub = sub.title, cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)
    x.cercle <- seq(-1, 1, by = 0.01)
    y.cercle <- sqrt(1 - x.cercle^2)
    lines(x.cercle, y = y.cercle)
    lines(x.cercle, y = -y.cercle)
    abline(v=0,lty=2, cex=cex)
    abline(h=0,lty=2, cex=cex)
    if (!is.null(res.shmfa$quanti.var$coord)) {
      coord.var <- res.shmfa$quanti.var$cor[, axes]
      for (v in 1:nrow(coord.var)) {
        if (sum(res.shmfa$quanti.var$cos2[v, axes], na.rm = TRUE) >= lim.cos2.var & !is.na(sum(res.shmfa$quanti.var$cos2[v, axes], na.rm = TRUE))) {
          arrows(0, 0, coord.var[v, 1], coord.var[v, 2], length = 0.1, angle = 15, code = 2,cex=cex)
          if (lab.var) {
            if (abs(coord.var[v,1])>abs(coord.var[v,2])){
              if (coord.var[v,1]>=0) pos<-4
              else pos<-2
            }
            else {
              if (coord.var[v,2]>=0) pos<-3
              else pos<-1
            }
            text(coord.var[v, 1], y = coord.var[v, 2], labels = rownames(coord.var)[v], pos = pos,cex=cex)
          }
        }
      }
    }
  }
  if (choix =="ind"){
    if (length(res.shmfa$call$H)>1){
      tab.coord.partial <- partial.tab.pour.plot(res.shmfa, coord = axes)
      for (nivo in 1:length(res.shmfa$call$H)){
        if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
        if (plot.nodes) plot.partial(res.shmfa, coord=axes, invisible = invisible, cex = cex, title = title, nivo = nivo)
      }
      ##        dev.new()
      ##        plot.partial.ind(res.shmfa, num = num, coord=axes, cex = cex)
    }
    if ((new.plot)&!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) dev.new()
    plot.moy(res.shmfa, coord=axes, invisible = invisible, cex = cex, title = title, xlim = xlim, ylim = ylim)
  }
}
