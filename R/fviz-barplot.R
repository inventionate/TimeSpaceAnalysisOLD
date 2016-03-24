#' Visualize a barplot.
#'
#' @param dfname categorical variable.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param sort sort bars (boolean).
#' @param relative realtive values (boolean).
#' @param labels label bars (boolean).
#' @param labels_inline inline labels (boolean).
#' @param amount include total amount of observations (boolean).
#' @param rotate rotate bars (boolean).
#' @param title plot title.
#' @param textsize sife of axes texts.
#' @param titlesize title text size.
#' @param labelsize label text size.
#' @param drop drop missing data (boolean).
#' @param digits amount of label value digits.
#' @param addsymb suffix symbol.
#' @param ylim y-axis range.
#' @param xlim y-axis range.
#'
#' @return
#' @export
#'
#' @examples
fviz_barplot <- function(dfname,xlab="", ylab="", sort=FALSE, relative=FALSE, labels=FALSE, labels_inline = TRUE, amount=FALSE, rotate=FALSE, title="",
                         textsize=14, titlesize=1.5*textsize, labelsize=textsize/3, drop=FALSE, digits=0, addsymb="", ylim=NA, xlim=NA){

  symbol <<- toString(addsymb)
  if(drop) {
    dfname <- dfname[drop=T]
  }
  if(sort) {
    c <- as.table(sort(table(dfname)))
  }
  else {
    c <- table(dfname)
  }
  if(relative) {
    relfreq <- prop.table(c)
    relfreq <- relfreq*100
    c <- round(relfreq, digits=digits)
  }
  c <- as.data.frame(c)
  colnames(c)[1] <- "Var1"
  if(ncol(c)==3){
    p <- ggplot(data=c, aes_string(x="Var1", y="Freq",fill=names(c[2])))
  }else{
    p <- ggplot(data=c, aes(x=Var1, y=Freq))
  }
  p <- p + geom_bar(stat="identity")
  p <- p + geom_hline(yintercept=0, colour="gray10") +
    geom_vline(xintercept=0, colour="gray10") +
    theme_minimal() +
    # theme(panel.grid.major = element_line(size=0.5, color="gray80", linetype="dashed")) +
    # theme(panel.grid.minor = element_line(size=0.5, color="gray80", linetype="dashed")) +
    xlab(toString(xlab)) +
    ylab(toString(ylab)) +
    ggtitle(toString(title)) +
    theme(plot.title = element_text(face="bold",vjust=1.5,size=titlesize)) +
    theme(text = element_text(size=textsize),axis.title.x = element_text(vjust=-0.5)) +
    theme(text=element_text(family="Myriad Pro"))
  if(!is.na(xlim[1])){
    p <- p + xlim(xlim)
  }
  if(!is.na(ylim[1])){
    p <- p + ylim(ylim)
  }
  o <- p
  if(labels) {
    if(labels_inline) o <- p + geom_text(aes(label = paste(Freq,symbol,sep=""), ymax= Freq, family="Myriad Pro"), vjust = 1.5, size = (labelsize), colour = "white", position = "stack")
    else o <- p + geom_text(aes(label = paste(Freq,symbol,sep=""), ymax= Freq, family="Myriad Pro"), vjust = -0.5, size = (labelsize), colour = "black", position = "stack")
  }
  if(rotate) {
    o <- o + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  if(amount) {
    # Ergänzt den x-Achsentitel um die Fallzahlangabe.
    if(ncol(c)==3){
      n <- nrow(na.omit(dfname))
    }else{
      n <- nrow(data.frame(na.omit(dfname)))
    }
    o <- o + xlab(bquote(paste(.(toString(xlab))," (n = ",.(n),")")))
    o <- o + theme(axis.title.x= element_text(vjust=-0.3))
  }
  o
}
