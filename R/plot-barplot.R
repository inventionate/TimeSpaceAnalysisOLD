#' @include utilities.R
NULL
#' Visualize a barplot.
#'
#' @param dfname categorical variable.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param sort sort bars (boolean).
#' @param labels_inline inline labels (boolean).
#' @param amount include total amount of observations (boolean).
#' @param rotate_x_axis_text rotate bars (boolean).
#' @param title plot title.
#' @param textsize sife of axes texts.
#' @param titlesize title text size.
#' @param labelsize label text size.
#' @param digits amount of label value digits.
#' @param ylim y-axis range.
#' @param xlim y-axis range.
#' @param include_na show NAs or not (boolean).
#'
#' @return ggplot2 barplot.
#' @export
plot_barplot <- function(dfname, xlab = "", ylab = "", title = "", sort = FALSE, labels_inline = FALSE, amount = FALSE, rotate_x_axis_text = FALSE,
                         textsize = 20, titlesize = 25, labelsize = 8, include_na = TRUE, digits = 0, ylim = NA, xlim = NA, , myriad = TRUE){

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  n <- nrow(data.frame(na.omit(dfname)))

  if(include_na) absolute_freq <- table(dfname, useNA = "always")
  else absolute_freq <- table(dfname)

  if(sort) absolute_freq <- as.table(sort(absolute_freq))

  relative_freq <- as.data.frame(round(prop.table(absolute_freq) * 100, digits = digits))

  data_freq <- data.frame(as.data.frame(absolute_freq), relative_freq[,2])

  colnames(data_freq) <- c("var", "absolute", "relative")

  p <- ggplot(data_freq, aes(var, absolute))+
    geom_bar(stat="identity") +
    geom_hline(yintercept = 0, colour = "gray10") +
    geom_vline(xintercept = 0, colour = "gray10") +
    theme_minimal() +
    xlab(xlab) +
    ylab(ylab) +
    ylim(0, n) +
    theme(plot.title = element_text(face = "bold",vjust = 1.5,size = titlesize),
          text = element_text(size = textsize, family = "Myriad Pro"),
          axis.title.x = element_text(vjust = -0.5),
          panel.grid.major = element_line(size = 0.5, color = "gray80", linetype = "dashed")) +
    ggtitle(title)

  if(!is.na(xlim[1])) p <- p + xlim(xlim)
  if(!is.na(ylim[1])) p <- p + ylim(ylim)

  if(labels_inline) {
    p <- p + geom_text(aes(label = absolute, ymax = absolute, family = "Myriad Pro"), vjust = 1.5, size = labelsize, colour = "white", position = "stack") +
    geom_text(aes(label = paste("(", relative, "%)",sep = ""), ymax = absolute, family = "Myriad Pro"), vjust = 4, size = labelsize/1.5, colour = "white", position = "stack")
  } else {
    p <- p + geom_text(aes(label = absolute, ymax = absolute, family = "Myriad Pro"), vjust = -1.5, size = labelsize, colour = "black", position = "stack") +
    geom_text(aes(label = paste("(", relative, "%)",sep = ""), ymax = absolute, family = "Myriad Pro"), vjust = -0.5, size = labelsize/1.5, colour = "black", position = "stack")
  }

  if(rotate_x_axis_text) p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if(amount) p <- p + xlab(bquote(paste(.(xlab)," (n = ",.(n),")"))) + theme(axis.title.x = element_text(vjust = -0.3))

  return(p)
}
