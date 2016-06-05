# Eine Sammlung hilfreicher Funktionen
.count_distinct_ind <- function(res_gda, axes = 1:2) {
  # Koordinaten der Individuen vorbereiten
  # Punkte an den gleichen Stellen vergößern und multiple entfernen.
  res_gda <- data_frame(Dim.1 = res_gda$ind$coord[, axes[1]], Dim.2 = res_gda$ind$coord[, axes[2]]) %>%
    group_by(Dim.1, Dim.2) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    as.data.frame()
}
# Data frame selection
.select <- function(d, filter = NULL, check= TRUE){

  if(!is.null(filter)){

    # Filter by name
    if(!is.null(filter$name)){
      name <- filter$name
      common <- intersect(name, d$name)
      diff <- setdiff(name, d$name)
      #if(check & length(common) == 0) stop("Can't find the specified names")
      # if(check & length(diff)!=0) warning("Can't find the the following name(s): ", diff)
      d <- d[common, , drop = FALSE]
    }

    # Filter by eta2
    if(!is.null(filter$eta2) & nrow(d) >= 1){
      cos2 <- round(filter$eta2)
      d <- d[which(d$eta2 >= filter$eta2), , drop = FALSE]
    }

  }

  return (d)
}
# Add Myriad Pro font
.add_fonts <- function () {
  showtext::showtext.auto()
  sysfonts::font.add("Myriad Pro", regular = "MyriadPro-Regular.otf", bold = "MyriadPro-Bold.otf", italic = "MyriadPro-It.otf", bolditalic = "MyriadPro-BoldIt.otf")
}
# Calculate crossed within variance
.crossed_within_variance <- function(var, weight, coord) {
  # Varianzen berechnen
  variances <- join(weight, coord) %>% group_by_(`var`) %>%
    mutate(total_weight = sum(weight),
           relative_weight = weight/total_weight) %>%
    mutate_each(funs(weighted.mean(., weight) - .), matches("Dim")) %>%
    summarise_each(funs(sum(relative_weight*(.^2))), matches("Dim"))%>%
    mutate_each(funs(. * eigenwerte$.), matches("Dim"))

  # Gesamte Anzahl an Personen
  weight_total <- weight %>% group_by_(`var`) %>% summarise(weight_total = sum(weight))

  # Age within gender variance
  within_variance <-
    join(variances, weight_total) %>%
    summarise_each(funs(weighted.mean(., weight_total)), matches("Dim"))

  return(within_variance)
}
