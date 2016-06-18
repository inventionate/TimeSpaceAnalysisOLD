# Eine Sammlung hilfreicher Funktionen
.count_distinct_ind <- function(res_gda, axes = 1:2) {
  # Koordinaten der Individuen vorbereiten
  # Punkte an den gleichen Stellen vergößern und multiple entfernen.
  res_gda <- data_frame(x = res_gda$ind$coord[, axes[1]], y = res_gda$ind$coord[, axes[2]]) %>%
    group_by(x, y) %>%
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
.crossed_within_variance <- function(var, weight, coord) {#, eigenvalues) {
  # Varianzen berechnen
  variances <- join(weight, coord) %>% group_by_(`var`) %>%
    mutate(total_weight = sum(weight),
           relative_weight = weight/total_weight) %>%
    mutate_each(funs(weighted.mean(., weight) - .), matches("Dim")) %>%
    summarise_each(funs(sum(relative_weight*(.^2))), matches("Dim"))# %>%
    #mutate_each(funs(. * eigenvalues$.), matches("Dim"))

  # Gesamte Anzahl an Personen
  weight_total <- weight %>% group_by_(`var`) %>% summarise(weight_total = sum(weight))

  # Age within gender variance
  within_variance <-
    join(variances, weight_total) %>%
    summarise_each(funs(weighted.mean(., weight_total)), matches("Dim"))

  return(within_variance)
}
# Extract eigenvalues
.get_eigenvalues <- function(res_gda) {

  eigenvalues <- res_gda$eig %>% data.frame %>% add_rownames %>% separate(rowname, c("dim", "num")) %>%
    select(num, eigenvalue) %>% mutate_each(funs(as.numeric), matches("num")) %>% spread(num, eigenvalue)

  colnames(eigenvalues) <- paste0("Dim.", 1:ncol(eigenvalues))

  return(eigenvalues)
}
# Select trajectory ind
.select_trajectory <- function(coord_all, select) {

  # Vollständige Fälle bestimmen
  selected_ind_complete <- coord_all %>% count(id) %>% filter(n == length(time_point_names)) %>% select(id)
  coord_complete <- coord_all %>% filter(id %in% selected_ind_complete$id)

  # Selection (es wird select_ind definiert)
  selected_ind <- coord_all %>% select(id)
  # Durch Angabe des Namens filtern.
  if(!is.null(select$name))
  {
    selected_ind <- selected_ind_name <- coord_all %>% filter(id %in% select$name)
  }
  # Durch die Un/Vollständigkeit der Fälle filtern
  if(!is.null(select$case))
  {
    if(select$case == "complete") selected_ind <- selected_ind_complete
    if(select$case == "incomplete") selected_ind <- selected_ind_incomplete <- coord_all %>% count(id) %>% filter(n != length(time_point_names)) %>% select(id)
  }
  # Durch Angabe der Varianz filtern.
  if(!is.null(select$within_inertia))
  {
    warning("Only complete cases will be used to calculate within inertia!")
    ind_mean_coord <- coord_complete %>% select(-time) %>% group_by(id) %>% summarise_each(funs(mean))
    ind_mean_coord_id <- data.frame(ind_mean_coord)$id

    # "within inertia" berechnen (adaptiert von FactoMineR)
    tmp <- array(0, dim = c(dim(ind_mean_coord %>% select(-id)), 2))
    for(i in 1:length(time_point_names)) {
      tmp[,,i] <- (coord_complete %>% filter(time == time_point_names[i] & id %in% ind_mean_coord_id) %>% select(-id, -time) - ind_mean_coord %>% select(-id))^2 / length(time_point_names)
    }
    variab.auxil <- apply(tmp,2,sum)
    tmp <- sweep(tmp,2,variab.auxil,FUN="/") * 100
    inertie.intra.ind <- apply(tmp,c(1,2),sum)
    rownames(inertie.intra.ind) <- ind_mean_coord_id
    colnames(inertie.intra.ind) <- colnames(coord_complete %>% select(-id, -time))
    ind_within_inertia <- inertie.intra.ind

    if(select$within_inertia[[2]] == 0) {
      selected_ind_high <- NULL
    } else {
      selected_ind_high <- ind_within_inertia %>% data.frame %>% select(Dim.1 = matches(paste0("Dim.", axes[1], "$")), Dim.2 = matches(paste0("Dim.", axes[2], "$"))) %>%
        add_rownames %>% rename(id = rowname) %>% mutate(within_inertia = Dim.1 + Dim.2) %>% arrange(desc(within_inertia)) %>% slice(1:select$within_inertia[[2]])
    }
    if(select$within_inertia[[1]] == 0) {
      selected_ind_low <- NULL
    } else {
      selected_ind_low <- ind_within_inertia %>% data.frame %>% select(Dim.1 = matches(paste0("Dim.", axes[1], "$")), Dim.2 = matches(paste0("Dim.", axes[2], "$"))) %>%
        add_rownames %>% rename(id = rowname) %>% mutate(within_inertia = Dim.1 + Dim.2) %>% arrange(within_inertia) %>% slice(1:select$within_inertia[[1]])
    }
    selected_ind <- selected_ind_wi <- rbind(selected_ind_high, selected_ind_low)
  }
  if(!is.null(select$name) & !is.null(select$within_inertia)) {
    selected_ind <- selected_ind_name %>% filter(id %in% selected_ind_wi$id)
  }

  return(selected_ind)
}
