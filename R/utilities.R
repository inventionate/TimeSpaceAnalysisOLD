# Eine Sammlung hilfreicher Funktionen
.count_distinct_ind <- function(res_gda, axes = 1:2, normalize = NULL) {
  # Koordinaten der Individuen vorbereiten
  # Punkte an den gleichen Stellen vergößern und multiple entfernen.
  coord_ind <- tibble(x = res_gda$ind$coord[, axes[1]], y = res_gda$ind$coord[, axes[2]]) %>%
    group_by(x, y) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    data.frame()

  if( !is.null(normalize) ) coord_ind <- coord_ind %>% mutate_at("count", funs( . * (mean(normalize))^(1/2) ))

  return(coord_ind)
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
  variances <- join(weight, coord, by = c("var1", "var2")) %>% group_by_(`var`) %>%
    mutate(total_weight = sum(weight),
           relative_weight = weight/total_weight) %>%
    mutate_at(vars(matches("Dim")), funs(weighted.mean(., weight) - .)) %>%
    summarise_at(vars(matches("Dim")), funs(sum(relative_weight*(.^2))))# %>%
    #mutate_each(funs(. * eigenvalues$.), matches("Dim"))

  # Gesamte Anzahl an Personen
  weight_total <- weight %>% group_by_(`var`) %>% summarise(weight_total = sum(weight))

  # Age within gender variance
  within_variance <-
    join(variances, weight_total, by = var) %>%
    summarise_at(vars(matches("Dim")), funs(weighted.mean(., weight_total)))

  return(within_variance)
}
# Extract eigenvalues
.get_eigenvalues <- function(res_gda) {

  eigenvalues <- res_gda$eig %>% data.frame %>% tibble::rownames_to_column() %>% separate(rowname, c("dim", "num")) %>%
    select(num, eigenvalue) %>% mutate_at(vars(matches("num")), funs(as.numeric)) %>% spread(num, eigenvalue)

  colnames(eigenvalues) <- paste0("Dim.", 1:ncol(eigenvalues))

  return(eigenvalues)
}
# Select trajectory ind
.select_trajectory <- function(coord_all, select, time_point_names, axes) {

  # Vollständige Fälle bestimmen
  selected_ind_complete <- coord_all %>% count(id) %>% filter(n == length(time_point_names)) %>% select(id)
  coord_complete <- coord_all %>% filter(id %in% selected_ind_complete$id)

  # Info bzgl. der Anzahl kompleter Fälle.
  print(glue("Info: {length(selected_ind_complete$id)} complete cases."))

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
    ind_mean_coord <- coord_complete %>% select(-time) %>% group_by(id) %>% summarise_all(funs(mean))
    ind_mean_coord_id <- data.frame(ind_mean_coord)$id

    # "within inertia" berechnen (adaptiert von FactoMineR)
    tmp <- array(0, dim = c(dim(ind_mean_coord %>% select(-id)), length(time_point_names)))
    for(i in seq_along(time_point_names)) {
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
      selected_ind_high <- ind_within_inertia %>% data.frame %>% select(Dim.1 = matches(glue("Dim.{axes[1]}$")), Dim.2 = matches(glue("Dim.{axes[2]}$"))) %>%
        tibble::rownames_to_column() %>% rename(id = rowname) %>% mutate(within_inertia = Dim.1 + Dim.2) %>% arrange(desc(within_inertia)) %>% slice(1:select$within_inertia[[2]])
    }
    if(select$within_inertia[[1]] == 0) {
      selected_ind_low <- NULL
    } else {
      selected_ind_low <- ind_within_inertia %>% data.frame %>% select(Dim.1 = matches(glue("Dim.{axes[1]}$")), Dim.2 = matches(glue("Dim.{axes[2]}$"))) %>%
        tibble::rownames_to_column() %>% rename(id = rowname) %>% mutate(within_inertia = Dim.1 + Dim.2) %>% arrange(within_inertia) %>% slice(1:select$within_inertia[[1]])
    }
    selected_ind <- selected_ind_wi <- rbind(selected_ind_high, selected_ind_low)
  }
  if(!is.null(select$name) & !is.null(select$within_inertia)) {
    selected_ind <- selected_ind_name %>% filter(id %in% selected_ind_wi$id)
  }

  return(selected_ind)
}
# Gruppennamen der einzelnen Kategorien extrahieren
.get_group_names <- function(res_gda, group, group_names) {

  data <- res_gda$call$X %>% mutate_all(funs( sub("\\.", "_", .) ))
  colnames(data) <- colnames(data) %>% sub("\\.", "_", .)

  if(is.null(res_gda$call$excl)) var_num <- GDAtools::getindexcat(data)
  else var_num <- GDAtools::getindexcat(data)[-res_gda$call$excl]

  var_num <- var_num %>%
    tibble(var.cat = .) %>% separate(var.cat, c("var", "cat"), sep = "[.]") %>%
    select(var) %>% count(var) %>% mutate_at(vars(var), funs(as.factor))

  var <- tibble(var = colnames(data)) %>% mutate_all(funs(as.factor))

  n_mod <- left_join(var, var_num, by = "var") %>% .$n
  # n_mod <- res_gda$call$X %>% mutate_each(funs(n_distinct)) %>% distinct

  n_mod_group <- NULL
  start <- 0
  for(i in seq_along(group)) {
    n_mod_group <- c(n_mod_group, sum( n_mod[(start + 1):(start + group[i])] ) )
    start <- sum( group[1:i] )
  }

  # Gruppenzuordnung der Modalitäten
  df_group_names <- data.frame(group = rep(group_names, n_mod_group))

  return(df_group_names)
}
# Beschriftung eines Plots anpassen
.gda_plot_labels <- function(res_gda, ggplot_gda, title, axes, plot_modif_rates = TRUE) {

  if( plot_modif_rates ) {

    modif_rates <- GDAtools::modif.rate(res_gda)

    xlab = glue("Achse {axes[1]} ({modif_rates[axes[1], 1]}%)")

    ylab = glue("Achse {axes[2]} ({modif_rates[axes[2], 1]}%)")

  } else {

    eig <- factoextra::get_eigenvalue(res_gda)[,2]

    xlab = glue("Achse {axes[1]} ({round(eig[axes[1]], 1)}%)")

    ylab = glue("Achse {axes[2]} ({round(eig[axes[2]], 1)}%)")

  }

  p <- ggplot_gda + labs(title = title, x = xlab, y = ylab)
}
