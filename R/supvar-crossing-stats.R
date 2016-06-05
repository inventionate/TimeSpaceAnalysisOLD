#' @include utilities.R
#' @include supvar-stats.R
NULL
#' Calculate crossed variables double breakdown of variance.
#'
#' @param res_gda GDA result.
#' @param var_quali_df the supplementary data frame.
#' @param var_quali crossed supplementary variable (vector separated by "_").
#' @param impute impute missing data (boolean).
#' @param axes the GDA dimensions to calculate.
#'
#' @return Returns a list:
#' \item{var}{double breakdown of variance.}
#' \item{reg}{result of linear regressions.}
#' @export
supvar_crossing_stats <- function(res_gda, var_quali_df, var_quali, impute = TRUE, axes = 1:2) {


  # Extract statistical values.
  var_crossed_stats <- supvar_stats(res_gda, var_quali_df, var_quali, impute)

  coord <- var_crossed_stats$coord %>% data.frame %>% add_rownames %>% separate(rowname, c("var1", "var2"), sep = "_")

  weight <- var_crossed_stats$weight %>% data.frame(weight = .) %>% add_rownames %>% separate(rowname, c("var1", "var2"), sep = "_")

  # Extract eigenvalues.
  eigenvalues <- res_gda$eig %>% data.frame %>% slice(1:res_gda$call$ncp) %>% add_rownames %>% select(rowname, eigenvalue) %>% spread(rowname, eigenvalue)
  names(eigenvalues) <- paste0("Dim.", names(eigenvalues))

  # Separate supplementary variables.
  var_crossed <- var_crossed_stats$supvar %>% data_frame(var_crossed = .) %>% separate(var_crossed, c("var1", "var2"), "_")
  var1 <- var_crossed$var1
  var2 <- var_crossed$var2

  # Var1 between-variance.
  var1_var <- supvar_stats(res_gda, data.frame(var1 = var1), "var1", impute)
  between_var1_1 <- var1_var$var %>% add_rownames %>% filter(rowname == "between") %>% select(dim = matches(paste0("Dim.", axes[1]))) %>% .$dim
  between_var1_2 <- var1_var$var %>% add_rownames %>% filter(rowname == "between") %>% select(dim = matches(paste0("Dim.", axes[2]))) %>% .$dim
  between_var1_tot <- between_var1_1 + between_var1_2

  # Var2 between-variance.
  var2_var <- supvar_stats(res_gda, data.frame(var2 = var2), "var2", impute)
  between_var2_1 <- var2_var$var %>% add_rownames %>% filter(rowname == "between") %>% select(dim = matches(paste0("Dim.", axes[1]))) %>% .$dim
  between_var2_2 <- var2_var$var %>% add_rownames %>% filter(rowname == "between") %>% select(dim = matches(paste0("Dim.", axes[2]))) %>% .$dim
  between_var2_tot <- between_var2_1 + between_var2_2

  # Between-variance var1 X var2
  between_var1_var2_1 <- var_crossed_stats$var %>% add_rownames %>% filter(rowname == "between") %>% select(dim = matches(paste0("Dim.", axes[1]))) %>% .$dim
  between_var1_var2_2 <- var_crossed_stats$var %>% add_rownames %>% filter(rowname == "between") %>% select(dim = matches(paste0("Dim.", axes[2]))) %>% .$dim
  between_var1_var2_tot <- between_var1_var2_1 + between_var1_var2_2


  # Within variance 1.
  crossed_within_variance1 <- .crossed_within_variance("var1", weight, coord, eigenvalues)

  # Total within variance 1.
  var2_within_var1_1 <- crossed_within_variance1$Dim.1
  var2_within_var1_2 <- crossed_within_variance1$Dim.2
  var2_within_var1_tot <- var2_within_var1_1 + var2_within_var1_2

  # Within variance 2.
  crossed_within_variance2 <- .crossed_within_variance("var2", weight, coord, eigenvalues)

  # Total within variance 2.
  var1_within_var2_1 <- crossed_within_variance2$Dim.1
  var1_within_var2_2 <- crossed_within_variance2$Dim.2
  var1_within_var2_tot <- var1_within_var2_1 + var1_within_var2_2

  # Additive cloud calculations.
  principle_variable <- data.frame(principle_1 = res_gda$ind$coord[, axes[1]], principle_2 = res_gda$ind$coord[, axes[2]])
  indicator_variables <- FactoMineR::tab.disjonctif(data.frame(var1, var2))
  crossed_base_data <- data.frame(principle_variable, indicator_variables)

  # Lineare Regressionen berechnen
  formula_1 <- paste("principle_1 ~ ", paste(names(crossed_base_data)[-c(1:3, ncol(crossed_base_data))], collapse = " + "))
  lm_axis_1 <- lm(as.formula(formula_1), data = crossed_base_data)
  lm_res_1 <- summary(lm_axis_1)

  formula_2 <- paste("principle_2 ~ ", paste(names(crossed_base_data)[-c(1:3, ncol(crossed_base_data))], collapse = " + "))
  lm_axis_2 <- lm(as.formula(formula_2), data = crossed_base_data)
  lm_res_2 <- summary(lm_axis_2)


  # Variance additive cloud.
  additive_1 <- lm_res_1$r.squared * between_var1_var2_1
  additive_2 <- lm_res_2$r.squared * between_var1_var2_2
  additive_tot <- additive_1 + additive_2

  # Variance interaction cloud.
  interaction_1 <- between_var1_var2_1 - additive_1
  interaction_2 <- between_var1_var2_2 - additive_2
  interaction_tot <- between_var1_var2_tot- additive_tot

  # Double breakdown of variance df.

  dim_1 <- c(between_var1_1, var1_within_var2_1, between_var2_1, var2_within_var1_1, additive_1, interaction_1, between_var1_var2_1)
  dim_2 <- c(between_var1_2, var1_within_var2_2, between_var2_2, var2_within_var1_2, additive_2, interaction_2, between_var1_var2_2)
  plane_1_2 <- c(between_var1_tot, var1_within_var2_tot, between_var2_tot, var2_within_var1_tot, additive_tot, interaction_tot, between_var1_var2_tot)
  db_var <- rbind(dim_1, dim_2, plane_1_2)
  # Names
  colnames(db_var) <- c("beween var1", "var1 within var2", "beween var2", "var2 within var1", "additive", "interaction", "var1 X var2")
  rownames(db_var) <- c(paste0("Dim.", axes[1]), paste0("Dim.", axes[2]), paste0("Plane.", axes[1], ".", axes[2]))

  # Export
  list(var = db_var, reg = list(Dim.1 = lm_res_1, Dim.2 = lm_res_2))
}
