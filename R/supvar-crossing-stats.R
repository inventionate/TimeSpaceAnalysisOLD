#' @include utilities.R
NULL
#' Calculate crossed variables double breakdown of variance.
#'
#' @param res_gda GDA result.
#' @param quali_var crossed variable (vector separated by "_").
#' @param impute handle missing values by imputation.
#'
#' @return Returns a list:
#' \item{between_var1}{between variance var1}
#' \item{var1_within_var2}{var1 within var2 variance}
#' \item{between_var2}{data frame of categories square cosine}
#' \item{var2_within_var1}{data frame of categories within variances, variance between and within categories and variable square correlation ratio (eta2)}
#' \item{additive}{data frame of categories test-values}
#' \item{interaction}{data frame of categories test-values}
#' \item{between_var1_var2}{data frame of categories test-values}
#' @export
supvar_crossing_stats <- function (res_gda, quali_var, impute = TRUE) {

  # Fehlende Werte checken

  # Hier die Variable checken, ob es NAs gibt

  # Evtl. imputieren

  # Ja nach GDA res MCA oder MFA imputieren

  # Extract and name eigenvalues.
  eigenvalues <- res_gda$eig %>% data.frame %>% slice(1:5) %>% add_rownames %>% select(rowname, eigenvalue) %>% spread(rowname, eigenvalue)
  names(eigenvalues) <- paste0("Dim.", names(eigenvalues))

  # Separate supplementary variables.
  var_crossed <- quali_var %>% data_frame(var_crossed = .) %>% separate(var_crossed, c("var1", "var2"), "_")
  var1 <- var_crossed$var1
  var2 <- var_crossed$var2

  # Extract statistical values.
  var_crossed_stats <- GDAtools::varsup(mca, quali_var)
  coord <- var_crossed_stats$coord %>% data.frame %>% add_rownames %>% separate(rowname, c("var1", "var2"), sep = "_")
  weight <- var_crossed_stats$weight %>% data.frame(weight = .) %>% add_rownames %>% separate(rowname, c("var1", "var2"), sep = "_")

  # Within variance 1.
  crossed_within_variance1 <- .crossed_within_variance("var2", weight, coord)

  # Total within variance 1.
  var1_within_var2_tot <- crossed_within_variance1$Dim.1 + crossed_within_variance1$Dim.2

  # Within variance 2.
  crossed_within_variance2 <- .crossed_within_variance("var1", weight, coord)

  # Total within variance 2.
  var2_within_var1_tot <- crossed_within_variance2$Dim.1 + crossed_within_variance2$Dim.2

  # Additive cloud calculations.
  principle_variable <- data.frame(principle_1 = res_gda$ind$coord[,1], principle_2 = res_gda$ind$coord[,2])
  indicator_variables <- FactoMineR::tab.disjonctif(data.frame(var1, var2))
  crossed_base_data <- data.frame(principle_variable, indicator_variables)

  # Lineare Regressionsformel basteln
  formula_1 <- paste("principle_1 ~ ",paste(names(crossed_base_data)[-c(1:3, ncol(crossed_base_data))], collapse=" + "))
  lm_axis_1 <- lm(as.formula(formula_1), data = crossed_base_data)
  lm_res_1 <- summary(axis_1)
  lm_res_1

  formula_2 <- paste("principle_2 ~ ",paste(names(crossed_base_data)[-c(1:3, ncol(crossed_base_data))], collapse=" + "))
  lm_axis_2 <- lm(as.formula(formula_2), data = crossed_base_data)
  lm_res_2 <- summary(axis_2)
  lm_res_2

  # Variance additive cloud.
  additive_tot <- lm_res_1$r.squared * var_crossed_stats$var$`Dim 1`[14] + lm_res_2$r.squared * var_crossed_stats$var$`Dim 2`[14]

  # Interactive cloud calculations.

  # Variance interaction cloud.
  interaction_tot <- var_crossed_stats$var$`Dim 1`[14] - lm_res_1$r.squared * var_crossed_stats$var$`Dim 1`[14] + var_crossed_stats$var$`Dim 2`[14] - lm_res_2$r.squared * var_crossed_stats$var$`Dim 2`[14]

  # Var1 between-variance.
  between_var1_tot <- varsup(mca, var2)$var %>% add_rownames %>% filter(rowname == "between") %>% mutate(total_1_2 = `Dim 1` + `Dim 2`) %>% .$total_1_2

  # Var2 between-variance.
  between_var2_tot <- varsup(mca, var1)$var %>% add_rownames %>% filter(rowname == "between") %>% mutate(total_1_2 = `Dim 1` + `Dim 2`) %>% .$total_1_2

  # Between-variance var1 X var2
  between_var1_var2_tot <- var_crossed_stats$var$`Dim 1`[14] + var_crossed_stats$var$`Dim 2`[14]

  # Double breakdown of variance export.
  list(between_var1 = between_var1_tot,
       var1_within_var2 = var1_within_var2_tot,
       between_var2 = between_var2_tot,
       var2_within_var1 = var2_within_var1_tot,
       additive = additive_tot,
       interaction = interaction_tot,
       between_var1_var2 = between_var1_var2_tot)
}
