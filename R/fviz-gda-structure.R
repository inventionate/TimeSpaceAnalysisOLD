fviz_gda_structure <- function(res_gda, df_var_quali, var_quali_names, title = "MCA quali interaction effects",
                               additive_cloud_alpha = 0.75, additive_cloud_linetype = "solid", additive_cloud_size = 1,
                               additive_cloud_colour = NULL, scale_additive_cloud_points = TRUE, axes = 1:2, palette = "Set1",
                               mod_level_order = NULL, myriad = TRUE) {
}

# Tests um additive Wolken zu konstruieren

# Lineare Regression
lm(bsp5$Gewicht~bsp5$Lebenstag)
