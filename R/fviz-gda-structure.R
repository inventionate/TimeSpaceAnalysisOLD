#' Visualize addtive cloud.
#'
#' @param res_gda GDA result.
#' @param df_var_quali crossed variable data frame.
#' @param var_quali_names names of crossed variables.
#' @param title plot title.
#' @param additive_cloud_alpha opacity of the cloud.
#' @param additive_cloud_linetype linetype of the cloud.
#' @param additive_cloud_size border size of the cloud.
#' @param additive_cloud_colour colour of the cloud.
#' @param scale_additive_cloud_points scale points (boolean).
#' @param axes which axis to plot.
#' @param palette colour palette (boolean).
#' @param mod_level_order order of modalities.
#' @param myriad use Myriad Pro font (boolean).
#'
#' @return ggplot2 visualization of additive cloud.
#' @export
fviz_gda_structure <- function(res_gda, df_var_quali, var_quali_names, title = "MCA quali interaction effects",
                               additive_cloud_alpha = 0.75, additive_cloud_linetype = "solid", additive_cloud_size = 1,
                               additive_cloud_colour = NULL, scale_additive_cloud_points = TRUE, axes = 1:2, palette = "Set1",
                               mod_level_order = NULL, myriad = TRUE) {
  # Dev message.
  print("Additive Wolke: Die Funktion befindet sich im Alpha-Stadium und ist noch nicht einsatzfähig.")

}
