#source('./R/setup.R')

#' Check Homoscedasticity
#'
#' @description Check homoscedasticity assumption of linear regression.
#'
#' @import ggplot2, ggfortify
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' check_homoscedasticity(tree_model)
#'
#' @export
check_homoscedasticity <- function(model, include_resid_fitted = T){
  cat('\n--- ASSUMPTION: HOMOSCEDASTICITY ---\n')

  # Plot residuals vs. fitted values of data
  if(include_resid_fitted) resid_fitted_plot <- autoplot(model, which = 1, ncol = 1, nrow = 1)

  ## Create scale-location plot
  scale_location_plot <- autoplot(model, which = 3, nrow = 1, ncol = 1)

  ## Print plots
  print(scale_location_plot)
  if(include_resid_fitted) print(resid_fitted_plot)

  cat("\nCreated scale-location plot\n")
  if(include_resid_fitted) cat("\nCreated residuals vs. fitted values plot\n")
}
