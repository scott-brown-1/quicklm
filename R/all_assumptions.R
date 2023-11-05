# source('./R/linearity.R')
# source('./R/homoscedasticity.R')
# source('./R/residual_normality.R')
# source('./R/influential_points.R')
# source('./R/multicollinearity.R')

#' Check All Linear Regression Assumptions
#'
#' @description Check the linearity, homoscedasticity, residual_normality, influential_points, and multicollinearity assumptions of linear regression.
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' check_all_assumptions(tree_model)
#'
#' @export
check_all_assumptions <- function(model){
  ## Check if this model is simple linear regression
  df <- model$model
  is_SLR <- ncol(df[2:ncol(df)]) <= 1

  ## Check all assumptions
  check_linearity(model, is_SLR = is_SLR)
  check_homoscedasticity(model, include_resid_fitted = F)
  check_residual_normality(model)
  check_influential_points(model)

  ## If MLR, check for multicollinearity
  if(!is_SLR) check_multicollinearity(model)
}
