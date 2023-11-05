#' Check Minimal Multicollinearity Assumption
#'
#' @description Check the minimal multicollinearity assumption of linear regression.
#'
#' @import car
#' @import GGally
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' check_multicollinearity(tree_model)
#'
#' @export
check_multicollinearity <- function(model){
  cat('\n--- ASSUMPTION: MINIMAL MULTICOLLINEARITY ---\n')

  ## Extract X from model
  df <- model$model
  x <- df[2:ncol(df)]

  ## Check if this model is simple linear regression
  is_MLR <- ncol(x) > 1

  if(!is_MLR){
    message('WARNING:\nMulticollinearity cannot exist in simple linear regression.\n')
    message('Only found predictor:\n')
    print(names(x))
    cat('\n')
    return()
  }

  ## Check variance inflation factors
  var_inflations <- vif(model)

  cat('\nVariance inflation factors:\n')
  print(var_inflations)
  cat('\nVIF summary:\n\n')
  print(summary(var_inflations))
  cat('\n')

  ## Create correlation matrix
  corr_plot <- GGally::ggpairs(df)

  ## Print plots
  print(corr_plot)
  cat('\nCreated correlation matrix\n')
}
