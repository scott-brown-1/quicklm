#' Regularization variable selection
#'
#' @description Perform regularization variable selection: LASSO or Elastic Net
#'
#' @import glmnet
#' @import ggplot2
#' @import ggfortify
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_regularization(tree_model, response = 'Volume', type='lasso', show_plot=T)
#'
#' @export
var_selection_regularization <- function(model, response=NULL, type='lasso', show_plot=T){
  ## Extract X and y from model
  df <- if(class(model) == 'lm') model$model else as.data.frame(model)

  y <- df[[response]]
  x <- as.matrix(df[colnames(df) != response])

  # y <- df[, ncol(df)]
  # x <- as.matrix(df[, 1:ncol(df)-1])

  ## Specify LASSO (alpha=1) vs elastic net (alpha=0.5)
  alpha = if(tolower(type) == 'lasso') 1 else 0.5

  ## Initialize CV
  cv <- cv.glmnet(x = x, y = y, type.measure = "mse", alpha = alpha)

  ## Plot log lambda vs MSE
  if(show_plot) print(autoplot(cv, label = FALSE))

  ## Print lambda within 1 standard error of min CV error
  cat('\nLAMBDA MIN :\n\n')
  print(coef(cv, s = "lambda.min"))
  cat('\nLAMBDA WITHIN 1 STD ERR OF MIN:\n\n')
  print(coef(cv, s = "lambda.1se"))
}

#' LASSO regularization variable selection
#'
#' @description Perform LASSO regularization variable selection
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_lasso(tree_model, response = 'Volume', show_plot=T)
#'
#' @export
var_selection_lasso <- function(model, response=NULL, show_plot=T){
  ## Perform variable selection via regularization
  cat('--- VARIABLE SELECTION: LASSO REGULARIZATION ---\n')

  var_selection_regularization(
    model = model,
    response = response,
    type = 'lasso',
    show_plot = show_plot)
}

#' Elastic Net regularization variable selection
#'
#' @description Perform Elastic Net regularization variable selection
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_elastic_net(tree_model, response = 'Volume', show_plot=T)
#'
#' @export
var_selection_elastic_net <- function(model, response=NULL, show_plot=T){
  ## Perform variable selection via regularization
  cat('--- VARIABLE SELECTION: ELASTIC NET REGULARIZATION ---\n')

  var_selection_regularization(
    model = model,
    response = response,
    type = 'elastic net',
    show_plot = show_plot)
}
