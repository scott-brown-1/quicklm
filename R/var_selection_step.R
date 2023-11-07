library(bestglm)

#' Stepwise variable selection
#'
#' @description Perform stepwise variable selection: forward, backward, or sequential replacement
#'
#' @import bestglm
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_step(tree_model, direction='forward', response='Volume', criteria='AIC', return_model=F)
#'
#' @export
var_selection_step <- function(model, direction, response = NULL, criteria='AIC'){
  ## Extract data from model
  df <- if(class(model) == 'lm') model$model else as.data.frame(model)

  if(is.null(response)) response <- names(df[1])[[1]]

  ## Create null and full models as baseline

  # Intercept only model
  base_mod <- lm(as.formula(paste0(response, '~1')), data = df)
  # All predictors in model
  full_mod <- lm(as.formula(paste0(response, '~.')), data = df)

  ## Create function for AIC vs BIC
  k_func <- if(tolower(criteria) == 'bic') log(nrow(df)) else 2

  start_mod <- if(direction == 'forward') base_mod else full_mod

  ## Calculate best model by AIC and BIC using step forward
  step_mod <- step(start_mod,
                   k = k_func,
                   direction = direction,
                   trace = 0,
                   scope=list(lower = base_mod, upper = full_mod))

  ## Display best models
  print(summary(step_mod))
  return(step_mod)
}

#' Step forward variable selection
#'
#' @description Perform step forward variable selection
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_forward(tree_model, response='Volume',criteria='AIC', return_model=F)
#'
#' @export
var_selection_forward <- function(model, response=NULL, criteria='AIC', return_model=F){
  cat(paste0('--- VARIABLE SELECTION: ',criteria,' STEP FORWARD ---\n'))
  ## Perform stepwise variable selection
  selected_model <- var_selection_step(
    model = model,
    response = response,
    direction = 'forward',
    criteria = criteria)

  ## Return results
  if(return_model) return(selected_model)
}

#' Step backward variable selection
#'
#' @description Perform step backward variable selection
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_backward(tree_model, response='Volume',criteria='AIC', return_model=F)
#'
#' @export
var_selection_backward <- function(model, response=NULL, criteria='AIC', return_model=F){
  cat(paste0('--- VARIABLE SELECTION: ',criteria,' STEP BACKWARD ---\n'))

  ## Perform stepwise variable selection
  selected_model <- var_selection_step(
    model = model,
    response = response,
    direction = 'backward',
    criteria = criteria)

  ## Return results
  if(return_model) return(selected_model)
}

#' Sequential replacement variable selection
#'
#' @description Perform sequential replacement variable selection
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_sequential(tree_model, response='Volume', criteria='AIC', return_model=F)
#'
#' @export
var_selection_sequential <- function(model, response=NULL, criteria='AIC', return_model=F){
  cat(paste0('--- VARIABLE SELECTION: ',criteria,' SEQUENTIAL REPLACEMENT ---\n'))

  ## Perform stepwise variable selection
  selected_model <- var_selection_step(
    model = model,
    response = response,
    direction = 'both',
    criteria = criteria)

  ## Return results
  if(return_model) return(selected_model)
}
