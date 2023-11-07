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
#' var_selection_step(tree_model, direction='forward', criteria='AIC', print_summary=T)
#'
#' @export
var_selection_step <- function(model, direction, criteria='AIC', print_summary=T){
  ## Extract data from model
  df <- model$model

  ## Create null and full models as baseline

  # Intercept only model
  base_mod <- lm(as.formula(paste0(names(df[1])[[1]], '~1')), data = df)
  # All predictors in model
  full_mod <- lm(as.formula(paste0(names(df[1])[[1]], '~.')), data = df)

  ## Create function for AIC vs BIC
  k_func <- if(tolower(criteria) == 'bic') log(nrow(df)) else 2

  ## Calculate best model by AIC and BIC using step forward
  step_mod <- step(base_mod,
                   k = k_func,
                   direction = direction,
                   trace = 0,
                   scope=list(lower = base_mod, upper = full_mod))

  ## Display best models
  if(print_summary) print(summary(step_mod))
  return(step_mod)
}

#' Step forward variable selection
#'
#' @description Perform step forward variable selection
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_forward(tree_model, criteria='AIC', print_summary=T)
#'
#' @export
var_selection_forward <- function(model, criteria='AIC', print_summary=T){
  ## Perform stepwise variable selection
  selected_model <- var_selection_step(
    model = model,
    direction = 'forward',
    criteria = criteria,
    print_summary = print_summary)

  ## Return results
  cat(paste0('--- VARIABLE SELECTION: ',criteria,' STEP FORWARD ---\n'))
  return(selected_model)
}

#' Step backward variable selection
#'
#' @description Perform step backward variable selection
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_backward(tree_model, criteria='AIC', print_summary=T)
#'
#' @export
var_selection_backward <- function(model, criteria='AIC', print_summary=T){
  ## Perform stepwise variable selection
  selected_model <- var_selection_step(
    model = model,
    direction = 'backward',
    criteria = criteria,
    print_summary = print_summary)

  ## Return results
  cat(paste0('--- VARIABLE SELECTION: ',criteria,' STEP BACKWARD ---\n'))
  return(selected_model)
}

#' Sequential replacement variable selection
#'
#' @description Perform sequential replacement variable selection
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_sequential(tree_model, criteria='AIC', print_summary=T)
#'
#' @export
var_selection_sequential <- function(model, criteria='AIC', print_summary=T){
  ## Perform stepwise variable selection
  selected_model <- var_selection_step(
    model = model,
    direction = 'both',
    criteria = criteria,
    print_summary = print_summary)

  ## Return results
  cat(paste0('--- VARIABLE SELECTION: ',criteria,' SEQUENTIAL REPLACEMENT ---\n'))
  return(selected_model)
}