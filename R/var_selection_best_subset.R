#' Best subsets variable selection
#'
#' @description Perform best subsets variable selection
#'
#' @import bestglm
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_best_subset(tree_model, criteria='AIC', return_model=F)
#'
#' @export
var_selection_best_subset <- function(model, criteria='AIC', return_model=F){
  ## Extract data from model
  # This df has response last
  df <- if(class(model) == 'lm') model$model else as.data.frame(model)

  ## Calculate best model by criteria using subsets
  best_subsets_mod <- bestglm(df, IC = criteria, method = "exhaustive")

  ## Display best models
  cat(paste0('--- VARIABLE SELECTION: ',criteria,' BEST SUBSETS ---\n'))
  print(summary(best_subsets_mod$BestModel))
  if(return_model) return(best_subsets_mod)
}

data(trees)
tree_model <- lm(Volume ~ Height + Girth, data=trees)
var_selection_best_subset(tree_model, criteria='AIC', return_model=F)



height      -0.44669    0.10406  -4.293 2.55e-05 ***
  neck        -0.48479    0.17997  -2.694  0.00755 **
  chest       -0.14382    0.08160  -1.762  0.07924 .
abdom        0.82586    0.06056  13.638  < 2e-16 ***
