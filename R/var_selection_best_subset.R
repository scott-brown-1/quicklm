#' Best subsets variable selection
#'
#' @description Perform best subsets variable selection
#'
#' @import bestglm
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' var_selection_best_subset(tree_model, criteria='AIC', print_summary=T)
#'
#' @export
var_selection_best_subset <- function(model, criteria='AIC', print_summary=T){
  ## Extract data from model
  # This df has response last
  df <- if(class(model) == 'lm') model$model else as.data.frame(model)

  ## Calculate best model by criteria using subsets
  best_subsets_mod <- bestglm(df, IC = criteria, method = "exhaustive")

  ## Display best models
  cat(paste0('--- VARIABLE SELECTION: ',criteria,' BEST SUBSETS ---\n'))
  if(print_summary) print(summary(best_subsets_mod$BestModel))
  return(best_subsets_mod)
}

data(trees)
tree_model <- lm(Volume ~ Height + Girth, data=trees)
var_selection_best_subset(tree_model, criteria='AIC', print_summary=T)
