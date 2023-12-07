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
var_selection_best_subset <- function(model, response=NULL, criteria='AIC', return_model=F){
  ## Extract data from model
  # This df has response last
  df <- if(class(model)[1] == 'lm') model$model else as.data.frame(model)

  if(!is.null(response)) df <- df[c(colnames(df)[colnames(df) != response], response)]

  ## Calculate best model by criteria using subsets
  best_subsets_mod <- bestglm(df, IC = criteria, method = "exhaustive")

  ## Display best models
  cat(paste0('--- VARIABLE SELECTION: ',criteria,' BEST SUBSETS ---\n'))
  print(summary(best_subsets_mod$BestModel))
  if(return_model) return(best_subsets_mod)
}
