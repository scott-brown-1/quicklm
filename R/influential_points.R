#' Check Unduly Influential Points Assumption
#'
#' @description Check the no unduly influential points assumption of linear regression.
#'
#' @import ggplot2
#' @import ggfortify
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' check_influential_points(tree_model)
#'
#' @export
check_influential_points <- function(model){
  cat('\n--- ASSUMPTION: NO UNDULY INFLUENTIAL POINTS --- \n')

  df <- model$model

  ## Define functions to calculate Cook's distance
  cd_cont_pos <- function(leverage, level, model) {sqrt(level*length(coef(model))*(1-leverage)/leverage)}
  cd_cont_neg <- function(leverage, level, model) {-cd_cont_pos(leverage, level, model)}

  ## Set Cook's distance threshold as 4/df
  cd_threshold <- 4 / model$df.residual

  ## Display Residuals vs. Leverage plot
  resid_lev_plot <- autoplot(model, which = 5) +
    stat_function(fun = cd_cont_pos,
                  args = list(level = cd_threshold, model = model),
                  xlim = c(0, 0.6), lty = 2, colour = "red") +
    stat_function(fun = cd_cont_neg,
                  args = list(level = cd_threshold, model = model),
                  xlim = c(0, 0.6), lty = 2, colour = "red") +
    scale_y_continuous(limits = c(-4, 4))


  cat("\nPoints with highest Cook's Distance:\n")
  print(head(
    cooks.distance(model)[
      order(cooks.distance(model), decreasing=T)]
    ))
  cat('\n')

  ## Print plots
  print(resid_lev_plot)

  cat("\nCreated Residuals vs. Leverage plot\n")
}
