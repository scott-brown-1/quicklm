#' Check Residual Normality
#'
#' @description Check residual normality assumption of linear regression.
#'
#' @import ggplot2
#' @import car
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' check_residual_normality(tree_model)
#'
#' @export
check_residual_normality <- function(model){
  cat('\n--- ASSUMPTION: NORMAL RESIDUALS ---\n')

  df <- model$model
  df['residuals'] <- model$residuals

  ## Print results of Shapiro-Wilk test on residuals
  cat('\nShapiro-Wilk Normality Test, null hypothesis that residuals ARE normal: \n')
  print(shapiro.test(df$residuals))

  ## Create histogram of residuals
  resid_hist <- ggplot(data = df) +
    geom_histogram(
      mapping = aes(
        x = residuals,
        y = after_stat(density))) +
    stat_function(
      fun = dnorm, color = "red", linewidth = 2,
      args = list(
        mean = mean(df$residuals),
        sd = sd(df$residuals)))

  ## Print plots
  # Create QQ plot of residuals
  car::qqPlot(model)
  suppressMessages(print(resid_hist))

  cat("\nCreated residual histogram plot\n")
  cat("\nCreated residual QQ plot\n")
}
