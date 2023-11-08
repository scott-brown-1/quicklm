# @import patchwork

#' Check Linearity
#'
#' @description Check linearity assumption of linear regression.
#'
#' @import ggplot2
#' @import ggfortify
#' @import car
#' @import patchwork
#'
#' @examples
#' data(trees)
#' tree_model <- lm(Volume ~ Height + Girth, data=trees)
#' check_linearity(tree_model)
#'
#' @export
check_linearity <- function(model, is_SLR = NULL){
  cat('\n--- ASSUMPTION: LINEARITY ---\n')

  ## Extract X and y from model
  df <- model$model
  y <- df[1]
  x <- df[2:ncol(df)]

  ## Check if this model is simple linear regression
  if(is.null(is_SLR)){
    is_SLR <- ncol(x) <= 1
  }

  if(is_SLR){
    ## If simple linear regression...

    x_name <- names(x)[1]
    y_name <- names(y)[1]

    ## Print correlation between x and y
    cat(paste0(
      "\nPearson's correlation between ",
      x_name,
      " and ",
      y_name,
      ": \n",
      cor(x,y),
      "\n"))

    ## Plot scatterplot of y vs x with OLS line overlaid
    scatterplot <- ggplot(
      data = df,
      mapping = aes(
        x = .data[[x_name]],
        y = .data[[y_name]])) +
      geom_point() +
      geom_smooth(
        method = "lm",
        se = FALSE,
        formula = y ~ x)

    ## Print plots
    print(scatterplot)
  }else{
    ## If multiple linear regression..

    # ## Plot every variable against the residuals
    # df['residuals'] <- model$residuals
    #
    # scatterplot <- NULL
    #
    # for (col in names(df)[names(df) != 'residuals']) {
    #   ## Plot scatterplot of x vs residuals with OLS line overlaid
    #   single_scatplot <- ggplot(
    #     data = df,
    #     mapping = aes(
    #       x = .data[[col]],
    #       y = .data[['residuals']])) +
    #     geom_point() +
    #     geom_smooth(
    #       method = "lm",
    #       se = FALSE,
    #       formula = y ~ x) +
    #     theme(aspect.ratio = 1) +
    #     labs(
    #       title = paste("Residuals vs.", col),
    #       x = col,
    #       y = 'Residuals')
    #
    #   if(is.null(scatterplot)){
    #     scatterplot <- single_scatplot
    #   }else{
    #     scatterplot <- scatterplot + single_scatplot
    #   }
    # }

    ## Print plots
    print(avPlots(model))
  }

  # Plot residuals vs. fitted values of data
  resid_fitted_plot <- autoplot(model, which = 1, ncol = 1, nrow = 1)
  print(resid_fitted_plot)

  cat("\nCreated scatterplot of y vs. X\n")
  cat("\nCreated residuals vs. fitted values plot\n")
}
