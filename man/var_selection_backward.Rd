% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/var_selection_step.R
\name{var_selection_backward}
\alias{var_selection_backward}
\title{Step backward variable selection}
\usage{
var_selection_backward(
  model,
  response = NULL,
  criteria = "AIC",
  return_model = F
)
}
\description{
Perform step backward variable selection
}
\examples{
data(trees)
tree_model <- lm(Volume ~ Height + Girth, data=trees)
var_selection_backward(tree_model, response='Volume',criteria='AIC', return_model=F)

}
