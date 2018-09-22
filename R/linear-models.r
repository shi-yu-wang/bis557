
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats as.formula model.matrix terms
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  f = all.vars(formula)
  
  ## Get the response variable and the design matrix
  X = model.matrix(formula, data = data)
  Y = data[, f[1]]
  
  ## Create a list to store all components of an object class of "lm"
  fit_linear_model = list()
  
  ## All components of an object class of "lm"
  fit_linear_model$coefficients = qr.coef(qr(X), Y)
  fit_linear_model$fitted.values = X %*% fit_linear_model$coefficients
  fit_linear_model$residuals = fit_linear_model$y - fit_linear_model$fitted.values
  fit_linear_model$rank = ncol(X)
  fit_linear_model$weights = NULL
  fit_linear_model$df.residual = nrow(X) - ncol(X)
  fit_linear_model$call = call("linear_model", formula)
  fit_linear_model$terms = terms(x = formula, data = data)
  fit_linear_model$contrasts = NULL
  fit_linear_model$xlevels = NULL
  fit_linear_model$offset = NULL
  fit_linear_model$x = X
  fit_linear_model$y = Y
  fit_linear_model$model = formula
  fit_linear_model$qr = qr(X)
  
  ## Set the list as an object class of "lm"
  class(fit_linear_model) = "lm"
  
  ## Return the list
  return(fit_linear_model)
}
