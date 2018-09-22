
#' Fit a linear model
#'
#' @description This function passes parameters to the linear_model function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  # extract variables
  var=all.vars(formula)
  # create design matrix
  x=model.matrix(formula, data = data)
  y=data[,var[1]]
  # qr decomposition
  xqr=qr(x)
  # list to contain lm items
  fit_model=list()
  # calculate coefficients
  fit_model$coefficients=qr.coef(xqr,y)
  fit_model$fitted.values=NULL
  fit_model$residuals=NULL
  fit_model$rank=NULL
  fit_model$weights=NULL
  fit_model$df.residual=NULL
  fit_model$call=call("linear_model",formula)
  fit_model$terms=NULL
  fit_model$contrasts=NULL
  fit_model$xlevels=NULL
  fit_model$offset=NULL
  fit_model$x=x
  fit_model$y=y
  fit_model$model=formula
  fit_model$qr=qr(x)
  class(fit_model)="lm"
  return(fit_model)
}
