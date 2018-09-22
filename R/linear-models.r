
#' Fit a linear model
#'
#' @description This function passes parameters to the linear_model function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats as.formula model.matrix terms
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  # extract variables
  var<-all.vars(formula)
  # create design matrix
  x<-model.matrix(formula, data = data)
  y<-data[,var[1]]
  # qr decomposition
  xqr<-qr(x)
  # calculate coefficients
  coefficients<-as.vector(qr.coef(xqr,y))
  fit_model<-list()
  fit_model$coefficients<-coefficients
  fit_model$residuals<-NA
  fit_model$fitted.values<-NA
  fit_model$rank<-NA
  fit_model$weights<-NA
  fit_model$df.residual<-NA
  fit_model$call<-NA
  fit_model$terms<-NA
  fit_model$contrasts<-NA
  fit_model$xlevels<-NA
  fit_model$offset<-NA
  fit_model$y<-NA
  fit_model$x<-NA
  fit_model$model<-NA
  fit_model$na.action<-NA
  class(fit_model)<-"lm"
  return(fit_model)
}
