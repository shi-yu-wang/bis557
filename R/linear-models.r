
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  intercept<-as.data.frame(rep(1,nrow(data)))
  formula_check<-as.character(formula)
  if ("." %in% formula_check){
    formula_check<-formula_check
  } else{
    tm<-terms(formula)
    var<-as.character(attr(tm, "variables"))[-1]
  }
  # generate dataset for analysis
  if ("." %in% formula_check){
    data<-data
  } else {
  data<-data[,match(var,colnames(data))]
  }
  # If factor variable exists, change factor variables to numeric variables
  colno<-c()
  for (i in 1:ncol(data)) {
    if (class(data[,i])!="numeric" & class(data[,i])!="integer"){
      data[,i]<-as.factor(data[,i])
      colno<-c(colno,i)
      f<-levels(data[,i])
      data[,i]<-as.character(data[,i])
      for (j in 2:length(f)) {
        v<-data[,i]
        v[v==f[j]]<-1
        v[v!="1"]<-0
        v<-as.numeric(v)
        v<-as.data.frame(v)
        colnames(v)<-paste(colnames(data)[i], f[j],sep = "")
        data<-cbind(data,v)
      }
    }
  }
  if (length(colno)>0){
    data<-data[,-colno]
  }
  # calculate coefficients
  if ("." %in% formula_check){
    y<-as.matrix((data[,match(formula_check[2],colnames(data))]))
    x<-as.data.frame(data[,-match(formula_check[2],colnames(data))])
    xvar<-colnames(x)
    x<-as.matrix(cbind(intercept,x))
    xqr<-qr(x)
    coefficients<-as.vector(qr.coef(xqr,y))
    names(coefficients)<-c("(Intercept)",xvar)
    fit_model<-list()
    fit_model$coefficients<-coefficients
    fit_model$residuals<-NA
    fit_model$fitted.values<-x %*% as.matrix(fit_model$coefficients)
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
  } else {
  resp<-attr(tm, "response")
  yvar<-var[resp]
  y<-as.matrix((data[,match(yvar,colnames(data))]))
  x<-as.data.frame(data[,-match(yvar,colnames(data))])
  xvar<-colnames(x)
  x<-as.matrix(cbind(intercept,x))
  xqr<-qr(x)
  coefficients<-as.vector(qr.coef(xqr,y))
  names(coefficients)<-c("(Intercept)",xvar)
  fit_model<-list()
  fit_model$coefficients<-coefficients
  fit_model$residuals<-NA
  fit_model$fitted.values<-as.vector(x %*% as.matrix(fit_model$coefficients))
  names(fit_model$fitted.values)<-as.character(seq(1,nrow(data)))
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
}
