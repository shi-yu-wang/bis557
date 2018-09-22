
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
  } else {
    data<-data
  }
  # calculate coefficients
  if ("." %in% formula_check){
    y<-as.matrix((data[,match(formula_check[2],colnames(data))]))
    x<-as.data.frame(data[,-match(formula_check[2],colnames(data))])
    xvar<-colnames(x)
    x<-as.matrix(cbind(intercept,x))
    xqr<-qr(x)
    coefficients<-qr.coef(xqr,y)
    fit_model<-list()
    fit_model$coefficients<-coefficients
    fit_model$fitted.values<-x %*% fit_model$coefficients
    fit_model$residuals<-fit_model$y - fit_model$fitted.values
    fit_model$rank<-ncol(x)
    fit_model$weights<-NULL
    fit_model$df.residual<-nrow(x)-ncol(x)
    fit_model$call<-call("linear_model", formula)
    fit_model$terms<-terms(x = formula, data = data)
    fit_model$contrasts<-NULL
    fit_model$xlevels<-NULL
    fit_model$offset<-NULL
    fit_model$y<-y
    fit_model$x<-y
    fit_model$model<-formula
    fit_model$qr<-qr(x)
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
  coefficients<-qr.coef(xqr,y)
  fit_model<-list()
  fit_model$coefficients<-coefficients
  fit_model$fitted.values<-x %*% fit_model$coefficients
  fit_model$residuals<-fit_model$y - fit_model$fitted.values
  fit_model$rank<-ncol(x)
  fit_model$weights<-NULL
  fit_model$df.residual<-nrow(x)-ncol(x)
  fit_model$call<-call("linear_model", formula)
  fit_model$terms<-terms(x = formula, data = data)
  fit_model$contrasts<-NULL
  fit_model$xlevels<-NULL
  fit_model$offset<-NULL
  fit_model$y<-y
  fit_model$x<-x
  fit_model$model<-formula
  fit_model$qr<-qr(x)
  class(fit_model)<-"lm"
  return(fit_model)
  }
}
