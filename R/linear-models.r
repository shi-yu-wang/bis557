
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
  # If factor variable exists, change factor variables to numeric variables
  colno<-c()
  for (i in 1:ncol(data)) {
    if (class(data[,i])=="factor"){
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
  # remove original factor variables
  if (length(colno>0)){
    data<-data[,-colno]
  }
  if ("." %in% formula_check){
    y<-as.matrix((data[,match(formula_check[2],colnames(data))]))
    x<-as.data.frame(data[,-match(formula_check[2],colnames(data))])
    xvar<-colnames(x)
    x<-as.matrix(cbind(intercept,x))
    svd_output <- svd(x)
    U <- svd_output[["u"]]
    D <- 1 / svd_output[["d"]]
    Sinv <- diag(D,nrow = length(D), ncol = length(D))
    V <- svd_output[["v"]]
    pseudo_inv <- V %*% Sinv %*% t(U)
    coefficients <- as.vector(pseudo_inv %*% y)
    names(coefficients)<-c("(Intercept)",xvar)
    linear_model<-list(coefficients=coefficients)
  } else {
  tm<-terms(formula)
  var<-as.character(attr(tm, "variables"))[-1]
  resp<-attr(tm, "response")
  yvar<-var[resp]
  xvar<-var[-resp]
  y<-as.matrix((data[,match(yvar,colnames(data))]))
  x<-as.data.frame(data[,match(xvar,colnames(data))])
  x<-as.matrix(cbind(intercept,x))
  svd_output <- svd(x)
  U <- svd_output[["u"]]
  D <- 1 / svd_output[["d"]]
  Sinv <- diag(D,nrow = length(D), ncol = length(D))
  V <- svd_output[["v"]]
  pseudo_inv <- V %*% Sinv %*% t(U)
  coefficients <- as.vector(pseudo_inv %*% y)
  names(coefficients)<-c("(Intercept)",xvar)
  linear_model<-list(coefficients=coefficients)}
}
