
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
  #generate "lm" class by a random linear regression
  d1 <- data.frame(x1 = runif(100, 0, 1),
             x2 = runif(100, 0, 1),
             x3 = runif(100, 0, 1),
             y1 = runif(100, 0, 1))
  m1 <- lm(y1 ~ x1 + x2 + x3, data = d1)
  
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
  if (length(colno>0)){
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
    m1$coefficients<-coefficients
    linear_model<-m1
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
  m1$coefficients<-coefficients
  linear_model<-m1
  }
}
