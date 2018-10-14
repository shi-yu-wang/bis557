#' Fit a ridge regression model
#'
#' @description This function passes parameters to the ridge_reg function.
#' @param formula a formula
#' @param data a data.frame
#' @return An ridge_reg object
#' @import stats MASS graphics
#' @examples
#' fit <- ridge_reg(Sepal.Length ~., 1.2,iris)
#' @export


# write the code as induced in class, credit to Professor Kane
ridge_reg <- function(formula, lambda, data){
  m <- model.matrix(formula, data)
  y <- matrix(data[,as.character(formula)[2]],ncol=1)
  y <- y[as.numeric(rownames(m)),,drop=FALSE]
  
  # calculate by svd
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  
  D<-diag(svals/(svals^2+lambda))
  beta<-V %*% D %*% t(U) %*% y
  ret<-list(coefficients=beta)
  class(ret)<-"ridge_reg"
  ret
}
