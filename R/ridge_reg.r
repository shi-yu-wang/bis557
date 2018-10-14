



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
