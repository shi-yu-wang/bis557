#' @description generate sparse matrix using sparse.matrix, using add, multiply and transpose
#' @param i row indeces of non-zero elements
#' @param j column indeces of non-zero elements
#' @param x non-zero values
#' @param dims the dimension of generated sparse matrix
#' @return a sparse.matrix
#' @export

# 3
### Note: get hints from Wenfeng Zhang for this problem. Credits to him!

# Implement a `sparse_multiply` function that multiplies two sparse matrices.
casl_sparse_multiply <-
  function(a, b){
    colnames(b) <- c("i2", "j2", "x2")
    c <- merge(a, b, by.x = "j", by.y = "i2",
               all = FALSE, suffixes = c("1", "2"))
    c$x <- c$x * c$x2
    c$key <- paste(c$i, c$j, sep = "-")
    x <- tapply(c$x, c$key, sum)
    key <- strsplit(names(x), "-")
    d <- data.frame(i = sapply(key, getElement, 1),
                    j = sapply(key, getElement, 2),
                    x = as.numeric(x))
    d
  }
a <- data.frame(i = c(1, 2),j = c(1, 1),x =c(3, 1))
b <- data.frame(i = c(1, 2, 2),j = c(1, 1, 2),x = c(4.4, 1.2, 3))
casl_sparse_multiply(a, b)

# Define the class
sparse.matrix<-function(i,j,x,dims=c(max(i),max(j))){
  if (length(i)!=length(j)||length(j)!=length(x))
  stop("Incorrect dimensions")
  structure(list(data.frame(i =c(1,2),j=c(1,1),x=c(3,1)),dims),class="sparse.matrix")
}

# Add
#' @description adding two sparse matrices
#' @param a a list for a sparse matrix
#' @param b a list for a sparse matrix
#' @return a sparse.matrix
#' @export
# The function
sparse_matrix_add<-function(a, b){
  if (any(a$dims!=b$dims)==TRUE)
  stop("Incorrect dimensions")
  c<-merge(a[[1]],b[[1]],by=c("i","j"),all=TRUE,suffixes=c("1","2"))
  c$x1[is.na(c$x1)]<-0
  c$x2[is.na(c$x2)]<-0
  c$x<-c$x1+c$x2
  c[,c("i","j","x")]
  spar_mtx<-list(mat=c,dims=a$dims) 
  class(spar_mtx)<-"sparse.matrix" 
  spar_mtx
}
# Implement
`+.sparse.matrix`<-function(x, y){
  sparse_matrix_add(x, y)
}

# Multiply
#' @description multiplying two sparse matrices
#' @param a a list for a sparse matrix
#' @param b a list for a sparse matrix
#' @return a sparse.matrix
#' @export
# The function
sparse_matrix_multiply<- function(a, b){
  if ((a[[2]][2]!=b[[2]][1]))
  stop("Incorrect dimensions")
  colnames(b[[1]])<-c("i2","j2","x2")
  c<-merge(a[[1]],b[[1]],by.x="j",by.y="i2",all=FALSE,suffixes=c("1", "2"))
  c$x<-c$x*c$x2
  c$key<-paste(c$i,c$j,sep="-")
  x<-tapply(c$x,c$key,sum)
  key<-strsplit(names(x),"-")
  d<-data.frame(i=sapply(key,getElement,1),j=sapply(key,getElement,2),x=as.numeric(x))
  d$i<-as.numeric(d$i)
  d$j<-as.numeric(d$j)
  spar_mtx<-list(mat =d,dims=c(a$dims[1],b$dims[2]))
  class(spar_mtx)<-"sparse.matrix"
  spar_mtx
}
# Implement
`%*%.default`=.Primitive("%*%")
`%*%`<-function(x,y){
  UseMethod("%*%",x)
}
`%*%.sparse.matrix`<-function(x,y){
  sparse_matrix_multiply(x,y)
}
# Transpose
#' @description transposing a sparse matrix
#' @param a a list for a sparse matrix
#' @return a sparse.matrix
#' @export
# The function
sparse_matrix_trans<-function(a) {
  mtx<-a$mat
  swt<-mtx$i
  mtx$i<-mtx$j
  mtx$j<-swt
  spar_mtx<-list(mat=mtx,dims=c(a$dims[2],a$dims[1])) 
  class(spar_mtx)<-"sparse.matrix" 
  spar_mtx
}
# Implement
`t.sparse.matrix`<-function(x) {
  sparse_matrix_trans(x)
}
