#' @description generate sparse matrix using sparse.matrix, using add, multiply and transpose
#' @param i row indeces of non-zero elements
#' @param j column indeces of non-zero elements
#' @param x non-zero values
#' @param dims the dimension of generated sparse matrix
#' @return a sparse.matrix
#' @export

# 3
### Note: get hints from Wenfeng Zhang for this problem.
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
a <- data.frame(i = c(1, 2), j = c(1, 1), x = c(3, 1))
b <- data.frame(i = c(1, 2, 2), j = c(1, 1, 2), x = c(4.4, 1.2, 3))
casl_sparse_multiply(a, b)

# Define the class
sparse.matrix<-function(i,j,x,dim=c(max(i),max(j))){
  structure(list(data.frame(i =c(1,2),j=c(1,1),x=c(3,1)),dim),class="sparse.matrix")
}

# Add
`+.sparse.matrix` <- function(a, b){
if (!identical(a[[2]], b[[2]]))
stop("dimensions do not match")
  c<-merge(a[[1]], b[[1]], by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c[, c("i", "j", "x")]
  sparse.matrix(c$i, c$j, c$x, dim = a[[2]])
}

# Multiply 
# %*% is not S3 object
`%*%.default` = .Primitive("%*%")  # keep defalut
`%*%` = function(x,...){ 
  UseMethod("%*%",x)
}
`%*%` <- function(x, y) {
  UseMethod("%*%", x)
}

`%*%.sparse.matrix` <- function(a, b){
if ((a[[2]][2] != b[[2]][1]))
stop("dimensions do not match")
  colnames(b[[1]]) <- c("i2", "j2", "x2")
  c <- merge(a[[1]], b[[1]], by.x = "j", by.y = "i2",
             all = FALSE, suffixes = c("1", "2"))
  c$x <- c$x * c$x2
  c$key <- paste(c$i, c$j, sep = "-")
  x <- tapply(c$x, c$key, sum)
  key <- strsplit(names(x), "-")
  d <- data.frame(i = sapply(key, getElement, 1),
                  j = sapply(key, getElement, 2),
                  x = as.numeric(x))
  sparse.matrix(c$i, c$j, c$x, dim = c(a[[2]][1], b[[2]][2]))
}

# Transpose
t <- function (x, ...) {
  UseMethod("t", x)
}

`t.sparse.matrix` <- function(a){
  temp <- a[[1]]$i
  a[[1]]$i <- a[[1]]$j
  a[[1]]$j <- temp
  a[[2]] <- rev(a[[2]])
  return(a)
}
