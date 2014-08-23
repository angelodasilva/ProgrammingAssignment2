## Functions below are able to cache the computations of
## the inverse of a matrix

## This first function creates a list of functions to:
## set the value of the matrix, get the value of the matrix,
## set the value of the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  library(MASS)
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function calculates the inverse of the matrix,
## no matter it has not yet been calculated.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- ginv(data,...)
  x$setinv(i)
  i
}
