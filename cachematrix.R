## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix, makeCacheMatrix
## makeCacheMatrix consists of set,get,setinv,getinv
## library(MASS)is used to calculate inverse for non squared as well as square matrices
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x          ## function to get matrix x
  setInverse <- function(inverse)inv <<- inverse
  getInverse <- function() {
    inver<-ginv(x)
    inver%*%x                 ## function to obtain inverse of the matrix
  } 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## Write a short comment describing this function
## This is used to get the cache data

cacheSolve <- function(x, ...) ## gets cache data
  {
  inv <- x$getInverse()
  if(!is.null(in)){           ## checking weather inverse is null
    message("getting cached data")
    return(inv)               ## returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat,...)       ## calculates inverse value
  x$setInverse(inv)
  inv                         ## Return matrix that is the inverse of 'x'
}
