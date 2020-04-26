## Put comments here that give an overall description of what your
## functions do

## This function contains the function to set the value of the matrix,get the value of the matrix, set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  matx <- NULL
  set <- function(y) {
    x <<- y
    matx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matx <<- inverse
  getinverse <- function() matx
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function calculates the inverse of the matrix using the function solve

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matx<-x$getinverse()
  if(!is.null(matx)) {
    message("getting cached data")
    return(matx)
  }
  data <- x$get()
  matx <- solve(data, ...)
  x$setinverse(matx)
  matx
}
