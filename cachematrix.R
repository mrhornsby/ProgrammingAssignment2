## This function creates a special "matrix" which
## is really a list containing functions to:
##
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the inverse of the matrix
##  4. get the inverse of the mtarix
##
makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    matrix <<- newMatrix
    inverse <<- NULL
  }
  get <- function() matrix
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of the special
## "matrix" created with the above function. It first
## checks to see if the inverse has already been calculated.
## If so it gets the inverse from the cache and skips
## the computation. Otherwise, it calucates the inverse
## of the data and sets the value of the inverse in the
## cache via the setInverse function
cacheSolve <- function(matrix, ...) {
  inverse <- matrix$getInverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  data <- matrix$get()
  inverse <- solve(data, ...)
  matrix$setInverse(inverse)
  inverse
}
