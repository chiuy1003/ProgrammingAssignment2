
## Matrix Inverse result caching
## This file contains 2 functions:
##  1. A matrix object creator
##  2. A matrix inverter that caches the result in the matrix object

## Creates a cacheMatrix object which contains
##   x - the input matrix
##   xInverse - the inverse of matrix x
##   set() and get() - sets and gets the matrix x
##   setInverse() and getInverse() - sets and gets the inverse of matrix x
makeCacheMatrix <- function(x = matrix()) {
    xInverse <- NULL
    set <- function(y) {
        x <<- y
        xInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) xInverse <<- inverse
    getInverse <- function() xInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Solves a matrix by 
##  1. Checking if the solution is already cached in the cacheMatrix object, 
##      if so return cached solution
##  2. If no solution exists, then call solve(), store solution in cacheMatrix object, 
##      return solution
cacheSolve <- function(x, ...) {
    dataInverse <- x$getInverse()
    if(!is.null(dataInverse)) {
        message("getting cached data")
        return(dataInverse)
    }
    data <- x$get()
    dataInverse <- solve(data, ...)
    x$setInverse(dataInverse)
    dataInverse
}
