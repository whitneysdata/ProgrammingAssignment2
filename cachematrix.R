## Overall description
# These functions work together to cache the inverse of a matrix.
# Matrix inversion is computationally expensive, so caching avoids
# recomputing the inverse when it has already been calculated.
# makeCacheMatrix creates a special object that stores a matrix and
# its inverse. cacheSolve computes the inverse or retrieves it from
# the cache if it has already been calculated

## makeCacheMatrix:
# This function creates a special "matrix" object that can store
# both the matrix itself and its inverse. It provides functions to:
#  - set the matrix
#  - get the matrix
#  - set the cached inverse
#  - get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- NULL
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve:
# This function computes the inverse of the special "matrix" created
# by makeCacheMatrix. If the inverse has already been calculated and 
# cached, it retrieves the inverse directly from the cache instead of
# recomputing it. Otherwise, it calculates the inverse, stores it in 
#the cache, and then returns it.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#testing the code
A <- matrix(c(2, 1, 1, 2), 2, 2)
special <- makeCacheMatrix(A)
cacheSolve(special)  #computes
cacheSolve(special)  #should print "getting cached inverse"