## 18/07/2020
## A method for solving large matrix using lexical scoping.
## The goal is to get the inverse of a matrix from cache, 
## instead of calculating it again.
## Pre-conditions: Make sure the matrix keeps unchanged.

# Test data
data=matrix(1:4,2,2)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) inverse <<- solve
  get_inverse <- function() inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# Computes the inverse of the object that is returned by the 
# makeCacheMatrix fuction.
# If the inverse has already been calculated (and the matrix has 
# not changed), then the cachesolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}

#Testing the functions
cacheSolve(makeCacheMatrix(data))
