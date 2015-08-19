## The following functions facilitate matrix inversion. Matrix inversion is 
## usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly. This file contains
## the following functions to facilitate matrix inversion:
##
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
##
## Computing the inverse of a square matrix is done with the solve function in 
## cacheSolve. For this assignment, it is assumed that the matrix supplied is 
## always invertible.

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get, setinv = setinv, getinv = getinv)
      
}

## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinv(m)
      m
      
}