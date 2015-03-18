##
## This file implements a cached computation of matrix inverse. During the first
## call, the inverse is computed and cached. There is the cached value returned 
## during subsequent calls.
##
## Usage example:
## m <- rbind(c(1, -1/4), c(-1/4, 1))
## cachedM <- makeCacheMatrix(m)
## inv1 <- cacheSolve(cachedM)
## ...
## inv2 <- cacheSolve(cachedM)
##

## Creates a matrix object wrapper able to cache the
## matrix inverse. Use together with cacheSolve(x) method.
makeCacheMatrix <- function(x = matrix()) {
  cInverse <- NULL
  set <- function(y) {
    x <<- y
    cInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cInverse <<- inverse
  getInverse <- function() cInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes a matrix inverse caching the result. Use the makeCacheMatrix()
## method to create a cached matrix passed as an argument of cacheSolve().
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("returning cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}


## Basic test for caching matrix inverse computation.
testCacheMatrix <- function() {
  
  m1 <- rbind(c(1, -1/4), c(-1/4, 1))
  cachedM1 <- makeCacheMatrix(m1)
  
  ## Chect that there is the correct inverse returned
  inv1a <- cacheSolve(cachedM1)
  referenceInv <- solve(m1)
  if (!identical(inv1a, referenceInv)) {
    error("Cached inverse not calculated correctly")
  }
  
  
  ## Check that caching works
  if (is.null(cachedM1$getInverse())) {
    error("Computed value not stored to cache - cache empty")
  }
  
  ## TODO: How to check that the instance is identical to inv1a in R?
  # inv1b = cacheSolve(cachedM1)
  
  message("test done")
  
}


