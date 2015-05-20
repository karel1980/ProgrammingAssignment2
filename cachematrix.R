## This script defines two functions which help you to cache the result of matrix inversion.
## Unless the data changes the cached inverse will be returned

## Example usage:

# d1 <- matrix(rnorm(9), nrow=3)
# d2 <- matrix(rnorm(9), nrow=3)
# m1 <- makeCacheMatrix(d1)
# s1 <- cacheSolve(m1) # the inverse is calculated
# s2 <- cacheSolve(m1) # the cached result is returned
# m1$set(d2)
# s3 <- cacheSolve(m1) # the inverse is calculated again

## This function returns a list which contains the basic helper functions for caching and cache invalidation
## Only the last value is cached.

makeCacheMatrix <- function(x=numeric()) {
  inv <- NULL
  cachekey <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setcachekey <- function(ck) cachekey <<- ck
  getcachekey <- function() cachekey
  setinv <- function(newInv) {
    inv <<- newInv
  }
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv,getcachekey=getcachekey,setcachekey=setcachekey) 
}

## This function returns the inverse of the matrix
## To do this it first checks if it needs to (re)calculate the inverse: the inverse has not been calculated
## yet or the backing matrix has changed.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(x$get()) && !is.null(x$getcachekey()) && all(x$get() == x$getcachekey())) {
    message("Returning cached inverse matrix")
    return(inv)
  }
  # We get here if the cache was never calculated
  # OR if the matrix was changed
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  x$setcachekey(matrix)
  inv
}


