## These functions cache the inverse of a matrix rather than computing it repeatedly
## so as to avoid costly computations.

## This function is similar to the example code provided for caching the mean of a vector.
## The example code sets and gets a vector and it's mean, this function does both tasks
## for a matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function calculates the inverse of the matrix created with the above function.
## It first checks to see if the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of
## of the data and sets the value of the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Cached Inverse Data:")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
} 
  