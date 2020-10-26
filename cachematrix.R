## The given codes are used to evaluate the inverse of
## a matrix and return its inverse, if available from
## cached data.

## The function makeCacheMatrix() returns the inverse
## of a matrix if stored as cached data or else computes
## the inverse explicitly.

makeCacheMatrix <- function(x = matrix()) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


## The function cacheSolve() computes the inverse of a
## matrix and stores it as cached data.

cacheSolve <- function(x, ...) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set,get = get,setinv = setinv,getinv = getinv)
}
