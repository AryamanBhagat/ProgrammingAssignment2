## Put comments here that give an overall description of what your
## functions do
#These functions use caching to reduce the time required to compute the inverse of a matrix.

## Write a short comment describing this function
#makeCacheMatrix makes a list of four functions that manipulate the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, setinv = setinv)
}


## Write a short comment describing this function
#cacheSolve is a fuction that works on the list created by the above function.
#It returns a cached value of the matrix if available and calculates it if unavailable.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- i$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
