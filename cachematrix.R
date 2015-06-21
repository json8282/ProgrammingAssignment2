## This script containts a pair of functions that cache the inverse of a matrix.  

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## sets value of i (inverse matrix) to null
  i <- NULL
    ## declare another function that caches input of matrix if new or changed (then sets inverse back to NULL) 
    set <- function(y) {
    x <<- y 
    i <<- NULL
    }
  ##gets value of inverse
  get <- function() x
  ##calculates inverse of matrix
  setsolve <- function(solve) i <<- solve
  ##gets inverse
  getsolve <- function() i
  ## passes the functions to a list
  list(set=set, get=get,
      setsolve = setsolve,
      getsolve = getsolve)
  }


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ##compares matrix to previous instance
  i <- x$getsolve()
  ##if already calculated, gets inverse
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if not calculated already, gets the value of the input matrix and calculates
  data <- x$get()
  i <- solve(data,...)
  x$setsolve(i)
  ## returns inverted matrix
  i
}
