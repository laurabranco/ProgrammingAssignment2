## These functions allow you to find and cache the inverse 
## of a matrix.

## The first function creates a special "matrix" object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }


## The second computes the inverse of the special "matrix" 
## created by "makeCacheMatrix". It can also retrieve the
## inverse from the cache if it has already been calculated.
## NOTE: This function only takes arguments in the form of
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

