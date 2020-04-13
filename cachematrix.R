## These functions allow the result of an inverse matrix calculation
## to be cached in order to eliminate unnecessary computations.

## makeCacheMatrix() creates a list of four functions:
##   set() - sets the value of the matrix
##   get() - returns the value of the matrix
##   setinverse() - sets the value of the inverse matrix
##   getinverse() - returns the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() either returns the cached inverse matrix, or
## if cache is empty, then the inverse matrix is calculated using
## solve() and then saved to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
