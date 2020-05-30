## makeCacheMatrix function creates a matrix wrapper that combines matrix data
## with its inverse
## cacheSolve computes the inverse of a matrix created with makeCacheMatrix,
## if it is called more than once with the same object, the cached value of
## matrix inverse is returned
##
## Example
## cm <- makeCacheMatrix(matrix(c(1, 0, -4, -2, 2, 5, 1, -8, 9), 3, 3))
## cacheSolve(cm)
## second call will return a cached value
## cacheSolve(cm)

## returns a matrix wrapper object allowing to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  getinv <- function() xinv
  setinv <- function(inverse) xinv <<- inverse
  get <- function() x
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  list(get = get, set = set,
       get.inverse = getinv, set.inverse = setinv)
}


## Cached version of solve for cache matrix objects,
## it returns the inverse of x if solve function succeeds

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv <- x$get.inverse()
  if (is.null(xinv)) {
    tryCatch({
      xinv <- solve(x$get(), ...)
      x$set.inverse(xinv)
    }, error = function(e) stop("Non inversible matrix"))
  } else {
    message("Getting cached matrix inverse")
  }
  xinv
}
