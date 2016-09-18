## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function( x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invers <<- inverse 
  getInverse <- function() invers
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has been calculated , then the CacheSolve 
## should retrive the inverse from the cache,
## given that the matrix has not changed.

cacheSolve <- function(x, ...) {
  invers <- x$getInverse()
  if (!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  matX <- x$get()
  invers <- solve(matX, ...)
  x$setInverse(invers)
  invers
}



