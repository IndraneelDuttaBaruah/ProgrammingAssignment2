## Caching Inverse Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(z = matrix()) {
  i <- NULL
  set <- function(y) {
    z <<- y
    i <<- NULL
  }
  get <- function() z
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(z, ...) {
  i <- z$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- z$get()
  i <- solve(data, ...)
  z$setinverse(i)
  i
}