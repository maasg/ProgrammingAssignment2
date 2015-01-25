## This function provides a cache for matrix inversion

## Creates cache for the inverse of the provided matrix 
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) cache <<- inv
  getInverse <- function() cache
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Retrieves the inverse of the wrapped matrix. The inverse is memoized the first time it's accessed 
## and this cache is further used while the base matric does not change

cacheSolve <- function(x, ...) {
  cache <- x$getInverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setInverse(cache)
  cache
}
