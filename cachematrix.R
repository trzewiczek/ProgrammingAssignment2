## Functions for creating and solving matrices using "special"
## optimized matrix object that can cache its solved result.

## Create a "special" matrix able to cache its solved version
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL

  get <- function() x
  set <- function(nx) {
    x <<- nx
    inversed <<- NULL
  }

  getInversed <- function() inversed
  setInversed <- function(ninv) inversed <<- ninv

  list(get = get, set = set,
       getInversed = getInversed,
       setInversed = setInversed)
}


## Solve the "special" matrix x using cached data if available
## or solving the matrix and caching result in x object
cacheSolve <- function(x, ...) {
  inversed <- x$getInversed()
  if(!is.null(inversed)) {
    message("Getting cached data")
    return(inversed)
  }

  m <- x$get()
  inversed <- solve(m, ...)
  x$setInversed(inversed)
  inversed
}
