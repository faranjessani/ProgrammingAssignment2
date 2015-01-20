## The cached matrix is a matrix designed to cache
## the expensive computation of inversion
## It exposes a constructor function and
## a varition of the solve method designed to achieve that end

## This constructor method takes a matrix as input and exposes methods to retrieve its fields

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function wraps the solve function with a simple cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}