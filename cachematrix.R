
## function to create an matrix "x" for caching 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  function to inverse the original matrix and stores as cachesolve

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("retrieving previously cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
