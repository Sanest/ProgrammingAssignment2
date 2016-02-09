## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      c <- NULL
      set <- function(y) {
            x <<- y
            c <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) c <<- inverse
      getInverse <- function() c
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


cachesolve <- function(x, ...) {
      c <- x$getInverse()
      if(!is.null(c)) {
            message("getting cached data")
            return(c)
      }
      data <- x$get()
      c <- solve(data, ...)
      x$setInverse(c)
      c
}

