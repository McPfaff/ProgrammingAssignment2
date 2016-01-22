## makeCacheMatrix creates a special matrix which can cache its inverse by
## using cacheSolve. If the matrix hasn't been changed in between two
## calls of cacheSolve the second call will return the cached result
## of the first call.
## Use $get and $set for getting and setting the matrix
## Only invertible matrices are supported.

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
    
}


## Inverts matrices created with makeCacheMatrix, returns cached inverse if available

cacheSolve <- function(x, ...) {
       inverse <- x$getinverse()
       if(!is.null(inverse)) {
         message("returning cached data")
         return(inverse)
       }
       data <- x$get()
       inverse <- solve(data)
       x$setinverse(inverse)
       inverse
}
