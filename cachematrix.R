## This script creates two functions:

## 1) This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 2) This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse of the matrix has been calculated previously and is cached, then the cached version will be used, otherwise
## it will be calculated.

  cacheSolve <- function(x, ...) {
    ## Return the inverse matrix
    m <- x$getinverse()
    ##Check if inverse of matrix already exists, and return if it does
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    ## Calculate inverse of the matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
  