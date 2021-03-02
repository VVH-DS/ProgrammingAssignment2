## These functions are used to compute the inverse of a square matrix and cache them for future
## functions do

## Create brandnew matrix object to later store the computed inverse of the square matrix

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setmatinverse <- function(matrixinverse) mi <<- matrixinverse
  getmatinverse <- function() mi
  list(set = set, get = get,
       setmatinverse = setmatinverse,
       getmatinverse = getmatinverse)
}


## Compute the inverse of the square matrix

cacheSolve <- function(x, ...) {
  mi <- x$getmatinverse()
  if(!is.null(mi)) {
    message("getting cached inverse matrix data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setmatinverse(mi)
  mi
}
