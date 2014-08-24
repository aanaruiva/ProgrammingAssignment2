## This pair of functions create a matrix which inverse can be stored, since matrix inversion is usually a costly computation.
## There's no verification whether the matrix supplied is always invertible.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Returns a matrix that is the inverse of the special "matrix" returned by makeCacheMatrix above.
## Retrieves the inverse from the cache if it has already been calculated.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Example:
# m <- makeCacheMatrix(matrix (c(2,9,3,3,9,2,1,5,7), nrow=3, ncol=3))
# cacheSolve(m)
# cacheSolve(m)
