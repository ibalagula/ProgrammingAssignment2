## These functions will calculate inverse of a matrix.
## If the inverse has already been calculated (and the matrix has not changed)
## then the cacheSolve will retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y   
    inv <<- NULL
  } 

  get <- function() x
  setInverse <- function(transpose) inv <<- transpose
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
     message("getting cached data")
     return(inv)  
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}