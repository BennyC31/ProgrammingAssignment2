## Two functions that cache the inverse of a matrix.

## Function creates a matrix object.  It's inverse is cached.

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y){
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setInverse <- function(solveM) inv_m <<- solveM
  getInverse <- function() inv_m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function creates the inverse of the matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #New code
  inv_m <- x$getInverse()
  if(!is.null(inv_m)) {
    message("getting data from cache")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data)
  x$setInverse(inv_m)
  inv_m
}
