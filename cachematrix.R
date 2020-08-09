## I'm writing two functions that compute and cache the inverse 
##of a matrix.

## The first function creates the matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(inverse) inv <<- inverse
  getInvMatrix <- function() inv
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)

}


## This function computes the inverse of the matrix. If the inverse
## already exists, then the function will take it from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInvMatrix()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInvMatrix(inv)
  inv
}
