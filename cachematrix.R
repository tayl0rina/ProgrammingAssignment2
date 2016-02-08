## R Programming Assignment 2
## Calculating the inverse of a matrix after checking whether it has already been computed and stored in the cache

## The first function creates a list for each matrix, which contains the matrix and the matrix inverse if it has already been computed.
## Otherwise inverse is set to null until the inverse is first computed.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function takes as input the list structures of the previous functions.
## It searches the list to see if the matrix inverse has already been computed. If it has, it retrieves it from the cache.
## Otherwise, it computes the matrix inverse and stores it in it's list for future computations.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
