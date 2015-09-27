## This set of functions obtain the inverse of a matrix and save it in the 
## cache for later use if necessary, as a way of optimizing time. 

## This function creates a matrix and 
## creates several variables to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL 
      set <- function(y) {
            x <<- y
            inv <<- NULL}
      get <- function () x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function checks if the inverse of the matrix has been calculated
## and if so, retrieves it from the cache, skipping calculations. 
## If it hasn't, then it calculates it.  

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
