## Put comments here that give an overall description of what your
## caching the inverse of a matrix rather than compute it repeatedly.
##The Functions below are used to create a special object that 
## stores a matrix and caches its inverse.



##This function, This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y ##creates x, which is to be retrieved by the function get()
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  ## creates a list of function to set and get the value of the vector, set and get the value of the inverse 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)##Computing the inverse of a square matrix
        x$setInverse(inv)
        inv
}
