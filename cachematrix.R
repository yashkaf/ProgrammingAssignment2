## The two functions allow the calculation and caching of the inverse
## of a matrix. The matrix and its cached inverse are stored in a list
## type object that can be modified or referenced using the defined functions.

## makeCacheMatrix creates a list of four functions based on a given matrix:
## set - Sets the value of the matrix.
## get - Returns the stored matrix.
## setinv - Sets the value of the stored inverse matrix.
## getinv - Returns the stored inverse.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) minv <<- inv
  getinv <- function() minv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes a cached matrix object created using makeCacheMatrix and
## returns its inverse. If the inverse is cached it returns the stored value,
## if the cache is empty it calculates the inverse, stores it in the cache and
## returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
      message("getting inverse matrix from cached data")
      return(minv)
  }
  mat <- x$get()
  minv <- ginv(mat)
  x$setinv(minv)
  minv
}
