## Functions that create a cached version of a matrix
## and a function to solve (invert) the cached matrix.
##
## Will only recompute the solution if the matrix has
## changed since the last time the solution was 
## computed.

## Function to create an object to store a special 
## matrix and its solution/inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## initially set the inverse to NULL
  set <- function(y) {
    x << y
    inv << NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to compute the solution/inverse of a 
## cached matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
