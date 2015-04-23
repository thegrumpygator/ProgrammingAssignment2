## Functions that create a cached version of a matrix
## and a function to solve (invert) the cached matrix.
##
## Will only recompute the solution if the matrix has
## changed since the last time the solution was 
## computed.

## Function to create an object to store a special 
## matrix and its solution/inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## initially set the inverse to NULL
  
  # create the parts of the function that handle the 
  # setting adn retrieval of the matrix and its inverse.
  set <- function(y) 
  {
    x <<- y      ## Set the value of the matrix.
    inv <<- NULL ## Reset the inverse to NULL when
                 ##    new value is assigned to matrix.
  }
  
  ## Create the part of the function to retrieve the 
  ##      value of the matrix.
  get <- function() x 
  
  ## Create the parts of the function that handles the
  ##      setting and retrieval of the inverse.
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  ## return the makeCacheMatrix functions and all its parts.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to compute the solution/inverse of a 
## cached matrix object.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv() ## Get the inverse from the CacheMatrix Object.
  
  if(!is.null(inv)) ## if the inverse has already been computed, return it.
  {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, if the inverse has not already been computed
   
  data <- x$get()     ## Get the matrix from the CacheMatrix Object. 
  inv <- solve(data)  ## Invert/solve the matrix.
  x$setinv(inv)       ## cache the solution in the CacheMatrix Object.
  inv                 ## Return the inverse/solution of the matrix.
}
