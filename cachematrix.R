###########################################
## Programming assignment 2
###########################################

## Pair of routines to create a matrix that can calculate and cache
## the inverse of a matrix

##
## Create a inverse caching matrix object
## param  x is matrix to use for initialization of the matrix
## returns caching matrix object
##

makeCacheMatrix <- function(x = matrix()) {
  ## invalidate matrix inverse
  inv <- NULL
  
  ## Create a setter for matrix data
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  
  ## Create a getter for matrix data
  get <- function() x
  
  ## Create a setter for inverse matrix
  setinv <- function(aInv) inv <<- aInv
  
  ## Create a getter for inverse matrix
  getinv <- function() inv
  
  ## Create and return object
  list( set=set, get=get, setinv=setinv, getinv=getinv )
}



##
## Create a function to get inverse of a caching matrix object
## param  x is a caching matrix object
## returns inverse matrix
##

cacheSolve <- function(x, ...) {
  ## Access cached inverse and return if valid
  inv <- x$getinv()
  if( !is.null(inv) ) {
    message( "getting cached inverse")
    return(inv)
  }
  
  ## calculate and return inverse of matrix and set cached inverse
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
