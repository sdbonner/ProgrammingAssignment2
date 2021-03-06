## Programming Assingment 2
## This submission will allow the caching of a calculated inverse of a matrix

## Function:  mackemakeCacheMatrix
## Input: x, a square matrix 
## Returns:  a list of set/get functions
## Desc:  This function creates a special matrix that which is contains a function to:
##  1) set/get value of the matrix
##  2) set/get the value of the inverted matrix
## Assumptions: this assingment assumes that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #defines the set function, this function will cache the value of the matrix and init the inverse matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #defines the get function, this function will return the value of the matrix
  get <- function() x
  
  #define the set Inverse Matrix function, this function will cache the inverted matrix that is it passed
  setInverse <- function(matrix) inv <<- matrix
  
  #define the get inverse matrix function, this function will return the inverted matrix
  getInverse <- function() inv
  
  #creates the list that will be returned as part of makeCacheMatrix call
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function:  cacheSolve
## Input: x, a makeCacheMatrix list containing set/get functions as well as a square matrix
## Returns:  a matrix containing the inverse of the makeCacheMatrix matrix, this value is cached if not previously 
## Desc:   This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##  If the inverse has already been calculated the inverse will be returned from cache
## Assumptions: this assingment assumes that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
  
  ##get the inverse matrix contained within the parameter x
  inv <- x$getInverse()
  
  ##if the inverse was already calculated and cached, notify that the cached values will be returned
  if(!is.null(inv)) {
    message("getting cached inverse")
  }
  ##if inverse not cacluated, get the matrix in x, cacluate the inverse and cache the result back in x
  else{
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
  }

  ## Return the matrix that is the inverse of 'x'
  return(inv)
}

