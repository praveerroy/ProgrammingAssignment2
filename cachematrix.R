## This file is the submission file for the assignment 2
## It uses the sample file that was forked as the base file
## The file is edited in R Studio and then comitted in the local Git Ripositary
## It is then synced in the Github Ripository ProgrammingAssignment2 for user praveerroy



## These functions are written to cache the inverse value of a matrix
## Using these functions save computation power and time in programs where inverse value of a matrix is required repeatedly
## If the matrix is not changed and invere is required then the cached value is used else the new inverse is cached


## This first function makeCacheMatrix creates a special Matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then this function retrieves the inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}

