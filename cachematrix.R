# R Programming Assignment 2, Brian Gilson

# # This code consists of two functions makeCacheMatrix and cacheSolve 
# which can be used together to utilize environments and <<- (superassignment)
# operators to speed processing for matrix inverse calculations

## Function : makeCacheMatrix
# The makeCacheMatrix function creates the inital special "matrix" object which is inpt
# to the cacheSolve function 

  makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) 
      i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
  }
  
  
## Function CacheSolve
  
# The cacheSolve function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix function. If the inverse has already been calculated 
# then the cachesolve retrieves the inverse from the cache

  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
  }


}




