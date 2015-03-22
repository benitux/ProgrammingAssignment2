## Put comments here that give an overall description of what your
## functions do

## These functions provide caching of 

## Write a short comment describing this function

## Given any matrix returns a list with four functions
## which allows to get and set the matrix value and its inverse
## This list could be thought as an object with four methods
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## The set function sets the value of the matrix and 
    ## clears the value of the cache
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    ## The get function returns the value of the matrix
    get <- function() x
    
    ## The setinverse function sets the value of the inverse matrix
    setinverse <- function(inverse) i <<- inverse
    
    ## The getinverse function returns the value of the inverse matrix
    getinverse <- function() i
    
    ## Returns a list with the four functions defined previousely
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## Given a list returned by makeCacheMatrix, cacheSolve returns 
## the inverse of the matrix contained.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Get the current value of the inverse 
    i <- x$getinverse()
    
    ## Check whether is null
    if(!is.null(i)) {
      ## The inverse is cached, return that
      message("getting cached data")
      return(i)
    }
    
    ## The inverse isn´t cached
    ## Get the original matix
    data <- x$get()
    ## Compute the inverse
    i <- solve(data, ...)
    ## Save the inverse for future queries
    x$setinverse(i)
    ## Return the inverse
    i
}
