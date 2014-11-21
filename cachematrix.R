## Put comments here that give an overall description of what your

## R Programming - Assignment 2
## Cache a matrix's inversion in a different environment when the first time to calculate it
## Use cached inversion the next time

###  Yilong Gao (Mark)
###  November 21, 2014

## function makeCacheMatrix(): cache matrix x and its inversion
## No inversion calculation in this function

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL  ## matrix inversion
    
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    
    get <- function() x
    
    setInversion <- function(Inversion) mi <<- Inversion
    
    getInversion <- function() mi
    
    list(set = set, get = get,
         setInversion = setInversion,
         getInversion = getInversion)
}


## Function cacheSolve(): calculate a matrix's inversion if it is not in cache; otherwise use cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      inv <- x$getInversion()
      
      if (!is.null(inv)) {
          message("getting cached inversion")
          return(inv)
      }
      
      inv <- solve(x$get())
      x$setInversion( inv )
      inv
      
}
