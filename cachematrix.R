## ------------------------------------------------------------------------------

## cachematrix.R 

## Consists of two functions:
##  1. makeCacheMatrix
##  2. cachesolve

## These functions allow you to cache potentially time-consuming computations. 
## In this case, computing the inverse of a matrix. 

## If the contents of the matrix are not changing, these functions will allow
## you to cache the value of the inverse, so if needed again it can be looked up 
## in the cache rather than recomputed. 


## ------------------------------------------------------------------------------

##  1. makeCacheMatrix

## The 'makeCacheMatrix' function creates a special "matrix" object that 
## can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {   
    
    inv <- NULL
    
    set <- function(y) {      ## set the value of the matrix
              x <<- y
              inv <<- NULL
    }
    
    get <- function() x       ## get the value of the matrix
    
    setInverse <- function(inverse) inv <<- inverse   ## set the value of the inverse
    getInverse <- function() inv                      ## get the value of the inverse
    
    list(set = set                                    ## create a list
          ,get = get
          ,setInverse = setInverse
          ,getInverse = getInverse)
    
}


## ------------------------------------------------------------------------------

##  2. cachesolve

## The 'cachesolve' function calculates the inverse of the matrix created 
## by the 'makeCacheMatrix' function. If the inverse has already been calculated,
## (and the matrix has not changed), then this function retrieves the inverse
## from the cache.


cacheSolve <- function(x, ...) {   
    
    inv <- x$getInverse()
    
    if(!is.null(inv)) {                       ## check if there exists a cache
              message("getting cached data")
              return(inv)                     ## if it does exist, get the cached value
    }
  
    data <- x$get()          ## if not in the cache
    inv <- solve(data)       ## calculate the inverse of the matrix
    x$setInverse(inv, ...)   ## add the result to the cache
    
    inv                      ## return the result (i.e. the inverse of 'x')
    
}


## ------------------------------------------------------------------------------


## Example
## > x <- makeCacheMatrix(matrix(1:4,2))
## > cacheSolve(x)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
