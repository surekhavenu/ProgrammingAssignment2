## Following are a pair of functions that cache the inverse 
## of a matrix.

## makeCacheMatrix(x) : 
## Input:
##       x <- invertible matrix
##
## Return value:
##      a list containing a function to 
##              1. set the value of the matrix
##              2. get the value of the matrix
##              3. set the value of the inverse
##              4. get the value of the inverse
##
## Description:
##      This function creates a special "matrix" object that
##      can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## initialize the inverse
        i <-  NULL
    
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
    
        ## set the value of the inverse
        setinverse <- function(inverse) i <<- inverse
        
        ## get the value of the inverse
        getinverse <- function() i
        
        ## Return a list containing the above defined funtions
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(x) : 
## Input:
##       x <- special matrix returned by makeCacheMatrix
##
## Return value:
##       inverse of the special "matrix" 
##
## Description:
##      This function computes the inverse of the special
##      "matrix" returned by makeCacheMatrix above. If the
##      inverse has already been calculated (and the matrix
##      has not changed), then the cachesolve should 
##      retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {        
        ## Get the cached inverse
        i <- x$getinverse()
        
        ## Check if inverse of matrix is cached
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## get the matrix
        data <- x$get()
        
        ## compute and store inverse of the matrix
        i <- solve(data, ...)
        
        ## cache the inverse
        x$setinverse(i)
        
        ## Return a matrix that is the inverse of 'x'
        i         
}
