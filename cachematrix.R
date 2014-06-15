# File: cachematrix.R
# Purpose: Contains functions for calclating the inverse of a matrix and
#          caching it so it can be recalled without recalculating it.
# Author: Randy Bohannon
# Date: 2014-06-10


#******************************************************************************
# makeCacheMatrix()
# Purpose: creates a special "matrix", which is really a list containing
#       functions to
#       set the value of the matrix
#       get the value of the matrix
#       set the value of the inverse of the matrix
#       get the value of the inverse of the matrix
#
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get the value of the matrix
        get <- function() {
                x
        }
        
        # set the value of the inverse of the matrix
        setinverse <- function(solve) {
                inv <<- solve
        }
        
        # get the value of the inverse of the matrix
        getinverse <- function () {
                inv
        }
        
        # create a list of functions and return
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
} # end makeCacheMatrix()

#******************************************************************************
# cacheSolve()
# Purpose: calculates the inverse of the special "matrix" created with the 
#       above function. However, it first checks to see if the inverse has 
#       already been calculated. If so, it gets the inverse from the cache 
#       and skips the computation. Otherwise, it calculates the inverse of 
#       the data and sets the value of the inverse in the cache via the 
#       setinverse function.
#
cacheSolve <- function(x, ...) {
        
        # get the inverse of x from cache
        inv <- x$getinverse()
        
        # if inv is not null, retrieval of inverse from cache was successful
        if(!is.null(inv)) {
                
                # display message and return inverse
                message("getting cached data")
                return(inv)
        }
        
        # if we get to here, inverse was not in cache;
        # retrieve matrix from cache
        data <- x$get()
        
        # calcualte inverse
        inv <- solve(data, ...)
        
        # save inverse to cache
        x$setinverse(inv)
        
        # return inverse
        inv
        
} # end cacheSolve()