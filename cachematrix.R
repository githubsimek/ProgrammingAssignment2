## This code gets a matrix and is supposed to cache the inverse if it already had been 
## calculated once

## Create the set/get functions for the matrix and for the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        
        set <- function( in_matrix) {
                x <<- in_matrix
                inverse <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse_matrix) inverse <<- inverse_matrix
        
        getInverse <- function() inverse
        
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
        

}


## This function is a wrapper for makeCacheInverse().
## It is where the Inverse is actually calculated.
## x is a list of makeCacheMatrix functions

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        get_inverse <- x$getInverse()
        if (is.null(get_inverse)) {
                print('Calculating...')
                get_inverse <- x$setInverse(solve(x$get(), ...))
        }
        else {
                print('Return cached inverse')
                get_inverse <- x$getInverse()
        }
        
        get_inverse        
}
