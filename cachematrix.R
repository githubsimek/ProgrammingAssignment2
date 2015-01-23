## This code gets a matrix and is supposed to cache the inverse if it already had been 
## calculated once

## Create the set/get functions for the matrix and for the inverse
## Returns a list of these functions, since a function can only return one object

makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize the inverse so it recognizes that we are sending in a new matrix
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
## It is where the inverse is actually calculated.
## x is a list of makeCacheMatrix functions

cacheSolve <- function(x, ...) {
        
        ## Get the inverse
        get_inverse <- x$getInverse()
        
        ## If the inverse is null calculate
        if (is.null(get_inverse)) {
                print('Calculating...')
                get_inverse <- x$setInverse(solve(x$get(), ...))
        }
        ## If there is an inverse just pull it from the cache
        else {
                print('Return cached inverse')
                get_inverse <- x$getInverse()
        }
        
        get_inverse        
}
