## Functions for creating a cache-enabled matrix, calculating the inverse, and storing 
## the matrix inverse in the cache.
## I simply followed the pattern of the cacheMean example given in the assignment.
## Jonathan Xia, December 21, 2014

## Takes a matrix of initial values. If not provided, defaults to an empty matrix.
## Returns a cache-enabled matrix "object" which is just a list of getter/setter methods.
makeCacheMatrix <- function(values = matrix()) {
    # Private data:
    inverse <- NULL

    # Create the public methods:
    set <- function(newValues) {
        values <<- newValues
        inverse <<- NULL
    }
    get <- function() values
    setInverse <- function(newInverse) inverse <<- newInverse
    getInverse <- function() inverse
    
    # Return all the methods as a list:
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Takes a cache-enabled matrix object, as created by makeCacheMatrix. Returns
## the inverse of the matrix, from cache if possible. Additional arguments to this function
## are optional and will be passed to solve(). Assumes that the matrix will always be
## invertible.
cacheSolve <- function(matrix, ...) {

    # Check the cache first:
    oldInverse <- matrix$getInverse()
    if (!is.null(oldInverse)) {
        message("Getting cached data.")
        return (oldInverse)
    }

    # any additional args to the function will be passed to solve
    inverse <- solve(matrix$get(), ...)

    # cache and return the newly generated inverse matrix.
    matrix$setInverse(inverse)
    inverse
}
