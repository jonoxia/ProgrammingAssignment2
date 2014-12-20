## Put comments here that give an overall description of what your
## functions do

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


## This is not how i'd personally design the class -- I'd rather just do the solving inside getInverse
## -- but I followed the pattern of the example.