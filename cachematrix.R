## In this example we introduce the <<- operator which can be used to assign a value to an object
## in an environment that is different from the current environment.
## Below are two functions that are used to create a special object that stores a numeric matrix
## and cache's its inverse.

## makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
## get the value of the matrix
## set the value of the matrix
## get the value of the inverse matrix (lazy)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inv <<- NULL ## reset the cache
    }
    inverse <- function(...) {
        if (is.null(inv)) {
            message("Caching the inverse matrix")
            inv <<- solve(x, ...)
        }
        message("Getting cached inverse matrix")
        inv
    }
    list(set = set, get = get, inverse = inverse)
}


## I have made this task in object oriented style. So cacheSolve function is
## not really needed. Because it just delegates the call to the inverse method of
## cacheMatrix object
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x$inverse(...)
}
