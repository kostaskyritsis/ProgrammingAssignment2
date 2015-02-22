## makeCacheMatrix and cacheSolve functions can be used together to cache a matrix,
## produce and cache the inverse of this matrix too

## The makeCacheMatrix function enables you to cache a matrix (that contains
## data of interest) and at the same time to cache the function that produces the
## inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) m <<- solve
    get_inverse <- function() m
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## The cacheSolve function uses a cached matrix (specified by the user as the 
## output of makeCacheMatrix and assigned to the argument x) to compute and 
## produce the inverse of this matrix. At the same time it caches its output
## in order to spare time from future computations if the inverse of this matrix
## is needed again

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversed <- x$get_inverse()
    if (!is.null(inversed)) {
        message("getting cached matrix")
        return(inversed)
    }
    inversed <- solve(x$get())
    x$set_inverse(inversed)
    inversed
}