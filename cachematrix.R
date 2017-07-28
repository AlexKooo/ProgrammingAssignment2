## Because matrix inversion is usually a costly computation, there is benefitial
# to caching the inverse of a matrix rather than compute it repeatedly to save the memory.
# The following two functions are used to cache the inverse of a matrix.

## The first function makeCacheMatrix creates a list 
# as a special vector containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setmatrix <- function(y){
                x <<- y
                inverse <<- NULL
        }
        getmatrix <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinv = setinv,
             getinv = getinv)
}

## The cacheSolve function calculates the inverse of the 
# matrix created with the makeCacheMatrix function. 
# It first checks to see if the inverse has already been calculated.
# If so, it gets it directly from the cache and skips the computation. 
# If not, it calculates the inverse of the matrix 
# and sets the value of the inverse in the cache via the setinv function.

## Caution: This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$getmatrix()
        inverse <- slove(data, ...)
        x$setinv(inverse)
        inverse
}
