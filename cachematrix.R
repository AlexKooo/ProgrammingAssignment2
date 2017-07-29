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
        inverse <- NULL ## create an empty vector to store the cache
        setmatrix <- function(y){
                x <<- y ## assign to x a new matrix value in the parent environment
                inverse <<- NULL ## clearing any cache from previous entry store in the parent environment
        }
        getmatrix <- function() x ## getmatrix will then returns x when it is being called
        setinv <- function(inv) inverse <<- inv 
        ## assign the inverse matrix to the cache through the inv argument
        getinv <- function() inverse ## it can therefore call the value from the stored cache
        ## A list is defined so that the $ operator can be used in the next function to call 
        #all the stored value
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinv = setinv,
             getinv = getinv)
}

## The cacheSolve function calculates the inverse of the 
# matrix created with the makeCacheMatrix function. 
# It first checks to see if the inverse has already been calculated.
# If so, it gets it directly from the cache and skips the computation. 
# If it is a NULL, it calculates the inverse of the matrix 
# and sets the value of the inverse in the cache via the setinv function.

## Caution: This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        inverse <- x$getinv() #get the value from the cache to prepare for the testing
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ##if there is already a value stored in the cache, it will be returned
        data <- x$getmatrix() #get the matrix value from the cache and assign it to the "data"
        inverse <- solve(data, ...)# get the inverse matrix and assign it to inverse
        x$setinv(inverse)
        inverse #return the inverse matrix
}
