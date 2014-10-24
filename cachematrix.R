## The code below defines a pair of functions that cache the inverse of a matrix.
## For this assignment, we assume that the matrix supplied is always invertible.
## And we invert it using the solve function.


## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions used to access the variables.
makeCacheMatrix <- function(x = matrix()) {
    # internal object to store the inverse matrix
    inv <- NULL
    # function that sets an original matrix (and clears the cache)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # function that returns the original matrix
    get <- function() x
    # function that sets the cached value
    setsolve <- function(solve) inv <<- solve
    # function that retrieves the cached value
    getsolve <- function() inv
    # the list of functions to be returned
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.
## The interactive statements below can be used to test
## tv <- rbind(c(1, -1/4), c(-1/4, 1))
## itv <- solve(tv)
## itv %*% tv # this should be the unity matrix
## ctvObj <- makeCacheMatrix(tv)
## citv <- cacheSolve(ctvObj)
## citv <- cacheSolve(ctvObj) # this one should tell you it's getting cached data
## citv %*% ctvObj$get() # this matrix should be the unity matrix
## identical(itv %*% tv, citv %*% ctvObj$get())
cacheSolve <- function(x, ...) {
    # try to get the value from the cache, and return it if found
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if the inverse is not found in the cache, get the orignal matrix
    data <- x$get()
    # calculate the actual matrix
    inv <- solve(data, ...)
    # set it back in the cache
    x$setsolve(inv)
    # and return it
    inv
}
