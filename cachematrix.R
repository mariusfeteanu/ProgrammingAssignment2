## The code below defines a pair of functions that cache the inverse of a matrix.
## For this assignment, we assume that the matrix supplied is always invertible.
## And we invert it using the solve function.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
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
## itv %*% tv
## ctvObj <- makeCacheMatrix(tv)
## citv <- cacheSolve(ctvObj)
## citv <- cacheSolve # this one should tell you it's getting cached data
## citv %*% ctvObj$get() # this matrix should be the unity matrix
## identical(itv %*% tv, citv %*% ctvObj$get())
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
