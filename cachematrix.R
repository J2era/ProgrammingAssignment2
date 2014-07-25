## Put comments here that give an overall description of what your
## functions do

## This function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setreverse <- function(reverse) m <<- reverse
    getreverse <- function() m
    list(set = set, get = get,
         setreverse = setreverse,
         getreverse = getreverse)    

}


## cacheSolve returns the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getreverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setreverse(m)
    m    
}
