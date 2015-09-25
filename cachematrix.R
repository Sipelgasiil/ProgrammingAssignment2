## These functions cache the inverse of a square invertible matrix

## makeCacheMatrix function creates a special matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinversion <- function(solve) inv <<- solve
    getinversion <- function() inv
    list(set = set, get = get, 
         setinversion = setinversion, 
         getinversion = getinversion)
}


## cacheSolve function calculates the inverse of a special matrix
## returned by makeCacheMatrix. If the inverse has already been 
## computed (and considering the matrix is the same), then cacheSolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinversion()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinversion(inv)
    inv
}
