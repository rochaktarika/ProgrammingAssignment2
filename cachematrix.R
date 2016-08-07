## The functions below help you calculate the inverse of a matrix and extracts
## the inverse from the cache if it has already been calculated

## This functions takes a matrix as a input and creates a list with the get, set , setinverse
## and getinverse functions

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This functions takes the input list created by the makeCacheMatrix and calculate the 
## inverse of the matrix if not available else extract it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
