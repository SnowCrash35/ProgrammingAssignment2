## A set of functions to create and maintain a cached results of
## inverting a matrix

## Makes a set of functions for working with a cached matrix object

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## Create a set, get, setinv and getinv function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    ## Creat a list for accessing the cache matrix helper functions
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Solves for the inverse of a matrix. If a cached result exists it
## is used, otherwise a result is computed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    message("computing result")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
