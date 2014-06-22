## The following two functions are designed to create a
## special matrix with a memory to cache the calculated
## inverse of the matrix to save time. First one is for
## storing the matrix and its inverse and the second
## one is for calculating the inverse if it is not
## already cached yet.


## The makeCacheMatrix creates a special matrix object
## with 4 functions to get and set both the matrix and
## its inverse but it does not calculate the inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## inverseCached holds the value of the inverse matrix
    ## it is NULL at first indicating the inverse is not
    ## calculated yet
    inverseCached <- NULL
    
    ## set function assigns a new value to the stored matrix
    ## and since the matrix is changed the cached inverse
    ## will be thrown away (set as NULL)
    set <- function(y) {
        x <<- y
        inverseCached <<- NULL
    }
    
    ## get functions returns the value of the stored matrix
    get <- function() x
    
    ## setInverse function assigns a value to the cached inverse
    setInverse <- function(inverse) inverseCached <<- inverse
    
    ## getInverse function returns the value of cached inverse
    getInverse <- function () inverseCached
    
    ## This will return an object for the matrix with
    ## 4 functions mentioned above
    list(set = set, get = get, setInverse=setInverse, getInverse = getInverse)
}


## The cacheSolve function will calculate the inverse of the
## matrix x if it is not already cached. If cached, it will 
## only return the cached value instead.
cacheSolve <- function(x, ...) {
    ## This will get the cached inverse value of the matrix object
    inv <- x$getInverse()
    
    ## If the cached inverse is not empty it will be returned and
    ## a message will be printed to indicate that.
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    
    ## If the cached inverse is empty, the inverse will be calculated here
    data <- x$get()
    inv <- solve(data, ...)
    
    ## The calculated inverse will be cached here
    x$setInverse(inv)
    
    ## The inverse will be returned here
    inv
}
