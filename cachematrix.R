## Programming Assignment 2 - week 3 - R Programming course
## Assignment: Caching the Inverse of a Matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        set <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) xInv <<- inv
        getinv <- function() xInv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        xInv <- x$getinv()
        if(!is.null(xInv)) {
                message("getting cached data")
                return(xInv)
        }
        data <- x$get()
        xInv <- solve(data, ...)
        x$setinv(xInv)
        xInv
}
