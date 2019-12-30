## Matrix inversion is a costly computation and it may be benefit 
## to cache the inverse of matrix rather then calculate it repeatedly

## "makeCacheMatrix" : This function creates some matrix object that caches its own inverse matrix.
## If the inverse matrix has already been calculated, the cachesolve retrives the inverse matrix from the cache.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

## If the inverse matrix has already been calculated, the cachesolve retrives the inverse matrix from the cache.

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
