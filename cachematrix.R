## makeCacheMatrix - creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## functions for cache matrix inverse 
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        
        
        m <- x$getInverse()
        
        ## if the inverse has already been calculated (and the matrix has no changed), retrieve the inverse from the cache.	
        ## otherwise calculates inverse
        
        if ( ! is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m <- solve(x$get())
        x$setInverse(m)
        m
}