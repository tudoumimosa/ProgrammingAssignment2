## The following functions cache the inverse of a matrix

## The first function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

## The second function "cacheSolve" computes tht inverse of the special "matrix" returned by "makeCacheMatrix" above. If the inverse has already been calculated, then "cacheSolve" retrives the inverse from the cache.

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

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
        ## Return a matrix that is the inverse of 'x'
}

