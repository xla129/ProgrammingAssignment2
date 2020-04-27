## Put comments here that give an overall description of what your
## functions do
##This program is a pair of two functions, makeCacheMatrix and
##cacheSolve that allows the user to use matrix inversion with
##the added benefit of caching the inverse of a matrix rather than
##compute it repeatedly

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache
##its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
##The function computes the inverse of the special "matrix" from 
##the previous function above. If the inverse has already been
##calculated (and the matrix has not changed), then the cachesolve
#should receive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}