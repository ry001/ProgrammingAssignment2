## Caching the Inverse of a matrix:
## Matrix inversion is usually a costly computation and there may be some benefits
## to caching the inverse of a matrix rather than compute it repeatedly.
## There are two functions to create an inverse cache of a matrix:
## 1.makeCacheMatrix and 2.cacheSolve

## ## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" created 
## by the function above.If the inverse has already been calculated,
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}