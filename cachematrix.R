## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a list of functions which act like
## an object, which stores the inverse of the matrix, once
## it is computed by cacheSolve.


## Write a short comment describing this function
## takes a matrix 
makeCacheMatrix <- function(currentMatrix = matrix()) {

    cachedInverse <- NULL
    setMatrix <- function(newMatrix) {
        currentMatrix <<- newMatrix 
        # resets the cached value when the matrix is updated.
        cachedInverse <<- NULL
    }
    getMatrix <- function() {
        currentMatrix
    }

    hasCachedInverse <- function() {
        !is.null(cachedInverse)
    }
    
    getCachedInverse <- function() {
        cachedInverse
    }

    setCachedInverse <- function(inverse) {
        cachedInverse <<- inverse
    }

    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         hasCachedInverse = hasCachedInverse,
         getCachedInverse = getCachedInverse,
         setCachedInverse = setCachedInverse
         )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (x$hasCachedInverse()) {
            x$getCachedInverse()
        } else {
            inverse <- solve(x$getMatrix())
            x$setCachedInverse(inverse)
            inverse
        }
}
