## This assignment consists in writing a pair of functions that cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
        
        #Initialize the inverse
        inv <- NULL
        
        # Set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Get the value of the matrix
        get <- function() { m }
        
        # Set the value of the inverse of the matrix
        setInverse <- function(inverse) { inv <<- inverse }
        
        #Get the value of the inverse of the matrix
        getInverse <- function() { inv }
        
        # Return a list of the previous methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
        
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.
## This function assumes that the matric is always invertible.

cacheSolve <- function(x, ...) {
        
        # Return a matrix that is the inverse of x
        inv <- x$getInverse()
        
        # If the inverse is already set, return it
        if (!is.null(inv)) {
                message("Getting cached data.")
                return(inv)
        }
        
        # Get the matrix
        data <- x$get()
        
        # Calculate the inverse of the matrix using 'solve'
        inv <- solve(data)
        
        # Set the inverse to the calculated inverse matrix
        x$setInverse(inv)
        
        inv
}
