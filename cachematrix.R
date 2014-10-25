## Global Description: Special Matrix structure that has the ability to cache
## inverse matrix calculation. More details about each function below.


## This function creates a special "matrix" object that has the ability to
## cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
        # Inverse Cache variable
        inverseCached <- NULL
        
        # get/set Matrix functions
        get <- function() matrix
        set <- function(newMatrix){
                matrix <<- newMatrix
                inverseCached <<- NULL
        }
        
        # get/set Inverse functions
        getInverse <- function() inverseCached
        setInverse <- function(newInverse) inverseCached <<- newInverse
        
        # Associate previous functions with their name
        list(get = get,
             set = set,
             getInverse = getInverse,
             setInverse = setInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # If inverse is cached, return it
        invCached <- x$getInverse()
        if (!is.null(invCached)){
                message("Returning Cached Inverse")
                return(invCached)
        }
        
        # if not, calculate inverse and cache it
        matrix <- x$get()
        inverseCalculated <- solve(matrix)
        x$setInverse(inverseCalculated)
        inverseCalculated
}
