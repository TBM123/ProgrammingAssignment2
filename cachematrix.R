## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # initilize the inverse to null since we do not have a matrix until we set it
    inverse <- NULL
    
    # This function sets the matrix. Since we did not calculate the inverse yet we set it to null
    set <- function(y) {
        mtrx <<- y
        inverse <<- NULL
    }
    
    # This function gets the matrix
    get <- function() mtrx
    
    # This function sets the inverse of the matrix with a given value i and saves it in the cache
    setInverse <- function(i) inverse <<- i
    
    # This function gets the inverse of the matri
    getInverse <- function() inverse
    
    # we return a list of the functions as our special "matrix" object that can cache its inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" x. 
##If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    # if the inverse is not null than it is in the cache and we do not need to calculate it
    if(!is.null(inverse)) {
        message("getting cached data")
    } else {
        # if the inverse is null we need to calculate it and we should save it back into the "special" matrix
        mtrx <- x$get()
        inverse <- solve(mtrx)
        x$setInverse(inverse)
    }
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}
