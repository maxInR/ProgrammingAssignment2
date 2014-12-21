## Put comments here that give an overall description of what your
## functions do


# This function creates a special "matrix" object that can cache its inverse.
# It returns 4 functions that gets the matrix, sets the matrix, gets the inverse
# of the matrix and sets the inverse of the matrix, respectively
makeCacheMatrix <- function (x=matrix()){
    inverse<-NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    get <- function() x
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
    
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix
# has not changed), then cacheSolve should retrieve the inverse from the cache.
# If not, the function would solve for the inverse of the matrix and cache it by
# using the "setInverse()" function defined within the "makeCacheMatrix()"function
cacheSolve <- function(x,...){
    inverse<-x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached inverse ")
        return(inverse)
    }
    mtrx <- x$get()
    inverse <- solve(mtrx, ...)
    x$setInverse(inverse)
    inverse
}