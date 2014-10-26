## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function makeCacheMatrix creates a list containing functions to
## set the value of the matrix, get the value of the matrix, 
## set the value of the inverse of the matrix, 
## and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverseCache <- NULL
        
        ## Sets the value of the matrix
        set <- function(y) {
                x <<- y
                inverseCache <<- NULL
        }
        
        ## Gets the value of the matrix
        get <- function() x
        
        ## Sets the value of the inverse of the matrix 
        setinverse <- function(solve) inverseCache <<- solve
        
        ## Gets the value of the inverse of the matrix
        getinverse <- function() inverseCache
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

## This function returns the inverse of the matrix created in makeCacheMatrix.
## First it checks if the inverse was already calculated. 
## If so, it gets the inverse from the cacche and skips the calculation.
## If not, it calculates the matrix inverse and sets that value in cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inverseCache <- x$getinverse()
        
        ## First checks if inverse of 'x' is already cached
        if(!is.null(inverseCache)) {
                message("getting cached inverse")
                return(inverseCache)
        }
        
        ## If not already cached, then calculates inverse and saves it in cache.
        data <- x$get()
        inverseCache <- solve(data, ...)
        x$setinverse(inverseCache)
        inverseCache                
}