
## Put comments here that give an overall description of what your
## functions do
##
## syntax: 
## 	cacheSolve(makeCacheMatrix(amatrix))
##		where amatrix is an invertible matrix
##		or result of matrix(), the default param
##		for makeCacheMatrix
##		
## external source used:
## 		https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/  
##   	
## makeCacheMatrix creates an object containing a functions for retrieving or resetting the matrix,
## 	and functions for setting and caching the inverse of a matrix, and retrieving the cached inverse. 
##
## cacheSolve computes the inverse of the special "matrix" 
## 	returned by makeCacheMatrix. If the inverse has already been 
## 	calculated (and the matrix has not changed), then the cacheSolve should 
## 	retrieve the inverse from the cache. Otherwise, the inverse is calculated.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

## This function creates an object containing a functions for retrieving or resetting the matrix,
## 	and functions for setting and caching the inverse of a matrix, and retrieving the cached inverse. 
##

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
