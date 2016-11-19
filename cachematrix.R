## Learning R - Progamming Assignment 2
## David Lazaroff - David@holistic.com

## This file contains two functions to define and cache the inverse
## of a matrix.

## 'makeCacheMatrix' takes a matrix as an argument and contains 
## functions for setting the matrix, getting the matrix, setting the 
## inverse, and getting the stored inverse value. 

## 'x' - the matrix for which the inverse is to be determined and 
## stored. It is assumed that the matrix is invertable.

## create an object 'myVar' to cache the matrix and inverse with the 
## following: "myVar <- makeCacheMatrix$set(myMatrix)"

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## 'cacheSolve' takes the following argument:
## 'x' - makeCacheMatrix object which has been set with 
## "myVar <- makeCacheMatrix$set(myMatrix)"
## e.g. "cacheSolve(myVar)"
## "cacheSolve" checks for a cached inverse and returns it if 
## found. If the cached value is null, the inverse is computed, 
## cached, and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinverse(inv)
    inv
}
