## Cache the inverse of a matrix and avoid computing it repeatedly
## There are two functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix 
## This function creates a special "matrix" that can cache its inverse by:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve
## This function computes the inverse of the special "matrix" returned by "makeCacheMatrix". 
## 1. check if the inverse has already been calculated,
##      If yes, retrieve the inverse from the cache.
##      If not, calculate the inverse and set it in the cache via the "setinverse" function.
## 2. return the inverse of matrix 'x'.
cacheSolve <- function(x, ...) {
       inverse <- x$getinverse()
       if(!is.null(inverse)) {
               massage("getting cached data")
               return(inverse) 
       }
       data <- x$get()
       inverse <- solve(data,...)
       x$setinverse(inverse)
       inverse
}
