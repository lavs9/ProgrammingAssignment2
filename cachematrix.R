## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Tasks : 
## 1 : makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

## How To : 
## 1. makeCacheMatrix : creates a list containing a function to 
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the value of the matrix
## d. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) m<<- inverse
     getinv <- function() m
     list(set = set, get = get, setinv = setinv, getinv = getinv)
     
     

}


## Write a short comment describing this function

##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data,...)
     x$setinv(m)
     m
}
