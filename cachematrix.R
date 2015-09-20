## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.


## makeCacheMatrix takes a square matrix input and contains 4 functions: set, get, setinverse, getinverse.
## set assigns the value of input_matrix to x
## get returns the square matrix value that is stored in the main function
## setinverse assigns the value inverted_matrix to inv
## getinverse returns the inverted matrix value that is stored in the main function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(input_matrix) {
    x <<- input_matrix
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverted_matrix) {
    inv <<- inverted_matrix
  }
  
  getinverse <- function() {
    inv
  }
  
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## Input of cacheSolve is the object where makeCacheMatrix is stored and returns the value of inv(inverse of the square matrix).
## cacheSolve validates the value inv, stored using getinverse. If inv exists in memory, 
## it returns a message and the value of inv. Else, data gets the square matrix stored with makeCacheMatrix, 
## solve calculates the inverse of the square matrix and x$setinverse(inv) stores it in the object generated assigned with makeCacheMatrix

cacheSolve <- function(x, ...) {
        
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
  
}
