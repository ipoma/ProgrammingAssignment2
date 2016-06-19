#This file contains two functions for caching the inverse of a matrix.
# the makeCacheMatrix function is a container object that holds a matrix and it's inverse
# the cacheSolve function should be called with the object returned from makeCacheMatrix. It returns the inverse of the matrix, using the cached result if it is already been calculated once
 
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  
  #getter and setter for the original matrix
  #when setting the matrix, clear previously computed inverse
  #   it would make more sense to me to calculate and cache the inverse inside the set method, 
  #   thus eliminating the need for the cacheSolve method and checks required to see if the inverse has already been calculated.
  #   But, this isn't what the assignment asks for
  set <- function(data){
    x<<-data
    inverse_matrix<<- NULL
  }
  get <- function() x
  
  #setinverse and getinverse are used to store and retrieve the inverse of the matix
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        inverse_matrix <- x$getinverse()
        if (!is.null(inverse_matrix)){
          # if inverse_matrix is not null, we have solved it before, nothing to do here, return the inverse
          message("getting cached inverse matrix")
          return(inverse_matrix)
        }
        message("calculating inverse")
        #if we made it this far, we haven't calculated the inverse before, do so now
        data <- x$get()
        #create the inverse matrix
        inverse_matrix <- solve(data,...)
        #save the inverse back to the object so we don't have to calculate it again
        x$setinverse(inverse_matrix)
        #return the inverse matrix
        inverse_matrix
}
