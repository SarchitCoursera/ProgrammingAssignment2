## These functions make a special matrix object that can store its inverse in a cache so it does 
## not need to be calculated more than once

## The makeCacheMatrix function takes a matrix input and stores it as an object
## with a list of functions that allow the matrix and its inverse to be set and retrieved

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The cacheSolve function looks to see if the special matrix's inverse has already been stored
## in the object created with makeCacheMatrix and returns that, otherwise calculates the inverse 
## stores it in the object, and then returns it

cacheSolve <- function(x, ...) {
        
    inv<-x$getinverse()              
    if(!is.null(inv)){               
      message("getting cached data") 
      return(inv)                    
    }
    data<-x$get()                    
    inv<-solve(data,...)             
    x$setinverse(inv)                
    inv                              
}
