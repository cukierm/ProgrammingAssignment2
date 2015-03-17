## Together, these functions find the inverse of a matrix.  makeCacheMatrix creates a list of functions
## for a given matrix that will be useful in this process.  cacheSolve then uses these functions 
## to compute the inverse.  If the inverse has been computed previously, cacheSolve returns a variable
## that has already been set when the inverse was computed the first time.

## The makeCacheMatrix function takes in a particular matrix and returns a list of functions ready
## to be used on that matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {  #in case the user wants to assign a value to inv without using cacheSolve to compute it.
    x <<- y
    inv <<- NULL
  }
  get <- function() x #accesses the original matrix
  setinv <- function(inverse) inv <<- inverse #this is designed to take a previously computed 
                                              #value of the inverse and assign into inv in the
                                              #makeCacheMatrix environment (parent environment 
                                              #of setinv).
  getinv <- function() inv  #will either be null, or the computed value from cacheSolve  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a matrix, either by computing the inverse and then storing
## the value or by looking up the previously computed value.

cacheSolve <- function(x, ...) {
  inv <- x$getinv() #accesses the value of inv 
  
  #If the inverse of the matrix has already been computed, then the inverse will be stored in the
  #variable inv.  The "if" statement checks to see if this is the case, and if so, returns this value.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #Otherwise, we'll access the original matrix, then compute the inverse.
  matrix <- x$get()
  inv <- solve(matrix)
  
  #now the inverse is stored to inv in the makeCacheMatrix environment, so that next time 
  #cacheSolve is called inv will have a non-null value.
  x$setinv(inv) 
  
  inv
}

