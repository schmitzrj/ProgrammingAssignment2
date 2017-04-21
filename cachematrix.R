## These functions deal with creating a special matrix object and then using 
## that object to return the inverse of the matrix either from cache if available 
## or from calculation if necessary.

## Creates a matrix with a variable for its own inverse
## This function is able to retrieve a chached inverse or create on if empty.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
    
  setInverse <- function(solve) inv <<- solve
  
  getInverse <- function() inv
  
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## Retrieves the inverse of the special matrix if it is already cached, 
## otherwise calculate the inverse and return it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting matrix inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
