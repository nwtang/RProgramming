## These functions compute the inverse of a matrix and stores its value
## If a new matrix is defined, the inverse is computed and stored again

## makeCacheMatrix description:
## Defines the matrix that will be inverted
## Sets the intial value of the inverse to NULL
## Defines the following anonymous functions, which are used in cacheSolve:
  ## set: Modify the original matrix, set inv back to NULL (erase stored inv)
  ## get: Returns the original matrix
  ## setinverse: Sets the variable inv to the computed inverse
  ## getinverse: Returns the current value of the variable inv (either NULL or a previously stored value)
## Makes a list of anonymous functions, so they can be stored as objects and called
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
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


## cacheSolve description:
## Returns the inverted matrix in two possible ways:
  ## If there is already a stored value for inv, return it
  ## If there is no stored value (inv is NULL), compute the inverse using solve, and set it using setinverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
