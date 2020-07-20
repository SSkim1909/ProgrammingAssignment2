## Matrix inversion is time-consuming and it'd be better to
## cache the inverse of a matrix rather than compute it each time.
## Below is a pair of functions creating an object that 
## stores a matrix as well as its inverse.

## creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## function below computes the inv of the matrix created above.
## inv will be retrieved when needed in computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setInverse(i)
  i
}
