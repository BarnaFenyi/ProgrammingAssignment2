## These pair of functions create a matrix object that can 
## get, set, invert and get the inverse of a matrix, 
## additionally the matrix object can also calculate the determinant 
## of the matrix (to find out if it is invertable)
## Using the matrix object the function pair can calculate and cache the 
## inverse of the matrix.

## When provided as input for this function the "get", "set", "set inverse", "get inverse"
## and "determinant" functions are created for the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  det <- det(x)
  
  set <- function(y) 
  { 
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  getdet <- function() det
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv,
       getdet = getdet)
}


## When the output of the above function is fed into this function it will
## calculate - after checking exitence - the inverse of the matrix. The inverse gets cached,
## so when it is available from cache the inverse is not calculated again.

cacheSolve <- function(x, ...) {
  minv <- x$getinv()
  mdet <- x$getdet()
  
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  if(!mdet==0) {
    data <- x$get()
    minv <- solve(data)
  }
  else
  {
    message("matrix not invertable")
    minv <- NULL
  }
  
  x$setinv(minv)
  minv 
}
