
## The purpose is to create the function makeCacheMatrix & cacheSolve that cache the invers of matrix

## makeCacheMatrix is the function to create special matrix that can cache the inverse of marix
## in this case we assume the matrix is invertible (determinant of the matrix is not zero)
makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) minverse <<- inverse
  getinverse <- function() minverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is the function to create the invers of special matrix
## if the inverse have been calculated and the matrix has not changed then this function will retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minverse <- x$getinverse()
  if(!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data, ...)
  x$setinverse(minverse)
  minverse
}
