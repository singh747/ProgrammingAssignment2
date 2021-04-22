## Matrices takes a lot of time in computations like inverse of a matrix.
## the below functions will not compute inverse of matrices that have
## already been computed. It will return the already computed
## inverse of the matrix which will save a lot of time.

## This function will create a matrix and the following codes can access
## the values of x or m using set and get functions .

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function will calculate inverse of a matrix using solve function
## and it will retrieve already computed inverse of a matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}