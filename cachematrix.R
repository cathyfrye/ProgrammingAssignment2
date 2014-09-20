## Two functions below where the output of the function makeCacheMatrix
## can be passed to the function cacheSolve.  The function cacheSolve will
## either retrives the inverse of the matrix from cache or calculates the 
## inverse of the matrix if the cached value does not exist.

## Creates a list containing a function that:
## 1. sets the value of a matrix as input from the user
## 2. gets the value of the matrix
## 3. sets the value of the inverse of the matrix and caches this inverse
## 4. gets the value of the inverse of the matrix
## Note:  this function does not compute the inverse of the matrix, computation
## of the inverse is in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## y can be set in order to test the output of this (f)
    x <<- y
    m << NULL
  }
  get <- function() x 
  setInvMatrix <- function(InvMatrix) m <<- InvMatrix
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## Computes or returns from cache the inverse of the matrix x.  Works with
## makeCacheMatrix which sets the cached values.  If the inverse of the matrix
## x already exists, then retrieves the inverse of the matrix value from cache

cacheSolve <- function(x, ...) { ## where x in this function is the  output from makeCacheMatrix
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInvMatrix(m)
  m
}
