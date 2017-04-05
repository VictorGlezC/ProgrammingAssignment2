## MakeCacheMatrix takes a matrix as an argument and returns a type of element composed
## of four functions: set(), get(), setinv(), getinv(). It also store in cache two data
## objects: the matrix passed as an argument and the inverse matrix (set to NULL. It 
## will be calculated using the cachesolve function)

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y){
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function takes an object of type makeVector as defined in makeCacheMatrix
## function. It checks if the inverse matrix is already stored in cache. If yes, it returns it
## If not, it calculates the the inverse matrix, stores it into cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)){
    message("getting inverted matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
