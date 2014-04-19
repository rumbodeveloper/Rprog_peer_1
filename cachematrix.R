## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a list with the following elements:
# set the matrix value
# get the matrix value
# set the inverse matrix value
# get the inverse matrix value



makeCacheMatrix <- function(x = matrix()) {
  # initialize the matrix inverse to store with a NULL  value 
  invmat <- NULL
  
  # first element of the list:  set the value of matrix
  set <- function(y) {
    x <<- y
    invmat <<- NULL   
  }
  
  # second element of the list: get the value of matrix
  get <- function() x
  # third element: set the value of the inverse
  setinvmat <- function(invmat1) invmat <<- invmat1
  # fourth element: get the value of the inverse
  getinvmat <- function() invmat
  
  # return a list of all the above functions
  return (list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat))   
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinvmat()
  # if it is cached:
  if(!is.null(invmat)) {
    return(invmat)
  }
  # if it is not cached
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinvmat(invmat)
  return(invmat)
}
