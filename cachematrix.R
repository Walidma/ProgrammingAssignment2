## makeVector creates a special "vector"
##, which is really a list containing a function to :
## set the value of the vector / get the value of the vector
## set the value of the inverse matrix
## and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix value
  matrix <- NULL
  ## set the value of the vector 
  set <- function(y){
  x <<- y
  inverse <<- NULL
  }
  # get the value of the vector
  get<- function() x
  ## set the value of the inverse matrix
  set_inverse <- function(inverse) matrix <<- inverse
  ## get the value of the inverse matrix
  get_inverse <- function() matrix
  ## the list containing all the functions
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # We check if the inverse matrix is available
  # If the answer is yes, we return directly the matrix
  matrix <- x$get_inverse()
  if(!is.null(matrix)) {
    message("getting cached inverse")
    return(matrix)
  }
  # if the answer is no, we get the vector first
  my_data <- x$get()
  # after that, we compute the inverse
  matrix <- solve(my_data, ...)
  # and then we cache the matrix inverse
  x$set_inverse(matrix)
  # we return the result finaly
  matrix
}
