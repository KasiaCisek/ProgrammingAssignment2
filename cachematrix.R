# makeCacheMatrix function
# parameters: x - matrix
# returns: a list with four functions:
#   get, set - gets and sets the value of the matrix
#   getinversed, setinversed - gets and sets the value inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversed <- function(inversed) i <<- inversed
  getinversed <- function() i
  
  list(set = set, get = get, setinversed = setinversed, getinversed = getinversed)
}

# cacheSolve function
# paremeters: x - cachedMatrix which is a result of makeCacheMatrix function
# returns: an inverse matrix which is calculated only the first time the function is run. 
#    On each of the following runs the cached version of inversed matrix is returned.

cacheSolve <- function(x, ...) {
  i <- x$getinversed()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversed(i)
  i
}
