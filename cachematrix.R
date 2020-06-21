## Put comments here that give an overall description of what your
## functions do

## This function makes its own cache matrix object that will chache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)} 
       
       








## This function computes the inverse of the special matrix object( one that's retuned by the previous function). 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
  if (!is.null(i)) {
    message("retreiving cached data")
    return(i)
  }
  
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
