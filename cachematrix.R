## Theses functions allow to create a matrix which can store the 
## last version of the inverse in order to avoid the cost of recalculate it every time

## Create a matrix, and prepare the cache to store the invert result
makeCacheMatrix <- function(x = matrix()) {
  x <<- y
  inverse <<- NULL

  get <- function() x
  setinvert <- function(inv) inverse <<- inv
  getinvert <- function() inverse
    
  list(get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getinvert()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m 
}
