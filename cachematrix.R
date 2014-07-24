## Create a matrix and set the functions associated with cache functionality
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Create get and set functions
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Check to see if the inverse of the matrix is stored otherwise calculate the inverse
cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## Get the inverse of the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## Return matrix from cache
  }
  data <- x$get() ##Get the matrix
  m <- solve(data) ## Calculate matrix inverse
  x$setinverse(m) ## Cache the inverse
  m
}
