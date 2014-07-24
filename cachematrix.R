## These functions create a list that can store and retrieve a matrix and 
## it's inverse without calculating it every time.

## Create a matrix and set the functions associated with cache functionality
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## Initialise m variable
  set <- function(y) {
    x <<- y ## Set the matrix x to the parameter y
    m <<- NULL ## Set the inverse matrix to NULL
  }
  ## Create get and set functions
  get <- function() x ## Get will return the matrix
  setinverse <- function(solve) m <<- solve ## This will set m to the inverse matrix
  getinverse <- function() m ## Return m
  ## Place all functions into a list so they can all be accessed easily
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Check to see if the inverse of the matrix is stored in the cache
## otherwise calculate the inverse and store it in the cache
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
