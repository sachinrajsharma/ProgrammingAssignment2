

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  z <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  z <- x$getinverse()
  # if the inverse has already been calculated
  if (!is.null(z)) {
    # get it from the cache and skips the computation.
    message("getting cached data")
    return(z)
  }
  # otherwise, calculates the inverse 
  data <- x$get()
  z <- solve(data, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(z)
  z
}
