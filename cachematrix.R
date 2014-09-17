## The functions take an invertible matrix and finds the inverse and caches it 
## to avoid recomputation in future

## makeCacheMatrix() takes an invertible matrix, returns a list of functions: set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) 
    {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() takes the list, checks if the inverse has already been computed. If so, returns it, else computes it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
