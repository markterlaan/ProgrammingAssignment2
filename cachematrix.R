## first function allows for creation of a matrix
## second solves inverse either from cache or new calculation

## function to create special Matrix, which is actually a list that allows to set and get the values and
## set and geat the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  CacheMatrix <<-list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## calculates the inverse of the special "Matrix"
## if inverse has been cached before, it returns cache

cacheSolve <- function(x, ...) {
  m <- CacheMatrix$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- CacheMatrix$get()
  m <- solve(data, ...)
  CacheMatrix$setInverse(m)
  m
}

