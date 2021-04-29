## Will going to create a matrix object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) {
  
  i <- NULL
  
  set <- function( matrix ) {
    i <<- NULL
    m <<- matrix
  }
  
  get <- function() {
    m
  }
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## The below function triggers the inverse of the above matrix,
## If the inverse has already been calculated, then will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInverse(m)

  m
}
