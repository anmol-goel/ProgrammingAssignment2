## functions that cache the inverse of a matrix
## makeCacheMatrix: creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  m<-NULL
  
  ## set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## get the matrix
  get <- function() {
    x
  }
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## get the inverse of the matrix
  getInverse <- function() {
    m
  }
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
       
}

## cacheSolve: computes inverse of the special matrix returned by makeCacheMatrix 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m      
}
