## These functions create a matrix object and allow the inverse of matrix to be stored and retrieved

## This function creates a special "matrix" object that can cache and retrieve its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
     
}


## This function checks to see if the inverse of the created matrix exists in the cache.
##if it does it retrieves it from the cache, if not it will create the inverse of the submitted matrix. 
##It assumes that the matrix is invertible. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}

