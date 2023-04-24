## ***Put comments here that give an overall description of what your functions do:***

  ## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse

## ***Write a short comment describing this function***
    ## Matrix inversion is usually a costly computation and there may be some benefit 
    ## to caching the inverse of a matrix rather than compute it repeatedly

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  tmset <- function(y) {
    x <<- y
    i <<- NULL
  }
  tmget <- function() x
  setinversetm <- function(inverse) i <<- inverse
  getinversetm <- function() i
  list(tmset = tmset, tmget = tmget,
       setinversetm = setinversetm,
       getinversetm = getinversetm)
}


## ***Write a short comment describing this function****
  ## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
            ## If the inverse has already been calculated (and the matrix has not changed), 
            ## then the cachesolve should retrieve the inverse from the cache.
              ## Computing the inverse of a square matrix can be done with the solve function in R. 
                  ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinversetm()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$tmget()
  i <- solve(data, ...)
  x$setinversetm(i)
  i
}



