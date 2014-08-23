## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) invmatrix <<- inv
  getinverse <- function() invmatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setinverse(invmatrix)
  ## Return a matrix that is the inverse of 'x'
  invmatrix
        
}

############Usage Example################
# > x <- rbind(c(1, 4), c(2, 3))
# > xx <- makeCacheMatrix(x)
# > cacheSolve(xx)
#      [,1] [,2]
# [1,] -0.6  0.8
# [2,]  0.4 -0.2
# > cacheSolve(xx)
# getting cached data
#      [,1] [,2]
# [1,] -0.6  0.8
# [2,]  0.4 -0.2
########################################