## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## For this purpose we write two functions and other functions within those functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inversed matrix
#get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  tmp_inv <- NULL
  set <- function(y) {
    x <<- y
    tmp_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) tmp_inv <<- inverse
  getInverse <- function() tmp_inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), then the 
##             cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getInverse()
  if(!is.null(inv_x)) {
    message("getting cached data.")
    return(inv_x)
  }
  df <- x$get()
  inv_x <- solve(df)
  x$setInverse(inv_x)
  inv_x
}

##Notes on how to run this: 
## > x = rbind(c(1,8), c(8,1))
##> m = makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1    8
##[2,]    8    1
##> cacheSolve(m)
##[,1]        [,2]
##[1,] -0.01587302  0.12698413
##[2,]  0.12698413 -0.01587302
##> cacheSolve(m)
##getting cached data.
##[,1]        [,2]
##[1,] -0.01587302  0.12698413
##[2,]  0.12698413 -0.01587302