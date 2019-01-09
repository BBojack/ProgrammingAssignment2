## Put comments here that give an overall description of what your
## functions do
## Give a new CacheMatrix when there is a new invertible matrix

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the matrix of the inverse
##4.get the matrix of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #Initialize the inverse
  inverse <- NULL
  set <- function(y){
    x <<- y
    #assign a matrix impling its change and the cached inverse is not valid   
    inverse <<- NULL
  }
  # make new methods
  get <- function() x 
  setinverse <- function(solve) inverse<<- inverse
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inversion has already been calculated. 
##If so, it gets the inversion from the cache and skips the computation.
##Otherwise, it calculates the inversion of the data and sets the inversion matrix in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message('getting cached data')
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...) %*% data
  x$setinverse(inverse)
  inverse
}
