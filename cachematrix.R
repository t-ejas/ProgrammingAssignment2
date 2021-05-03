
##This function will create a "matrix" object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ##defining makeCacheMatrix with argument matrix
  i <- NULL  			 ##initialise variable that will hold result as null
  set <- function(y){		 ##assigns new matrix value in parent environment
  x <<- y
  i <<- NULL
  }
  get <- function()x		 ##get function returns value of matrix argument
  setInverse <- function(inverse) i <<- inverse	   ##assigns inverse value in parent environment 
  getInverse <- function() i 			   ##gets inverse value
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}



##This function will calculate the inverse from the "matrix" object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
  message("getting cached data")
  return(i)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(i)
  j
}
