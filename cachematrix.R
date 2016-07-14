## Daniel Sargent 2016.07.14
## 
# creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
	
	# Define the functions
	# set the value of the matrix
    # get the value of the matrix
    # set the value of the inverse matrix
    # get the value of the inverse matrix
    setm <- function(y) {
	  # store matrix to cache
      x <<- y
      invm <<- NULL
    }
    getm <- function() x
    setinvm <- function(inverse) invm <<- inverse
    getinvm <- function() invm
	
	# create a list containing functions to set matrix, get matrix, set inverse matrix, get inverse matrix
    list(setm=setm, 
		getm=getm,
		setinvm=setinvm,
		getinvm=getinvm)
}

## Returns inverse of x object from makeCacheMatrix() either by computing it (solve) or returning the cache value if any
cacheSolve <- function(x, ...) {
    # get cache value
    invm <- x$getinvm()
    # does invm exist in the cache?
    if (is.null(invm)){
      # no, get matrix and compute the inverse
      mat <- x$getm()
	  # solve function to compute the inverse of a square matrix
      invm <- solve(mat, ...)
	  # sets the value of the inverse in the cache via the setinvm function.
      x$setinvm(invm)
    }
    return(invm)
}
