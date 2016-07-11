## Daniel Sargent 2016.07.06
##
# creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    invm = NULL
    set = function(y) {
      x <<- y
      invm <<- NULL
    }
    get = function() x
    setinvm = function(inverse) invm <<- inverse
    getinvm = function() invm
    list(set=set, get=get,
      setinvm=setinvm,
      getinvm=getinvm)
}
## Returns inverse of myMatrix object from makeCacheMatrix()
cacheSolve <- function(myMatrix, ...) {
    # 
    invm = myMatrix$getinvm()
    # does invm exist?
    if (is.null(invm)){
      # no, get matrix
      m.data = myMatrix$get()
	  # solve function to compute the inverse of a square matrix
      invm = solve(m.data, ...)
	  # sets the value of the inverse in the cache via the setinvm function.
      myMatrix$setinvm(invm)
    }
    return(invm)
}
