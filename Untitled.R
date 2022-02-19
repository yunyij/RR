
#makeCacheMatrix creates a special “matrix”, which is a list containing a function to
#set and get the value of the matrix/set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve will retrieve the inverse from the cache
#when the inverse is calculated

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


A <- matrix(c(1,2,3,4),2,2)

B <- makeCacheMatrix(A)
cacheSolve(B)




