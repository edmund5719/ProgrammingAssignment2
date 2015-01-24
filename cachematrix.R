## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Basically this project is a mimicing exercise of the makeCacheVector example

makeCacheMatrix <- function(x = matrix()) {
      # m notes for the inverse matrix, default is NULL
	m <- NULL
      # set a new matrix, inverse reset to NULL
	set <- function(y) {
               x <<- y
               m <<- NULL
      }
	# return the matrix
      get <- function() x
	# store the inverse of the matrix in Cache
      setinverse <- function(inverse) m <<- inverse
	# return the inverse, from catch if computed before
      getinverse <- function() m
      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	# Try to get it directed, check if computed before
	m <- x$getinverse()
	# If so, return the cached result
	if(!is.null(m)){
		message("getting cached inverse matrix")
		return(m)
	} 
	# Otherwise, compute the inverse
	data <- x$get()
	m <- solve(data,...)
	# And store it in catch, then return inverse
	x$setinverse(m)
	m
}
