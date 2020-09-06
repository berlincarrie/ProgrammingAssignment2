## Put comments here that give an overall description of what your
## functions do
  # makeCacheMatrix is a function that returns a list of functions
  # Its puspose is to store a martix and a cached value of the inverse of the 
  # matrix. It contains a list of 4 functions.
  # * setMatrix      set the value of a matrix
  # * getMatrix      get the value of a matrix
  # * cacheInverse   set the cached value (inverse of the matrix)
  # * getInverse     get the cached value (inverse of the matrix)

## Write a short comment describing this function

  #makeCacheMatrix is a function that creates a special matrix object that can
  #cache its inverse.
  #Initially the function determines if the matrix is already cached.

makeCacheMatrix <- function(x = matrix()) {

  #holds the cached value or NULL if nothing is cached
  #initially nothing is cached so set it to NULL

	inv <- NULL

  #store a matrix
	set <- function(y){
	x <<- y
	inv <<- NULL
	}
	get <- function() {x}
	setInverse <- function(inverse) {inv <<- inverse}
	getInverse <- function() {inv}
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

  #cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  ##  This function returns the inverse of a matrix created with
  ##  makeCacheMatrix

	inv <- x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat,...)
	x$setInverse(inv)
	inv
}







}
