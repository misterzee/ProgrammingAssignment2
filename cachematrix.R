## r-prog13 Programming Assignment 2
## 
## makeCacheMatrix() function creates a
## matrix object that can cache its inverse
## cacheSolve computes and caches inverse
##--------------------------------------
## makeCacheMatrix provides methods set(), 
## get(), setinverse(), and getinverse()
##----------------------------------------
makeCacheMatrix <- function(x = matrix()){
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function()x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list (set=set, get = get,
		  setinverse = setinverse, getinverse = getinverse)
}

##---------------------------------------
## First call to cacheSolve calculates
## inverse and caches it
## Subsequent calls retrieve cached value
##---------------------------------------
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return (m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m
}
