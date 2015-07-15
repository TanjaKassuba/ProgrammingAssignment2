## makeCacheMatrix() and cacheSolve() form a pair of functions
## that cache the inverse of a matrix:
## makeCacheMatrix() creates the cache
## cacheSolve() returns the inverse of a matrix by either getting it from the
## cache in case it has already been calculated or else
## calculating it and storing it in cache


## returns a list containing a function to
## 1. set the value of the matrix: set
## 2. get the value of the matrix: get
## 3. set the value of the inverse: setinverse
## 4. get the value of the inverse: getinverse

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	
	# set the value of the matrix
	set <- function(y){
		x <<- y
		inv <<- NULL		
	}
	
	# get the value of the matrix
	get <- function() x
	
	# set the value of the inverse
	setinverse <- function(solve) inv <<- solve
	
	# get the value of the inverse
	getinverse <- function() inv
	
	# return special "matrix" object that can cache its inverse
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}



## Returns the inverse of the matrix: retrieves it from cache if it has been already calculated, 
## else it caluclates it and sets it in cache

cacheSolve <- function(x, ...) {
  
    # get inverse matrix object from makeCacheMatrix
	inv <- x$getinverse()
	
	# check if inverse has already been calculated (i.e., is not NULL),
	# if so, return inverse and exit function
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)		
	}
	
	# if not, get data
	data <- x$get()
	
	# and calculate inverse
	inv <- solve(data, ...)
	
	# set inverse matrix object in cache with calculated inverse
	x$setinverse(inv)
	
	# return inverse
	inv        
}
