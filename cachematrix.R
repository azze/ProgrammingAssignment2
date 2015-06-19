## a pair of functions that cache the inverse of a given matrix to save computation time

## creates a "cacheMatrix" object, a matrix that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(r){
		x<<-r
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) inv <<-inverse
	getinverse<-function() inv
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## computes and caches the inverse of the "cacheMatrix" returned by the above function. If
## inverse has already been computed, the inverse is instead retrieved from the cache


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  #if inverse is cached return the cached inverse
	if(!is.null(inv)){
		return(inv)
	}
  #otherwise compute inverse and return it
	mat<-x$get()
	inv<-solve(mat)
	x$setinverse(inv)
	inv
}
