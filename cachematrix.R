
## makeCacheMatrix creates a "m" object to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
  x <<- y
  m <<- NULL
  }

get <- function() x


setInverse <- function(matrix) m <<- matrix
getInverse <- function() m

list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## cacheSolve calculates the inverse of the "m" object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## tests is the cache already exists
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## calculating the inverse and return the result
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
  
}
