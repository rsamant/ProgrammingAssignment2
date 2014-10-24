## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## following makeCacheMatrix function takes square matrix as an input argument
## This function sets and gets the input matrix 
## it also help in caching the inveresed matrix with the help of setinverse (for setting in cache)
## and get inverse for getting from cache 

makeCacheMatrix <- function(x = matrix()) {
  
  #holds cache value of inversed matrix, initially set to null
  inv <- NULL
  
  #sets input matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #returns input matrix
  get <- function() {
    return (x)
  }
  
  #caches inverse matrix i.e. output of solve function  
  setinverse <- function(solveop) {
    inv <<- solveop
  }
  
  #returns stored inverse matrix
    getinverse <- function() {
      return (inv)
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function gets input matrix, then looks if the inversed result is already cached
## if cached then return cached matrix
## or else compute new result, cache it and return computed matrix

cacheSolve <- function(x=matrix(), ...) {
  
  #get the cached value of inverse matrix
  inv <- x$getinverse()
  
  # return inverese matrix if already cached
  if(!is.null(inv)){
    message("getting cached data !!!")
    return(inv)
  }
  
  # if not then get the input matrix, compute the inverse,store it in the cache
  
  matrixdata<-x$get()
  
  inv <-solve(matrixdata, ...)
  
  x$setinverse(inv)
  
  #return inversed data
  return (inv)
  
}
