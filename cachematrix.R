## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix funciton provides a list of functions including
## intializing/getting matrix and setting/getting matrix inversion
## by taking a matrix parameter.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  ##return a list object with four funcions
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)

}


## Write a short comment describing this function

## cacheSolve function can calculate matrix inversion with a 
## makeCacheMatrix object. If matrix inversion has been calculated
## an extra message will be prompted.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ##if matrix inversion has been caculated, inversed matrix is returned directly.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
