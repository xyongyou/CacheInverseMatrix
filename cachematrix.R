## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this is a function that producing a matrix we need
## it includes initializing and setting a matrix, at the same time,
## it provides two function setInverse and getInverse,
## at last, it returns a list containing the above things. 
makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y){
    x <<- y
    inverse_x <<- NULL
    
  }
  get <- function() x
  setInverse <- function(i) inverse_x <<- i
  getInverse <- function() inverse_x
  
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
## this is a function that calculate the inverse of matrix x, firstly,
## we need to search in the cache whether it is already calculated. If so,
## we can return it directly, otherwise, we will calculate it by the function
## solve().
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    print(i)
    return(i)
    
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  print(i)
  i
  
}
