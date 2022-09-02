## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix consisting of set, get, setinv, getinv
##Use library(MASS) to calculate inverse for square and non squared matrices 
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL          #initialize inverse as NULL
  set <- function(y){
                     x<<-y
                     inv<<-NULL
                      }
  get<-function()x      #function to get matrix x
  setinv<-function(inverse) inv <<- inverse
  getinv<-function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This will be used to get the cache data

cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {        #inspect if inverse if NULL
                      message("getting cached data!")
                      return(inv)        #inverse value returned
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)     #inverse value calculated
  x$setinverse(inv)
  inv    ## Return a matrix that is the inverse of 'x'
}
