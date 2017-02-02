## The function makeCacheMatrix creates a list containing 4 functions, 
## I. set: sets the value of the matrix
## II. get: gets the value of the matrix
## III. setInverse: sets the value of the inverse
## IV. getInverse: gets the value of the inverse
## makeCacheMatrix creates an object to store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
set<-function(y=matrix()){ 
  x<<-y
  i<<-NULL
}
get<-function()x
setInverse<-function(inverse=matrix()) i<<-inverse
getInverse<-function()i
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## the cachesolve function calculates the inverse of the matrix stored in makeCacheMatrix.
##However, it first checks if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips calculation. 

cacheSolve <- function(x, ...) {
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  matrx<-x$get()
  i<-solve(matrx,...)
  x$setInverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
