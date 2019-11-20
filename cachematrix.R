## makeCacheMatrix: this function creates a matrix object which can cache its inverse.
## cacheSolve: computes the inverse of the matrix returned by the makeCacheMatrix function

## this function takes a matrix x inputed and cache in m the inverse 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## this function calculates the inverse of the matrix from the  function "makeCacheMatrix"
##if the inverse has already been calculated, cacheSolve returns the inverse from the cache
cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}