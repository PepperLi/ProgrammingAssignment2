## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
        x<<-y
        m<<-NULL
  }
## setinverse assigns inv to parent environ's inverse
## getinverse returns the inverse value 
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function
## this function calculate the inverse the a matrix and also a vectoe
##  If the inverse has already been calculated and the matrix has not changed, then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("get cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
