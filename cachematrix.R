## A set of functions that calculates the inverse of a matrix and caches the result to save computation time if needed in the future##

## This function returns a list containing functions that set a matrix and the inverse of that matrix##

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##This function calculates the invrese of a matrix after chicking if the inverse has not been previously calculated and cached, if it has it returns the cached value##
cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

##Test#
sample_matrix<-makeCacheMatrix()
sample_matrix$set(matrix(data=1:4,ncol=2,nrow=2,byrow=FALSE))
sample_matrix$get()
sample_matrix$setinverse(solve(sample_matrix$get()))
sample_matrix$getinverse()

cacheSolve(sample_matrix)

