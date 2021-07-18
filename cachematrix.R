## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL            #initialise inv as null
  set<-function(y){
    
    x<<-y              
    inv<<-NULL
  }
  get<-function(){x}       #function to get matrix x
  setinverse<-function(inverse){inv<<-inverse}  #to inverse matrix
  getinverse<-function(){inv}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){              # to check invverse is null
    message('get cached data')
    return(inv)                   #return inverse value
  }
  mat<-x$get()
  inv<-solve(mat,...)            #calculate inverse matrix
  x$setinverse(inv)
  inv
}
