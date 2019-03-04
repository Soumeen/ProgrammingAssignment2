##The first function, makeCacheMatrix creates a special "CacheMatrix", which is really a list containing a function to
makeCacheMatrix<- function(x=matrix()){
  inv<- NULL
  set<- function(y){
    x<<-y
    inv<- NULL
  }
  get<- function()x
##set the value of the inverse  
##get the value of the inverse  
  setinverse<- function(inverse)inv<<-inverse
  getinverse<- function()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



##set the value of the CacheMatrix

##get the value of the CacheMatrix


cacheSolve<- function(x,...){
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data,...)
  x$setinverse
  inv
}