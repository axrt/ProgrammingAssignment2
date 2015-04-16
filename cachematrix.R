## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mtx.inv<-NULL
  
  ##Getter-setter-like functions
  ##Setter for the matrix
  set<-function(y){
    x<<-y
    mtx.inv<-NULL
  }
  ##Getter for the matrix
  get<-function(){
    return(x)
  }
  ##Setter for the inverse
  setMtxInv <- function(inv){
    mtx.inv <<- inv
  }
  ##Getter for the inverse
  getMtxInv <- function() {
    return(mtx.inv)
  }
  
  #Return
  return(list(set = set, get = get,
              setMtxInv = setMtxInv,
              getMtxInv = getMtxInv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  mtx.inv <- x$getMtxInv()
  if(!is.null(mtx.inv)) {
    message("Getting cached inverse..")
    return(mtx.inv)
  }
  
  message("Calculating inverse..")
  data <- x$get()
  mtx.inv <- solve(data, ...)
  x$setMtxInv(mtx.inv)
  
  #Return
  return(mtx.inv)
}


