#' makeCacheMatrix caches a given matrix and its inverse. Use the return of this
#' functon to chache a result of a matrix inverse calculation by cacheSolve 
#' instead of recalculating.
#' 
#' @param x : matrix that needs to be cached
#' @return a list of 4 getter-setter-like functions with corresponding names:
#' get, set, setMtxInv and getMtxInv. get/set pair get/sets matrix for which
#' the inverse needs to be calculated. set caches the matrix under "x"
#' getMtxInv/setMtxInv get/sets a precalculated
#' inverse. setMtxInv caches the inverse under "mtx.inv"


makeCacheMatrix <- function(x = matrix()) {
  mtx.inv<-NULL
  
  ##Getter-setter-like functions
  ##Setter for the matrix
  set<-function(y){
    x<<-y
    mtx.inv<<-NULL
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


#' cacheSolve solves the matrix and caches the result (inverse) for further reuse.
#' 
#' @param a list of 4 getter/setter functions from makeCacheMatrix
#' @param ... variadic argumets list used by the "solve" function 
#' within the body of cacheSolve
#' @return an inverse of a given matrix. In case the inverse has been calculated
#' before - returns the chashed value.
#'

cacheSolve <- function(x, ...) {
  
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


