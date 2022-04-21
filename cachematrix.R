makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set_matrix<-function(y){
    x<<-y
    m<<-NULL
  }
  get_matrix<-function() x
  set_Inverse<-function(solve) m<<- solve 
  get_Inverse<-function() m
  ##we are calculate the inv matrix 
  list(set_matrix=set_matrix, get_matrix=get_matrix,
       set_Inverse=set_Inverse,
       get_Inverse=get_Inverse)
}

cacheSolve <- function(x, ...) {
  m<-x$get_Inverse()
  if(!is.null(m)){
    message("caching inverse matrix")
    return(m)
  }
  s<-x$get_matrix()
  m<-solve(s, ...)
  x$set_Inverse(m)
  m
}
##testing the assignmanet
matrix <-makeCacheMatrix(matrix(c(2,3,2,5,6,3,1,3,0),ncol=3,nrow=3))
cacheSolve(matrix)
x$get_matrix()
x$get_Inverse()
