#Assignment-2_Week 3
#Author: Monica Araya Salas
#Date: 14/10/2020

#Part 1_Function:	makeCacheMatrix

makeCacheMatrix <- function(M = matrix()) {
  invM <- NULL
  set <- function(x) {
    M <<- x
    invM <<- NULL
  }
  get <- function() M
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() invM
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Example

Matr<-matrix(c(2,10,2,5),nrow=2) 
Matrz<-makeCacheMatrix(Matr)
Matrz



#Part 2_Function:	cacheSolve

cacheSolve <- function(M, ...) {
  inv <- M$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(invM)
  }
  mat <- M$get()
  invM <- solve(mat, ...)
  M$setInverse(invM)
  invM
}


cacheSolve(Matrz)