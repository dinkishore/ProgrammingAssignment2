##Caching the Inverse of Matrix


#Including Library File(For finding inverse of matrix)

library(matrixcalc)

#Making a cache Matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(matrix.inverse) i <<- matrix.inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Finding the Inverse of Cached Matrix

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- matrix.inverse(data)
  x$setInverse(i)
  i
}


M = matrix(data=c(2,1,3,5,4,1,6,7,2),nrow = 3,ncol = 3)
if(nrow(M) != ncol(M)){
  print("Row and Column are not equal")
}else if(det(M) == 0){
  print("Determinant is Zero")
}else{
  cache = makeCacheMatrix(M)
  print(cacheSolve(cache))
}
