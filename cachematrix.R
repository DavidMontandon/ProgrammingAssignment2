##
## A set of functions that cache the inverve of a matrix
## if the calculation of this inverve is requested for the same matrix it will return data from cache
## 
## makeCacheMatrix( x ) : calculate the inverse of x and put result in cache
## cacheSolve( x ) : return the inverse of x, in case x is the matrix in cache, the inverse will not be calculated again
##
## exemple of use 
## makeCacheMatrix( matrix(1:4, 2, 2) )  - this will add the matrix and its inverse in cache
## cacheSolve( matrix(1:4, 2, 2) )       - this will return the inverve from cache
## cacheSolve( matrix(1:6, 2, 3) )       - this will return the inverve calculted on the fly (without cache)


## Cache the inverse of 'x
## To get this inversed matrix faster, use cacheSolve(x)
makeCacheMatrix <- function(x = matrix()) {
  if(is.matrix(x) != TRUE) {
    message("This matrix is not valid")
  } else {
    message("This matrix is now in cache")
    cacheX <<- x  ## a copy of matrix x to be compared later in cacheSolve()
    cMatrixInverse <<- solve(x) ## the reverse of the matrix x
  }
  
}


## Return a matrix that is the inverse of 'x
## is matrix 'x has been cached by makeCacheMatrix(x)
## it will return the inverse matrix from the cache directly 
## in other case it will will calculate the inverse of x on the fly
cacheSolve <- function(x, ...) {

  if(is.matrix(x) != TRUE) {
    message("This matrix is not valid")
    
  }  else if (identical(x, cacheX) & class( cMatrixInverse ) == "matrix") {  ##check if matrix x is already in cache
    message("This matrix is in cache")
    cMatrixInverse   ## return cached inverse
  } else {
    message("This matrix is not in cache")
    solve(x) ## return on the fly inverse calculation
  }
  
}
