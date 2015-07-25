## This R code creates two different functions for: 1)  creating a special "matrix" 
## objet able to cache its inverse,  makeCacheMatrix(); 2) returning the inverse of 
## matrix  of a Cached matrix special, cacheSolve(x).

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  inv_x <- NULL # sets the value of inv_x to NULL (default values if cacheSolve has not been run)
  y <- NULL # sets the value of y to NULL (default values if cacheSolve has not been run)
  setmatrix <- function(y) { #set the value of the matrix
    x <<- y ## cache the input matrix  
    inv_x <<- NULL # # sets the value of inv_x
  }
  getmatrix <- function() x
  setinverse <- function(solve) inv_x <<- solve
  getinverse <- function() inv_x
  # creates a list to house the four functions
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function  bellow return the inverse of the matrix 'x'
# In the case that the inverse is already computed, the function find out
# if the matrix has changed before return the inverse. If so, the inverse
# is recomputed and updated

cacheSolve <- function(x,...){
  
  inv_x <- x$getinverse() # getting the inverse
  if(!is.null(inv_x)){# check if the inverse has been calculated before
    y <- x$getmatrix()# get the matrix
    inv_aux <- solve(y, ...) # solve the inverse to compare with the catched one
    if(identical(inv_x,inv_aux)){ # check if the matrix has not changed, if not, retur the inverse
          message("Getting the cached inverse matrix")
          return(inv_x)
    }  
  }
    y <- x$getmatrix() # getting the matrix from the input special "matrix" objet
    x$setmatrix(y)
    inv_x <- solve(y, ...) # compute the inverse of the matrix
    x$setinverse(inv_x) #  set  the inverse to cache it 
    inv_x # return the inverse matrix
}
