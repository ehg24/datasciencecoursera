##**************************************************************
## My Version of Programming Assignment #2
## The following is a pair of functions that cache and compute the 
## inverse of a matrix.
# 1. set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #2. get the value of the matrix
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# 3. set the value of inverse of the matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  
  inv <- solve(data)
  x$setinverse(inv)
  ## 4. get the value of inverse of the matrix
  inv
}

## Sample run for 3X3 matrix
x = rbind(c(1, -1/4, 1), c(-1/4, 1, 2), c(-1/4, 1, 3) )
m = makeCacheMatrix(x)
A <- m$get()
A
B <- cacheSolve(m)
B
A%*%B

## end of my submition 
## **********************************************************************************************************
## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function( x  {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}

mdat <- matrix(c(1,2,3, 11,12,13, ), nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("row1", "row2", ),
                               c("C.1", "C.2", "C.3"))
mdat
### This function creates a special "matrix" object
## that can cache its inverse.
mdat1 <- matrix(c(11,12,13, 21, 22,23, 31,32,33), nrow = 3, ncol = 3, byrow = TRUE,
                +                dimnames = list(c("row1", "row2", "row3"),
                                                 +   c("C.1", "C.2", "C.3"))
                mdat1
                x=mdat1
makeCacheMatrix <- function (x {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return (mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## ## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function (mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }



