## This set of functions calculate the inverse of a
##   matrix, and if the inverse of a given matrix
##   was already calculated in the session, it will
##   fetch the value.
## Usage: create a list of functions using makeCacheMatrix,
##   then solve using cacheSolve
##   func <- makeCacheMatrix(matrix)
##   cacheSolve(func)
##
## NOTE TO EVALUATORS: This is an exact copy of the
##   algorithm to cache the mean of a vector. I just
##   changed the names of functions and variables for
##   fun, but I don't feel like I've grasped the
##   concept.

## makeCacheMatrix works by creating a set of functions
##   to get a matrix, set a matrix, get the inverse of
##   the matrix and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  give <- function(y) {
    x <<- y
    inv <<- NULL
  }
  take <- function () x
  giveinv <- function(x_inverse) inv <<- x_inverse
  takeinv <- function() inv
  list(give = give, take = take,
       giveinv = giveinv,
       takeinv = takeinv)
}

## cacheSolve will check if a given matrix's inverse
##   already exists in the environment. If it does, 
##   return the calculated value. If it doesn't, solve
##   for the inverse and cache it for future operations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$takeinv()
  if (!is.null(inv)) {
    message ("this one was already calculated; fetching...")
    return(inv)
  }
  mtx <- x$take()
  inv <- solve(mtx)
  x$giveinv(inv)
  inv
}

## load some example data in the environment
d1 <- matrix(c(1,-1,1,2),2,2)          
# d1_inverse = matrix(c(2/3,1/3,-1/3,1/3),2,2)
d2 <- matrix(c(1,0,5,2,1,6,3,4,0),3,3) 
# d2_inverse = matrix(c(-24,20,-5,18,-15,4,5,-4,1),3,3)