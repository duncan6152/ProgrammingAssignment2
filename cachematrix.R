## Put comments here that give an overall description of what your
## functions do
##----------------------
#  A call   fnlist <- makeCacheMatrix(x)  creates an environment
#     containing the matrix, x, and its inverse xinv.
#  It returns a list of functions 
#   ie,  fnlist <- list( get, set, getinv, setinv)
#  which can be used by a function such as cacheSolve
#  to access x and xinv
#
#  Then a call xinv <- cacheSolve(fnlist), using this function list,  
#  returns the inverse of the matrix x stored in the environment of
#    makeCacheMatrix(x)
#  If the inverse does not exist it is first calculated and cached,
##-----------------------

## Write a short comment describing this function
##-----------------------------------------------
## A call to makeCacheMatrix(x) creates an environment
#    which contains a copy of the matrix argument x.
## The environment also contains a local variable, xinv, which 
#    is initially NULL but will be set to the inverse of x.
## This environment persists.
## It returns a list of functions which access x and xinv.

## A sample call is 
#   fnlist <- makeCacheMatrix(matrix(1:9, 3, 3)) 
#   which initialises x to matrix(1:9, 3, 3)
#   and sets xinv to NULL
##------------------------

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
  ## the enviromnent stores x and xinv

  ## the following functions manipulate x and xinv
  ##   (using <<- to access the defining environment)
  ## set - resets the defining environment values
    set <- function(y) {
              x <<- y
              xinv <<- NULL
           }  
  ## return the matrix being inverted 
    get <- function() x
  ## reset xinv to a new matrix yinv
    setinv <- function(yinv)  xinv <<- yinv
  ## return the inverse of x
    getinv <- function() xinv 

  ## return a list of the 4 functions to access x, xinv
    list( set= set,         get= get, 
          setinv = setinv,  getinv= getinv
        )
}

## Write a short comment describing this function
##-----------------------------------------------
## The first argument to cacheSolve is a list of functions
#    returned by a call such as  fnlist <- makeCacheMatrix(x).
#    These functions access a matrix, x, and its inverse xinv 
#    in the environment created by makeCacheMatrix(x).
#
## The call, inv <- cacheSolve(fnlist), where fnlist is the list 
#  of   fnlist$set, fnlist$get,  fnlist$setinv, fnlist$getinv,
#  is used get the inverse of matrix x, using fnlist$getinv(), 
#  and return its value.  
# 
#  Typically the first call finds the inverse to be NULL so it 
#  then calculates the inverse and caches it.
#  Subsequent calls just return the cached inverse.
##-------------------------

cacheSolve <- function(x, ...) {
     ## x is a list of functions which access the matrix
     ## Return the inverse of the matrix

        # get local copy of the inverse
        inv <- x$getinv()
        if( !is.null(inv)) {  ## the inverse exists
            ## message("getting cached inverse")
            return( inv)
        }
        # else get the matrix and calculate the inverse
        mat <- x$get()         ## get the matrix
        inv <- solve( mat )   ## get its inverse
        x$setinv( inv )       ## cache the inverse
        inv                   ## return the inverse         
}

##-----------------------------------------------------
## Usage
## fnlist <- makeCacheMatrix(x)  creates an environment 
#     containing argument x initialised to the data matrix 
#     and a local variable xinv to hold the inverse. 
#  It returns a list of functions to manipulate these.
#
##  Sample call:
#   fnlist <- makeCacheMatrix(matrix(1:4,2,2))
#   initialises x to matrix(1:4,2,2) and xinv to NULL
#
## Then the call
#  inv <- cacheSolve(fnlist) 
#  is used calculate and store the inverse if it is NULL
#  It returns the value of the inverse.

## Another call,
#      m2 <- makeCacheMatrix(matrix(c(1:5,4:7),3,3))
#  creates a separate environment for this matrix and 
#  and returns its functions to list m2.   The cached inverse
#  is then returned by the call:  m2inv <- cacheSolve( m2 )

## Test
# n <- 300
# big <- matrix(sample(1:2000,n*n, replace=TRUE), n, n)
# m2 <- makeCacheMatrix(big)
# mxinv <- cacheSolve(m2)
# summary(big%*%mxinv)    ## check identity matrix

