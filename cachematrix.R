## Put comments here that give an overall description of what your
## functions do

#  fnlist <- makeCacheMatrix(x) stores a matrix, x, and its inverse.
#  and it returns a list of functions, fnlist, to access these.
#
#  cacheSolve(fnlist) returns the inverse of the matrix x stored
#    by the call fnlist <- makeCacheMatrix(x)
#  If the inverse does not exist it first calculates and caches it.
#  If the matrix is singular an error message is printed  

## Write a short comment describing this function

## A call to makeCacheMatrix creates an environment
#    which contains a copy, x, of the matrix argument
## This environment persists.
## The environment contains a local variable, xinv, 
#  which is initially NULL.
#
## The function returns a list of functions (defined in
#    makeCacheMatrix) which access x and xinv.
## These functions can be used by a function such as 
#    cacheSolve to set and get the values of xinv and x

## A sample call is 
#    inv1 <- makeCacheMatrix(matrix(1:9, 3, 3)) 
#   which initialises x to matrix(1:9, 3, 3)
#   and sets xinv to NULL

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    ## set - resets of the environment values
    set <- function(y) {
              ## change the matrix x to y - DANGEROUS
              x <<- y
              xinv <- NULL
           }  
    ## return the matrix being inverted 
    get <- function() x
    ## set the inverse xinv to a new matrix yinv
    ## TAKE CARE that yinv is the inverse of x
    setinv <- function(yinv)  xinv <<- yinv
    ## return the inverse of x
    getinv <- function() xinv 

    ## return a list of the 4 functions to access x, xinv
    ##   as free variables and use <<- to assign to them
    list( set= set,         get= get, 
          setinv = setinv,  getinv= getinv
        )
}

## Write a short comment describing this function

## The first argument to cacheSolve is a list of functions
#    returned by a call such as  mm <- makeCacheMatrix,
#
## The call,   cacheSolve(mm)
#    where mm is a list of these functions,
#    eg:  mm$set, mm$get,  mm$setinv, mm$getinv
#  is used initialise the inverse of mm$xinv if it is NULL
#    or return its value if non-NULL.
#
##  Typically the first call finds the inverse is NULL so it 
#    calculates the inverse and caches it.
#   Subsequent calls just returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # get local copy of the inverse
        xinv <- x$getinv()
        if( !is.null(xinv)) {  ## the inverse exists
            ## message("getting cached inverse")
            return( xinv)
        }
        # else get the matrix and calculate the inverse
        mat <- x$get()         ## get the matrix
        xinv <- solve( mat )   ## get its inverse
        x$setinv( xinv )       ## cache the inverse
        xinv                   ## return the inverse         
}

## Usage
## makeCacheMatrix(x) creates an environment 
#     containing argument x initialised to the data matrix 
#     and a local variable xinv tho hold the inverse. 
#   and returns list of functions to manipulate these
## The environment, with x and xinv, persists
#
##  Sample call:
#   mm <- makeCacheMatrix(matrix(1:4,2,2))
#   initialises x to matrix(1:4,2,2) and xinv to NULL
#   mm is a list of the functions returned and accessed as
#    mm$get(), mm$set(y), mm$setinv(yinv), mm$getinv()

## The call
#     cacheSolve(mm) 
#  is used initialise the inverse of mm$xinv if it is NULL
#    or return its value if non-NULL.
#  Typically the first call finds mm$xinv is NULL so it 
#    calculates the inverse and caches it.
#  Subsequent calls just return the non-NULL mm$xinv

## Another call,
#      m2 <- makeCacheMatrix(matrix(c(1:5,4:7),3,3))
#  creates a separate environment for this matrix and 
#  and returns its functions in list m2.   The cached inverse
#  is then returned by the call:  cacheSolve( m2 )

## NOTE the value of x should not be set by cacheSolve
#    as it is input as the argument of makeCacheMatrix.
#  WARNING  Other functions could be written to change x 
#    and xinv to corrupt these essentially private values

## Test
# n <- 300
# big <- matrix(sample(1:2000,n*n, replace=TRUE), n, n)
# m2 <- makeCacheMatrix(big)
# mxinv <- cacheSolve(m2)
# big%*%mxinv
