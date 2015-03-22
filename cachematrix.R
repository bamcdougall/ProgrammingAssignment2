## Put comments here that give an overall description of what your
## functions do
####
## The actions of these two function (method) blocks is storing a matrix (Mtrx), generating its inverse (MtrxInv),
## and storing both the matrix and its inverse in a data.frame separate from the originating data.frame.
##
## The purpose of these actions is computational efficiency.  Generating inverse matrices is an ~O(n^3) computation.
## (See http://en.wikipedia.org/wiki/Computational_complexity_of_mathematical_operations)
## If the originating matrix is not changing, then each call to its inverse need not be 
## calculated at each call.
##
## For more detail regarding lexical scope including the superassignment operator <<-,
## msee http://cran.r-project.org/doc/manuals/r-release/R-intro.html#Scope
## (Section 10.7 Scope)

## Write a short comment describing this function
####
## makeCacheMatrix is a function that stores a matrix and its inverse.  Once the inverse matrix is
## generated, subsequent calls return the buffered inverse matrix.

makeCacheMatrix <- function(Mtrx = matrix()) {
    MtrxInv <- NULL
    set <- function(Y) {
        Mtrx <<- Y
        MtrxInv <<- NULL
    }
    get <- function() Mtrx
    setInverse <- function(solve) MtrxInv <<- solve
    getInverse <- function() MtrxInv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
####
## This cacheSolve function generates the inverse matrix object that is subsequently stored 
## in the makeCacheMatrix method.
##   NOTE:  as of this writing, <object>$setInverse() returns argument "solve" is missing, with no default
##          However, <object$setmean() from the example function has a comparable return with 
##          "argument "mean" is missing, with no default".

cacheSolve <- function(Mtrx, ...) {
        ## Return a matrix that is the inverse of 'Mtrx'
        ## Problem assignment states that executing these methods is not required.
        ## Presumably, getting this method working is part of a subsequent problem assignment.
    MtrxInv <- Mtrx$getInverse()
    if(!is.null(MtrxInv)) {
        message("getting cached data")
        return(MtrxInv)
    }
    data <- Mtrx$get()
    MtrxInv <- solve(data, ...)
    Mtrx$setInverse(MtrxInv)
    MtrxInv
}


## One simple way to test functionally is generating a diagonal matrix using the Matrix library.
## library(Matrix)
## test <- 4*Diagonal(3,1)
## 
## BufferTest <- makeCacheMatrix(test)
## BufferTest$get()
## BufferTest$getInverse()
## BufferTest$setInverse()
## BufferTest$setInverse(solve(BufferTest$get()))
## BufferTest$getInverse()

