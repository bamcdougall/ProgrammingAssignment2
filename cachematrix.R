## Put comments here that give an overall description of what your
## functions do
####
## The actions of these two code blocks is storing a matrix (Mtrx), generating its inverse (MtrxInv),
## and storing both the matrix and its inverse in a data.frame separate from the originating data.frame.
##
## The purpose of these actions is computational efficiency.  Generating inverse matrices is computationally
## intensive.  If the originating matrix is not changing, then each call to its inverse need not be 
## generated each time its needed.
##
## For more detail regarding lexical scope, see http://cran.r-project.org/doc/manuals/r-release/R-intro.html#Scope
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
## This function generated the inverse matrix object that is subsequently stored 
## in the makeCacheMatrix method

cacheSolve <- function(Mtrx, ...) {
        ## Return a matrix that is the inverse of 'Mtrx'
    MtrxInv <- Mtrx$getInverse()
    if(!is.null(MtrxInv)) {
        message("getting cached data")
        return(MtrxInv)
    }
    data <- Mtrx$get()
    MtrxInv <- mean(data, ...)
    Mtrx$setInverse(MtrxInv)
    MtrxInv
}
