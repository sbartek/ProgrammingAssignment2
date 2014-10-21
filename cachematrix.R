## This is implementation of cached version of solve() function that
## compute the inverse of a matrix.

## I have included few unit and class tests in tests/. They are
## written using "testthat" library (need to be installed). Call them
## in R by:
## > library("testthat")
## > test_file("tests/cachematrix_tests.R")


## Function makeCacheMatrix() returns an "object-like" list that have
## two getters and two setters function for two private
## variables. Variable x is where the matrix is stored, and its
## inverse is stored in xInv.

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setInv <- function(newInv) xInv <<- newInv
    getInv <- function() xInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## Function cacheSolve() has an argument being a list like the one
## produced by makeCacheMatrix(). It returns the value of "private"
## variable xInv by calling getInv. If the variable is null it calls
## solve() and then sets xInv calling "method" setInv.

cacheSolve <- function(m, ...) {
    xInv <- m$getInv()
    if(is.null(xInv)) {
        x <- m$get()
        xInv <- solve(x, ...)
        m$setInv(xInv)
    }
    xInv
}
