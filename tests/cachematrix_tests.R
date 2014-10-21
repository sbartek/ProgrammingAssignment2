#TODO: I dont know if it is possible to use mocks and stubs in testthat

setwd("..")
source("cachematrix.R")

context("makeCacheMatrix")

test_that("it sets getters as expected", {
    m <- makeCacheMatrix()
    expect_equal( m$get(), matrix())
    expect_null( m$getInv() )

    x <-  cbind(c(1,0),c(0,1))
    m2 <- makeCacheMatrix(x)
    expect_equal( m2$get(), x)
})


test_that("its setters work as expected", {
    x <-  cbind(c(1,1),c(0,1))
    (m <- makeCacheMatrix())$set(x)
    expect_equal( m$get(), x )
    m$setInv(x)
    expect_equal( m$getInv(), x )
})

context("cacheSolve")

test_that("it sets mInv when null", {
    x <-  cbind(c(1,0),c(0,1))
    m <- makeCacheMatrix(x)
    sm <- cacheSolve(m)
    expect_that( m$getInv(), equals(sm) )
})

test_that("it does not touch mInv when not null", {
    x <-  cbind(c(1,0),c(0,1))
    m <- makeCacheMatrix(x)
    xPseudoInv <- cbind(c(0,0),c(0,0))
    environment(m$setInv)$xInv <- xPseudoInv
    sm <- cacheSolve(m)
    expect_that( m$getInv(), equals(xPseudoInv) )
})

test_that("it calculate inverse as it should", {
    x <-  cbind(c(1,0),c(0,1))
    m <- makeCacheMatrix(x)
    sm <- cacheSolve(m)
    expect_that( sm, equals(x) )
    x <-  cbind(c(1,0),c(1,1))
    m <- makeCacheMatrix(x)
    sm <- cacheSolve(m)
    expect_that( sm, equals(solve(x) ))
})
