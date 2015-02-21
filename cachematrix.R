###########################################################
## Author: Stefano Oldeman
## https://twitter.com/stefanooldeman
##
## cachematrix.R
## when using cacheSolve it wil internally verify the input data and check if it has a corresponding cache object. 
## The cache object will be recreated if the input is different.
## Then, when the inverse of a matrix is calculated, this is set to the cache with matrix$inverse.set(x).
## The second time cacheSolve will be called with the same data, the calculation is skipped by returning the previous value.
###########################################################

## makeCacheMatrix
##
## @return 2 functions and data variable that contains original matrix
##   set and get can be used to cache a calculation related to this data
makeCacheMatrix <- function(matrix = matrix()) {
  cache <- NULL
  list(inverse.get = function() cache,
       inverse.set = function(val) cache <<- val,
       data = matrix)
}

## cacheSolve
##
## @return the inverse of a matrix and re-uses cache through using makeCacheMatrix
## note it will create a variable in the parent scope: cacheSolve.matrix
## of the cache object, in order to validate if the cache is related to the input data matrix (given to cacheSolve)
## only one matrix can be cached at any given time
cacheSolve <- function(matrix) {
  if (!exists("cacheSolve.matrix") || !identical(matrix, cacheSolve.matrix$data)) {
    cacheSolve.matrix <<- makeCacheMatrix(matrix) # store a copy to verify if the input data is the same
  }
  m <- cacheSolve.matrix
  cached_value <- m$inverse.get()
  if(!is.null(cached_value)) {
    return(cached_value)
  }
  # calculate inverse
  inv <- solve(m$data)
  m$inverse.set(inv)
  inv
}


### start test scenario ####

test <- function(k = 200) {
  print ("preparing large dataset for testcase")
  library(MASS)
  setup = function (k) {
    print(sprintf("computing a %d*%d matrix.. wait a sec", k, k))
    rho <- .3
    S       <- matrix(rep(rho, k*k), nrow=k)
    diag(S) <- 1
    dat <- mvrnorm(10000, mu=rep(0,k), Sigma=S) ### be patient!
    cor(dat)  
    # retrieved from Stackoverflow: http://goo.gl/yoyR8S
  }
  
  print ("first call to cacheSolve (cache warmup, please wait..)")
  dataset1 = setup(k)
  c1 <- system.time(RI1 <- cacheSolve(dataset1))
  print (c1)
  print("second call: expect time to be less")
  c2 <- system.time(RI1 <- cacheSolve(dataset1))
  print( c2)
  
  print ("with different object it should not use cache, expect elapsed > 0")
  dataset2 <- setup(k - 100)
  c3 <- system.time(RI1 <- cacheSolve(dataset2))
  print( c3)
  all(c1[[3]] > c2[[3]], c3[[3]] > c2[[3]])
}

test(200)
