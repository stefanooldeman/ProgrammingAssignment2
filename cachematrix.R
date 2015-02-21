###########################################################
## Author: Stefano Oldeman
## https://twitter.com/stefanooldeman
##
## cachematrix.R
## First an instance of makeCacheMatrix is created, then cacheSolve is called with a matrix and the cache
## the first time the cache is empty
## every other call is using the cache.
## An example usage:
data = matrix(c(2:7), nrow=2, ncol=2, byrow=TRUE)
cache <- makeCacheMatrix()
cacheSolve(data, cache)
cacheSolve(data, cache) # this call will always be faster
###########################################################

## makeCacheMatrix
##
## @return a list of two methods
##   save: stores a value to cache
##   get: retrieves the value from cache
makeCacheMatrix <- function(storage = NULL) {
  list(get = function() storage,
       save = function(val) storage <<- val)
}

## cacheSolve
##
## @return the inverse of a matrix
##   when cache$get() is null, it's calculated. Otherwise a cache$get() is returned
cacheSolve <- function(matrix, cache) {
  cached_value <- cache$get()
  if(!is.null(cached_value)) {
    return(cached_value)
  }
  # calculate inverse
  calculated_value <- solve(matrix)
  cache$save(calculated_value)
  calculated_value
}


test <- function(k = 1000) {
  print ("preparing large dataset for testcase")
  ##### setup #######
  library(MASS)
  rho <- .3
  S       <- matrix(rep(rho, k*k), nrow=k)
  diag(S) <- 1
  dat <- mvrnorm(10000, mu=rep(0,k), Sigma=S) ### be patient!
  R <- cor(dat)
  ##### retrieved from Stackoverflow: http://goo.gl/yoyR8S #######
  print ("first call to cacheSolve (cache warmup, please wait..)")

  cache <- makeCacheMatrix()
  c1 <- system.time(RI1 <- cacheSolve(R, cache))
  print (c1)
  print("second call: expect time to be less")
  c2 <- system.time(RI1 <- cacheSolve(R, cache))
  print( c2)
  c1[[3]] > c2[[3]]
}

test()