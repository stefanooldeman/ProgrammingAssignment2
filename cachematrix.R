## First an instance of makeCacheMatrix is created, then cacheSolve is called with a matrix and the cache
## first time the cache is empty
## every other call is using the cache.

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

# example
data = matrix(c(2:7), nrow=2, ncol=2, byrow=TRUE)
cache <- makeCacheMatrix()
cacheSolve(data, cache)
# call for second time will use cached value
cacheSolve(data, cache)