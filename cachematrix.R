## Together these functions set a matrix, calculate the inverse of the matrix,
## and return the inverted matrix.  If the inverse has already been calculated
## it is just returned from the cache and not calculated again.

## This function sets a list of 4 functions which set the matrix and get the 
## inverted matrix. No calculations are done within this function, just 
## defining the parameters for the CacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
      m <- matrix()
      set <- function(y) {
            x <<- y
            m <<- matrix()
      }
      get <- function() x
      setInv <- function(solve) m<<-solve
      getInv <- function() m
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## This function uses the parameters defined in makeCacheMatrix to recall the
## matrix from the cache.  If the inverted matrix has previously been calculated
## it returns it from the cache with the message "getting cached data". If the 
## inverted matrix is NA (not previously calculated), it calculates the 
## inverted matrix and returns it.

cacheSolve <- function(x, ...) {
      m <- x$getInv()         ##Recalls the inverted matrix from cache
      if(!is.na(m)) {         ##If inverted matrix is not NA give message
            message("getting cached data")
            return(m)         ##and return (m)
      }
      data <- x$get()        ##These 4 lines calculate/print the inverted 
      m <- solve(data, ...)  ## matrix if it can't be pulled from cache 
      x$setInv(m)
      m                       ## Return a matrix that is the inverse of 'x'
}
