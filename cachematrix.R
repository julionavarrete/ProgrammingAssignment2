## Caching the Inverse of a Matrix
## Help to cache the inverse of a Matrix to avoid
## re computation when the matrix already exist.

## Create functions to cache the matrix and the inverse of it

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {                ## Inicialize the matrix and its inverse in the chache
                x <<- y
                m <<- NULL
        }
        
        get <- function() x                 ## Define function set
        
        setinv <- function(inv) m <<- inv   ## Define function to set the inverse matrix
        getinv <- function() m              ## Define function to get the inverse matrix
        
        list(set = set, get = get, setinv = setinv, getinv = getinv) ## Define a list of functions
}


## Computes the inverse of the matrix returned
## by makeCacheMatrix(), unless the inverse has
## already been calculated, in which case
## it retrieves it from the cache.


cachesolve <- function(x, ...) {
  
  data <- x$get()     ## Getting the cached matrix
  m <- x$getinv()     ## Getting the inverse cached matrix
  
  if (x == data) {    ## Compare input and cached matrices, 
    message("getting cached inverse")
    return(m)         ## return inverse matrix and exit of the function
  }
  
  m <- solve(x)       ## If matrices are different, computate the inverse
  x$setinv(m)         ## cache the new inverse matrix
  m                   ## return the inverse matrix
}

