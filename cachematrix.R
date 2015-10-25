## calculate the inverse of the matrix
## make calculations more efficient


## set a new matrix, preparing for the later tests
## include 4 functions of set, get, setsolve and getsolve
makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setsolve <- function(solve) m <<- solve
       getsolve <- function() m
       list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}


## test if we already have the calculated result
## return from the cashe if yes
## calculate if not, from the makeCacheMatrix function

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       m <- x$getsolve()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setsolve(m)
       m
}
