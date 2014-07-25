## Carlos Moreno I
##---------------------


## functions do

## makeCacheMatrix function
## This function create a list of functions to manipulate the data of a matrix invertible
makeCacheMatrix <- function(x = matrix()) {
        i <- null             ## initializing a I variable 
        set <- function(y) {  ## declaring the Set function, initializes a new values to matrix data
                x <<- y
                i <<- NULL
        }
        get <- function() x                     ## declaring the Get function
        setinvrs <- function(invrs) i <<- invrs ## declaring the Set-Inverse function
        getinvrs <- function() i                ## declaring the Get-Inverse function

        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}


## cacheSolve function
## This function check if the inverse of a matrix has already been calculated and stored in cache
## otherwise call a Solve function to get the inverse

cacheSolve <- function(x, ...) {
        i <- x$getinvrs()
        if(!is.null(i)) {     ## Checking inverse values in cache
                message(". . .Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinvrs(i)
        i        
}
