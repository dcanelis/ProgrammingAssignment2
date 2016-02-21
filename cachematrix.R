## Description:
## The makeCacheMatrix function takes a matrix as an argument and retruns a "special" matrix that can be inverted
## along with a list of functions that can be used to invert the matrix and cache the inversion.

## Use: create an invertable matrix then call makeCacheMatrix 
## e.g. k <- matrix(c(1, 2, 4, 7, 3, 5, 5, 9, 8), 3, 3))  #Note do not use sequential numbers like 1:9
##      g <- makeCacheMatrix(k)

makeCacheMatrix <- function(x = matrix()){
       
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m

        ## function returns a list with the functions defined to get & set the value of the input vector and 
        ## functions to get and set the inverse of the argument vector
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve) 
        
}

## Description
## This function:
## 1) checks to see if the inverted matrix has been cached
## 2) if it has been cached then return the cached inversion and, if not, invert it and cache it
## Use: call cacheSolve with the matrix returned by makeCacheMatrix
## e.g. cacheSolve(g)

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