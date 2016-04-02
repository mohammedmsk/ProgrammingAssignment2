##These two functions will return the inverse of a matrix and
##cache it for future use 
## USAGE:
## temp <- makeCacheMatrix(InputMatrix)
## cacheSolve(temp)


## makeCacheMatrix creates a list containing functions to:
################## set and get the value of a matrix
################## set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
        MyMatrix <- NULL
        set <- function(y)
        {
                x <<- y
                MyMatrix <<- NULL
        }

        get <- function() x
        
        setinv <- function(inv) MyMatrix <<- inv
        
        getinv <- function() MyMatrix

        list(
                set = set,
                get = get,
                setinv = setinv,
                getinv = getinv
                )
}


## cacheSolve calculates the inverse of input matrix
## First checks if its already calculated and returns that.
## Otherwise calculates it and saves it in cache.

cacheSolve <- function(x, ...)
{
        m <- x$getinv()
        if(!is.null(m))
        {
                message("Getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        x$setinv(m)
        return(m)
}
