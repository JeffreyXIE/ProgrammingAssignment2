
# This function create a list containing four functions to 1.set the value of the matrix 2.get the value of
# the matrix 3.set the value of the inverse matrix and 4.get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
     # set the inverse to be null first to make sure out of data inverse will not be used
        i <- NULL
     # if set function is called, the matrix will be updated by the funtion and the inverse 
     # is set to be null
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
     # the get funtion is called to get the matrix
        get <- function() x
     # setinverse is used to store the inverse matrix value to i 
        setinverse <- function(inverse) i <<- inverse
     # getinverse is used to get the inverse matrix that has already been calculated
        getinverse <- function() i
     # a list containing the four function is created for calling them by name in other functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse  = getinverse )
}

# The function first checks if the inverse has already been calculated. If so it will get it from
# the cache. If not it will get the matrix, calculate the inverse matrix, store the value by calling 
# setinverse from makeCacheMatrix and return the inverse.

cacheSolve <- function(x, ...) { 
     # the inverse matrix from makeCacheMatrix cache is first captured by calling getinverse
        i <- x$getinverse()
     # if the value is not null, the inverse matrix from the cache will be returned
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
     # else the inverse matrix will be calculated and cached
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
      # the inverse matrix will be returned finally
        i
}

