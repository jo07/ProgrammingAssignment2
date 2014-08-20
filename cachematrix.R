## Two functions which finds the inverse of a matrix with less computation
## using the cache 

	# This function, makeCacheMatrix creates a list containing 4 functions
	# set() - takes a matrix and caches its value and set the attribute inverse to null
	# get() - returns the matrix from cache
	# setinverse() - takes a matrix and caches its value to the inverse attribute in the cacheSolve
	# getinverse() - returns the matrix which is in the cache under the name 'inverse' which is also
	# the inverse matrix of the matrix x in the cache
	# Input : a matrix
	# Output : a list of 4 functions


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL                                  # sets inverse to NULL
        set <- function(y) {
                x <<- y                                  # caches the value of y in x
                inverse <<- NULL                         # caches the value of inverse to NULL
        }
        get <- function() x                              # returns matrix from cache
        setinverse <- function(inv) inverse <<- inv      # caches the value of inverse
        getinverse <- function() inverse                 # returns inverse of the matrix from cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)                    # function returns this list of 4 variables
}

	# this function, cacheSolve finds the inverse of the matrix created in the above function
	# it uses the cache to save computation by storing the value of the  inverse of a matrix
	# Input :  a matrix
	# Output : inverse of the matrix

cacheSolve <- function(x) {                              
        inverse <- x$getinverse()                       # fetches the value from cache            
        if(!is.null(inverse))                           # checks if it's null or not
                message("getting cached data")
                return(inverse)                         # if not NULL returns the inverse of the matrix
        }
        data <- x$get()                                 # if not available fetches the matrix
        inverse <- solve(data)                          # finds the inverse of the matrix
        x$setinverse(inverse)                           # stores the value to cache
		inverse                                         # Return a matrix that is the inverse of 'x'
}
