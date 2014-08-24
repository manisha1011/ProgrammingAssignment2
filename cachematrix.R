# The code calculates the inverse of the matrix 'x'


# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {           
        i <- NULL
        set <- function(y) {                            # Saves the input matrix and resets the inverse to NULL
                x <<- y
                i <<- NULL
        }
        get <- function() x                             # returns the value of original matrix
        setinv <- function(inv) i <<- inv               # Stores value of inverse in i using superassignment
        getinv <- function() i                          # returns value of i
        list(set = set, get = get,                      # creates a list of all functions
             setinv = setinv,
             getinv = getinv)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {                         # The input is an object created by makeCacheMatrix function      
        i <- x$getinv()                                 # Value of the inverse is obtained by accessing getinv function
        if(!is.null(i)) {                               # If the inverse was already cached, message is printed and its value is returned
                message("getting cached data")
                return(i)
        }
        data <- x$get()                                 # Else obtain the matrix
        i <- solve(data,...)                            # Compute its inverse
        x$setinv(i)                                     # Store the inverse matrix
        i                                               # Return the inverse matrix
}
