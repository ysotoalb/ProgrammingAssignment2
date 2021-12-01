## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
makeCacheMatrix <- function(x = matrix()) {
        p <- NULL
        
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                p <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        
        #set the value of the inverse
        setInverse <- function(inverse) p <<- inverse
        
        #get the value of the inverse
        getInverse <- function() p
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

#calculate the inverse of the special "matrix" created with the above function
cacheSolve <- function(x, ...) {
        
        #Check to see if the inverse has already been calculated
        p <- x$getInverse()
        
        #If so, it gets the inverse from the cache and skips the computation
        if(!is.null(p)) {
                message("getting cached data")
                return(p)
        }
        #Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the Solve function.
        data <- x$get()
        p <- solve(data, ...)
        x$setInverse(p)
        p
}

