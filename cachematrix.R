## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
## Parameter x is A matrix
## This function returns A list containing four functions to set and get the value of the
## matrix and to set and get the inverse of the matrix
##     


makeCacheMatrix <- function(x = matrix()) 
{
       
        m <- NULL          #Initializing m

        # Defining function to set the value of the matrix. 

        set <- function(y) {
                x <<- y    # Set the value
                m <<- NULL # Initializing the global cache
        }

        # Define function to get the value of the matrix
        get <- function() x
       
        # Define function to set the inverse. 
        setinverse <- function(solve) m <<- solve

        # Define function to get the inverse
        getinverse <- function() m
        
        # Return a list with the above four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the "matrix" returned by the function
## makeCacheMatrix above. If the inverse has already been calculated 
## and the matrix has not changed, then the cachesolve retrieves the 
## inverse from the cache. 
## Parameter x a special matrix created with makeCacheMatrix function above
## This function returns the inverse of the matrix x
 


cacheSolve <- function(x) {
         m <- x$getinverse() # Get value of matrix
        if(!is.null(m)) {
                message("getting cached data")
                # If the cache is not empty we can return it
                return(m)
        }
        # If the cache is not empty we can get the matrix, inverse it and return it

               data <- x$get()  # Get the value of matrix
               m <- solve(data) # Calculate the inverse
               x$setinverse(m)  # Cache the result
               m                # Return the inverse

}

