## makeCacheMatrix is a function that creates a special matrix object that can cache the inverse of it.

makeCacheMatrix <- function(x = matrix()) { # "x" is em empty matrix 
        mymatrix <- NULL                    # initialize "mymatrix" as NULL. 
        
        set <- function(y) {
                x <<- y
                mymatrix <<- NULL
        }
        
        get <- function() 
                x 
                
        setinverse <- function(inverse)   # assigns value of "mymatrix" in parent environment
                mymatrix <<- inverse
        
        getinverse <- function() 
                mymatrix
                
        # this list if needed in order to refer to the functions with the $ operator in the "cacheSolve" function below:
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
               
}


# The "cacheSolve" function calculates the inverse of the special matrix returned by the makeCacheMatrix function above
# if the inversion has already been calculated and the matrix has not changed since last calculation, 
# then this function will retrieve from the cache, otherwise, it will calculate the inverse of the matrix. 

cacheSolve <- function(x, ...) {  # Return a matrix that is the inverse of 'x'

   mymatrix <- x$getinverse()
   if (!is.null(mymatrix)) {
           message("getting cache data...")
           return(mymatrix)
   }
   
   cachedata <- x$get()
   
   mymatrix <- solve(cachedata, ...)
   
   x$setinverse(mymatrix)
   
   mymatrix
   
}


