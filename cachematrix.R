## PURPOSE: to cache the inverse of a given matrix
## Two functions are created:
## (1) makeCacheMatrix: creates a blank global matrix and has a subfunction, which
## can cache the inverse of an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
   # create a vector mx with NULL value in the function environment
   mx <- NULL
   #create function setm, which creates two 'global' objects
   setm <- function(y) {
      x <<- y
      mx <<- NULL
   }
   #creates getm function, which simply returns x
   getm <- function() x 
   # creates setim function, which assigns the provided value to 
   # 'global' object called mx 
   setim <- function(invm) mx <<- invm 
   # creates function getim, which returns value of object mx
   getim <- function() mx
   # create a list containing the created functions for easy referral
   list(setm = setm, 
        getm = getm,
        setim = setim,
        getim = getim)
}

## (2) cacheSolve: computes the inverse of the matrix, which was input into  
## makeCacheMatrix function. If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve function should retrieve the inverse from the cache.
cacheSolve <- function(a, ...) {
   # Creates a local object called mx, which contains the inverse matrix
   # in this step local object mx takes the value from the global object mx.
   # If the inverse matrix was not computed yet, by calling function cacheSolve,
   # then the value is NULL
   mx <- a$getim()

   # If mx is NOT NULL (i.e. inverse matrix has already been computed and 
   # was stored in the global mx), then this cached matrix is printed
   if(!is.null(mx)) {
      message("getting cached inverse matrix")
      return(mx)
   }
   # Create inputmx and assign input matrix from function makeCacheMatrix to it
   inputmx <- a$getm()
   # Assign inverse matrix of inputmx (input matrix) to local object mx
   mx <- solve(inputmx, ...)
   message("solve function provides the inverse matrix")
   # Function setim takes value of (local) mx from previous step and assigns it 
   # to 'global'object mx
   a$setim(mx)
   # Display object mx
   mx
}
##################################################################