## The first function of this pair creates a special matrix object that allows the user to
## create a matrix that can cache its inverse by use of special internal functions.
## The second function will access to cache to return the inverse if it is has already been
## calcualted, and calculate and store it if it has not. The hope is that using this function
## pair will reduce computation time in other programs written with them since solving for the
## inverse of a matrix is computationally expensive, and cacluating it more than once for a
## matrix that may be re-used would increase runtimes, so having the inverse cached saves time.

## This function creates a special matrix object that  is a list 4 assocaited functions. 
## One to set the value, one to get the value, one to set the value of the inverse, and one to
## get the value of the inverse.
## It can be used to create a special matrix object with a cached inverse.
## Use the Set method to set the value of the matrix, and the get method to return it.
## Similarly, use the setInverse to set the inverse, and getInverse to return it.
## Suppose B is a square, invertible matrix.
## To use the function, simply assign it to a variable:  matrixB <- makeCacheMatrix
## then use the set method to define the matrix of interest: matrixB$set(B)
## and use the other functions in a sinilar fashion.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function (solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, 
         setInverse =setInverse, 
         getInverse = getInverse)
}


## This function takes a special matrix object (from the makeCacheMatrix matrix above) as it's
## argument and returns the inverse of the matrix from the cache if it exists, and calculates
## and stores it in the matrix object if it does not. It uses functions defined in the scope of
## the makeCacheMatrix function to achieve this. Following the above function, you would use
## cacheSolve(matrixB) to first solve and define the inverse, then for each time the inverse
## is subsequently needed cacheSolve(matrixB) will return the cached inverse of matrixB since it
## was already computed once. 

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)){
        message("Getting Matrix Inverse Data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m
}
