## We are programming two functions intended to salve computation time
## when working on the command line and we aim to perform a time consuming
## task as computing the inversion of a matrix.

## First we create a list of four elements, each containing a funcion:
        ## set, loading the matrix itself
        ## get, providing the matrix elements
        ## setinv, loading the inverse of the matrix, this function assign the 
            ## value not from the current environment, but from the containing one
            ## (global, in this case) if the solve computation has been peformed 
            ## previously. 
        ## getinv, making available the inverse matrix   



makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL     
        set <- function(y){
        x <<- y
        inv <<- NULL
        }
        get <- function()x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function()inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Then, we program a function that checks if the matrix inversion has
## already been carried out. If so, it prints a warning message noticing the
## value is being retrieved from cache and shows the value. Not having a stored value 
## (in other words, not having a cached value because the calculation has not been 
## performed previously), the function computes the matrix inversion and cache its value.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix,...)
        x$setinv(inv)
        inv
}
