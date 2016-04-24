## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        ## given: x is an invertible matrix
        ## to do: a set of functions that will:
        ## set the matrix
        ## get the matrix
        ## set the inverse
        ## get the inverse
        
        inv = NULL
        set = function(y) {
                ## we will use <<- to assign a value in an environment different from the current
                
                x <<- y
                inv <<- NULL 
                }
                
        get = function () x
        setinverse = function(inverse) inv <<- inverse
        getinverse = function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## where x is the output of makeCacheMatrix()
        ## result - an inverse of the matrix in makeCacheMatrix()
        
        inv = x$getinverse()
        
        ## when inverse has already been calculated
        
        if(!is.null(inv)) {
                
                ## retrieves from cache by skipping operation
                
                message("getting data from cache")
                return(inv)
        }
        
        ## if negative, calculates inverse
        
        matrix.data = x$get()
        inv = solve(matrix.data, ...)
        
        ## setting the value of the inverse in cache
        
        x$setinverse(inv)
        return(inv)

}
