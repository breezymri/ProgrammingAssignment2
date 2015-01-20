## Cached implementation of matrix inverse calculation
## Usage: 
##   1. Initialization
##      cm <- makeCacheMatrix()
##      cm$set(my_matrix)
##    or
##      cm <- makeCacheMatrix(my_matrix)
##   2. To retrieve the inverse, do:
##      my_inverse_matrix <- cacheSolve(cm)
## You only need to initialize the matrix once. cacheSolve can be 
## called multiple times. If you need to change the matrix, simply
## call 
##      cm$set(my_new_matrix)


## generate an R object consisting a list of functions to set and
## retrieve matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL
    
    set <- function(y){
        if(!identical(x, y)){
            x <<- y
            xi <<- NULL
        }
    }

    get <- function() x
    
    setinverse <- function(inverse) xi <- inverse
    
    getinverse <- function() xi

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes or retrieves the inverse of the input x, which 
## is an R object that has been generated with the 
## makeCachedMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xi <- x$getinverse()
        if(!is.null(xi)){
            message('getting cached inverse')
            return(xi)
        }
        
        mat <- x$get()
        xi <- solve(mat, ...)
        x$setinverse(xi)

        return(xi)
}
