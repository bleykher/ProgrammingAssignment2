## The functions are used to create a specical object
## that stores a matrix and caches its inverse matrix
## (matrix inversion is usually a costly computation)

## The function creates special matrix" which is a list containing the following functions:
## set value of the matrix, get value of the matrix
## set value of the inverse matrix, get value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # cache for inverse matrix
        
        # set function for main matrix
        set <- function(y) {
                x <<- y
                i <<- NULL # if we reinitialize matrix we should drop cache
        }
        
        # get value of main matrix
        get <- function() x
        
        # set value of inverse matrix (cache)
        setinverse <- function(inverse) i <<- inverse
        
        # get value of inverse matrix (from cache)
        getinverse <- function() i
        
        #return list with all functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function calculates inverse matrix for special "matrix"
## created by makeCacheMatrix
cacheSolve <- function(x, ...) {
        # read data from cache
        i <- x$getinverse()
        if(!is.null(i)) { # if cache data is not null then use cache
                message("getting cached data")
                return(i)
        }
        
        data <- x$get() # get main matrix
        i <- solve(data, ...) # calculate inverse matrix
        x$setinverse(i) # set cache
        i #return inverse matrix
}
