#This is a program that demonstrates how user can make code in R more effecient.
#The program will fetch matrix and its inversed form from cache if it exists,
#and thus skips resource consuming calculation.


#This function creates a matrix object that has list of function user can use.
#For example to set a matrix or set its inverse. 
#The function then saves this knowledge to cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
#This function first checks if there is a cached matrix/inverse matrix.
#If there is not it will calculate the inversed matrix. If it exist, it 
#takes if from cache.

cacheSolve <- function(x, ...) {
    #this checks if inversed matrix exist.
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Cached inverse exists, fetching it.")
        return(inv)
    }
    data <- x$get()
    #calculation if it does not exist.
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

