##
# Coursera | R programming Assignment 2: Caching the Inverse of a Matrix
#
# For more details, please reference: https://class.coursera.org/rprog-013/human_grading/view/courses/973494/assessments/3/submissions
##


#
# Creates a special "matrix" object that can cache its inverse.
#
makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(y){
        m <<- y
        inv <<- NULL 
    }
    get <- function() m
    setinverse <- function(inverse) inv  <<- inverse
    getinverse <- function() inv
    list(set= set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}

#
# Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cach
#
cacheSolve <- function(m, ...) {
    inv <- m$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data, ...)
    m$setinverse(inv)
    inv
}

