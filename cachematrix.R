##Caching the inverse of a matrix

##Functions makeCacheMatrix and CacheSolve are ment to be used in conjuction to
#calculate and cache the inverse of an inversible matrix.
#While makeCacheMatrix is used to store the values of both the original matrix
#and inverted matrix, CacheSolve evaluates whether the inverted matrix is
#already cached and returns it, otherwise it inverts the original matrix.

##makeCacheMatrix contains 4 sub-functions (set, get, setinverse, getinverse)
#and is used to set/store an inversible matrix x and its inversed matrix after 
#being calculated by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
       
       #comment 1
       
       inv <- NULL
       set <- function (y) {
              x <<- y
              inv <<- NULL
       }
       
       #comment 2
       
       get <- function() x
       setinverse <- function(inverse) inv <<- inverse
       getinverse <- function() inv
       
       #comment 3
       
       list(set = set, get = get, setinverse = setinverse,
            getinverse = getinverse)
}

#comment 1
#Sub-function 'set' sets the value of matrix x or
#overwrites the matrix x stored, ideally with a new matrix

#comment 2
#Sub-function 'get' returns a matrix x stored in makeCacheMatrix
#Sub-function 'getinverse' is used to store the value of the variable inv,
#after cacheSolve is executed
#Sub-function 'setinverse' sets the value of variable inv (ideally the
#inversed matrix x) or overwrites variable inv stored

#comment 3
#In order to use variable matrix x in the sub-functions, they should be
#included in a list.



##CacheSolve uses the output of makeCacheMatric to reverse matrix x.

cacheSolve <- function(x, ...) {
       
       #comment 4
       
       inv <- x$getinverse()
       if(!is.null(inv)){
              message('getting cached data')
              return(inv)
       }
       
       #comment 5
       
       data <- x$get()
       inv <- solve(data, ...)
       x$setinverse(inv)
       inv
}

#comment 4
#the fucntion firstly checks if there is a value stored/cached in getinverse
#If yes then it returns this value with a message

#comment 5
#if the value in getinverse is NULL then the reverse of matrix x is calculated
#using function 'solve'. It then stores the outcome in setinverse and returns it

