## The objective of this code is to cache the inverse of a matrix.
## This is done by 2 function: the first one ('make...') defines a list
## of functions used to store and get the value of both 
## the matrix we want inverted and its inverse.
## The second function ('cacheSolve') gets the cached value
## of the inverse, if it exists.
## Otherwise it calculates it and stores it using function 1.

## Function 1 creates a list of 4 other functions that will cache
## the matrix we want inverted 'x' and the value of its inverse: 'inv'.
## The functions are:
## 1.'set()': sets the input matrix 'x' (and clears any previous
## cached inverses there may have been).
## 2.'get()': returns the matrix we want inverted 'x'.
## 3.'setinv()': sets the inverse of 'x'
## 4.'getinv()': returns the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
        ## Initializes the inverse to NULL
        inv <- NULL
        
        ## Takes the free parameter 'y' and updates the 
        ## cached matrix 'x' also cleaning up cached inverses
        ## related to old values of 'x'
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## Returns the stored value of the matrix
        get <- function() x
        ## 
        setinv <- function(inverse) inv <<- inverse
        ## Returns the stored value of the inverse
        getinv <- function() inv
        
        ## The output of the function is a list of the 4
        ## functions defined above
        list(set = set, get=get, setinv = setinv,
             getinv = getinv)
}


## Function 2 returns a matrix that is the inverse of 'x'
## if this value was already cached (by the previous function),
## it also prints a message stating so.
## Otherwise, it calculates the inverse of the cached matrix
## and stores its value for future use (also using makeCacheMatrix).

cacheSolve <- function(x, ...) {        
        ## First, it attempts to get the cached inverse
        inv <- x$getinv() 
        
        ## If not empty, prints message and returns inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }        
        ## Otherwise, gets cached matrix and calculates inverse
        data <- x$get()
        inv <- solve(data)
        ## Saves inverse in cache and returns its value
        x$setinv(inv)
        inv
}
