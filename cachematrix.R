## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # caching objects x and m
    set <- function(y) {
        ## assign matrix y input argument to object 'x' 
        ## in the parent environment
        x <<- y
        
        ## assign value of NULL to m object in parent environ
        ## clears previous values of m
        m <<- NULL
    }
    
    # accessor method retrieves x from parent environ
    get <- function() x
    
    # mutator method assigns input argument to value of m
    # in the parent environment
    setinverse <- function(solve) m <-- solve
    
    ## get the inverse m from the parent environment
    getinverse <- function() m
    
    ## assigns each function as an element within a list()
    ## and returns it to the parent environment
    ## elements in the list  are name so we can use the 
    ## $ extract operator later
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## function gets cached inverse matrix, if has already been calculated
## otherwise it computes the inverse and sets the value in the cache
## mean has already been calculated. If so, it `get`s the mean from the

## Requires input argument of type makeCacheMatrix()
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    # check whether the inverse has already been computed
    if(!is.null(m)){
          message("getting cached data")
          return(m)
    }
    
    # parse matrix x
    data <- x$get()
    
    #compute the inverse
    m <- solve(data, ...)
    
    # set the inverse to the parent environment
    x$setinverse(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
