## Functions makeCacheMartix and cacheSolve compute the inverse
## of a square matrix and cache the value

## makeCacheMatrix takes in a matrix object x and creates a special
## "matrix" list that can cache its inverse m
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## initialize m
    
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
    ## elements in the list are named so we can use the 
    ## $ extract operator later
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the cached inverse matrix, if has already been calculated
## Otherwise, it computes the inverse and sets the value in the cache

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
