## The makeCacheMatrix creates special matrix object that is capable of caching its mean
## The cacheSolve finds an inverse of a matrix. It returns cached result when possible

## Creates matrix that is capable of remembering its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(i) inv <<- i
    
    getInv <- function() inv
    
    list(get = get, set = set, getInv = getInv, setInv = setInv)
    
}


## Calculates inverse. It may use cached value if inverse was previously calculated.
## The x argument is a matrix created using makeCahceMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        inv <- x$getInv()
        if(!is.null(inv))
        {
            message("inverse from cache")
            return(inv)
        }
        
        matrix <- x$get()
        inv <- solve(matrix)
        
        x$setInv(inv)
        
        inv
}
