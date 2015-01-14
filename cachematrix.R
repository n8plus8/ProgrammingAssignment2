## caching the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x) {        # assuming that object 'x' is an
                                        # invertible matrix
        
        inv <- NULL                     # object 'inv', inverse of matrix 'x'
                                        # initially set to NULL
        
        set <- function(y) {            # method from "makeVector" example
                x <<- y                 # ('cacheSolve' does not call this)
                                        # 'm' replaced by 'inv'        
                inv <<- NULL            # resets inverse to NULL
                
        }
        get <- function() { x }		# gets object 'x'
        
        setinv <- function(solve) inv <<- inv
                # called by 'cacheSolve', stored by 'makeCacheMatrix'
        
        getinv <- function() inv	# cached inverse for successive
                                        # calls of 'cacheSolve'
        
        list(set = set, get = get,	# 'makeCacheMatrix' object method list
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by
## 'makeCacheMatrix' above. If the inverse has already been calculated (and the
## matrix has not changed), then 'cacheSolve' should retrieve the inverse from
## the cache
cacheSolve <- function(x) {		# 'x' = 'makeCacheMatrix' object
        
        inv <- x$getinv()		# gets 'inv' if 'inv'=!NULL
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)		# returns 'inv'
        }
        data <- x$get()			# gets object 'x'
        inv <- solve(data)		# calculates inverse if 'inv'=NULL
        x$setinv(inv)			# stores 'inv' in 'makeCacheMatrix'
        inv				# returns 'inv'
}