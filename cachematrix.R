# The following function will used to caluclate inverse of matrix
# and store result in cache so that when need to calculate inverse again
# it just fetch that from cache
# this will help to save repeatative calculations


# This function define a special "matrix" object, which is really a list 
# containing a function for
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                # parent environment
                # re-initialize m in the parent environment to null
                m <<- NULL 
        }
        # Get function return x
        get <- function() x 
    
        # set the cache m equal to inverse of mat
        setinverse <- function(inverse) m <<- inverse 
    
        #return the cached inverse of x
        getinverse <- function() m 
        
        # return
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



# This function first check if we have cached version of inverse matrix
# if found then return that matrix else
# it will calculate inverse and save in cache and return inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                # inverse found in cache so returning this inverse
                message("getting cached data")
                return(m)
        }
        data <- x$get()
    
        m <- solve(data, ...)
        # adding inverse data in cache 
        x$setinverse(m)
    
        # return
        m
}

