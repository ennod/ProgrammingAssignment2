##Functions to compute the inverses of a matrices. Computed inverses are cached so that subsequent function calls 
##for the same matrix can retrieve cached data rather than recomputing.

## create object to store matrix values and subsequent matrix inverse values created by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    # variable to hold solve() output in cacheSolve
        get <- function() x    # get first contents of matrix for use in solve() in cacheSolve
        setinverse <- function(solve) m <<- solve    # cacheSolve calls this to set cached inverse on first run
        getinverse <- function() m    # cacheSolve calls this to retrieve cached inverse on subsequent runs
        list(get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## calculate or get cached inverse values for matrix values cached by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()    # assign getinverse value to m, this may be null if cacheSolve not yet run
        if(!is.null(m)) {    # if not null return m and indicate cached data being used
                message("getting cached data") 
                return(m)
        }
        data <- x$get()    # otherwise if m is null assign contents of get matrix to data variable
        m <- solve(data, ...)    # calculate inverse of data and assign to m
        x$setinverse(m)    # assign value of setinverse to be m
        m   # print m
}
