## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL                       # create an empty matrix with the name inver (inversion)
    
    set <- function(y) {                # set the "set inver" function
        x <<- y                         # the x is going to be used as input for the get to verify if it
        inver <<- NULL                  # has been calculated already
    }
    
    get <- function() x                  
    
    setinver <- function(inverse) inver <<- inverse   # calculate the inverse and set the matrix 
    getinver <- function() inver        # put the inverse in the cache
    
    list(set = set, get = get, setinver = setinver, getinver = getinver)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inver <- x$getinver()                    # check if inverse has been caluclated
    
    if(!is.null(inver)) {                    # if already calculated, use the cached inversion
        message("getting cached data")
        return(inver)
    }
    
    data <- x$get()                         # put the matrix into data 
    inver <- solve(data, ...)             # and calculate the invers e matrix
    x$setinver(inver)                        # store the inverse matrix in inver (makeCacheMatrix fucntion)
    inver                                    # print inver matrix on screen
    

}
