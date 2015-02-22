## The following two functions are reverse engineered from the example
## code (makeVector & cachemean). In order to test the functions, an
## inversable (solvable) matrix needs to be created, e.g. xx <- matrix (c(1:8, 10), 3, 3).

## The makeCacheMatrix function takes the freshly defined matrix (e.g. xx),
## clears out any previous values, and sets the stage for the next function
## (cacheSolve) to work. More details are embedded below each part of the code.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## Resets the "i" value, which is where the inversed matrix later gets saved
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## This is a function that can (when needed) change the initial matrix
        get <- function() x
        ## This is a function that can (when needed) return the initial matrix
        setinverse <- function(solve) i <<- solve
        ## This is a function that can (when needed) change the inversed matrix
        getinverse <- function() i
        ## This is a function that can (when needed) return the inversed matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## Returns a list of the previously described functions, so the next function can call them


## This is the "meat" of the assignement, the part where the inversed matrix actually gets
## calculated, gets saved (or cached) for future use, OR (importantly) recalled,
## so the calculation doesn't need to be repeated.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        ## This part fetches the current value of the inversed matrix
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If it exists (if it's not NULL), it simply returns it and stops execution. 
        ## If not, it continues.
        data <- x$get()
        ## This part fetches the initial matrix.
        i <- solve(data)
        ## And this part then calculates it's inverse and saves it to "i"
        x$setinverse(i)
        ## Which is then saved in cache by calling the subfunction defined above.
        i
        ## Returns the solution.
}