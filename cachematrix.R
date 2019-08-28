## makeCacheMatrix is providing the functions needed to check if a matrix inverse 
## was already created, and saving the inverse.
## cacheSolve checks using the functions of makeCacheMatrix, if the matrix inverse was already created,
## and if not it calculates it, if yes it gets it from the cache. 


## makeCacheMatrix is giving a list of functions to
## set the values of the matrix (set)
## get the values of the matrix (get)
## set the values of the inverse matrix (setsolve)
## get the values of the inverse matrix (getsolve)

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## The CacheSolve function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated (if(!is.null(m))).
## If so, it gets the inverse from the cache and skips the computation (return(m)). 
## Otherwise, it calculates the inverse of the data (data <- x$get()       m <- solve(data, ...)) 
## and sets the value of the inverse in the cache via the setsolve function (x$setsolve(m))

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
