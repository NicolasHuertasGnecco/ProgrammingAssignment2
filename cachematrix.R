## The function will reduce the time it takes to calculate an invertibale matrix 
## it will baasiclly search if the matrix has been already inverted if it does it 
##will return the inverted matrix 

## the function makes the list to get the original matrix set de matrix 
## get the inverse or set de inverse

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inversa <<- inverse
        getinverse <- function() inversa
        list(set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse)
        
}


##The function checks if there has already been a calculation of the inverse and
## if there has been it returns the inverse if not it calculates it

cacheSolve <- function(x, ...) 
{
     
        inversa <- x$getinverse()
        if (!is.null(inversa)) {
                message("getting cached data")
                return(inversa)
        }
        print("recalculating")
        data <- x$get()
        inversa <- solve(data, ...)
        x$setinverse(inversa)
        inversa
        
}
