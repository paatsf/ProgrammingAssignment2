## These functions will calculate the inverse of a matrix
## And also will cache the inverse of the matrix, so it is not necessary to 
## calculate it everytime

## This function will store several functions and return them to the parent
# environment

makeCacheMatrix <- function(x = matrix()) { # defining x as the matrix
        m <- NULL # m is set to NULL, because its value will be calculed later on the function
        set <- function(y) { # defining how the data will be changed
                x <<- y # assigning the y to the x on the parent environment
                m <<- NULL # assigning m to the object m on the parent environment
        }
        get <- function() x  # will acess data of the object
        setinverse <- function(inverse) m <<- inverse # defining how data will be changed
        getinverse <- function() m # will acess data of the object
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse) # this list gives the names for the functions that were defined
}


## This function wull cache in its memory the inverse for a matrix
## so, when this value is required, it will not be necessary to recalculate it

cacheSolve <- function(x, ...) {
        m <- x$getinverse() # here the function will get the inverse stated on the previous function
        
        if(!is.null(m)){ # will check if the value is null or not
                message("getting cached data") # if the value is not null, it will have stored the data on cache
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) # here the value stored will have its inverse calculates
        x$setinverse(m) # setting the inverse on the input object
        m # returning the object to the parent environment
}
