
##Caching the inverse of a matrix,
##using the <<- operator to assign a value to a different environment

## Creating a special "matrix", which is a list containing a function to do:
##set the values of the matrix
##get the values of the matrix


makeCacheMatrix <- function(x = matrix()) { 
        i <- NULL 
        set <- function(y) { 
                x <<- y 
                i <<- NULL 
        } 
        get <- function() x 
        setinverse <- function(inv) i <<- inv 
        getinverse <- function() i 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
} 

## Calculate the inverse matrix 
## cached result if it is available 

cacheSolve <- function(x, ...) { 
        i <- x$getinverse() 
        if(!is.null(i)) { 
                message("getting cached data") 
                return(i) 
        } 
        m <- x$get() 
        i <- solve(m, ...) 
        x$setinverse(i) 
        i 
} 

## Testing:
## testmatrix <- matrix(c(10, 0, 0, 20), c(2, 2))
## m <- makeCacheMatrix(testmatrix) 
## cacheSolve(m) 
## result:
##     [,1] [,2]
##[1,]  0.1 0.00
##[2,]  0.0 0.05