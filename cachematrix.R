## Functions that cache the inverse of a matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix(NA, nrow=dim(x)[1], ncol = dim(x)[2]) #Initiate inv as a matrix filled with NA values
        set <- function(y) {
                x <<- y #Change stored value of the matrix using new value contained in y
                inv <<- matrix(NA, nrow=dim(x)[1], ncol = dim(x)[2]) #reset value of the inverse matrix
        
        }
        get <- function() x #gets the matrix stored in x
        setinv <- function(inverse) inv <<- inverse #Change stored value of the inverse matrix
        getinv <- function() inv #gets stored values for the inverse matrix
        list(set = set, get = get,  
             setinv = setinv,        
             getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()                       
        if (!any(is.na(inv))) {                      #Evaluates matrix values, if none of them are NA, it returns the stored inverse matrix.
                message("getting inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)     #On the other hand, if at least one is NA, it calculates the inverse matrix and store the result in inv.
        x$setinv(inv)
        inv
}
