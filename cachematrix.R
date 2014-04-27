## This function takes a matrix and then prints out its inverse.




## Input of this function is a matrix, then function initiates the matrix variables

## and its inverse variables! Output is a list containing input and output variables 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Input of this function is a list. Function checks if the inverse of a matrix(defined in list) is defined; 

## if it "IS" defiened, it prints out the inverse matrix from cache, 

## if it is "NOT" defined, it calculates the inverse and then prints it out. 

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv

}
