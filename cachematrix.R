## If we have big quantity of data, for example big matrix and we wont to calculate its inverse matrix (expensive operation)
## we can do that once and that calculation use more times without recalculating. Here, I put an example which explain 
## the idea of using cachig by two function (makeCacheMatrix() and casheSolve() below. Here is used function solve() and 
## we suppose that our matrix is invertible 

## m <- makeCacheMatrix()
## a <- matrix(c(1,2,0,3,-2,0,-1,2,3),3,3)
## m$set(a)
## m$get()
##     [,1] [,2] [,3]
## [1,]    1    3   -1
## [2,]    2   -2    2
## [3,]    0    0    3
## cacheSolve(m)
##     [,1]   [,2]       [,3]
##[1,] 0.25  0.375 -0.1666667
## [2,] 0.25 -0.125  0.1666667
## [3,] 0.00  0.000  0.3333333
##  m$setInverse()
## m$getInverse()
##     [,1]   [,2]       [,3]
## [1,] 0.25  0.375 -0.1666667
## [2,] 0.25 -0.125  0.1666667
## [3,] 0.00  0.000  0.3333333
## cacheSolve(m)
## Cahed data
##     [,1]   [,2]       [,3]
## [1,] 0.25  0.375 -0.1666667
## [2,] 0.25 -0.125  0.1666667
## [3,] 0.00  0.000  0.3333333


## Function makeCacheMatrix() is a constructor for a matrix (get, set functions and getInverse, setInverse functions 
## for get and set Inverse matrix
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function() m <<- solve(get())
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



##Function cacheSolve() return inverse matrix on two ways: 
##1) If it is already in cache then it return it from cache
##2) If it is not in cache it calculate it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
	
	if(!is.null(m)){
		message("Cahed data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setInverse()
	m
}
