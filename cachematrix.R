## Put comments here that give an overall description of what your
## functions do

##The first function  makeCacheMatrix will create a special "matrix"
## The function stores a list of functions that will be used in the cacheSolve function to 
##find the inverse of a given matrix. 
##If it is the first time this calculation is made 
## the functions will solve it and store the result in the cache memory
## if the inverse of the matrix was calculated before, the result will be retrieved from 
##cache memory instead. Making the process more efficient. 


## Write a short comment describing this function
##  makeCacheMatrix is a function that creates a list object containing 4 functions that make use of the special matrix
#created in the first place. 
## set = sets the values of the matrix 
## get = gets the value of the matrix
## setinverse = sets the values of the inverse matrix 
## getinverse = gets the values of the inverse matrix
## This function stores values in the cache memory and allows to retrieve information stored at it. 

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL 
        set <- function(y) {
                x<<- y
                i<<- NULL
                }
        get<- function()x 
        setinverse<- function(inv) i <- inv
        getinverse<- function() i 
         list(   set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse    ) 
}


## Write a short comment describing this function
## cacheSolve is the function that calculates the inverse matrix of a given matrix. 
## It first, asks to find if this calculation have already been done: 
        ## if not: it calculates the inverse of the square matrix imputed and then 
                ## stores the result in the cache memory using the functions defined in makeCacheMatrix 
        ## if yes: it retrieves the values of the inverse matrix of the given matrix stored in the cache memory 
                ## using the functions defined in makeCacheMatrix 
                

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

}
