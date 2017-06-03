## Put comments here that give an overall description of what your
## functions do

## These functions written for Coursera Data Science: R Programming 
## Write a short comment describing this function

##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
##get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
##can cache its own object. 

##<<- operator is used to assign a value to an object in an environment that is different 
##from the current environment 

##take the matrix as an inpu

makeCacheMatrix <- function(x = matrix()) {
   invMatrix <- NULL
   
 #set the value of the Matrix
   setMatrix <- function(y) {
     x <<- y
     invMatrix <<- NULL
   }
   
   getMatrix <- function() x                              #get the value of the Matrix
   setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
   getInverse <- function() invMatrix                     #get the value of the invertible matrix
   list(setMatrix = setMatrix, getMatrix = getMatrix,
        setInverse = setInverse, getInverse = getInverse)
  
  }



## Write a short comment describing this function
## This function computes the inverse of the "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
   
 #get the value of the invertible matrix from the makeCacheMatrix function
           invMatrix <- x$getInverse()
         if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
           message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
           return(invMatrix)                             #return the invertible matrix
         }
           
 #if value of the invertible matrix is NULL then  
         MatrixData <- x$getMatrix()                     #get the original Matrix Data 
         invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
         x$setInverse(invMatrix)                         #set the invertible matrix 
         return(invMatrix)                               #return the invertible matrix
         ## Return a matrix that is the inverse of 'x'
  }