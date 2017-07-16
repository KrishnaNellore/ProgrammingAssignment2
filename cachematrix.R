## **********   cachematrix documentation - Edited by Krishna Nellore (16th July, 2017)   ************* ##

## makeCacheMatrix and cacheSolve together serve as a cache for matrix inversion function "solve"       ##
## Once a matrix inverse is computed through these functions, it is stored in a special list of         ##
## functions, taking advantage of R's lexical scoping                                                   ##

## The functions are invoked as follows, assuming the inverse needs to be calculated for two matrices   ##
## a <- matrix(1:4,2,2) and b <- matrix(21:29,3,3)                                                      ##
##      # x <- makeCacheMatrix(a)  ------ For the creation of the function list and cache environment   ##
##      # cacheSolve(x)            ------ For calculating the inverse of a                              ##
##      # x$set(b)                 ------ For changing the input matrix subsequently                    ##

## **************************************************************************************************** ##

## makeCacheMatrix function takes a matrix as an input argument and returns a list of four functions    ##
## 1) setMatrix, 2) getMatrix, 3) setInverse, 4) getInverse. It also defines a variable matrix_inverse  ##
## in its environment.                                                                                  ##



makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL
        setMatrix <- function(y){
                x <<- y
                matrix_inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) matrix_inverse <<- inverse
        getInverse <- function() matrix_inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function takes as input the list that is an output of makeCacheMatrix function and        ##
## returns the inverse of the matrix stored in the variable "x" in makeCacheMatrix function environment.##

cacheSolve <- function(x, ...) {
        
        ## Check if inverse of matrix is already stored in cache and retrieve if stored
        matrix_inverse <- x$getInverse()
        if(!is.null(matrix_inverse)){
                message("Retrieving inverse from cache")
                return(matrix_inverse)
        }
        temp_matrix <- x$getMatrix()
        
        ## Check if matrix is invertible and square. Display error and return if it's not.
        if(round(det(temp_matrix),15)==0 | nrow(temp_matrix)!=ncol(temp_matrix)){
                message("Error: The input matrix is not invertible. Please enter a square matrix that is invertible as input using setMatrix function")
                return()
        } 
        
        ## Calculate the inverse and store inverse in cache if matrix is invertible and square
        else{
                matrix_inverse <- solve(temp_matrix, ...)
                x$setInverse(matrix_inverse)
                matrix_inverse
        }
}
