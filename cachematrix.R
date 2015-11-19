## R Programming - Week 3 - Assignment 2
## Guilherme Vaccaro


## This file contains two functions for handling cumbersome square matrices. 
## makeCacheMatrix(x = matrix()) : creates an object for storing the matrix, 
##                                 its inverse and its determinant
## cacheSolve(x,...) : calculates or retrieves the inverse of the matrix
##                     stored in such object. Also checks if the matrix is
##                     invertible


## This function creates an object with attributes x, xInv and xDet
## x is a square matrix provided by the user
## xInv caches the inverse of x
## xDet caches the determinant of x
## The associated calculations are performed by the function
## cacheSolve(x, ...)

makeCacheMatrix <- function(x = matrix()) {

    ## Attributes
    xInv <- NULL  # The inverse of x
    xDet <- NULL  # The determinant of x
    ## Getters and Setters
    get <- function() {
        # Getter for the matrix x
        x
    }
    set <- function(y) {
        # Setter for the matrix x
        x <<- y
        xInv <<- NULL
        xDet <<- NULL
    }
    getInv <- function() {
        # Getter for the inverse of x
        xInv
    }
    setInv <- function(yInv) {
        # Setter for the inverse of x
        xInv <<- yInv
    }
    getDet <- function() {
        # Getter for the determinant of x
        xDet
    }
    setDet <- function(yDet) {
        # Setter for the determinant of x
        xDet <<- yDet
    }
    ## Create object
    list(get = get, set = set, getInv = getInv, setInv = setInv, getDet = getDet, setDet = setDet)
}


## This function calculates and caches the inverse and the determinant of an object 
## created with makeCacheMatrix(x = matrix())
## The determinant is used for checking the invertibility of the matrix. If necessary
## it is calculated and cached
## If the marix is invertible and its inverse was previously calculated, it retrieves
## the inverse previously cached
## If the matrix is not invertible, it returns NULL

cacheSolve <- function(x, ...) {
    
    # Get the matrix as data
    data <- x$get()
    # Check the matrix's determinant for invertibility
    xDet <- x$getDet()
    if( is.null(xDet) ) {
        # If the determinant is unknown, calculate and cache the determinant
        message("Verifying if the matrix is invertible...")
        xDet <- det(data)
        x$setDet(xDet)
        message(paste("Determinant = ",xDet))
    }
    if( xDet != 0 ) {
        # If the the matrix is invertible, check for a prevously cached inverse
        xInv <- x$getInv()
        if( !is.null(xInv) ) {
            # If the inverse is known, sinalize the use of cached data
            message("Getting cached inverse of x")
        } else {
            # If the inverse is unknown, calculate and cache the inverse
            message("Calculating the inverse matrix...")
            xInv <- solve(data)
            x$setInv(xInv)
        }
        ## Return a matrix that is the inverse of 'x'
        xInv
    } else {
        # If the determinant is zero, the matrix is not invertible
        message("This matrix is not invertible")
        return()
    }
    
}
