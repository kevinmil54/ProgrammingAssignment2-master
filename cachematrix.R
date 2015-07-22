## Put comments here that give an overall description of what your
## functions do

## There are two parts to this program

## makeCacheMatrix returns a list of functions that serve as input to
##
## cacheSolve(), which actually does the calculation



## Write a short comment describing this function

## makeCacheMatrix will return a list that contains functions to
##
##  set up the matrix
##  get the matrix
##  set up the inverse
##  get the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    ## get the value of the matrix
    
    get  <- function() x
    setinv  <-  function (inv) cache <<- inv
    getinv  <-  function() cache
    list(set=set, get = get, setinv=setinv, getinv=getinv)
    
}



## cacheSolve calculates the inverse of the matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    
    cache <- x$getinv()
    
    ## has the inverse already been calculated?
    
    if (!is.null(cache)){
        #if so, just copy it from the cache and return
        message("getting cached data")
        return(cache)
    }
    
    ## if not, create the matrix
    matrix <-x$get()
    
    ## see if you can invert it, return error if can't
    
    tryCatch ( {cache <- solve(matrix, ...)
    },
    error = function(e) {
        message("Error:")
        message(e)
        
        return (NA)
    },
    warning= function(e) {
        message("Warning:")
        message(e)
        
        return (NA)
    },
    
    finally = {
        ## if it is invertible, proceed to invert
        {x$setinv(cache)}
    })
    return(cache)
}

## solved with extensive help from Stackoverflow, and particularly from these examples:
## Bob Fridley: http://rstudio-pubs-static.s3.amazonaws.com/56438_c8c1b3a349d84e02996f91b394779b06.html
## sefakilic: https://github.com/sefakilic/coursera-rprog-assignment2/blob/master/cachematrix.R
