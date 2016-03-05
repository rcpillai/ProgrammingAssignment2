## makeCacheMatrix is a function that returns a list of functions
## The following are the functions:
## setCacheMatrix      set the value of a matrix
## getCacheMatrix      get the value of a matrix
## setInverseMatrix   	get the cached value 
## getInverseMatrix   	get the cached value 

  makeCacheMatrix <- function(a = matrix()) {
  ## Define a null variable
    cacheinvmatrix <- NULL
    ## set a matrix
    setCacheMatrix <- function(b) {
      ## assign to a variable
      a <<- b
      ## cachedinvmatrix nullified because matrix got a new value
      cacheinvmatrix <<- NULL
      }
    ## function to get a matrix
    getCacheMatrix <- function(){ 
    a
    }
  ## function to set Inverse Matrix
  setInverseMatrix <- function(inverse){
      cacheinvmatrix <<- inverse
    }
  ## function to get Inverse Matrix
  getInverseMatrix <- function(){
      cacheinvmatrix
    }
  ## returns a list, each argument is a function
  list(setCacheMatrix = setCacheMatrix,
       getCacheMatrix = getCacheMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  }

  ## This function computes the inverse of the special "matrix" 
  ## returned by makeCacheMatrix above

  cacheSolve <- function(a, ...) {
  ## get the cached inverse matrix
    cacheinvmatrix <- a$getInverseMatrix()
    ## check matrix
    if (!is.null(cacheinvmatrix)) {
      message("getting cached data")
      return(cacheinvmatrix)
    }
  ## get data
  matrixData <- a$getCacheMatrix()
  ## calculate 
  cacheinvmatrix <- solve(matrixData, ...)
  ## set the matrix 
  a$setInverseMatrix(cacheinvmatrix)
  ## display 
  cacheinvmatrix
}
