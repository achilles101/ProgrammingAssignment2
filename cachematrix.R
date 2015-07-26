## Programming Assignment: Caching the Inverse of a Matrix
## Two functions are written to cache the inverse of a Matrix

##     This assignment will use and take advantage of the scoping 
##     rules of the R language and how they can be manipulated to preserve state 
##     inside of an R object for repetitive computations
##     By caching, the contents can be made use of again in case the values are not changing



## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #Setting the cache object to hold the inverse
  cacheMatrixInverse <- NULL
  
  #Function for setting the  matrix object
  set <- function(y) {
    x<<- y
    cacheMatrixInverse <<-NULL
  }
  
  #Function to retrieve the matrix
  get <- function() {
    x
  }
   
  #Setting the Inverse of the matrix
  setMatrixInverse <- function(inverse) {
    cacheMatrixInverse <<- inverse
  }
  
  
  #Retrieving the Inverse of the matrix
  getMatrixInverse <- function() {
    cacheMatrixInverse
  }
  
  #List of the functions
  list(set = set, 
       get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##   If the inverse has already been calculated (and the matrix has not changed), 
##   then the cachesolve should retrieve the inverse from the cache.
##   cacheSolve - Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
  inverseMatrix <- x$getMatrixInverse()
  
  #Check if the inverse matrix object is null and if it is already set, return the inverse matrix object
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  #Retrieve the data from x
  data <- x$get()
  
  #Calculate the Inverse of the matrix
  inverseMatrix <- solve(data) %*% data
  
  #Set the object to the calculated inverse   
  x$setMatrixInverse(inverseMatrix)
  
  #Return the matrix object
  inverseMatrix
  
}
