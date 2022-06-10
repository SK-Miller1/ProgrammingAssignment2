## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix is a function to: set the value of the matrix, get the value 
##of the matrix, set the inverse of the matrix and to retrieve the inverse of 
##the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(m){
    x <<- m
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(a){inv <<- a}
  
  getInv <- function() inv
  
  list(set = set, 
       get = get,
       setInv = setInv, 
       getInv = getInv)

}


## Write a short comment describing this function

#cacheSolve takes a makeCacheMatrix object and returns the inverse of the 
## associated matrix.
#If the inverse is cached, cacheSolve retrieves and returns it; otherwise, it 
##calculates the inverse, caches it before returning the inverse to the user.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  
  if(!is.null(inv)){
    message('Getting Cached inverse...')
    return(inv)
  }else{
    message('Inverse is not cached! Calculating inverse...')
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
  }
  
  
}


### TESTING
set.seed(1)
m <- matrix(sample(0:10, 9), nrow = 3, ncol = 3)
m


## Instantiate an instance of makeCacheMatrix
M <- makeCacheMatrix(m)

##Get Inverse of M(an object of makeCacheMatrix)
## Since this is the first time cacheSolve is called for object M, the inverse
## is not stored (i.e. M$getInv() would yield NULL). cacheSolve(M) calculates and 
## returns the inverse
cacheSolve(M) # (1)


## Running `cachSolve(M)` a second time yields:
cacheSolve(M)

## The cached inverse was retrieved since inverse was already calculated in (1) above.
