## These functions create a special "matrix" object that can cache its inverse
## and computes the inverse of the special "matrix"


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  #Declare parameter
  x.inverse <- NULL
  
  #Set the matrix
  set <- function(y){
    x <<- y
    x.inverse <- NULL
  }
  
  #Get the matrix
  get <- function(){
    x
  }
  
  #Set the inverse matrix
  setinverse <- function(inverse){
    x.inverse <<- inverse
  }
  
  #Get the inverse matrix
  getinverse <- function(){
    x.inverse
  }
  
  list(set = set ,get = get,
       setinverse = setinverse,
       getinberse = getinverse
       )
  
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

  #Get the cached inverse matrix
  x.inverse <- x$getinverse()
  
  if(!is.null(x.inverse)){
    
    message("getting cached data")
    return(x.inverse)
    
  }else{
    
    data <- x$get()
    
    #Calculate the inverse matrix
    x.inverse <- solve(data,...)
    
    #Set the inverse matrix to cache
    x$setinverse(x.inverse)
  
    # Return a matrix that is the inverse of 'x'
    return(x.inverse)
  }

}
