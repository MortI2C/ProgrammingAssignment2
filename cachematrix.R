## Returns getters and setters with for the given matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # If we set a new matrix, the inverse is not cached so we nullify it
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # The getters are just returning the given matrix or stored inverse
  # The inverse set just stores the new inverse in the makecachematrix scope inverse variable
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  # Returns the list with the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Gets an invertible matrix, and returns its cached inverse or computes it
#The first parameter is the matrix, and the second a cache that should be provided
cacheSolve <- function(x, cache = makeCacheMatrix(x)) {
  if(is.null(cache))
    cache <- makeCacheMatrix(x)
  
  ## Return a matrix that is the inverse of 'x'
  i <- cache$getinverse()
  ## check if its inverse is cached
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #it is not cached, calculate it with solve()
  data <- cache$get()
   i <- solve(data)
  #cache the inverse
  cache$setinverse(i)
  #return the inverse
  i
}
