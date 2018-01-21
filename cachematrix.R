## These functions work together to return the inverse of a square, invertible matrix. The functions may reduce the computational-load 
## of the solve() function by storing a cached version of the result in the makeCacheMatrix closure, which can be retrieved if 
## cacheSolve function is asked to invert the same matrix more than once.
## To Use: pass the matrix to invert as an argument to makeCacheMatrix and store the result in a new object.
## Calculate the inverse of the matrix by calling cacheSolve() with the object returned by the makeCacheMatrix function as the argument
## To calculate the inverse of a new matrix, either create a new object as above or pass a new matrix to the existing object using the 
## syntax: object$set(new.matrix)


makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix takes a square invertible matrix as an argument and returns an object containing 1: the matrix in "x", 2: a cached copy of the
## inverted matrix in "m" (once it has been calculated) and 3: a set of 4 functions that store or fetch "those"x" and "m" from the parent closure. 
## lexical scoping allow the four functions to access the x and m variables, which are stored in the  
## makeCacheMatrix closure
   m <- NULL  # sets the cached matrix variable to null when a new instance of the makeCacheMatrix is defined
  set <- function(y) {
    x <<- y  # stores the incoming matrix in "x" stored in the makeCacheMatrix closure
    m <<- NULL  # when a new matrix is added, delete the cached inverted matrix stored in the makeCacheMatrix closure
  }
  get <- function() x  # returns the matrix stored in the closure
  setInverse <- function(set_cache) m <<- set_cache # adds the newly inverted matrix (calculated in solveMatrix) to the cache "m" in the makeCacheMatrix closure
  getInverse <- function() m  # returns the cached inverted matrix
  list(set = set, get = get, # returns the functions defined above as a list with named elements that correspond to the function names
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(z, ...) {
  ## cacheSolve takes the object defined in the makeCacheMatrix containing a matrix "x" as an argument and returns the inverse.
  ## cacheSolve will return a cached result if one has been calculated previously, thus eliminating the need to recalculate. 
  ## If no cached version is available the solve() function is used to calculate the inverse then the result is both stored in the cache and returned
  cached.matrix <- z$getInverse() # fetches the cached matrix stored in the makeCacheMatrix closure
  if(!is.null(cached.matrix)) {  # return the value of the cached matrix if it exists
    message("getting cached matrix")
    return(cached.matrix)
  }
  data <- z$get() # if no cached matrix exists fetch the matrix "x" 
  cached.matrix <- solve(data, ...) # calculate the inverse of matrix "x"
  z$setInverse(cached.matrix) # store the inverted matrix in the "m" variable stored in the makeCacheMatrix object
  cached.matrix 
}

fxn.test <- function() {
  the.input <- matrix(c(2, 2, 3, 2), nrow = 2)
  mcm <- makeCacheMatrix(the.input)
  mcm$set(matrix(c(-1, 1, 1.5, -1), nrow = 2))
  invm <- cacheSolve(mcm)
  invm <- cacheSolve(mcm)
  print(invm)
  mcm <- makeCacheMatrix(invm)
  invm <- cacheSolve(mcm)
  invm <- cacheSolve(mcm)
  print(invm)
}
