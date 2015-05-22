##  makeCacheMatrix will create a special matrix object and then cache it's inverse
##  cacheSolve will reutrn the inverse of a matrix.  If the matrix inverse
## already exists, it will return the object already stored in the cache.


## Write a short comment describing this function
##  makeInverseMatrix takes a matrix object will cache its inverse.
## the variable which holds the cached inverted matrix is actually defined 
## in the function cacheSolve, the value of the inverted matrix is stored there.
## Hence, the '<<-' operator is needed to assign its value.
makeInverseMatrix <- function(x = matrix()) {
  m <- NULL
  #while this is never called, it could be saved for future reference
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #note, get returns the value of x, the original code lacked curly braces
  # and for those coders not familar with this facet of R syntax, it can be confusing
  #I have added them back in.
  get <- function() {x}
  #solve is the name of a function, but in this case its serving as the name of a 
  #function variable (not the best of coding standards, but I'm leaving it here 
  #since it was part of the original assignment)
  setInvMatrix <- function(solve) {m <<- solve}
  #returns the value of m
  getInvMatrix <- function(){m} 
  #this function returns a list of other functions
  list(set = set, get = get,
       setInvMatrix= setInvMatrix,
       getInvMatrix = getInvMatrix)
}


##  cacheSolve
## takes the result of makeInverseMatrix
## if the matrix has already been inverted, it will returned the cached version,
## otherwise, it will cache the matrix and return the result.
cacheSolveMatrix <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInvMatrix(m)
  m
}

dodo<-matrix(1:4,2)
aa<-makeInverseMatrix(x = dodo)
bb<-cacheSolveMatrix(aa)
print(bb)
# > print(bb)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
bb<-cacheSolveMatrix(aa)
# getting cached data #note: this shows that the function worked to get the cached value
print(cacheSolveMatrix(aa))
# getting cached data #this shows the cache works and matrix is indeed inverted
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


##EXAMPLE FOR RUNNING
# > dodo<-matrix(1:4,2)
# > aa<-makeInverseMatrix(x = dodo)
# > bb<-cacheSolveMatrix(aa)
# > print(bb)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > bb<-cacheSolveMatrix(aa)
# getting cached data
# > print(cacheSolveMatrix(aa))
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5