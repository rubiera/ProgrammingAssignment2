## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix generates a generic matrix
## with a generic inverse matrix set/get method

listMatrix = vector(mode = "list", length=itercache)
listMatrixInverse = vector(mode = "list", length=itercache)
listMatrixDone = array(dim = length(itercache))

itercache <- 1

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse )
}

## cacheSolve takes as an argument a call to makeCacheMatrix
## The call has the matrix to be inverted
## As matrices are called, they are entered in a list of vectors listMatrix
## A separate list stored as an array stores a 1 if this 
## matrix has already had its inverse calculated
## and a zero otherwise.

cacheSolve <- function(x) {
  listMatrix[[itercache]] <<- x$get()
  for (iclean in 1:itercache) { listMatrixDone[[iclean]] <<- 0 }
  
  if(itercache > 1) {  
    for (i in 1:(itercache-1)) {
      if ( is.matrix(listMatrix[[i]]) && is.matrix(x$get())  
           && ( dim(listMatrix[[i]]) == dim(x$get())) 
           && all ( listMatrix[[i]] == x$get() )  ) {
        listMatrixDone[[i]] <<- 1
      } 
    }
  }
  
  ##if the sum of the array listMatrixDone is zero, we have a new matrix
  ##for which we calculate the inverse
  
  if(sum(listMatrixDone) == 0) {
    m <- x$getinverse()
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
    
    print("m calculated")
    print(m)
    
    listMatrixInverse[[itercache]] <<- m
  }
  
  ##in this if loop, we compare the current matrix with all previous matrices
  ##if found, we store a '1' in listMatrixDone

  if(itercache > 1) { 
    for (j in 1:(itercache-1)) {
      if ( listMatrixDone[[j]] == 1 ) {
        message("getting cached data")
        m <<- listMatrixInverse[[j]]
        return(m)
      }
     } 
    }

  itercache <<- itercache + 1
}


##sample input matrices defined
##sampleMatrix1 <- matrix(rnorm(4),2,2)
##sampleMatrix2 <- matrix(rnorm(9),3,3)
##sampleMatrix3 <- matrix(rnorm(16),4,4)
##sampleMatrix4 <- matrix(rnorm(25),5,5)
##sampleMatrix5 <- matrix(rnorm(36),6,6)

##sample calls
##cacheSolve(makeCacheMatrix(sampleMatrix1))
##cacheSolve(makeCacheMatrix(sampleMatrix2))
##cacheSolve(makeCacheMatrix(sampleMatrix1)) 
##cacheSolve(makeCacheMatrix(sampleMatrix3))
##cacheSolve(makeCacheMatrix(sampleMatrix4))
##cacheSolve(makeCacheMatrix(sampleMatrix2))
##cacheSolve(makeCacheMatrix(sampleMatrix5))
##cacheSolve(makeCacheMatrix(sampleMatrix4))
         



