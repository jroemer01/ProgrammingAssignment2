## This function takes a matrix and calculates the inverse of the matrix which is the mathamatical
## mirror of the matrix when used in multiplication that would make the origial matrix equal to one
## THis function leverages the ability to set variables in the parent function to cache a result so if
## function is run more than once in the same session it grabs the answer from memory
## for more about matrix inversion read info at http://www.purplemath.com/modules/mtrxinvr.htm


## This function creates a special matrix object the has sub functions of setters and getters that
## can be called using the object reference notation

makeCacheMatrix <- function(x = matrix()) {
  #print("test")
  i<- NULL
  set <-function(y){
    x <<- y
    i <<- NULL
  }
  get<-function() x
  setInverse<-function(solve) i <<-solve
  getInverse<-function() i
  list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
}


## This function looks to solve th inverse.  It first checks if the matrix passed in already has
## a solution and if it does it returns that answer, (i), and if not then it calculates it,
## sets the value in the object, and then returns the answer
## solve() is the R function to get the Matrix inverse
## Test example used:
## c1<-c(1,1,1)
## c2<-c(3,4,3)
## c3<-c(3,3,4)
## my _matrix<-cbind(c1,c2,c3)
## my_cachce_matrix<-makeCacheMatrix(my_matrix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse()
  if (!is.null(i)){
    print("cache version")
    return(i)
  }
  #Function to calculate the inverse of the matrix 
  data<-x$get()
  i<-solve(data)
  x$setInverse(i)
  print("no cache")
  i
}
