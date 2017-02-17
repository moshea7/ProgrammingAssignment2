##############################
#### R Programming Week 3 ####
##############################

##############################
##### makeCache Function #####
##############################

#1.sets the value of the Matrix
#2.gets the value of the Matrix
#3.sets the value of the inverse
#4.gets the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

###############################
##### cacheSolve Function #####
###############################

#Calculates the inverse of the"Matrix" created with the above function. 
#it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. Otherwise, 
#it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

