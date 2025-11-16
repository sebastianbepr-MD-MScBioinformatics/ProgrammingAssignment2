#We are going to apply makeVector/cacheman examples for our inverted matrix = makeCacheMatrix

#First step is to define our vector: 
makeCacheMatrix <- function(x = matrix()) {
#now we are going to start an object in 0   
  inverse_var<-NULL
#now we stablish the function that will state a new maxtrix y
  set<-function(y) {
    x<<-y
    inverse_var<<-NULL
  }

#now we define a new function for getting new x
get<-function() {
  x
}

#now we will state the inverse de funcion and printing it
setinverse<-function(inv) {
  inverse_var <<- inv 
}
getinverse<-function() {
  inverse_var
}
  #print final results
  list(set=set, 
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
  }

# this funcion stablishes matrixes in the enviroment
# now with cacheSolve we will operate the previous function
cacheSolve <- function(x, ...) {
    inv<-x$getinverse()
#in this point we will check if there is already a inv established
    if(!is.null(inv)){
      message("getting cached data")
#if there is already a value saved it will be returned
      return(inv)
    }
#if there is no previous value, we will get a new matrix and solve it
    data<-x$get()
    inv<-solve(data,...)
#now we must save the new inv value
    x$setinverse(inv)
#finaly we print the result
    inv
    }
