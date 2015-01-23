## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function takes the matrix that the user gives, and assigns the different functions the user can do 
#with the matrix (all the functions listed in the list at the end of the function)

makeCacheMatrix <- function(x = matrix()) { #this is the input that the user will put in as a square matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x #this gets the matrix and would output the matrix to be used in the next step
  setinv <- function(solve) m <<- solve #this sets the inverse of the matrix and sets the answer
                                        #an m variable, which is solved in the next function
  getinv <- function() m #this prints the m variable matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) #these are the lists of the functions that can be 'solved' in the 
                        #makeCacheMatrix function
}


## Write a short comment describing this function
#This solves the inverse of the matrix, unless it has already been solved then it will solve for the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() #this is the getinv function created from the previous function, and assigns it to 'm'
  if(!is.null(m)) { #if the m value is not null (has been solved) then it will print the inverse
    message("getting cached data")
    return(m)
  }
  data <- x$get() #if the inverse has not been solved here it will solve the inverse and print in the console.
  m <- solve(data, ...)
  x$setinv(m)
  m
}
