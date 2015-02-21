## This program is able to cache the inverse of a given matrix, thus if needed it can look it up
## in the cache instead of recomputing it again 

## makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL   #sets inv (used for the cached inverse) to NULL
        set <- function(y) {  #creates a function that sets y to x and inv to NULL
              x <<- y
              inv <<- NULL
        }
        get <- function() x #creates a function that returns the matrix x
        set_inverse <- function(inverse) inv <<- inverse #creates a function that saves the inverse of the matrix to inv
        get_inverse <- function() inv #creates a function that returns the inverse
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse) #makeCacheMatrix
        #returns a list containing all the functions defined above

}


## cacheSolve calculates the inverse of the matrix returned from function makeCacheMatrix
## If the inverse of the matrix has been calculated and not changed, this function gets the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse() #inv gets the value of inv from the above function
        if(!is.null(inv)) { #if the inverse has computed before inv isn't NULL
              message("getting cached data") # and prints the message 
              return(inv) # and returns the inverse of the matrix
        } # else if the inverse hasn't computed before
        matr <- x$get() # function get() is called and the matrix x is assigned to matr
        inv <- solve(matr, ...) # and then solve() calculates the inverse of matr and saves it to inv
        x$set_inverse(inv) # and sets the inverse value to the inv of the above function by calling set_inverse
        inv #finally the inverse of the matrix given is returned
}

# functions calls are grouped together
main <-function(m){
  my_matrix<- makeCacheMatrix(m)
  return(cacheSolve(my_matrix))
}
m<-matrix(2:5,nrow=2,ncol=2) #my test matrix
main(m)
