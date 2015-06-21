
## Objective of this assignment:

  #  Find Inverse of a Matrix. Cache the inverse and get it form cache if matrix is not changing, otherwise, compute it. 
  #Caching is beneficial as inversion of matrix is expensive operation. 

##Matrix inversion rule:

  # 1. Matrix should be square matrix
  # 2. Determinant of Matrix is non-zero

##Assumtpion:

  #Maxtix is always invertible matrix.(Always abide by above rules.)


##Function: makeCacheMatrix

  # This function performs following operations:
  #1. Set the value of matrix (This function may not be needed as vaule is already set by top function makeCacheMatrix. Created for completeness purpose)
  #2. Get the value of matrix
  #3. Set the value of inverse matrix
  #4. Get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)

}


##Function: cacheSolve
  # This function compute the inverse of the matrix.It first checks whether value is already computed. In this case, 
  # it returns result from the cache, otherwise, computes using solve() function and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
	    if(!is.null(inv)) {
	        message("getting cached data.")
	        return(inv)
	    }
	    data <- x$getmatrix()
	    inv <- solve(data)
	    x$setinverse(inv)
	    inv
}


####Examples###

#1.Creates 4x4 matrix using rnorm
 
   #> source("cachematrix.R")
   #> x <- matrix(rnorm(16), 4)
   #> mtrx <- makeCacheMatrix(x)
   #> mtrx$getmatrix()
   #          [,1]       [,2]       [,3]       [,4]
   #[1,] 0.2799293  1.5564607 -0.7368055  0.6340678
   #[2,] 1.6228798  0.2398194 -0.1911448 -0.9651605
   #[3,] 0.5740129 -0.6910296  1.0430930 -0.3478863
   #[4,] 0.3943108 -1.9732883 -0.6691680  0.4133718
   #> cacheSolve(mtrx)
   #          [,1]        [,2]       [,3]       [,4]
   #[1,] 0.5436545  0.25179413  0.5919102  0.2521348
   #[2,] 0.2455371  0.03656062 -0.0145917 -0.3035440
   #[3,] 0.1770231 -0.29610292  0.8949123 -0.2097476
   #[4,] 0.9400855 -0.54498905  0.8144157  0.3900707
   #> cacheSolve(mtrx)
   #getting cached data.
   #          [,1]        [,2]       [,3]       [,4]
   #[1,] 0.5436545  0.25179413  0.5919102  0.2521348
   #[2,] 0.2455371  0.03656062 -0.0145917 -0.3035440
   #[3,] 0.1770231 -0.29610292  0.8949123 -0.2097476
   #[4,] 0.9400855 -0.54498905  0.8144157  0.3900707

#2. Create static matrix and compute inverse 
   
   #> x <- rbind(c(.1,.2),c(.8,3))
   #> x
   #     [,1] [,2]
   #[1,]  0.1  0.2
   #[2,]  0.8  3.0
   #>  mtrx <- makeCacheMatrix(x)
   #> mtrx$getmatrix()
   #     [,1] [,2]
   #[1,]  0.1  0.2
   #[2,]  0.8  3.0
   #> cacheSolve(mtrx)
   #          [,1]       [,2]
   #[1,] 21.428571 -1.4285714
   #[2,] -5.714286  0.7142857
   #> cacheSolve(mtrx)
   #getting cached data.
   #          [,1]       [,2]
   #[1,] 21.428571 -1.4285714
   #[2,] -5.714286  0.7142857