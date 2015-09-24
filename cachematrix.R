## ###########################################################################
##
## PURPOSE: Finding Inverse of a given matrix -- serving the solution from Cache 
## if available
##
## USAGE: if x is the matrix for which you would like to get an inverse, the
## the following is a suggested way to compute the inverse:
##  cacheSolve(makeCacheMatrix(x, ...))))
##
## TEST cases are included at the end of the script in comments.
## TEST Strategy: Basic idea behind testing method is to multiply the input
## matrix with its inverse and verify it if yields an Identity matrix
## where primary diagonal elements are 1's and all other elements are zero or
## very close to zeros (due to precision involved in "digital" representation
## involved in computations) 
##
## Program author: Gopal G
## Modification History
## 23-Sep-2015   Original creation by forking a template from instructor/GitHub
##
## Credits: Template for this program as well as a analogous solution for
## cached mean was provided by the instructors
##
## SOLUTION APPROACH: Modeling solution to "mimic" a cached mean solution.
## 
## As part of the assignment, a solution for finding a mean of a vector
## and first checking to see if it was already computed earlier and serving
## the same, and if not available in cache, it will be computed. Same approach 
## is followed to find inverse of a matrix.
##
## Two functions in this program as detailed below:
## makeCacheMatrix creates a list of set and get functions for values
## of the matri and its inverse in the form of a List
## cacheSolve invokes thse functions and gets the Inverse of matrix if 
## exists, and invoke R's solve() to compute, if it does not exist.

## function makeCacheMatrix
##
## Build simple List of functions for setting and getting 
## Matrix and its inverse
## which will be used in the next function.

makeCacheMatrix<- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## function cacheSolve
##
## First check if the inverse of the matrix already exists in the environment
##   If Inverse already exists (that is inverse i is not null) return it
##   no need for computing it.
##  
##   if the inverse does not exist, invoke R's "solve" function to compute
##   the inverse of the matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}





## TEST CASES
## (1) Identity Matrix
##     Inverse of Identify Matrix should yield Identity Matrix
##     You can create an identify matrix using "diag (n)" where n is the size
##        x<- diag(7). Uncomment the following lines (You can use Cont-Shift-C to
##        toggle comment/ no comments for selected lines)
##
#     x<- diag(7)
#     print (cacheSolve(makeCacheMatrix(x)))
#
##  (2) Simple matrix (See: http://www.mathwords.com/i/inverse_of_a_matrix.htm)
##        x <- matrix(c(4,3,3,2), 2,2). Its inverse is: matrix(c(-2,3,3,-4),2,2)
##        Uncomment the following lines
##
#      x <- matrix(c(4,3,3,2), 2,2)
#      print (cacheSolve(makeCacheMatrix(x)))
##
##  (3) A little more realistic case of matrix of arbitrary random real numbers
##        Uncomment the following lines.One way to easily verify that the inverse
##        matrix generated ir right is to compute x multiplied by inverse 
##        produces Identify matrix with 1's on primary diagonal and zeros 
##        on all other positions. The following test attempts at this.
##        result should be an identify matrix. Notice carefully that non-diagonal
##        elements are very close to zero (large negative exponent of e)
##
#      x <- matrix(rnorm(100), 10,10)
#      print (x %*% (cacheSolve(makeCacheMatrix(x))))

############################################################################


