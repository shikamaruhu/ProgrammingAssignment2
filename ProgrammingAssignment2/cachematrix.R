## This solution is mainly based on the example provided in the description of the task
## The makeCacheMatrix will return a list of functions which will later be called by cacheSolve

## makeCacheMatrix is structured like makeVector and consists of four different functions

makeCacheMatrix <- function(x = matrix()) {
  
      ## This value is the return of the function getim, it will change when setim is called
        inv<-NULL
        
      ## set matrix (setm) allows the user to manually specify a new matrix, thus reseting inv
        setm<-function(y){
                x<<-y
                inv<<-NULL
        }
      
      ## get matrix (getm) stores the current matrix to be evaluated, and will be called if a new
      ## calculation is required
        getm<-function()x
      
      ## set inverse matrix (setim) will be called when the inverse matrix is calculated and
      ## it will assign its value to inv (it can also be used to set the inverse matrix manually)
        setim<-function(im)inv<<-im
      
      ## get inverse matrix (getim) stores the inverse matrix, and will be checked by cacheSolve
      ## before starting a new calculation
        getim<-function()inv
      
      ## This list (containing 4 functions) is the actual return of the makeCacheMatrix function
        list(setm=setm,getm=getm,setim=setim,getim=getim)

}


## This function will check if the inverse matrix is already stored in cache and will otherwise
## calculate it via the solve(x) function and the store it in cache

## mCMres stands for makeCacheMatrix result and should be a list such as the one provided by
## makeCacheMatrix

cacheSolve <- function(mCMres, ...) {
      ## Recovers the value of inv, by calling getim from makeCacheMatrix
        inv<-mCMres$getim
        
      ## Checks if the inverse matrix has already been calculated, and returns its value if so
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
     
      ## If it has not been done yet, it recovers the matrix by calling getm...
        matrix<-mCMres$getm()
      
      ## ... then it calculates the inverse matrix through the solve(x) function...
        inv<-solve(matrix)
      
      ## ... stores it in cache by calling setim...
        mCMres$setim(inv)
      
      ## ... and finally prints it.
        inv
}
