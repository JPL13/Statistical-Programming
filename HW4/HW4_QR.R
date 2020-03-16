#########################################################
## Stat 202A - Homework 4
## Author:
## Date :
## Description: This script implements QR decomposition,
## linear regression, and eigen decomposition / PCA 
## based on QR.
#########################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to 
## double-check your work, but MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not use the function "setwd" anywhere
## in your code. If you do, I will be unable to grade your 
## work since R will attempt to change my working directory
## to one that does not exist.
#############################################################

##################################
## Function 1: QR decomposition ##
##################################

myQR <- function(A){
  
  ## Perform QR decomposition on the matrix A
  ## Input: 
  ## A, an n x m matrix
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
 
  n = dim(A)[1]
  m = dim(A)[2]
  R = matrix(A, ncol = m)
  #print(R)
  Q= diag(n)
  
  for( k in 1:(m)){
    #print(k)
    #browser()
    x <-rep(0, n)
    
    x[k:n]<- R[k:n,k]
    v<-x
    v[k]<- x[k] + sign(x[k])* norm(x, type="2")
    s<-norm(v, type="2")
    
    if(s!=0){
      u<- v/s
      R=R-2*(u %*% (t(u) %*% R))
      Q=Q-2*(u %*% (t(u) %*% Q))
     
    }
    
  }
  ## Function should output a list with Q.transpose and R
  ## Q is an orthogonal n x n matrix
  ## R is an upper triangular n x m matrix
  ## Q and R satisfy the equation: A = Q %*% R
  return(list("Q" = t(Q), "R" = R))
  
}

###############################################
## Function 2: Linear regression based on QR ##
###############################################

myLinearRegression <- function(X, Y){
  
  ## Perform the linear regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Do NOT simulate data in this function. n and p
  ## should be determined by X.
  ## Use myQR inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  
  n = dim(X)[1]
  p = dim(X)[2]
  X<-cbind(rep(1,n),X)
  decomp<-myQR(X)
  Q<-decomp[[1]]
  R<-decomp[[2]]
  
  Y_star<- t(Q) %*% Y 
  beta_hat<-rep(0, p+1)
  
  for(i in (p+1):1){
      if(i== p+1)
      beta_hat[i]<-(Y_star[i])/R[i,i]  
    else{
    beta_hat[i]<-(Y_star[i]-R[i, (i+1):(p+1)] %*% beta_hat[(i+1):(p+1)])/R[i,i]}
    
  }
  error<-sum(Y_star[(p+2):n]^2)
  #print(Y_star[(p+2):n])
  
  ## Function returns the 1 x (p + 1) vector beta_ls, 
  ## the least squares solution vector
  return(list(beta_hat=beta_hat, error=error))
  
}

# testing <- function(){
# 
#   ## This function is not graded; you can use it to
#   ## test out the 'myLinearRegression' function
# 
#   ## Define parameters
#    n    <- 100
#    p    <- 5
# 
#    ## Simulate data from our assumed model.
#    ## We can assume that the true intercept is 0
#    X    <- matrix(rnorm(n * p), nrow = n)
#    beta <- matrix(1:p, nrow = p)
#    Y    <- X %*% beta + rnorm(n)
# 
#    ## Save R's linear regression coefficients
#    R_coef  <- coef(lm(Y ~ X))
#    #print(R_coef)
#    print(aov((lm(Y~X))))
#    print(summary((lm(Y~X))))
# 
#    ## Save our linear regression coefficients
#    my_coef <- myLinearRegression(X, Y)
# 
#    #print(my_coef[[1]])
#    print(myLinearRegression(X, Y))
# 
#    ## Are these two vectors different?
#    sum_square_diff <- sum((R_coef - my_coef[[1]])^2)
#    if(sum_square_diff <= 0.001){
#      return('Both results are identical')
#    }else{
#      return('There seems to be a problem...')
#    }
# 
# }
