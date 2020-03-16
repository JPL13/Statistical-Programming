#########################################################
## Stat 202A - Homework 5
## Author: 
## Date : 
## Description: This script implements logistic regression
## using iterated reweighted least squares using the code 
## we have written for linear regression based on QR 
## decomposition
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

  ## Function returns the 1 x (p + 1) vector beta_ls, 
  ## the least squares solution vector
  return(list(beta_hat=beta_hat, error=error))
  
}

##################################################
## Function 3: Eigen decomposition based on QR  ##
##################################################
myEigen_QR <- function(A, numIter = 1000) {
  
  ## Perform PCA on matrix A using your QR function, myQR or Rcpp myQRC.
  ## Input:
  ## A: Square matrix
  ## numIter: Number of iterations
  
  ########################
  ## FILL IN CODE BELOW ##
  ######################## 
  n = dim(A)[1]
  A_copy=matrix(A, ncol = n)
  V=matrix( rnorm(n*n), n, n)
  
  for(i in 1:numIter){
    Q= myQR(V)[[1]]
    V= A_copy %*% Q
    i=i+1
  }
  
  Q=myQR(V)[[1]]
  R=myQR(V)[[2]]
  ## Function should output a list with D and V
  ## D is a vector of eigenvalues of A
  ## V is the matrix of eigenvectors of A (in the 
  ## same order as the eigenvalues in D.)
  
  return(list("D" = diag(R), "V" = Q))
}

###################################################
## Function 4: PCA based on Eigen decomposition  ##
###################################################
myPCA <- function(X) {
  
  ## Perform PCA on matrix A using your eigen decomposition.
  ## Input:
  ## X: Input Matrix with dimension n * p
  n = dim(X)[1]
  p = dim(X)[2]
  
  li1=myEigen_QR(var(X))
  
  Q=li1[[2]]
  
  
  Z =X %*% Q
  
  ## Output : 
  ## Q : basis matrix, p * p which is the basis system.
  ## Z : data matrix with dimension n * p based on the basis Q.
  ## It should match X = Z %*% Q.T. Please follow lecture notes.
 # print(Z %*% t(Q))
  
  return(list("Q" = Q , "Z" = Z ))
}
