#include <Rcpp.h>
using namespace Rcpp;

/*
####################################################
## Stat 202A - Homework 6
## Author: 
## Date : 
## Description: This script implements QR and Sweep
####################################################
 
###########################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change your working directory
## anywhere inside of your code. If you do, I will be unable 
## to grade your work since R will attempt to change my 
## working directory to one that does not exist.
###########################################################
 
 */ 

// [[Rcpp::export]]
NumericMatrix mySweepC(const NumericMatrix A, int k){
    
    NumericMatrix B= clone(A);
  
  int n=A.nrow();
  
  for(int m=0; m<k; m++){
    
    for(int i=0; i<n; i++){
      for(int j=0; j<n; j++){
        if((i!=m)&&(j!=m))
        { B(i,j)=B(i,j)-B(i,m)*B(m,j)/B(m,m);}
      }
      
      if(i!=m)
        B(m,i)=B(m,i)/B(m,m);
      
      if(i!=m)
        B(i,m)=B(i,m)/B(m,m);
    }
    
    B(m,m)= -1/B(m,m);
  }
  
  
  // Return swept matrix B
  return(B);
  
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*

n    <- 100
p    <- 3

# ## Simulate data from our assumed model.
# ## We can assume that the true intercept is 0
X    <- matrix(rnorm(n * p), nrow = n)
  beta <- matrix(1:p, nrow = p)
  Y    <- X %*% beta + rnorm(n)
  
# ## Save R's linear regression coefficients
  R_coef  <- coef(lm(Y ~ X))
  print(R_coef)
# ## Save our linear regression coefficients
  Z<-cbind(rep(1,n), X, Y)
  newMat<-t(Z)%*%(Z)
  resC<-mySweepC(newMat, p+1)
  my_coef <- resC[0:p+1, p+2]
  print(my_coef)
# ## Are these two vectors different?
  sum_square_diff <- sum((R_coef - my_coef)^2)
  print(sum_square_diff)
  if(sum_square_diff <= 0.001){
    return('Both results are identical')
  }else{
    return('There seems to be a problem...')
  }
  
*/
