#############################################################
## Stat 202A - Homework 7
## Author:
## Date :
## Description: This script implements the lasso
#############################################################

# 1) Write R code using the included script 'Lasso.R' for computing the Lasso solution path using coordinate descent. Please include but do not penalize the intercept term (as we did for ridge regression). 

# 2) Use epsilon-boosting technique. Compare the difference.

# 2) For Lasso, plot the estimation error over the different values of lambda.

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

#####################################
## Function 1: Lasso solution path ##
#####################################

myLasso <- function(X, Y, lambda_all){

  # Find the lasso solution path for various values of
  # the regularization parameter lambda.
  #
  # X: n x p matrix of explanatory variables.
  # Y: n dimensional response vector
  # lambda_all: Vector of regularization parameters. Make sure
  # to sort lambda_all in decreasing order for efficiency.
  #
  # Returns a matrix containing the lasso solution vector
  # beta for each regularization parameter.

  #######################
  ## FILL IN CODE HERE ##
  #######################

  n = nrow(X)
  X = cbind(rep(1, n), X)
  p = ncol(X)
  S = 10
  len = length(lambda_all)
  beta_all = matrix(rep(0,len*(p)), nrow = p)

  R = Y
  beta = rep(0,p)
  SS = rep(0, p)
  for (j in 1:p)
    SS[j] = sum(X[,j]^2)
  for (l in 1:len) {
    lambda = lambda_all[l]
    for (t in 1:S)
      db = sum(R * X[,1]) / SS[1]
      b = beta[1] + db
      b = sign(b) * max(0, abs(b))
      db = b - beta[1]
      R = R - X[,1] * db
      beta[1] = b
      
      for (k in 2:p) {
        # R = R + X[,k] * beta[k]
        # db = sum(R * X[,k]);
        # beta[k] = sign(db) * max(0, (abs(db)-lambda)/SS[k])
        # R = R - X[,k] * beta[k]
        db = sum(R * X[,k]) / SS[k]
        b = beta[k] + db
        b = sign(b) * max(0, abs(b)-lambda/SS[k])
        db = b - beta[k]
        R = R - X[,k] * db
        beta[k] = b
      }
    beta_all[,l] = beta
  }

  ## Function should output the matrix beta_all, the
  ## solution to the lasso regression problem for all
  ## the regularization parameters.
  ## beta_all is (p+1) x length(lambda_all)
  return(beta_all)

}



myLassoEboosting <- function(X, Y, epsilon){
  

  X = cbind(rep(1, nrow(X)), X)

  #Y <- Y-mean(Y)

  beta <- matrix(0,ncol=ncol(X),nrow=1)
  r <- Y
  eps <- 0.1
  lots <- 4000
  
  for(i in 1:lots){
    co <- t(X)%*%r
    j <- (1:ncol(X))[abs(co)==max(abs(co))][1]
    delta <- eps*sign(co[j])
    b <- beta[nrow(beta),]
    b[j] <- b[j] + delta
    beta <- rbind(beta,b)
    r <- r - delta*X[,j]
    #browser()
  }
  
  return(t(beta))
  matplot(beta,type="l",lty=1,xlab="step number",ylab="beta",main="stagewise")
  matplot(apply(beta,1,function(x) sum(abs(x))),
          beta,type="l",lty=1,xlab="sum abs(beta)",ylab="beta",main="stagewise")
  matplot(apply(
    beta/matrix(s,ncol=ncol(beta),nrow=nrow(beta),byrow=T),1,function(x) sum(abs(x))),
    beta/matrix(s,ncol=ncol(beta),nrow=nrow(beta),byrow=T),
    type="l",lty=1,xlab="sum abs(beta)",ylab="beta",main="stagewise (original scale)")
  
 
  
}


test <- function() {
  set.seed(10086)
  n = 50
  p = 200
  lambda_all = (100:1) * 10
  X = matrix(rnorm(n*p), nrow = n)
  beta_true = matrix(rep(0,p), nrow = p)
  beta_true[1:5] = 1:5
  Y = 1 + X %*% beta_true + rnorm(n)
  beta_all <- myLasso(X, Y, lambda_all)

  X = cbind(rep(1, n), X);
  Y_hat = X %*% beta_all;
  estimation_error = rep(0,100)
  for (i in 1:100)
    estimation_error[i] = sum((Y-Y_hat[,i])^2)
  matplot(t(matrix(rep(1,p+1),nrow=1)%*%abs(beta_all)), t(beta_all), type = 'l')

  matplot(estimation_error,type = 'l')
}

test2 <- function() {
  set.seed(10086)
  n = 50
  p = 200
  lambda_all = (100:1) * 10
  X = matrix(rnorm(n*p), nrow = n)
  beta_true = matrix(rep(0,p), nrow = p)
  beta_true[1:5] = 1:5
  Y = 1 + X %*% beta_true + rnorm(n)
  #lars(x, y, type = c("lasso"))
  
  beta_all <- myLassoEboosting(X, Y, 0.1)
  
  X = cbind(rep(1, n), X);
  Y_hat = X %*% beta_all;
  #estimation_error = rep(0,100)
  #for (i in 1:100)
  #  estimation_error[i] = sum((Y-Y_hat[,i])^2)
  matplot(t(matrix(rep(1,p+1),nrow=1)%*%abs(beta_all)), t(beta_all), type = 'l')
  
  #matplot(estimation_error,type = 'l')
}
