#########################################################
## Stat 202A - Homework 6
## Author: 
## Date : 
## Description: See CCLE
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
      u <- v/s
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

myLM <- function(X, Y){
  
  ## Perform the linear regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Use myQR inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  n = dim(X)[1]
  p = dim(X)[2]
  
  #X<-cbind(rep(1,n),X)
  decomp<-myQR(X)
  Q<-decomp[[1]]
  #Q<-qr.Q(qr(X))
  R<-decomp[[2]]
  
  #Y[which(is.nan(Y))]=0
  
  Y_star<- t(Q) %*% Y 
  beta_ls<-rep(0, p)
  
  for(i in (p):1){
    if(i== p)
      beta_ls[i] <- (Y_star[i]) / R[i,i] 
      
    
    else{
      beta_ls[i] <- (Y_star[i]- R[i, (i+1):(p)] %*% beta_ls[(i+1):(p)]) / R[i,i] }
    
  }
  #error<-sum(Y_star[(p+2):n]^2)
  
  ## Function returns the 1 x p vector beta_ls, notice this version do not add intercept.
  ## the least squares solution vector
  return(beta_ls)
  
}

######################################
## Function 3: Logistic regression  ##
######################################

## Expit/sigmoid function
expit <- function(x){
  1 / (1 + exp(-x))
}

myLogisticSolution <- function(X, Y){

  ########################
  ## FILL IN CODE BELOW ##
  ########################
x<-X
y<-Y
n = dim(X)[1]
c = dim(X)[2]

beta<-rep(0, c)
epsilon<-1*10^-6

  while(TRUE){
   
    s <- x %*% beta 
   
    p <- expit(s)
   
    w <- p*(1-p)
    
    w<- replace(w, w==0, 0.000001)
    
    r<- (y-p)/w 
    #print(r)
    
    x_tilda<- matrix(rep(0, n*c), nrow=n)
    y_tilda<- rep(0, n)
    
    for (i in 1: n){
     x_tilda[i, 1:c ] <- sqrt(w[i]) * x[i, 1:c ]
     y_tilda[i] <- sqrt(w[i]) * r[i]
    }
    
    beta_change <- myLM( x_tilda,  y_tilda )
        #print(beta)
  
    if(  sum(beta_change^2) <  epsilon   )
      break
    
    else
      beta = beta + beta_change
  }
  
return(beta)
    
}

###################################################
## Function 4: Adaboost  ##
###################################################

myAdaboost <- function(x1, x2, y) {
  
  n <- length(y)
  c<-2
  X <- cbind(x1, x2)
  n_train <- as.integer(0.8*n)
  #Subset X and y
  X_train <- X[1:n_train, ]
  X_test <- X[(n_train+1) : n, ]
  
  y_train<- y[1: n_train]
  y_test<-y[(n_train+1) : n]
  
  epsilon<-1.0e-03
  
  
  s <- rep(-1, n_train)
  
  cut <- seq(0.1, 0.9, 0.1)
  c1 <- matrix(rep(1, 2*9), nrow = 2)
  c2 <- matrix(rep(-1, 2*9), nrow = 2)
  
  
  beta<-1
  
  training_error<-c()
  testing_error<-c()
  
  li<- list()
  count=0
  w <- rep(1/n_train, n_train)

  # Training model  
  while(TRUE){ 
    if(count>0)
      w <- exp( - y_train * s) 
    sw = sum(w)
    w = w / sw
    #print(w)
    errormin = 1

    # compute beta   
    for (j in 1:c){
      for (t in 1:9){
        
        serr=0
        
        # Compute serr
        for (i in 1:n_train){
          # the case that c1=1 and c2= -1
          if (X_train[i,j] <= cut[t]){
            if(y_train[i] != 1)
              serr = serr + w[i]
            #print(serr)
          }
          
          else{
            if(y_train[i] != -1)
              serr = serr + w[i]
          }
        } # end for
        #print(serr)
        if( 1-serr < serr){
          c1[j, t] = -1
          c2[j, t] = 1
          serr= 1-serr
        }
        
        if(serr < errormin){
          errormin = serr
          jmin = j
          tmin = t
        }

      }## end of t
  }## end of j
    

    
    # compute beta
    beta <- 0.5 * log( (1- errormin)/ errormin ) 
    
    # compute s
    for(i in 1:n_train){
      if( X_train[i, jmin] < cut[tmin] )
        s[i] = s[i] + beta * c1[jmin, tmin] 
      else
        s[i] = s[i] + beta * c2[jmin,tmin] 
    }   
    
   
    
    #increment the number of iterations  
    count = count+1   #print(count)
    
    if(count>2 ){
      if( (li[[count-1]][1] == jmin) & (li[[count-1]][2]== tmin) )
        break
    }
    
    # Store j, t, c1, c2, beta
    li[[count]] <- c(jmin, tmin, c1[jmin, tmin], c2[jmin, tmin], beta)
    
    # save the training error
    #training_error <- c(training_error, errormin)
    training_error <- c(training_error, sum((y_train-sign(s))^2))
    
    print(training_error)
    
  } ## end of while
  
  ###Testing
  st <- rep(0, n-n_train)
  color <- rep("b", n-n_train)
  
  print(li)
  arr1 <- c(1)
  arr2 <- c(1)
  
  for(k in 1: length(li)){
    
    jmin <- li[[k]][1]
    tmin <- li[[k]][2]
    c1 <- li[[k]][3]
    c2 <- li[[k]][4]
    beta <- li[[k]][5]
    
    if(jmin==1.) arr1<-c(arr1, cut[tmin])
    if(jmin==2.) arr2<-c(arr2, cut[tmin])
    
    for(i in 1: (n-n_train)){
      if( X_test[i, jmin] < cut[tmin] ){
        st[i] = st[i] + beta * c1
      }
      else{
        st[i] = st[i] + beta * c2
      }
    }
    
    wt <- exp(- y_test * st)
    swt = sum(wt)
    wt = wt / swt
    
    serr=0
    # Compute serr
    for (i in 1:(n-n_train)){
      if (X_test[i,jmin] <= cut[tmin]){
        if(y_test[i] != c1)
          serr = serr + wt[i]
      }
      
      else{
        if(y_test[i] != c2)
          serr = serr + wt[i]
      }
    }
    
    #error <- sum((st - y_test)^2)
    #testing_error <- c(testing_error, serr)
    testing_error <- c(testing_error,  sum((y_test-sign(st))^2))
   
    #print(testing_error)
  }
  

  #print(st)
  
  arr1<-sort(unique(arr1))
  arr2<-sort(unique(arr2), decreasing = TRUE)
  print(arr1)
  print(arr2)
  

      for( k in 1:length(arr1)){
        for(i in 1: (n-n_train)){
          if((X_test[i, 1] <= arr1[k]) & (X_test[i, 2] <= arr2[k]))
            color[i]<-"a"
        }  
      }
 
 
  
  predict<-cbind(X_test, color)
  predict[ ,3] <- as.factor(predict[ ,3])
  #print(predict[ ,3])
  plot(X_test[, 1], X_test[, 2], col=predict[ ,3], pch=21 ) 
  
  plot(seq(1, length(li)), training_error, type="l", col="red", ylim= c(0, 12000), ylab = "Error", xlab= "Iteration")
  lines(seq(1, length(li)), testing_error,  col="blue")
  legend(0.1, 1.8, legend=c("Training","Testing"), col=c("red","blue"), lty = c(1, 1), ncol=1)
  

}

###################################################
## Function 5: XGBoost  ##
###################################################

myXGBoost <- function(x1, x2, y) {
  n <- length(y)
  c<-2
  X <- cbind(x1, x2)
  n_train <- as.integer(0.8*n)
  #Subset X and y
  X_train <- X[1:n_train, ]
  X_test <- X[(n_train+1) : n, ]
  
  y_train<- y[1: n_train]
  y_test<-y[(n_train+1) : n]
    
  epsilon<-1*10^-6
  
  
  s <- rep(0, n_train)
  p <- rep(0, n_train)
  w <- rep(1/n_train, c)
    
  Rmin <- 1e10
  
  jmin <- 1
  tmin <- 5
  
  cut <- seq(0.1, 0.9, 0.1)
  c1 <- matrix(rep(0, 2*9), nrow = 2)
  c2 <- matrix(rep(0, 2*9), nrow = 2)
  c1[jmin, tmin] <- mean(y)
  c2[jmin, tmin] <- mean(y)
  
  training_error<-c()
  testing_error<-c()
  
  li<- list()
  
  count=0

  # Training model  
while(TRUE){  
  
# compute s
  for(i in 1:n_train){
    if( X_train[i, jmin] < cut[tmin] )
        s[i] = s[i] + c1[jmin, tmin] 
    else
        s[i] = s[i] + c2[jmin,tmin] 
  }   
# compute p, w, r      
    p <- expit(s)
    w <- p * (1-p)
    r <- (y_train - p)/ w

# compute c1, c2      
    for (j in 1:c){
      for (t in 1:9){
        s1=0
        s2=0
        sw1=0
        sw2=0
        
        for (i in 1:n_train){
          
          if (X_train[i,j] <= cut[t]){
            s1 = s1 + w[i] * r[i]
            sw1 = sw1 + w[i]
          }
          
          else{
            s2 = s2 + w[i] * r[i]
            sw2 = sw2 + w[i]
          }
          
          c1[j, t] = s1 / sw1
          c2[j, t] = s2 / sw2
        }
        
      # Compute R
        R<-0
        for (i in 1:n_train){
          if (X_train[i, j]<= cut[t])
            R = R + w[i] * (r[i]- c1[j, t])^2
          else
            R = R + w[i] * (r[i]- c2[j, t])^2
        }
        
        if(R < Rmin){
          Rmin = R
          jmin = j
          tmin = t
        }
        
      }## end of t
      
    }## end of j
  
  #increment the number of iterations  
  count=count+1  
  #print(count)
  # save the training error
  training_error <- c(training_error, Rmin)
  #print(training_error)
  
  # Store j, t, c1, c2
  li[[count]] <- c(jmin, tmin, c1[jmin, tmin], c2[jmin, tmin])
  
  if(count>2){
   if(  (training_error[count-1]- training_error[count] < epsilon ))
   {  #print(training_error[count]- training_error[count-1] )
     break; }
  }
     
  } ## end of while
  

###Testing
st <- rep(0, n-n_train)
color <- rep("b", n-n_train)

print(li)

for(k in 1: length(li)){
 
  jmin <- li[[k]][1]
  tmin <- li[[k]][2]
  c1 <- li[[k]][3]
  c2 <- li[[k]][4]
  
  for(i in 1: (n-n_train)){
    if( X_test[i, jmin] < cut[tmin] ){
      st[i] = st[i] + c1
      color[i] <- "a"
    }
    else{
      st[i] = st[i] + c2
        color[i] <- "b"
    }
  }
  
  pt <- expit(st)
  wt <- pt * (1-pt)
  rt <- (y_test - pt)/ wt
  # Compute R
  R<-0
  for (i in 1:(n-n_train)){
    if ( X_test[i, jmin] < cut[tmin] )
      R = R + wt[i] * (rt[i]- c1)^2
    else
      R = R + wt[i] * (rt[i]- c2)^2
  }

  #error <- sum((st - y_test)^2)
  testing_error <- c(testing_error, R)
  #print(testing_error)
  
}
  
for(i in 1: (n-n_train)){
  if(st[i]>0)
    color[i]<-"a"
  else
    color[i]<-"b"
}
#print(st)
 
  predict<-cbind(X_test, color)
  predict[ ,3] <- as.factor(predict[ ,3])
  #print(predict[ ,3])
  plot(X_test[, 1], X_test[, 2], col=predict[ ,3], pch=21 ) 
  
  plot(seq(1, length(li)), training_error, type="l", col="red", ylim=c(0, 5000), ylab = "Error", xlab= "Iteration")
  lines(seq(1, length(li)), testing_error,  col="blue")
  legend(4, 5000, legend=c("Training","Testing"), col=c("red","blue"), lty = c(1, 1), ncol=1)
  
  
}## end of XGBoost


## Simulation

test <- function() {

  # Test (1)
  n <- 5000
  p <- 4
  
  X    <- matrix(rnorm(n * p), nrow = n)
  beta <- c(12, -2,-3, 4)
  Y    <- 1 * (runif(n) < expit(X %*% beta))
  
  ## Our solution
  logistic_beta <- myLogisticSolution(X, Y)
  logistic_beta    
  
  ## R's solution
  coef(glm(Y ~ X + 0, family = binomial(link = 'logit')))


  # Test (2, 3)

  num_sample <- 10000

  x1 <- runif(num_sample)
  x2 <- runif(num_sample)
  y <- as.integer((x1^2+x2^2 < 1))

  
  myXGBoost(x1, x2, y)
  
  x1 <- runif(num_sample)
  x2 <- runif(num_sample)
  y <- 2*as.integer((x1^2+x2^2 < 1)) -1
  myAdaboost(x1, x2, y)
  
}

