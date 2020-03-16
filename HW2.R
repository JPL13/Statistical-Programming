############################################################# 
## Stat 202A 2019 Fall - Homework 02
## Author: 
## Date : 
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the corresponding function. Do not change function names, 
## function inputs or outputs. Do not write anything outside the function. 
## See detailed requirement in python files.
#############################################################

###############################################
## Function 1A                  			 ##
###############################################

sample_uniform <- function(size=10000, low=0, high=1){
  X<- rep(0, size)
  
  a<-7**5
  b<-0
  M<-2**31 - 1
  
  # Set the seed using the current system time in microseconds
  X[1]<-as.numeric(Sys.time())
  
  if(size>1){
  for (t in 1:(length(X)-1)){
    
    X[t+1]<- (a * X[t] + b) %% M
  }
  }
  
  X <- ( X/ M ) * (high- low) + low

    return(X)

}

###############################################
## Function 1B                  			 ##
###############################################

sample_normal_chain <- function(x0, c, chain_length=100, mean=0, var=1){
  x <-rep(0, length(x0))
  Y<-matrix(x0, nrow=length(x0))
  

  for(i in 2:chain_length){
    
    currentx = Y[,i-1]
    # y ~ Uniform[x-c, x+c]
    proposedx = currentx + sample_uniform(length(x0), -c, c)
    #print(proposedx)
    
    for (j in 1: length(x0)){
    
    A = exp(((currentx[j]-mean)^2-(proposedx[j]-mean)^2)/2/var)
    #A=min(dnorm(proposedx[j], log=TRUE)-dnorm(currentx[j], log=TRUE), 0)
    
    #u<-sample_uniform(size=1)
    u<-runif(1)
    if(u<=A){
      x[j] = proposedx[j]       # accept move with probabily min(1,A)
    } else {
      x[j] = currentx[j]        # otherwise "reject" move, and stay where we are
    }
    #print(x)
}
  Y<- cbind(Y, x)
}
    return(Y)
}

###############################################
## Function 1C                  			 ##
###############################################

metropolis_simulation <- function(num_chain=1000, chain_length=100, mean=0, var=1){
  
        a=0
        b=1
        c=1
        
        x0<-sample_uniform(num_chain, a, b)
        
        X_normal<-sample_normal_chain(x0, c, chain_length, mean, var)
        
        saveGIF(for(i in 1:chain_length) hist(X_normal[,i], breaks = 20, main=(i), xlim = c(-6, 6), ylim=c(0,350)), interval=0.2)
}


###############################################
## Function 2A                  			 ##
###############################################

gibbs_sample <- function(x0, y0, rho, num_chain=1000, chain_length=100, mean=0, var=1){
    
    sigma=sqrt(1-rho^2)
    
    X <- array(rep(0, num_chain*chain_length*2), c(num_chain, chain_length, 2))
    
    for (j in 1: num_chain){
      X[j, 1, 1]=x0
      X[j, 1, 2]=y0
      
      for (i in 2: chain_length){
        X[j, i, 1]=rho*X[j, i-1, 2] + sigma*rnorm(1)
        X[j, i, 2]=rho*X[j, i, 1] + sigma*rnorm(1)
        
      }
    
    }
  return(X)
}

###############################################
## Function 2B                  			 ##
###############################################

gibbs_simulation <- function(){
  #library(magick)
  X<-gibbs_sample(1, 2, .9)
  chain_length<-100
  
  saveGIF(for(i in 1:chain_length) plot(X[, i, 1], X[, i, 2], main=(i), xlim = c(-4, 4), ylim=c(-4,4)), interval=0.2)
  }

########################################################
## Optional examples (comment out before submitting!) ##
########################################################

## test()

