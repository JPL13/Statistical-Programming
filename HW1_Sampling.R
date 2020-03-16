############################################################# 
## Stat 202A 2019 Fall - Homework 01
## Author: 
## Date : 
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the corresponding function. Do not change function names, 
## function inputs or outputs. Do not write anything outside the function. 
## Do not use any of Python's built in functions for matrix inversion or for linear modeling 
## (except for debugging or in the optional examples section).
#############################################################

###############################################
## Function 1: (2a) Uniform 				 ##
###############################################

sample_uniform <- function(low=0, high=1, size, seed){
  X<- rep(0, size)
 
  a<-7**5
  b<-0
  M<-2**31 - 1
  
  # Set the seed using the current system time in microseconds
  X[1]<-seed
  
  for (t in 1:(length(X)-1)){

    X[t+1]<- (a * X[t] + b) %% M
  }
  
  U <- ( X/ M ) * (high- low) + low

  return (U)
}

###############################################
## Function 2: (2b) Exponential				 ##
###############################################

sample_exponential <- function(k=1){
  
u<-sample_uniform(size=5000, seed=as.numeric(Sys.time()) )
x<- - log(u)/k
#hist(x)
return(x)
}

###############################################
## Function 3: (2c) Normal	 				 ##
###############################################

sample_normal <- function(mean=0, var=1){
  u<-sample_uniform(size=10000, seed=as.numeric(Sys.time()) )
  u1<-head(u, 5000)
  u2<-tail(u, 5000)
  t<- -log(u2)
  R<-sqrt(2*t)
  theta<-2*pi*u1
  x<-R*cos(theta) * var + mean
  y<-R*sin(theta) * var + mean
 plot(x, y)
 hist(t)
}

###############################################
## Function 4: (3) Monte Carlo 				 ##
###############################################

monte_carlo <- function(d=2){

      unit<-sample_uniform(size=5000*d, seed=112345214561)

      unit<-matrix(unit, nrow=5000, ncol=d)
      
      #print(dim(unit))

      inside<- (rowSums(unit**2) <1)
      
      print(mean(inside))
#return (unit)
}

########################################################
## Optional examples (comment out before submitting!) ##
########################################################

# test<- function(){
# ### 1)  
#   U<-sample_uniform(size=1000, seed=12345678)
#   hist(U)
#   plot(U[-length(U)], U[-1])
#   
# ### 2)  
#   exponential<-sample_exponential()
#   hist(exponential)
#   
# ### 3)
#   sample_normal()
# }

