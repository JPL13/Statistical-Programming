library(dslabs)
mnist <- read_mnist()
n_iter<-25


########################################
split_data <- function(){
  train_data <- mnist$train$images[1:200, ]/255
  test_data <- mnist$test$images[1:100, ]/255
  
  train_label <- mnist$train$labels[1:200]
  train_label<- replace(train_label, train_label <5, -1)
  train_label<-replace(train_label, train_label!= -1, 1)
  
  
 
  test_label <- mnist$test$labels[1:100]
  test_label<- replace(test_label, test_label <5, -1)
  test_label<-replace(test_label, test_label!=-1, 1)
  
  return (list("train_data"=train_data, "test_data"=test_data, "train_label"=train_label, "test_label"=test_label) )

}


mySVM <- function( train_data, test_data, train_label, test_label, kernel=1, training_method="gradient"){
  n <- dim(train_data)[1]
  p <- dim(train_data)[2]
  
  alpha <- rep(0, n)
  
  acc <- c()
  acc_test<-c()

  if(training_method=="gradient")  {
 
    learning_rate = 1e-2
    regularization = 0.1
    
    while(TRUE){
      #browser()
     
      alpha_old <- alpha
      
      for(i in 1:n){ 

        score = kernMult(train_data, train_data, kernel, alpha * train_label)
        score_test <- kernMult(test_data, train_data , kernel, alpha * train_label)
        acc <- c(acc, mean(sign(score)==train_label))
        acc_test <- c(acc_test, mean(sign(score_test)==test_label))
        
        dalpha = (score * train_label <=1)/n
        
        alpha[i] = alpha[i] + learning_rate * dalpha[i] - regularization*alpha[i]
        if(alpha[i]<0)
          alpha[i]=0
        if(alpha[i]> 1/regularization)
          alpha[i]=1/regularization
        print(i)

      }
      
      if(sum((alpha_old-alpha)^2)<10^-6)
        break
    }
    
   }## end of gradient method
    
  if(training_method=="dual"){
    while(TRUE){
      #browser()
      Q<-computeQ(train_data, train_label)
      
      
      for(j in 1:n){
        alpha_old <- alpha
        sum<-0
        for(k in 1:n){
          
          if(k != j ){
            sum = sum + alpha[k] * Q[j,k]
          }

        }
        #update alpha[j]
        alpha[j] = (1- sum)/Q[j,j]
        if(alpha[j]<0)
          alpha[j]=0
        print(j)
        print(alpha[j])
        
        score = kernMult(train_data, train_data, kernel, alpha * train_label)
        score_test <- kernMult(test_data, train_data , kernel, alpha * train_label)
        acc <- c(acc, mean(sign(score)==train_label))
        acc_test <- c(acc_test, mean(sign(score_test)==test_label))
        
      }
      
      break
      #print((alpha_old-alpha)^2)
      if(sum((alpha_old-alpha)^2)<10^-6)
        break


    }# end of while loop
    
  }# end of dual method
  
  return( list("acc"=acc, "acc_test"= acc_test))
}





####################################################
# 0 -- linear: u'*v                                #
#	1 -- polynomial: (gamma*u'*v + coef0)^2          #       
# 2 -- radial basis function: exp(-gamma*|u-v|^2)  #
# 3 -- sigmoid: tanh(gamma*u'*v + coef0)           #
####################################################
kernMult<-function( test_data, train_data, kernel=1, alpha){
  
  t<-dim(test_data)[1] 
  n<-dim(train_data)[1] 
  
  f<-rep(0, t)
 
  for(i in 1:t){
    
    k<-rep(0, n)
    
    if(kernel==2){
     
      for(j in 1:n){
        m=(test_data[i, ]- train_data[j, ])
        k[j] =  exp(-t(test_data[i, ]- train_data[j, ]) %*% (test_data[i, ]- train_data[j, ]))
       }
        f[i] <- t(k) %*% alpha
    }
  else{
    for(j in 1: n){
      if(kernel==0)
        k[j] = t(test_data[i, ]) %*% train_data[j, ]
      if(kernel==1)
        k[j] = (t(test_data[i, ]) %*% train_data[j, ] + 1)^2
     
      if(kernel==3)
        k[j] = tanh( (1/784.)*t(test_data[i, ]) %*% train_data[j, ] + 1)
    } 
    f[i]<- t(k) %*% alpha
  }
  }

  return(f)
}

computeQ<-function( train_data, y, kernel=2){
  
  #t<-dim(test_data)[1] 
  n<-dim(train_data)[1] 
  
  Q<- matrix(rep(0, n*n), nrow = n)
  
  for(i in 1:n){
    
    k<-rep(0, n)
    
    for(j in 1: n){
      
      if(kernel==0)
        k[j] = t(train_data[i, ]) %*% train_data[j, ]
      if(kernel==1)
        k[j] = (t(train_data[i, ]) %*% train_data[j, ] + 1)^2
      if(kernel==2){
        k[j] =  exp(- t((train_data[i, ]- train_data[j, ])) %*% (train_data[i, ]- train_data[j, ]))
      }
      if(kernel==3)
        k[j] = tanh( (1/784.)*t(train_data[i, ]) %*% train_data[j, ] + 1)
      
      Q[i,j]= y[i]*y[j]*k[j]
    } 

  }
  
  return(Q)
}


test <- function(){
 
  data<-split_data ()
  
  train_data<-data$train_data
  test_data<-data$test_data
  train_label<-data$train_label
  test_label<-data$test_label

  li<-mySVM ( train_data, test_data, train_label, test_label, 3)
  acc <- li$acc
  acc_test <- li$acc_test
  
  plot(1:length(acc), acc, type="l", col="red")
  lines(1:length(acc), acc_test, col="blue")
  
  legend("bottomright", legend=c("Train", "Test"),
         col=c("red", "blue"), lty=1, cex=0.8)
}
