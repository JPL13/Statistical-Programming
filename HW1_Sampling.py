# -*- coding: utf-8 -*-
"""

 Stat 202A 2019 Fall - Homework 01
 Author: 
 Date : 

 INSTRUCTIONS: Please fill in the corresponding function. Do not change function names, 
 function inputs or outputs. Do not write anything outside the function. 
 Do not use any of Python's built in functions for matrix inversion or for linear modeling 
 (except for debugging or in the optional examples section).
 
"""

import numpy as np
#import time
import matplotlib.pyplot as plt

###############################################
## Function 1: (2a) Uniform 			    ##
###############################################

def sample_uniform(low=0., high=1.,size=5000, seed=135789):
    x=np.zeros(size)
    U=np.zeros(size)
    M=2**31 - 1.
    a=7**5
    b=0
    x[0]=seed
    for i in range(len(x)-1):
        x[i+1]=(a * x[i] + b) % M
    
    for i in range(len(x)):
        U[i]=x[i]/M
    
    #plt.hist(U)
    #plt.scatter(U[:-1], U[1:] )
    return U

	
  
###############################################
## Function 2: (2b) Exponential				 ##
###############################################

def sample_exponential(k=1):
    u=sample_uniform()
    x= -np.log(u)/k
    plt.hist(x, bins=100)
	
  
###############################################
## Function 3: (2c) Normal	 				 ##
###############################################

def sample_normal(mean=0, var=1):
    u = sample_uniform(size=10000,seed=123607239 )
    u1=u[:5000]
    u2 =u[-5000:]
    t= -np.log(u1)
    #print(t)
    R= np.sqrt(2*t)
    theta = 2* np.pi * u2
    #print(theta)
    x = R * np.cos(theta)*var+mean
    y = R * np.sin(theta)*var+mean
    #print(y)
    #plt.hist(t)
    plt.scatter(x, y, s=1)
    
###############################################
## Function 4: (3) Monte Carlo 				 ##
###############################################

def monte_carlo(d=2):

    pts=sample_uniform(size=5000*d, seed=156239)
    pts=np.reshape(pts,(5000, d))
#    i=1
#    while i<d:
#        current=sample_uniform(seed=time.time())
#        pts=np.column_stack((pts,current))
#        i=i+1
    
    print pts.shape
    
    idx = (pts**2).sum(axis=1)  < 1.0
    
    print idx.sum()/(pts.shape[0]*1.0)
        
    
  
########################################################
## Optional examples (comment out before submitting!) ##
########################################################

## test()

