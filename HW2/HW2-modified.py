# -*- coding: utf-8 -*-
"""

 Stat 202A 2019 Fall - Homework 02
 Author: 
 Date : 

 INSTRUCTIONS: Please fill in the corresponding function. Do not change function names, 
 function inputs or outputs. Do not write anything outside the function.
 
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
#from matplotlib import animation, rc



### Part 1 : Metropolis Algorithm

def sample_uniform(size=10000,  low=0, high=1):
    X=np.zeros(size)
    M=2**31 - 1.
    a=7**5
    b=0
    #X[0]=int( time.time())
    X[0]=13523986
    for i in range(len(X)-1):
        X[i+1]=(a * X[i] + b) % M
    
    X=(X/M)*(high-low)+low

    return X



"""
Function 1B: Normal by Metropolis
ail : 	This function return multiple chains by Metropolis sampling from N(mean, var).
				For every train, the proposal distribution at x is y ~ Uniform[x-c, x+c].
				The input x0 is a 1 dimension np.ndarray. 
				Return a np.ndarray X with size x0.shape[0] * chain_length. In X, each row X[i]
				should be a chain and t-th column X[:, t] corresponding to all different X_t. 
"""
def sample_normal_chain(x0, c, chain_length=100, mean=0, var=1):

    ndim= x0.shape[0]
    
    X= np.zeros((ndim,chain_length))
    
    #prob=np.zeros(chain_length)
    
    X[:, 0]=x0
    
    for j in range(1, chain_length):
        
        currentx = X[:, j-1]
        #print(currentx)
        
        #propose
        x_star = currentx + np.random.uniform(-c, c, ndim)
        
        #sample_uniform(ndim, -c, c)
        
        for i in range(ndim):
        
            # draw random uniform number
            u=np.random.uniform(0, 1)
            
            #compute hastings ratio
            A=np.exp( ((currentx[i]-mean)**2-(x_star[i]-mean)**2) /(2*var)  )
            #B=norm.pdf(x_star[i])/norm.pdf(currentx[i])
            
            if u<=A:
                X[i, j]=x_star[i]
            else:
                X[i, j]=currentx[i]
            
        
    return X

def metropolis_simulation(num_chain=1000, chain_length=100, mean=0, var=1):

    """
    Function 1C: Simulate metropolis with different setting.
    Detail : 	Try different setting and output movie of histgrams.
    """
    list_a = [0, -0.1, -1, -10] # Add other value as you want.
    list_b = [1, 0.1, 1, 10]
    list_c = [1, 2]
    

    for a, b, c in [(a, b, c) for a in list_a for b in list_b for c in list_c]:
        x0 = sample_uniform(num_chain, a, b)

        X_normal = sample_normal_chain(x0, c, chain_length, mean, var)
        
        #fig, ax = plt.subplots(figsize=(5, 3))
        #ax.set(xlim=(-6, 6), ylim=(0, 400))
        #plt.hist(X_normal[:, 5])
      
        fig = plt.figure()
    
        hist = plt.hist(X_normal[:, 0])

        def update_hist(num, X_normal):
            plt.cla()
            plt.hist(X_normal[:, num])
            if a <= -10 or b>=10: plt.xlim(-11, 11)
                   
            else: plt.xlim(-6, 6)
            plt.ylim(0, 350)
            plt.title(num)

        

        anim = animation.FuncAnimation(fig, update_hist, chain_length, fargs=(X_normal, ) )
        filename='Metro_a='+ str(a) +'_b='+str(b)+ '_c=' + str(c)+ '.gif'
        anim.save(filename, writer='imagemagick')
        plt.show()
    
        

		# Plot movie and save 
		# Here plot chain_length graphs, each of them is a histogram of num_chain point.
		# You may use matplotlib.animation and matplotlib.rc to save graphs into gif movies.


### Part 2 : Gibbs Sampling

def gibbs_sample(x0, y0, rho, num_chain=1000, chain_length=100, mean=0, var=1):

    """
    Function 2A: Bivariate normal with correlation rho
    Detail : 	This function return multiple chains by Gibbs sampling
				The input x0, y0, rho, num_chain is a number. This time, we use same starting point. 
				Return a np.ndarray X with size num_chain * chain_length * 2. In X, each row X[i]
				should be a chain and t-th column X[:, t] corresponding to all different pair (X_t, Y_t). 
    """
    sigma=np.sqrt(1-rho**2)
    
    X=np.zeros((num_chain, chain_length, 2))
    
    for j in range(num_chain):
        X[j, 0, :]=(x0, y0)
    
        for i in range(1, chain_length):
            x = np.random.normal(rho*X[j, i-1, 1], sigma)
            X[j, i, 0]=x
            y = np.random.normal(rho*X[j, i, 0], sigma)
            
            X[j, i, 1]=y
       
    
    return X

def gibbs_simulation():

    """
    Function 2B: Simulate Gibbs with different rho and plot
    Detail : 	Try different setting and output movie of histgrams. 
				Discard first 50 steps and output 50~100 steps only.
    """
    
    list_rho = [0, -1, 1, 0.5, 0.1, 0.9] # Add other value as you want.
    
    for rho in list_rho:
		# Run Gibbs Sampling
        X_normal = gibbs_sample(1,1,rho)
        chain_length=X_normal.shape[1]
		# Plot movie and save
        def animate(i):
            plt.cla()
            plt.title("Gibbs Sampling (rho=%s) Time: %d" % (rho,i))
            plt.scatter(X_normal[:,i,0], X_normal[:,i,1])
            plt.xlim(-5, 5)
            plt.ylim(-5, 5)

        # Define the graph for first frame
        fig = plt.figure()
        scatter = plt.scatter(X_normal[:,0,0], X_normal[:,0,1])

        # Define animation class and save 
        ani = animation.FuncAnimation(fig, animate, frames=chain_length, repeat_delay=3000, repeat=True)
        ani.save('Gibbs_rho=%s.gif' % (rho), writer='imagemagick')
        
        # single chain from B to T
        #plt.scatter(X_normal[0,49:,0], X_normal[0,49:,1])
