library(tidyverse)

# choose parameters: c, lambda, n 
# sample thousands of observations from T's distribution and then observe it's variance

set.seed(1)
n = 100000
c = 1
lambda = 10
random_sample_T = sample_T(n = n,c = c, lambda = lambda)
#create function to generate/sample observations from T's distribution
sample_T = function(n, c, lambda){
  #first sample from a truncated exponential distribution, Random Variable X
  X = rexp(n,rate = lambda)
  
  #create a vector to store values drawn from T's distribution: T = min(X,c)
  rv_T = vector(mode = "numeric", length = n) #this is more efficient than creating an empty vector and then                                                      #lengthening it with every iteration in the following for loop
  
  for(i in 1:n){
    rv_T[i] = min(X[i],c) #generate observation and assign it to the ith elemtn in the rv_T vector 
  }
  return(rv_T)
  
}


#calculate the variance (don't consider sample variance)
set.seed(1)
var_T = mean((random_sample_T - mean(random_sample_T))^2)
var_T

simulated_variance = mean(random_sample_T^2) - (mean(random_sample_T))^2



set.seed(1)

#define global parameters c and lambda for the simulation
lambda = 10
c = 1


#This is the E(T|M = X):
E_t_given_mequals_x = function(x){
  x*lambda*exp(-lambda*x)/(1 - exp(-lambda*c)) #appears correct when compared to ti89 output
}


#This is Var(T|M = X) (when M = c the variance is 0)
var_t_given_mequals_x = function(x){
  (x^2*lambda*exp(-lambda*x))/(1 - exp(-lambda*c)) - (integrate(E_t_given_mequals_x,0,c)$value)^2  #appears correct when compared to ti89 output
}



#important probabilities
prob_m_equals_c = exp(-lambda*c)
prob_m_equals_X = 1 - exp(-lambda*c)
var_T = function(){
  #calculate E[Var(T|M)]
  E_var = 0^2*prob_m_equals_c + integrate(var_t_given_mequals_x,0,c)$value * prob_m_equals_X
  
  #calculate Var[E(T|M)]
  second_moment = c^2*prob_m_equals_c + integrate(var_t_given_mequals_x,0,c)$value*prob_m_equals_X
  
  mean_squared = (c*prob_m_equals_c + integrate(E_t_given_mequals_x,0,c)$value*prob_m_equals_X)^2
  
  var_E = second_moment - mean_squared
  
  
  return(list(E_var, var_E, E_var + var_E))
  
}



calculated_variance = var_T()[[3]] #this is the total variance of T

#compare calculated variance to simulated variance
calculated_variance/simulated_variance #veryyyyy close

data.frame(calculated_variance = calculated_variance, simulated_variance = simulated_variance, pct_diff = (simulated_variance - calculated_variance)/calculated_variance )
