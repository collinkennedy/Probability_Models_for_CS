library(tidyverse)


#Need to show the tower property, E[ E( Y | U,V)] = E(Y | U)
#but for a context where


#Draw samples from these Discrete distributions
set.seed(1)
A_sample = sample(c(1,2,3), size = 25, prob = c(.5,.25,.25), replace = TRUE) #

set.seed(2)
B_sample = sample(c(4,5,6), size = 25, prob = c(.5,.25,.25), replace = TRUE)

set.seed(3)
C_sample = sample(c(7,8,9), size = 25, prob = c(.5,.25,.25), replace = TRUE)
W_sample = sample(c(-3,3), size = 25, prob = c(.5,.5), replace = TRUE)


#now create U and V vectors: #then have Y = U + V + W
#and create a dataframe of the data
sampled_data = data.frame(U = A_sample + C_sample,
                          V = B_sample + C_sample,
                          W = W_sample,
                          Y = A_sample +C_sample + B_sample + C_sample + W_sample)


#verified by hand in the example case of U = 10 that E[E(U,V)|U = 10] = E(Y|U = 10). now show it for all values of U
#calculate RHS and LHS separately, then throw them in a dataframe together and visually inspect them, or just test for equivalence




#Let RHS = E(Y|U = i)
RHS = numeric() 

#loop over every value of U and calculate E(Y|U = i)
for(i in unique(sampled_data$U)){
  
  #subset the data properly, ie, condition on U = i
  Y_given_U = sampled_data %>% 
    filter(U == i)
  RHS = c(RHS,mean(Y_given_U$Y))
}




#Let LHS = E[E(Y|U,V)|U = i] 
LHS = numeric()
for(i in unique(sampled_data$U)){
  
  #create a dataframe which is basically E(Y|U = i, V) (a discrete random variable with condition expectations as its values)
  tower_prop = sampled_data %>% filter(U == i) %>% 
    group_by(U,V) %>% #     don't actually need to group by the U here since its fixed at value i
    summarise(counts = n(), mean_Y = mean(Y)) %>% 
    mutate(props = counts / sum(counts)) #this is P(V = j | U = some value i)
  
  double_e_given_some_U = sum(tower_prop$mean_Y*tower_prop$props) #this is the tower property calculation for the ith U- a 
                    #weighted average of averages (mean Y)
  
  #append it to the LHS vector
  LHS = c(LHS, double_e_given_some_U)
  
}




#add LHS and RHS to a dataframe and compare
simulation_comp_df = data.frame(U = unique(sampled_data$U), tower_prop_LHS = LHS, E_Y_given_U_RHS = RHS)
simulation_comp_df

#understanding the RHS
sampled_data %>% 
  group_by(U) %>% 
  summarise(counts = n(), meann = mean(Y)) %>% 
  mutate(props = counts/sum(counts))

#understanding the LHS
sampled_data %>% filter(U == 10) %>% 
  group_by(V) %>% 
  summarise(counts = n(), mean_Y = mean(Y)) %>% 
  mutate(props = counts / sum(counts))

