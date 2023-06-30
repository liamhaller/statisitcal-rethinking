#Chapter Two
library(ggplot2)

#2M1
#plot grid approximate posterior distribution for each of the following sets of observations
#Assume a uniform prior for p

#1. W,W,W
#2. W,W,W,L
#3. W,W,W,W,W,L,L

#define grid
p_grid <- seq(from=0, to=1, length.out=20)

#define prior
prior <- rep(1,20)

#compute likelihood at each value in grid
likelihood <- dbinom(5,size=7, prob=p_grid)

##compute product of likelihood and prior
unstd.posterior <- likelihood*prior

#standardize the posterior so it sums to 1

posterior <- unstd.posterior/sum(unstd.posterior)

plot(p_grid, posterior, type = 'b')


#2M2
#define prior such that if p <.5 it's zero and greater than .5 it's a constatn
prior <- ifelse(p_grid < 0.5, 0, 1)

#compute likelihood at each value in grid
likelihood <- dbinom(5,size=7, prob=p_grid) # here we are varying the probability, that is what we're looking for....

##compute product of likelihood and prior
unstd.posterior <- likelihood*prior

#standardize the posterior so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

plot(p_grid, posterior, type = 'b')




