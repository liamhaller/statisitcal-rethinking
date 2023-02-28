#Chapter Two

#2M1

#plot grid approximate posterior distribution for each of the following sets of observations
#Assume a uniform prior for p

#1. W,W,W

#define grid
p_grid <- seq(from=0, to=1, length.out=20)

#define prior
prior <- rep(1,20)

#compute likelihood at each value in grid
likelihood <- dbiom(6,size=9, prob=p_grid)

##compute product of likelihood and prior
unstd.posterior <- likelihood*prior

#standardize the posterior so it sums to 1

posterior <- unstd.posterior/sum(unstd.posterior)