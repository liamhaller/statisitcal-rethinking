#Chapter 3



p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )



# Easy --------------------------------------------------------------------

#1 How much posterior probability lies below p = 0.2?

sum(samples < .2)/10000

# 2 How much posterior probability lies above p = 0.8?

sum(samples > .8)/10000


#3 How much posterior probability lies between p = 0.2 and p = 0.8?

sum(samples > .2 & samples < .8)/10000


#4 20% of the posterior probability lies below which value of p?

quantile(samples, .2)

##check
sum(samples < .5185185)/10000

#5 20% of the posterior probability lies above which value of p?

quantile(samples, .8)

#6 Which values of p contain the narrowest interval equal to 66% of the posterior probability?


#3E7. Which values of p contain 66% of the posterior probability,
#assuming equal posterior probability both below and above the interval?\


# Medium ------------------------------------------------------------------


#Suppose the globe tossing data had turned out to be 8 water in 15 tosses. 
#Construct the poste- rior distribution, using grid approximation. 
#Use the same flat prior as before.

p_grid <-seq(from=0 , to=1 , length.out=1000 )
prior <- rep(1, 1000)
likelihood <- dbinom(8, 15, prob = p_grid)
posterior <- prior * likelihood
posterior <- posterior/sum(posterior) 

#2. Draw 10,000 samples from the grid approximation from above. 
#Then use the samples to cal- culate the 90% HPDI for p.
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )



#3 Construct a posterior predictive check for this model and data. 
#This means simulate the distribution of samples, averaging over the posterior uncertainty in p. 
#What is the probability of observing 8 water in 15 tosses?

#my way
#dbiniom calucates probabilities given parameters
mean(dbinom(8,15,prob = samples))

#answerff 
#rbinom simulates toss numbers given size and probabilities

w <- rbinom( 1e4 , size=15 , prob=samples )
sum(w==8)/1e4


#Usingtheposteriordistributionconstructedfromthenew(8/15)data,
#nowcalculatetheprob- ability of observing 6 water in 9 tosses.

mean(dbinom(6,9, prob = samples))
w <- rbinom( 1e4 , size=9 , prob=samples )
sum( w==6 ) / 1e4


#useapriorthatiszerobelowp=0.5andaconstantabovep=0.5.

p_grid <-seq(from=0 , to=1 , length.out=1000 )
prior <- ifelse(p_grid < .5, 0, 1)
likelihood <- dbinom(8, 15, prob = p_grid)
posterior <- prior * likelihood
posterior <- posterior/sum(posterior) 

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

mean(dbinom(8,15,prob = samples))
mean(dbinom(6,9, prob = samples))

# hard --------------------------------------------------------------------

birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
            1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
            1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
            0,0,0,1,1,1,0,0,0,0)

total_boys <- sum(birth1 + birth2)


#Using grid approximation, compute the posterior distribution for the probability of a birth being a boy. 
#Assume a uniform prior probability. Which parameter value maximizes the posterior probability?

p_grid <- seq(0, 1, length.out = 500)
prior <- rep(1, 500)
liklihood <- dbinom(total_boys , 200, prob = p_grid)
posterior <- prior*liklihood
posterior <- posterior/sum(posterior)

p_grid[which.max(posterior)]
p_grid[ which.max(posterior) ]

