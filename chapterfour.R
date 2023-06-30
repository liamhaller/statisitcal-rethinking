library(rethinking)


# What is a normal dist? --------------------------------------------------

pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
hist(pos)

#normality by multiplicaiton
prod( 1 + runif(12,0,0.1) )
growth <- replicate( 10000 , prod( 1 + runif(12,0,0.1) ) )
dens( growth , norm.comp=TRUE )

#test for size of multiplication
big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
dens( big , norm.comp=TRUE )
dens( log(big) , norm.comp=TRUE ) #notice log big is then normal


small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )
dens( small , norm.comp=TRUE )



data("Howell1")
d <- Howell1
d2 <- d[ d$age >= 18 , ]

dens(d2$height)



# Building height model ---------------------------------------------------


#bplot the priors
#mean height
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
#standard deviation
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )


##now that we've set the priors we should look at hte prior predictive 

sample_mu <- rnorm( 1e4 , 178 , 20 ) #prior for mean
sample_sigma <- runif( 1e4 , 0 , 50 ) #prior for variance
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma ) # h is dist normally
dens( prior_h )

### brute force calucaltions of the posterior
mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )

#for each value of a height,  calculates the density (probabilty) of that height 
#ocuring, varied across all of the means and sd from 150-160, and sd 7-9
post$LL <- sapply( 1:nrow(post) , function(i) sum(
  dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )


post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )


dnorm(151.765, 150.0000, 7.000000, log = TRUE)

#plot result
contour_xyz( post$mu , post$sigma , post$prob )
image_xyz( post$mu , post$sigma , post$prob )


#Sampling from posterior  
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )

dens( sample.mu )
dens( sample.sigma )

PI( sample.mu )


# Quadratic Approx --------------------------------------------------------

flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

#Approximate
m4.1 <- quap( flist , data=d2 )

#now take a look at the posteiro dis
precis( m4.1 )



# Sampling the quadratic approx -------------------------------------------

vcov( m4.1 )

#variances
diag( vcov( m4.1 ) )
#cov matrix
cov2cor( vcov( m4.1 ) )

post <- extract.samples( m4.1 , n=1e4 )
precis(post)
dens(post)



# Linear prediction -------------------------------------------------------

plot( d2$height ~ d2$weight )
library(rethinking)
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]
# define the average weight, x-bar
xbar <- mean(d2$weight)

m4.3b <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + exp(log_b)*( weight - xbar ),
    a ~ dnorm( 178 , 20 ) ,
    log_b ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )

pairs(m4.3b)



## Plotting the posterior



plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples( m4.3b )
a_map <- mean(post$a)
b_map <- exp(mean(post$log_b))
curve( a_map + b_map*(x - xbar) , add=TRUE )



#let's add uncertienty

#extract some samples
post <- extract.samples( m4.3b )
post[1:5,]


#recalculate with fewer data (so we can better)
#appreciate spread of the lines
N <- 100
dN <- d2[ 1:N , ]
mN <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - mean(weight) ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=dN )


# extract 20 samples from the posterior
post <- extract.samples( mN , n=20 )
# display raw data and sample size
plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))
# plot the lines, with transparency
for ( i in 1:20 )
  curve( post$a[i] + post$b[i]*(x-mean(dN$weight)) ,
         col=col.alpha("black",0.3) , add=TRUE )

## istead of just a bunch of lines
## how can we compute an interval/contor


mu <- link( m4.3b )



# curved liens ------------------------------------------------------------

library(rethinking)
data(Howell1)
d <- Howell1


plot( height ~ weight , d )



d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d )

precis( m4.5 )




# Splines -----------------------------------------------------------------




library(rethinking)
data(cherry_blossoms)
d <- cherry_blossoms
precis(d)



plot(doy ~ year, d)


##Decide where to put knots
d2 <- d[ complete.cases(d$doy) , ] # complete cases on doy
num_knots <- 15
knot_list <- quantile( d2$year , probs=seq(0,1,length.out=num_knots) )

#Decide polynomial degree
library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)] ,
        degree=3 , intercept=TRUE )


plot( NULL , xlim=range(d2$year) , ylim=c(0,1) , xlab="year" , ylab="basis" )
for ( i in 1:ncol(B) ) lines( d2$year , B[,i] )



m4.7 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(100,10),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data=list( D=d2$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )


post <- extract.samples( m4.7 )
w <- apply( post$w , 2 , mean )
plot( NULL , xlim=range(d2$year) , ylim=c(-6,6) ,
      xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i]*B[,i] )

mu <- link( m4.7 )
mu_PI <- apply(mu,2,PI,0.97)
plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 )
shade( mu_PI , d2$year , col=col.alpha("black",0.5) )




# questions ---------------------------------------------------------------

library(rethinking)

#4M1 -- Simulated boserved y values from the prior

sample_mu <- rnorm( 1e4 , 0 , 10 ) #prior for mean
sample_sigma <- rexp(1e4, 1)


prior_y <- rnorm( 1e4 , sample_mu , sample_sigma ) # h is dist normally
dens( prior_y )


#M42 -- Translate math --> quap

m42 <- quap(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <-dnorm(0,10),
    sigma ~ dexp(1)
  ), data=list( D=d2$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )




#4M4
#what does a piror predictive look like


sample_alpha <- rnorm(50 , 100 , 10 ) #prior for mean
sample_beta <- rlnorm(50, 1)
sample_sigma <- rexp(50, 1)
year <- c(1:3)
sample_mu <- sample_alpha + (year - mean(year))*sample_beta
sample_height <- rnorm(sample_mu, sample_sigma)


library(tidyverse)
library(ggplot2)
n <- 50
tibble(group = seq_len(n),
       alpha = rnorm(n, 100, 10),
       beta = rlnorm(n, 0, .5),
       sigma = rexp(n, .8)) %>%
  expand(nesting(group, alpha, beta, sigma), year = c(1, 2, 3)) %>% 
  mutate(height = rnorm(n(), alpha + beta * (year - mean(year)), sigma)) %>%
  ggplot(aes(x = year, y = height, group = group)) +
  geom_line() +
  labs(x = "Year", y = "Height")


rethinking::HPDI(rlnorm(1e3, 1, .8))



#4H1
library(rethinking)

data("Howell1")
d <- Howell1


##Create cubic model of height and weigiht

d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2
d$weight_s3 <- d$weight_s^3
m4.6 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 + b3*weight_s3 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 10 ) ,
    b3 ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d )


#Get table of coefficents
precis( m4.6 )[1,1]
unknown_weight <- data.frame(matrix(0, nrow = 5))
unknown_weight$original <- c(46.95, 43.72, 64.78, 32.59, 54.63)
unknown_weight$standerd <- (unknown_weight$original - mean(d$weight))/sd(d$weight)
unknown_weight$sqrd <- unknown_weight$standerd^2
unknown_weight$cubed <- unknown_weight$standerd^3
unknown_weight <- unknown_weight[,-1]

mean_estimates <- precis( m4.6 )[1,1] + precis( m4.6 )[2,1]*unknown_weight$standerd  +
  precis( m4.6 )[3,1]*unknown_weight$sqrd + precis( m4.6 )[4,1]*unknown_weight$cubed 


interval_low <- precis( m4.6 )[1,3] + precis( m4.6 )[2,3]*unknown_weight$standerd  +
  precis( m4.6 )[3,3]*unknown_weight$sqrd + precis( m4.6 )[4,3]*unknown_weight$cubed 
  
interval_high <- precis( m4.6 )[1,4] + precis( m4.6 )[2,4]*unknown_weight$standerd  +
  precis( m4.6 )[3,4]*unknown_weight$sqrd + precis( m4.6 )[4,4]*unknown_weight$cubed 
  


H1 <- data.frame(original = unknown_weight$original,
                 means = mean_estimates,
                 interval = paste0("low:", round(interval_low,2), " high:",  round(interval_high, 2)))



#interesting here I sampled directly from the estimate of the posterior but I don't think that is correct
#instead I should have extracted samples 

post <- extract.samples( m4.6 )
str(post)

#Simulate indivdual heights using the given distribution
y <- rnorm( 1e5 , post$a + post$b*( 46.95 - xbar ) + ("squared and cubed too") , post$sigma )

#Then calcuate teh average of those simulated heights
mean(y)

#Then calcuated the probabilty invercal 
PI(y,prob=0.89)


unknown_weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
##oooor we could have used the link function 
mu <- link(m4.6,data = unknown_weights )



#looks like I computed striaght from the MAP and not the distribution, i'm not entirally sure
#how those two things are different at the moment
