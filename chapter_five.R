
library(rethinking)

## 5M4

# In the divorce data, States with high numbers of members of the Church of Jesus Christ of Latter-day Saints (LDS) 
# have much lower divorce rates than the regression models expected. Find a list of LDS population by State and use 
# those numbers as a predictor variable, predicting divorce rate using marriage rate, median age at marriage,
# and percent LDS population (possibly standardized). You may want to consider transformations of the raw percent LDS variable.


data(WaffleDivorce)
d <- WaffleDivorce
d$pct_LDS <- c(0.75, 4.53, 6.18, 1, 2.01, 2.82, 0.43, 0.55, 0.38,
               0.75, 0.82, 5.18, 26.35, 0.44, 0.66, 0.87, 1.25, 0.77, 0.64, 0.81,
               0.72, 0.39, 0.44, 0.58, 0.72, 1.14, 4.78, 1.29, 0.61, 0.37, 3.34,
               0.41, 0.82, 1.48, 0.52, 1.2, 3.85, 0.4, 0.37, 0.83, 1.27, 0.75,
               1.21, 67.97, 0.74, 1.13, 3.99, 0.92, 0.44, 11.5 )


#standardize variables

zscore <- function(vector){
  
  zscore <- (vector - mean(vector))/sd(vector)
  return(zscore)
  
}


d$A <- zscore(d$MedianAgeMarriage)
d$M <- zscore(d$Marriage)
d$LDS <- zscore(d$pct_LDS)
d$D <- zscore(d$Divorce)



m5.4 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A + bLD*LDS,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    bLD ~ dnorm(0, .9 ) , 
    sigma ~ dexp( 1 )
  ) , data = d )
precis( m5.4 )



plot( coeftab(m5.4), par=c("bA","bM", "bLD") )



# One way to reason through multiple causation hypotheses is to imagine detailed mechanisms through which
# predictor variables may influence outcomes. For example, it is sometimes argued that the price of gasoline
# (predictor variable) is positively associated with lower obesity rates (outcome variable). However, there are
# at least two important mechanisms by which the price of gas could reduce obesity. First, it could lead to less
# driving and therefore more exercise. Second, it could lead to less driving, which leads to less eating out,
# which leads to less consumption of huge restaurant meals. Can you outline one or more multiple regressions
# that address these two mechanisms? Assume you can have any predictor data you need.










