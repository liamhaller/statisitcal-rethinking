#chapter 6
library(tidyverse)
library(dagitty)
library(tidybayes)
library(brms)
## 6M3


#number of data points
n    <- 200 

x_z <- 8  # strength of relationship between x and z
z_y <- 1  # strength of relationship between z and y

# simulate triads
set.seed(1)
d <-
  tibble(x = rnorm(n, mean = 1, sd = 1)) %>% 
  mutate(z = rnorm(n, mean = x_z*x, sd = .1)) %>% 
  mutate(y = rnorm(n, mean = z_y*z , sd = 5))



cor(x = d$x, y = d$z) # .99
cor(x = d$x, y = d$y) # .36
cor(x = d$z, y = d$y) # .36


summary(lm(y~ x + z, data = d))


#answer key,
#thier data generating was similar to mind

#but included this model 
b6m2 <- brm(y ~ 1 + x + z, data = dat, family = gaussian,
            prior = c(prior(normal(0, 0.2), class = Intercept),
                      prior(normal(0, 0.5), class = b),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = here("fits", "chp6", "b6m2"))

as_draws_df(b6m2) %>%
  as_tibble() %>% 
  select(b_Intercept, b_x, b_z, sigma) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value, y = name)) +
  stat_halfeye(.width = c(0.67, 0.89, 0.97))



### 6H1

#load data
data(WaffleDivorce, package = "rethinking")
d <- WaffleDivorce

# standardize the continuous focal variables.
d <-
  d %>% 
  mutate(a = rethinking::standardize(MedianAgeMarriage),
         d = rethinking::standardize(Divorce),
         m = rethinking::standardize(Marriage),
         s = factor(South, levels = 0:1, labels = c("North", "South")),
         w = rethinking::standardize(WaffleHouses))

# tidy up
rm(WaffleDivorce)




waffle_dag <- dagitty("dag { S -> W -> D <- A <- S -> M -> D; A -> M }")


adjustmentSets(waffle_dag, exposure = "W", outcome = "D")


b6.h1 <- 
  brm(data = d, 
      family = gaussian,
      d ~ 1 + w + s,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 6,
      file = "b6.h1")


draws <- as_draws_df(b6.h1)

draws %>% 
  pivot_longer(b_Intercept:sigma) %>% 
  ggplot(aes(x = value, y = name)) +
  stat_pointinterval(.width = .95, color = "forestgreen")



draws %>% 
  pivot_longer(b_Intercept:sigma) %>% 
  group_by(name) %>% 
  mean_qi(value, .width = .89)


## ANSWER KEY

spread_draws(b6.h1, b_w) %>% 
  ggplot(aes(x = b_w)) +
  stat_halfeye(.width = c(0.67, 0.89, 0.97)) +
  labs(x = expression(beta[W]), y = "Density")




##6H2


waffle_dag <- dagitty("dag { S -> W -> D <- A <- S -> M -> D; A -> M }")

#identify conditional dependencies
dagitty::impliedConditionalIndependencies(waffle_dag)

#load data
data(WaffleDivorce, package = "rethinking")
d <- WaffleDivorce

# standardize the continuous focal variables.
d <-
  d %>% 
  mutate(a = rethinking::standardize(MedianAgeMarriage),
         d = rethinking::standardize(Divorce),
         m = rethinking::standardize(Marriage),
         s = factor(South, levels = 0:1, labels = c("North", "South")),
         w = rethinking::standardize(WaffleHouses))

# tidy up
rm(WaffleDivorce)





#test first set A _||_ W | S

b6.h2.a <- 
  brm(data = d, 
      family = gaussian,
      a ~ 1 + w + s,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 6,
      file = "b6.h2.a")


draws <- as_draws_df(b6.h2.a)


draws %>% 
  pivot_longer(b_Intercept:sigma) %>% 
  ggplot(aes(x = value, y = name)) +
  stat_pointinterval(.width = .95, color = "forestgreen")


draws %>% 
  pivot_longer(b_Intercept:sigma) %>% 
  group_by(name) %>% 
  mean_qi(value, .width = .89)


#test second set D _||_ S | A, M, W

b6.h2.c <- 
  brm(data = d, 
      family = gaussian,
      m ~ 1 + w + s,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 6,
      file = "b6.h2.c")


draws <- as_draws_df(b6.h2.c)


draws %>% 
  pivot_longer(b_Intercept:sigma) %>% 
  ggplot(aes(x = value, y = name)) +
  stat_pointinterval(.width = .95, color = "forestgreen")


draws %>% 
  pivot_longer(b_Intercept:sigma) %>% 
  group_by(name) %>% 
  mean_qi(value, .width = .89)




#M _||_ W | S


#answer key, gathered draws, and then stathalf eye to look at spread of an indidvual varaible

lbls <- c(expression("Model 1:"~beta[W]),
          expression("Model 2:"~beta[S]),
          expression("Model 3:"~beta[W]))

bind_rows(
  gather_draws(waff_ci1, b_W) %>%
    ungroup() %>%
    mutate(model = "ICI 1"),
  gather_draws(waff_ci2, b_S1) %>%
    ungroup() %>%
    mutate(model = "ICI 2"),
  gather_draws(waff_ci3, b_W) %>%
    ungroup() %>%
    mutate(model = "ICI 3")
) %>%
  ggplot(aes(x = .value, y= model)) +
  stat_halfeye(.width = c(0.67, 0.89, 0.97)) +
  scale_y_discrete(labels = lbls) +
  labs(x = "Parameter Estimate", y = "Implied Conditional Independency")



#6H3
#load data
data(foxes, package = "rethinking")
f <- foxes

# standardize the continuous focal variables.
f <-
  f %>% 
  mutate(avgf = rethinking::standardize(avgfood),
         gszie = rethinking::standardize(groupsize),
         a = rethinking::standardize(area),
         g = as.factor(group),
         w = rethinking::standardize(weight))
rm(foxes)



#Use a model to infer the total causal influence of area on weight

fox_dag <- dagitty("dag { A -> F -> G -> W; F -> W }")

adjustmentSets(fox_dag, exposure = "A", outcome = "W")


#add nothing to the model 


#understand priors for the model, simulation.

set.seed(2971)
# how many lines would you like?
n_lines <- 100


tibble(n = 1:n_lines,
       alpha = rnorm(n_lines, mean = 0, sd = 0.2), #simulate intercept
       beta = rnorm(n_lines, mean = 0, sd = .5)) %>% #sim beta
  expand_grid(weight = range(f$w)) %>% 
  mutate(w = alpha + beta*f$a ) %>% 
  
  
  ggplot(aes(x = weight, y = a, group = n)) +
  geom_line(alpha = 1/10) +
  theme_classic()


#prior are not particurally informative


b6.h3.a <- 
  brm(data = f, 
      family = gaussian,
      w ~ 1 + a,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 6,
      file = "b6.h3.a")


draws <- as_draws_df(b6.h3.a)

draws %>% 
  pivot_longer(b_Intercept:sigma) %>% 
  ggplot(aes(x = value, y = name)) +
  stat_pointinterval(.width = .95, color = "forestgreen")

draws %>% 
  pivot_longer(b_Intercept:sigma) %>% 
  group_by(name) %>% 
  mean_qi(value, .width = .89)



#### answer key

#create prior preditive simulation
n <- 1000

#Manufacture data set
#1. simulate distributions for parameters
#2. set range of values that you'd like to observe (mutate area)
#3. simulate regression for each observation (mutate weight)


#Graphing
#1. Calculate reasonable bounds for observations
no_weight <- (0 - mean(f$weight)) / sd(f$weight) #this is zero weight, repressented in standaredized tersm
max_weight <-  (max(f$weight) - mean(f$weight)) /  sd(f$weight) #the max weight obseved represted in standardized terms

tibble(group = seq_len(n),
       alpha = rnorm(n, 0, 0.2),
       beta = rnorm(n, 0, 0.5))  %>%  #only need to sim single beta (here there's only one)
  expand(nesting(group, alpha, beta),
         area = seq(from = -2, to = 2, length.out = 100)) %>% 
  mutate(weight = alpha + beta * area) %>% 
  
  
  
  ggplot(aes(x = area, y = weight, group = group)) +
  geom_line(alpha = 1 / 10) +
  
  #visualize extreme bounds 
  geom_hline(yintercept = c(no_weight, max_weight),
             linetype = c("dashed", "solid"), color = "red") +
  annotate(geom = "text", x = -2, y = no_weight, hjust = 0, vjust = 1,
           label = "No weight") +
  annotate(geom = "text", x = -2, y = max_weight, hjust = 0, vjust = 0,
           label = "Maximum weight") +
  expand_limits(y = c(-4, 4)) +
  #add text
  labs(x = "Standardized Area", y = "Standardized Weight")




#curent prefered method of looking at variables

draws <- as_draws_df(b6.h3.a)


draws %>%   as_tibble() %>%
  select(b_Intercept, b_a, sigma) %>%
  pivot_longer(everything()) %>%
  mutate(name = factor(name, levels = c("b_Intercept", "b_a", "sigma"))) %>%
  ggplot(aes(x = value, y = fct_rev(name))) +
  stat_halfeye(.width = c(0.67, 0.89, 0.97)) +
  labs(x = "Parameter Estimate", y = "Parameter")
