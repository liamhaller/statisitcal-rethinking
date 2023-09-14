#Chapter 7
library(rethinking)
library(tidybayes)
library(brms)
library(tidyverse)

#7E2
-1*((.7*log(.7))+ (.3*log(.3)))

#7E3

entropy <- function(p){
  -sum(p*log(p))
  
}
probs <- c(.2,.25,.25,.3)

#7E4
probs <- c(.333,.333,.333)

entropy(probs)
#1.093375



#7H1
data(Laffer)

#Fit model(s) that usee tax rate to predict tax revenue
#Compare, using WAIC or PSIS

laffer_dat <- Laffer %>%
  as_tibble() %>%
  mutate(across(everything(), standardize)) %>% 
  mutate(tax_rate2 =tax_rate^2 )


#Specify Models
H7.1 <- 
  brm(data = laffer_dat, 
      family = gaussian,
      tax_revenue ~ 1 + tax_rate, #line
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 5,
      file = "fits/H7.1")

H7.1c <- 
  brm(data = laffer_dat, 
      family = gaussian,
      tax_revenue ~ 1 + tax_rate + tax_rate2, #curve
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 5,
      file = "fits/H7.1c")



#Calculate Waic and add to mdel
H7.1 <- add_criterion(H7.1, criterion = "waic") 
H7.1c <- add_criterion(H7.1c, criterion = "waic") 

#compare models
w <- loo_compare(H7.1, H7.1c, criterion = "waic")
w


##Bonus: how to plot model to data

#sequence from min to max tax rate with 100 intervels
tr_seq <- tibble(tax_rate = seq(0, 40, length.out = 100)) %>% 
#then standardize each of those values with respect to the actual data, 
#so they have mean of the actual tax rate
  mutate(tax_rate = (tax_rate - mean(Laffer$tax_rate)) / sd(Laffer$tax_rate),
         tax_rate2 = tax_rate ^ 2)

#use new dataset to make predictions
predictions <- bind_rows(
  predicted_draws(H7.1, newdata = tr_seq) %>%
    median_qi(.width = 0.89) %>%
    mutate(type = "Linear"),
  predicted_draws(H7.1c, newdata = tr_seq) %>%
    median_qi(.width = 0.89) %>%
    mutate(type = "Quadratic"))

#add teh 69 and 97 estimates?
fits <- bind_rows(
  epred_draws(H7.1, newdata = tr_seq) %>%
    median_qi(.width = c(0.67, 0.89, 0.97)) %>%
    mutate(type = "Linear"),
  epred_draws(H7.1c, newdata = tr_seq) %>%
    median_qi(.width = c(0.67, 0.89, 0.97)) %>%
    mutate(type = "Quadratic"))


ggplot() +
  facet_wrap(~type, nrow = 1) +
  geom_ribbon(data = predictions,
              aes(x = tax_rate, ymin = .lower, ymax = .upper),
              alpha = 0.2) +
  geom_lineribbon(data = fits,
                  aes(x = tax_rate, y = .epred, ymin = .lower, ymax = .upper),
                  size = 0.6) +
  #overlay with roiginal data
  geom_point(data = laffer_dat, aes(x = tax_rate, y = tax_revenue),
             alpha = 0.5) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Blues'),
                    breaks = c(0.67, 0.89, 0.97)) +
  labs(x = "Standardized Tax Rate", y = "Standardized Tax Revenue",
       fill = "Interval")



# 7H2 ---------------------------------------------------------------------

# 7H2. In the Laffer data, there is one country with a high tax revenue that is an outlier. Use PSIS
# and WAIC to measure the importance of this outlier in the models you fit in the previous problem.
# Then use robust regression with a Student’s t distribution to revisit the curve fitting problem. How
# much does a curved relationship depend upon the outlier point?




#Identify which points are using PSIS
loo(H7.1) %>%
  pareto_k_ids(threshold = .5) 

#what is the value of the outleir
pareto_k_values(loo(H7.1))[12]

waci <- brms::waic(H7.1) #20 is huge outlier




library(gghighlight)

#get pointwise pareto and wais scores
criteria_influence <- function(model) {
  require(brms)
  
  model <- brms::add_criterion(model, criterion = c('loo', "waic"))
  tibble(pareto_k = model$criteria$loo$diagnostics$pareto_k,
         p_waic = model$criteria$waic$pointwise[, "p_waic"]) %>%
    rowid_to_column(var = "obs")
}


criteria_influence(H7.1)


influ <- bind_rows(
  criteria_influence(H7.1) %>%
    mutate(type = "Linear"))

#custom plot of influence 
ggplot(influ, aes(x = pareto_k, y = p_waic)) +
  facet_wrap(~type, nrow = 1) +
  geom_vline(xintercept = 0.7, linetype = "dashed") +
  geom_hline(yintercept = 0.4, linetype = "dashed") +
  geom_point() +
  gghighlight(pareto_k > 0.7 | p_waic > 0.4, n = 1, label_key = obs,
              label_params = list(size = 3)) +
  labs(x = "Pareto *k*", y = "p<sub>WAIC</sub>")



#refit the models using student t distribution
H7.1t <- brm(bf(tax_revenue ~ 1 + tax_rate, nu = 1), #whar does nu = 1 mean?
                 data = laf_dat, family = student, #!!!!!!
                 prior = c(prior(normal(0, 0.2), class = Intercept),
                           prior(normal(0, 0.5), class = b),
                           prior(exponential(1), class = sigma)),
                 iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                 file = here("fits/H7.1t"))

#comaprision is the same but intervals are a little narrower, quadratic si still slightly the best



#7H3

island1 <- c(rep(.2,5))
island2 <- c(.8,.1,.05,.025, .025)
island3 <- c(.05,.15,.7,.05,.05)

island <- data.frame(island1, island2, island3)

#compute the entropy for each island's bird distribution and interprete the values

entropy <- function(p){
  -sum(p*log(p))
  
}


sapply(island, entropy)

#island two has the lowest entropy meaning that there is less uncertianty contained in the probabilty distriution
#and therefore easier to predict, my guess is the reason that's teh case is because of the .8

#and that makes sense giventhe first would be the most difficult to predict 



cross_entropy <- function(p,q){
  
  sum(p*log(p/q))
}

#using island 1 to predict island 2 & 3
cross_entropy(island2, island1) + ## 0.866434
cross_entropy(island3, island1) ## 0.6258376

#using island 2 to predict island 1 & 3
cross_entropy(island1, island2) +## .97
cross_entropy(island3, island2) ## 1.8

#using island 3 to predict island 1 & 2
cross_entropy(island1, island3) +## 0.6387604
cross_entropy(island2, island3) ## 2.010914


#Total
#Island 1: 1.492272
#island 2: 2.809251
#island 3: 2.649675

#island one best predicts the other island since it assigns equal probabily to each distirbution
#therefore is the least suprised and leasat incorect

#high entropy models make for good predictors !!!, compared to other distributions which are more conenterated



#7H4

#conditiong on a collider improves preidctiosn but not causal interpretation.
#therefore must exercise caution before selecting model with PSIS or WAIC



#7H5
library(dagitty)


data(foxes)

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


#Use WAIC or PSIS based model comparison

# (1) avgfood + groupsize + area
# (2) avgfood + groupsize
# (3) groupsize + area
# (4) avgfood
# (5) area

# (1) avgfood + groupsize + area
H75.a <- 
  brm(data = f, 
      family = gaussian,
      w ~ 1 + avgf + gszie + a,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 6,
      file = "H75.a")


# (2) avgfood + groupsize
H75.b <- 
  brm(data = f, 
      family = gaussian,
      w ~ 1 + avgf + gszie,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 6,
      file = "H75.b")


# (3) groupsize + area
H75.c <- 
  brm(data = f, 
      family = gaussian,
      w ~ 1 + gszie + a,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 6,
      file = "H75.c")


# (4) avgfood

H75.d <- 
  brm(data = f, 
      family = gaussian,
      w ~ 1 + avgf,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 6,
      file = "H75.d")

# (5) area

H75.e <- 
  brm(data = f, 
      family = gaussian,
      w ~ 1 + a,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 6,
      file = "H75.e")


#Compare WAIC
#Calculate Waic and add to mdel
H75.a <- add_criterion(H75.a, criterion = "waic") 
H75.b <- add_criterion(H75.b, criterion = "waic") 
H75.c <- add_criterion(H75.c, criterion = "waic") 
H75.d <- add_criterion(H75.d, criterion = "waic") 
H75.e <- add_criterion(H75.e, criterion = "waic") 

#compare models
w <- loo_compare(H75.a, H75.b, H75.c, H75.d, H75.e, criterion = "waic")
w

#differences are smaller than the standard error meaning that the models are tied 
#in the view of WAIC

#the reason then the models are teh same is that area is routed entirally though
#average food so including average food should result in the same inferences
#a key observation that i missed was that there are two groups of models, those that ocntain group size and those without
