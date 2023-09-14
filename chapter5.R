#Chapter 5
setwd("~/Desktop/DeZIM/Basteln-mit-R/statisitcal-rethinking")

library(rethinking)
library(tidyverse)
library(brms)
library(ggrepel)


data(WaffleDivorce)
d <- WaffleDivorce


#standardize variables

d <-
  d %>% 
  mutate(d = rethinking::standardize(Divorce),
         m = rethinking::standardize(Marriage),
         a = rethinking::standardize(MedianAgeMarriage))

glimpse(d)

#plot waffle houses vs marriage rate
d %>%
  ggplot(aes(x = WaffleHouses/Population, y = Divorce)) +
  stat_smooth(method = "lm", fullrange = T, linewidth = 1/2,
              color = "firebrick4", fill = "firebrick", alpha = 1/5) +
  geom_point(size = 1.5, color = "firebrick4", alpha = 1/2) +
  geom_text_repel(data = d %>% filter(Loc %in% c("ME", "OK", "AR", "AL", "GA", "SC", "NJ")),  
                  aes(label = Loc), 
                  size = 3, seed = 1042) +  # this makes it reproducible
  scale_x_continuous("Waffle Houses per million", limits = c(0, 55)) +
  ylab("Divorce rate") +
  coord_cartesian(xlim = c(0, 50), ylim = c(5, 15)) +
  theme_bw() +
  theme(panel.grid = element_blank())


#my first Brms

b5.1 <- 
  brm(data = d, 
      family = gaussian, 
      d ~ 1 + a, # means that both the intercept of the model and the 
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 5,
      sample_prior = T,
      file = "/Users/haller/Desktop/DeZIM/Basteln-mit-R/statisitcal-rethinking/fits/b05.01")




#sample the priors with brms

prior <- prior_draws(b5.1)
prior %>% glimpse()

#we want to plot but not everything so we use slice sample to take a random chunk of our sample
set.seed(5)

prior %>% 
  slice_sample(n = 50) %>%
  rownames_to_column("draw")  %>% 
  expand_grid(a = c(-2, 2)) %>% 
  mutate(d = Intercept + b * a) 
  
  ggplot(aes(x = a, y = d)) +
  geom_line(aes(group = draw),
            color = "firebrick", alpha = .4) +
  labs(x = "Median age marriage (std)",
       y = "Divorce rate (std)") +
  coord_cartesian(ylim = c(-2, 2)) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
  
  
  
  #get the posteriorsi predictions
  # determine the range of `a` values we'd like to feed into `fitted()`
  nd <- tibble(a = seq(from = -3, to = 3.2, length.out = 30))

  
  # now use `fitted()` to get the model-implied trajectories
  fitted(b5.1,
         newdata = nd) %>% 
    data.frame() %>% 
    bind_cols(nd)   


  

# Multiple linear regressions ---------------------------------------------

  b5.3 <- 
    brm(data = d, 
        family = gaussian,
        d ~ 1 + m + a,
        prior = c(prior(normal(0, 0.2), class = Intercept),
                  prior(normal(0, 0.5), class = b),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 5,
        file = "fits/b05.03")
  
  
  N <- 50 # number of simulated States
  age <- rnorm( N ) # sim A 
  mar<-rnorm(N,-age) #simA->M 
  div<-rnorm(N, age) #simA->D
  
  
  

# Predictor residual plots ------------------------------------------------

  #predict marriage rate using age
  b5.4 <- 
    brm(data = d, 
        family = gaussian,
        m ~ 1 + a,
        prior = c(prior(normal(0, 0.2), class = Intercept),
                  prior(normal(0, 0.5), class = b),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 5,
        file = "fits/b05.04")

#fitted gives us the expected value for each state
  f <- 
    fitted(b5.4) %>%
    data.frame() %>%
    bind_cols(d)
  
  glimpse(f)  

  #could make a few plots
  
  r <- 
    residuals(b5.4) %>%
    # to use this in ggplot2, we need to make it a tibble or data frame
    data.frame() %>% 
    bind_cols(d)
  
  
  
  

# Masked relationships ----------------------------------------------------


  
  data(milk, package = "rethinking")
  d <- milk
  rm(milk)
  
  glimpse(d)  

  #standardize variables
  d <-
    d %>% 
    mutate(kcal.per.g_s     = (kcal.per.g - mean(kcal.per.g)) / sd(kcal.per.g), 
           log_mass_s       = (log(mass) - mean(log(mass))) / sd(log(mass)), 
           neocortex.perc_s = (neocortex.perc - mean(neocortex.perc, na.rm = T)) / sd(neocortex.perc, na.rm = T))
  
  
#draft of model
  b5.5_draft <- 
    brm(data = d, 
        family = gaussian,
        kcal.per.g_s ~ 1 + neocortex.perc_s,
        prior = c(prior(normal(0, 1), class = Intercept),
                  prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 5,
        sample_prior = T,
        file = "fits/b05.05_draft")
  
  #WE HAVE NAS 
  #drop rows with NA (eventhough BRMS does that)
  dcc <- 
    d %>%
    drop_na(ends_with("_s"))
  
  #update the model with the new data
  b5.5_draft <- 
    update(b5.5_draft,
           newdata = dcc,
           seed = 5)
  
  
  # i don't know why but my absoulte favorite thing to do is to simulate priors
  #it is like a new fun tool that brings a much deeper understanding
  
  set.seed(5)
  
  prior_draws(b5.5_draft) %>% 
    slice_sample(n = 50) %>% 
    rownames_to_column() %>% 
    expand_grid(neocortex.perc_s = c(-2, 2)) %>% 
    mutate(kcal.per.g_s = Intercept + b * neocortex.perc_s) %>% 
    
    ggplot(aes(x = neocortex.perc_s, y = kcal.per.g_s)) +
    geom_line(aes(group = rowname),
              color = "firebrick", alpha = .4) +
    coord_cartesian(ylim = c(-2, 2)) +
    labs(x = "neocortex percent (std)",
         y = "kilocal per g (std)",
         subtitle = "Intercept ~ dnorm(0, 1)\nb ~ dnorm(0, 1)") +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  
  #better priors
  b5.5 <- 
    brm(data = dcc, 
        family = gaussian,
        kcal.per.g_s ~ 1 + neocortex.perc_s,
        prior = c(prior(normal(0, 0.2), class = Intercept),
                  prior(normal(0, 0.5), class = b),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 5,
        sample_prior = T,
        file = "fits/b05.05")
  
  nd <- tibble(neocortex.perc_s = seq(from = -2.5, to = 2, length.out = 30))
  
  fitted(b5.5, 
         newdata = nd,
         probs = c(.025, .975, .25, .75)) %>%
    data.frame() %>%
    bind_cols(nd) %>% 
    
    ggplot(aes(x = neocortex.perc_s, y = Estimate)) +
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
                fill = "firebrick", alpha = 1/5) +
    geom_smooth(aes(ymin = Q25, ymax = Q75),
                stat = "identity",
                fill = "firebrick4", color = "firebrick4", alpha = 1/5, linewidth = 1/2) +
    geom_point(data = dcc, 
               aes(x = neocortex.perc_s, y = kcal.per.g_s),
               size = 2, color = "firebrick4") +
    coord_cartesian(xlim = range(dcc$neocortex.perc_s), 
                    ylim = range(dcc$kcal.per.g_s)) +
    labs(x = "neocortex percent (std)",
         y = "kilocal per g (std)") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  
  #model with both coefficents neocortx, logmass
  b5.7 <- 
    brm(data = dcc, 
        family = gaussian,
        kcal.per.g_s ~ 1 + neocortex.perc_s + log_mass_s,
        prior = c(prior(normal(0, 0.2), class = Intercept),
                  prior(normal(0, 0.5), class = b),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 5,
        file = "fits/b05.07")
  
  
  #coeftab plot
  bind_cols(
    as_draws_df(b5.5) %>% 
      transmute(`b5.5_beta[N]` = b_neocortex.perc_s),
    as_draws_df(b5.6) %>% 
      transmute(`b5.6_beta[M]` = b_log_mass_s),
    as_draws_df(b5.7) %>% 
      transmute(`b5.7_beta[N]` = b_neocortex.perc_s,
                `b5.7_beta[M]` = b_log_mass_s)
  ) %>% 
    pivot_longer(everything()) %>% 
    group_by(name) %>% 
    summarise(mean = mean(value),
              ll   = quantile(value, prob = .025),
              ul   = quantile(value, prob = .975)) %>% 
    separate(name, into = c("fit", "parameter"), sep = "_") %>% 
    # complete(fit, parameter) %>% 
    
    ggplot(aes(x = mean, y = fit, xmin = ll, xmax = ul)) +
    geom_pointrange(color = "firebrick") +
    geom_hline(yintercept = 0, color = "firebrick", alpha = 1/5) +
    ylab(NULL) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "transparent", color = "transparent")) +
    facet_wrap(~ parameter, ncol = 1, labeller = label_parsed)
  
  

# Categorical variables ---------------------------------------------------


  data(Howell1)
  d <- Howell1
  d$sex <- ifelse( d$male==1 , 2 , 1 )
  
  d <-
    d %>% 
    mutate(sex = factor(sex))
  
  b5.8 <- 
    brm(data = d, 
        family = gaussian,
        height ~ 0 + sex,
        prior = c(prior(normal(178, 20), class = b),
                  prior(uniform(0, 50), class = sigma, ub = 50)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 5,
        file = "fits/b05.08")
  
  
  print(b5.8)
  
  #how to get typical model output when requesting two different intercepts
  library(tidybayes)
  
  as_draws_df(b5.8) %>% 
    mutate(diff_fm = b_sex1 - b_sex2) %>% 
    pivot_longer(cols = c(b_sex1:sigma, diff_fm)) %>% 
    group_by(name) %>% 
    mean_qi(value, .width = .89)
  
  
  #now many categories 
  data(milk)
  d <- milk
  class(d$clade)
  
  #standardize nutritional density 
  d <-
    d %>% 
    mutate(kcal.per.g_s = (kcal.per.g - mean(kcal.per.g)) / sd(kcal.per.g))
  
  
  b5.9 <- 
    brm(data = d, 
        family = gaussian,
        kcal.per.g_s ~ 0 + clade,
        prior = c(prior(normal(0, 0.5), class = b),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 5,
        file = "fits/b05.09")
  
  
  
  mcmc_plot(b5.9, variable = "^b_", regex = TRUE)
sessionInfo()  





# Questions ---------------------------------------------------------------

data(WaffleDivorce)
d <- WaffleDivorce

lds <- read.csv('/Users/haller/Desktop/lds-data-2021.csv')
colnames(lds) <- c('Location', 'members', 'population')

#standardize percet lds
lds <- lds %>% 
  mutate(pct_lds = members/population) %>% 
  mutate(lds = rethinking::standardize(pct_lds)) %>% 
  select(Location, lds)


#join lds with waffle data
d <- inner_join(d, lds, by = 'Location')

#standardize divorce, marriage, and median age
d <-
  d %>% 
  mutate(d = rethinking::standardize(Divorce),
         m = rethinking::standardize(Marriage),
         a = rethinking::standardize(MedianAgeMarriage))


sd(d$lds)

bM4.5 <- 
  brm(data = d, 
      family = gaussian,
      d ~ 1 + m + a + lds,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 5,
      file = "fits/bM4.5")


bM4.5a_nolds <- 
  update(bM4.5,
         newdata = d, 
         formula = d ~ 1 + m + a,
         seed = 5,
         file = "fits/bM4.5_nolds")




#Coef plot of function
mcmc_plot(bM4.5, variable = "^b_", regex = TRUE)



#lets look at coef plot with and without lds as a predictor variable
# first, extract and rename the necessary posterior parameters
bind_cols(
  as_draws_df(bM4.5) %>% 
    transmute(`bM4.5_beta[A]` = b_a,
              `bM4.5_beta[M]` = b_m,
              `bM4.5_beta[lds]` = b_lds),
  as_draws_df(bM4.5a_nolds) %>% 
    transmute(`bM4.5a_beta[A]` = b_a,
              `bM4.5a_beta[M]` = b_m)
)  %>% 
  # convert them to the long format, group, and get the posterior summaries
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value),
            ll   = quantile(value, prob = .025),
            ul   = quantile(value, prob = .975)) %>% 
  # since the `key` variable is really two variables in one, here we split them up
  separate(col = name, into = c("fit", "parameter"), sep = "_") %>% 
  
  # plot!
  ggplot(aes(x = mean, xmin = ll, xmax = ul, y = fit)) +
  geom_vline(xintercept = 0, color = "firebrick", alpha = 1/5) +
  geom_pointrange(color = "firebrick") +
  labs(x = "posterior", y = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "transparent", color = "transparent")) +
  facet_wrap(~ parameter, ncol = 1, labeller = label_parsed)

##answer
library(tidybayes)
spread_draws(bM4.5, `b_.*`, regex = TRUE) %>% 
  pivot_longer(starts_with("b_"), names_to = "parameter",
               values_to = "value") %>% 
  ggplot(aes(x = value, y = parameter)) +
  stat_halfeye(.width = c(0.67, 0.89, 0.97)) +
  labs(x = "Parameter Value", y = "Parameter")


## let's try this again but now looking at the log of lds
data(WaffleDivorce)
d <- WaffleDivorce

lds <- read.csv('/Users/haller/Desktop/lds-data-2021.csv')

lds <- lds %>%   mutate(lds_prop = members / population,
         lds_per_capita = lds_prop * 100000)



lds_divorce <- WaffleDivorce %>%
  as_tibble() %>%
  select(Location, Divorce, Marriage, MedianAgeMarriage) %>%
  left_join(select(lds, state, lds_per_capita),
            by = c("Location" = "state")) %>%
  mutate(lds_per_capita = log(lds_per_capita)) %>%
  mutate(across(where(is.numeric), standardize)) %>% 
  filter(!is.na(lds_per_capita)) %>% 
  rename(d = Divorce, m = Marriage, a = MedianAgeMarriage, log.lds = lds_per_capita)


l.lds <- brm(d ~ 1 + m + a + log.lds,
               data = lds_divorce, family = gaussian,
               prior = c(prior(normal(0, 0.2), class = Intercept),
                         prior(normal(0, 0.5), class = b),
                         prior(exponential(1), class = sigma)),
               iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
               file =  "fits/l.lds")

l.ldsa <- 
  update(l.lds,
         newdata = lds_divorce, 
         formula = d ~ 1 + m + a,
         seed = 1234,
         file = "fits/l.ldsa")




#Coef plot of function
mcmc_plot(l.lds, variable = "^b_", regex = TRUE)

mcmc_plot(l.ldsa, variable = "^b_", regex = TRUE)


#lets look at coef plot with and without lds as a predictor variable
# first, extract and rename the necessary posterior parameters
bind_cols(
  as_draws_df(l.lds) %>% 
    transmute(`l.lds_beta[A]` = b_a,
              `l.lds_beta[M]` = b_m,
              `l.lds_beta[lds]` = b_log.lds),
  as_draws_df(l.ldsa) %>% 
    transmute(`l.ldsa_beta[A]` = b_a,
              `l.ldsa_beta[M]` = b_m)
)  %>% 
  # convert them to the long format, group, and get the posterior summaries
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value),
            ll   = quantile(value, prob = .025),
            ul   = quantile(value, prob = .975)) %>% 
  # since the `key` variable is really two variables in one, here we split them up
  separate(col = name, into = c("fit", "parameter"), sep = "_") %>% 
  
  # plot!
  ggplot(aes(x = mean, xmin = ll, xmax = ul, y = fit)) +
  geom_vline(xintercept = 0, color = "firebrick", alpha = 1/5) +
  geom_pointrange(color = "firebrick") +
  labs(x = "posterior", y = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "transparent", color = "transparent")) +
  facet_wrap(~ parameter, ncol = 1, labeller = label_parsed)

##answer
library(tidybayes)
spread_draws(l.lds, `b_.*`, regex = TRUE) %>% 
  pivot_longer(starts_with("b_"), names_to = "parameter",
               values_to = "value") %>% 
  ggplot(aes(x = value, y = parameter)) +
  stat_halfeye(.width = c(0.67, 0.89, 0.97)) +
  labs(x = "Parameter Value", y = "Parameter")





#not a huge difference! turns out i read the graph wrong before since the 
#variabels were in a different order, but that's a dub.



###### 5H1 ##########
library(dagitty)



dag.5h1 <- dagitty('dag{ M -> A -> D }')
impliedConditionalIndependencies( dag.5h1 )
equivalentDAGs(dag.5h1)

####### 5H2 ########
#create a counterfactual plot assuming the real relationship is M → A → D
#of cutting the mariage rate in half


data(WaffleDivorce)
d <- WaffleDivorce

#standardize our variables
d <-
  d %>% 
  mutate(d = rethinking::standardize(Divorce),
         m = rethinking::standardize(Marriage),
         a = rethinking::standardize(MedianAgeMarriage))

a_model <- bf(a ~ 1 + m)
d_model <- bf(d ~ 1 + a + m)

H51 <- brm(data = d,
           family = gaussian,
           d_model + a_model + set_rescor(FALSE),
           prior = c(prior(normal(0, 0.2), class = Intercept, resp = d),
                     prior(normal(0, 0.5), class = b, resp = d),
                     prior(exponential(1), class = sigma, resp = d),
                     
                     prior(normal(0, 0.2), class = Intercept, resp = a),
                     prior(normal(0, 0.5), class = b, resp = a),
                     prior(exponential(1), class = sigma, resp = a)),
                     iter = 2000, warmup = 1000, chains = 4, cores = 4,
                     seed = 5,
                     file = "fits/H51")
          



m_seq <- tibble(m = seq(from = -4, to = 0, length.out = 30),
                      a = 0)

p1 <-
  predict(H51,
          resp = "d",
          newdata = m_seq) %>% 
  data.frame() %>% 
  bind_cols(m_seq)


ggplot(p1, aes(x = m, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_smooth(stat = "identity",
              fill = "firebrick", color = "firebrick4", alpha = 1/5, linewidth = 1/4) +
  labs(subtitle = "Total counterfactual effect of M on D",
       x = "manipulated M",
       y = "counterfactual D") +
  coord_cartesian(ylim = c(-2, 2)) +
  theme_bw() +
  theme(panel.grid = element_blank()) 


#the data shows us that M-->D is small so makes sensce that this souldn' be huge either

               

#now to answer what's the expected causal effect of decreaseing median mariage rate

mean(d$Marriage) # = 20
sd(d$Marriage)

#ahhhh okay this is how we compute how many sd a half reduction change is
nd <- tibble(m = (c(20.114, 10) - 20.114) / 3.797905,
             a = 0)



p2 <- predict(H51,
        resp = "d",
        newdata = nd,
        summary = F) 



  p2 %>%
    data.frame() %>% 
  set_names("m20", "m10") %>% 
  mutate(difference = m20 - m10) %>% 
  summarise(mean = mean(difference))

  
  
  

# 5H3 ---------------------------------------------------------------------

 # Return to the milk energy model, m5.7.
  #Suppose that the true causal relationship among the variables is

  
  
  library(dagitty)
  
  
  dag.5h3 <- dagitty('dag{ M -> N ;
                           M -> K ;
                           N -> K}')
  equivalentDAGs(dag.5h3)
  #Now compute the counterfactual effect on K of doubling M
  
  
  
  #Load data
  data(milk, package = "rethinking")
  d <- milk
  rm(milk)
  
  glimpse(d)
  
  
  #standardize varaibles
  d <-
    d %>% 
    mutate(
          log_mass = log(mass),
          K = rethinking::standardize(kcal.per.g),
           M = rethinking::standardize(log_mass),
           N = rethinking::standardize(neocortex.perc)) %>% 
    drop_na(everything()) 
  
  #models
  #(for the counterfactual i'm guessing I model each variable, 
  #that I won't be changing)
  K_model <- bf(K ~ 1 + M + N)
  N_model <- bf(N ~ 1 + M)

  
  
  H53.1 <- brm(data = d,
             family = gaussian,
             K_model + N_model + set_rescor(FALSE), #no autocorelation between the models
             prior = c(prior(normal(0, 0.2), class = Intercept, resp = K),
                       prior(normal(0, 0.5), class = b, resp = K),
                       prior(exponential(1), class = sigma, resp = K),
                       
                       prior(normal(0, 0.2), class = Intercept, resp = N),
                       prior(normal(0, 0.5), class = b, resp = N),
                       prior(exponential(1), class = sigma, resp = N)),
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             seed = 5,
             file = "fits/H53.1")
print(H53.1)


#point perdiction for doubling M(ass)
mean_M <- mean(d$mass)
sd_M <- sd(d$mass)


nd <- tibble(M = (c(mean_M, mean_M*2) - mean_M) / sd_M,
             N = 0)


p2 <- predict(H53,
              resp = "K",
              newdata = nd,
              summary = F) 



p2 %>%
  data.frame() %>% 
  set_names("M14", "M28") %>% 
  mutate(difference = M14 - M28) %>% 
  summarise(mean = mean(difference))

## = .310  (now with log m .36)

#the average difference in K from doubling M is .31 meaning 
#larger mass means one third of the calores per gram

## in graph form 


#let's see sd change from -2 to 2
M_seq <- tibble(M = seq(from = -2, to = 2, length.out = 30),
                N = 0)

p1 <-
  predict(H53,
          resp = "K", #look at outcome on K var
          newdata = M_seq) %>% 
  data.frame() %>% 
  bind_cols(M_seq)


g1 <- ggplot(p1, aes(x = M, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_smooth(stat = "identity",
              fill = "firebrick", color = "firebrick4", alpha = 1/5, linewidth = 1/4) +
  labs(subtitle = "Total counterfactual effect of M on K",
       x = "manipulated M",
       y = "counterfactual K") +
  coord_cartesian(ylim = c(-2, 2)) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
g1




### answer key ##

#just the counterfactual plot (or nto)

milk_cf <- as_draws_df(H53.1) %>%
  as_tibble() %>% 
  select(.draw, b_K_Intercept:sigma_N) %>% 
  expand(nesting(.draw, b_K_Intercept, b_N_Intercept, b_K_M, b_K_N, b_N_M,
                 sigma_K, sigma_N),
         mass = seq(from = 0.5, to = 80, by = 0.5)) %>% 


  mutate(log_mass = log(mass),
         M = (log_mass - mean(d$log_mass)) / sd(d$log_mass),
         n_sim = rnorm(n(), mean = b_N_Intercept + b_N_M * M, sd = sigma_N),
         k_sim = rnorm(n(), mean = b_K_Intercept + b_K_N * n_sim + b_K_M * M,
                       sd = sigma_K)) %>% 


  pivot_longer(ends_with("_sim"), names_to = "name", values_to = "value") %>%
  group_by(mass, name) %>%
  mean_qi(value, .width = c(0.89)) %>%
  ungroup() %>%
  filter(name == "k_sim") %>%
  mutate(name = case_when(name == "n_sim" ~ "Counterfactual effect M on N",
                          TRUE ~ "Total Counterfactual effect of M on K"))

ggplot(milk_cf, aes(x = mass, y = value, ymin = .lower, ymax = .upper)) +
  geom_smooth(stat = "identity") +
  labs(x = "Manipulated Mass", y = "Counterfactual K")





(log(c(15, 30)) - mean(log(d$mass))) / sd(log(d$mass)) 
#> [1] 0.6253176 0.9839050

as_draws_df(H53.1) %>% 
  mutate(n_avg = rnorm(n(), b_N_Intercept + b_N_M * 0.6253176, sigma_N),
         n_dbl = rnorm(n(), b_N_Intercept + b_N_M * 0.9839050, sigma_N),
         k_avg = rnorm(n(), b_K_Intercept + b_K_M * 0.6253176 + b_K_N * n_avg,
                       sigma_K),
         k_dbl = rnorm(n(), b_K_Intercept + b_K_M * 0.9839050 + b_K_N * n_dbl,
                       sigma_K),
         diff = k_dbl - k_avg) %>% 
  median_hdi(diff, .width = 0.89)
#> # A tibble: 1 × 6
#>     diff .lower .upper .width .point .interval
#>    <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
#> 1 -0.151  -2.45   1.82   0.89 median hdi
