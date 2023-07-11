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
    