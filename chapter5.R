#Chapter 5

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

