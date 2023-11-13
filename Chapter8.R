#Chapter 8

library(rethinking)
library(tidybayes)
library(brms)
library(tidyverse)

data(tulips, package = "rethinking")
d <- tulips
rm(tulips)

glimpse(d)


d <-
  d %>% 
  mutate(blooms_std = blooms / max(blooms),
         water_cent = water - mean(water),
         shade_cent = shade - mean(shade))



sd(log(d$shade))



M8.4a <-
  brm(data = d, 
      family = gaussian,
      blooms_std ~ 1 + water + shade + water:shade,
      prior = c(prior(normal(0.5, 0.25), class = Intercept),
                prior(lognormal(0.5, 0.25), class = b, coef = water),
                prior(lognormal(0.5, 0.25), class = b, coef = shade),
                prior(lognormal(0.5, 0.25), class = b, coef = "water:shade"),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 8,
      file = "fits/M8.4a")


print()

###

tulip_dat <- tulips %>%
  as_tibble() %>%
  mutate(light = -1 * shade,
         blooms_std = blooms / max(blooms),
         water_cent = water - mean(water),
         shade_cent = shade - mean(shade),
         light_cent = light - mean(light))

#By making light we can then say the effect of light should always be positive

b8m4 <- brm(blooms_std ~ 1 + water_cent + light_cent + water_cent:light_cent,
            data = tulip_dat, family = gaussian,
            prior = c(prior(normal(0.5, 0.25), class = Intercept),
                      prior(normal(0, 0.25), class = b, lb = 0), #set lower bound equal to zero
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = "fits/b8m4")
summary(b8m4)



##Plot the posterior distribution



#Step 1: Our data
points <-
  tulip_dat %>%
  expand_grid(fit = c("b8m4")) %>% #the rows will be the models (here only one)
  mutate(x_grid = str_c("light_cent = ", light_cent), #the columns will be the different levels of shade
         y_grid = fit)


# redefine the points in each plot we ill plot ver
nd <- crossing(light_cent = -1:1, 
               water_cent = c(-1, 1))


# use `fitted()`
set.seed(8)

fitted(b8m4, newdata = nd, summary = F, ndraws = 20) %>% 
  data.frame() %>% 
  set_names(mutate(nd, name = str_c(light_cent, water_cent, sep = "_")) %>% pull()) %>%  #Set dataframe names as values
  mutate(row = 1:n(),
       fit = rep(c("b8m4"),n())) %>% #specify row and fit 
  pivot_longer(-c(row:fit), values_to = "blooms_std") %>%  #move data for each column into a single row since we're preding blooms
  separate(name, into = c("light_cent", "water_cent"), sep = "_") %>%  #now undo the name and get shade and light columns back
  mutate(light_cent = light_cent %>% as.double(),
         water_cent = water_cent %>% as.double()) %>%
  mutate(x_grid = str_c("light_cent = ", light_cent),
         y_grid = fit) %>% 
  
  # plot!
  ggplot(aes(x = water_cent, y = blooms_std)) +
  geom_line(aes(group = row),
            color = palette_pander(n = 6)[6], alpha = 1/5, linewidth = 1/2) +
  geom_point(data = points,
             color = palette_pander(n = 6)[6]) +
  scale_x_continuous("Water (centered)", breaks = c(-1, 0, 1)) +
  scale_y_continuous("Blooms (standardized)", breaks = c(0, .5, 1)) +
  ggtitle("Posterior predicted blooms") +
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(0, 1)) +
  theme(strip.background = element_rect(fill = alpha(palette_pander(n = 2)[2], 1/3))) +
  facet_grid(y_grid ~ x_grid)
  
  
#Now for prior preditive 


b8m4p <-
  update(b8m4,
         sample_prior = "only", #only take the prior
         iter = 2000, warmup = 1000, chains = 4, cores = 4,
         seed = 8,
         file = "fits/b8m4p")


#now we can copy the same code and plot

fitted(b8m4p, newdata = nd, summary = F, ndraws = 20) %>% 
  data.frame() %>% 
  set_names(mutate(nd, name = str_c(light_cent, water_cent, sep = "_")) %>% pull()) %>%  #Set dataframe names as values
  mutate(row = 1:n(),
         fit = rep(c("b8m4"),n())) %>% #specify row and fit 
  pivot_longer(-c(row:fit), values_to = "blooms_std") %>%  #move data for each column into a single row since we're preding blooms
  separate(name, into = c("light_cent", "water_cent"), sep = "_") %>%  #now undo the name and get shade and light columns back
  mutate(light_cent = light_cent %>% as.double(),
         water_cent = water_cent %>% as.double()) %>%
  mutate(x_grid = str_c("light_cent = ", light_cent),
         y_grid = fit) %>% 
  
  # plot!
  ggplot(aes(x = water_cent, y = blooms_std)) +
  geom_line(aes(group = row),
            color = palette_pander(n = 6)[6], alpha = 1/5, linewidth = 1/2) +
  geom_point(data = points,
             color = palette_pander(n = 6)[6]) +
  scale_x_continuous("Water (centered)", breaks = c(-1, 0, 1)) +
  scale_y_continuous("Blooms (standardized)", breaks = c(0, .5, 1)) +
  ggtitle("Posterior predicted blooms") +
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(0, 1)) +
  theme(strip.background = element_rect(fill = alpha(palette_pander(n = 2)[2], 1/3))) +
  facet_grid(y_grid ~ x_grid)




## 8H1


data(tulips, package = "rethinking")



tulips <-
  tulips %>% 
  mutate(blooms_std = blooms / max(blooms),
         water_cent = water - mean(water),
         shade_cent = shade - mean(shade),
         bed = factor(bed))



b8h1 <- brm(blooms_std ~ 0 + water_cent + light_cent + water_cent:light_cent + bed,
            data = tulip_dat, family = gaussian,
            prior =   c(prior(normal(0, 0.25), class = b), 
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = "fits/b8h1")
summary(b8h1)

summary(b8m4)

mcmc_plot(b8h1, variable = "^b_", regex = TRUE)
mcmc_plot(b8m4, variable = "^b_", regex = TRUE)


#8H2

b8h1 <- add_criterion(b8h1, criterion = "waic") 
b8m4 <- add_criterion(b8m4, criterion = "waic") 

#compare models
w <- loo_compare(b8h1, b8m4, criterion = "waic")
w


#the standard error of the difference is larger than the magnitude of the difference, indicating that the WAIC is not able to meaningfully differentiate between the two models. 

#the model with the bed apears to be a better predictor which makes sense given the differences between the beds
#as compared to a single intercept as they appear statisclly different from easch other




#8H3

data(rugged, package = "rethinking")
d <- rugged
rm(rugged)

d <- 
  d %>%
  mutate(log_gdp = log(rgdppc_2000))

dd <-
  d %>%
  filter(complete.cases(rgdppc_2000)) %>% 
  # re-scale variables
  mutate(log_gdp_std = log_gdp / mean(log_gdp), 
         rugged_std  = rugged / max(rugged),
         rugged_std_c  = rugged_std - mean(rugged_std),
         cid = if_else(cont_africa == 1, "1", "2"))
         

b8.3 <- 
  brm(data = dd, 
      family = gaussian,
      bf(log_gdp_std ~ 0 + a + b * rugged_std_c, 
         a ~ 0 + cid, 
         b ~ 0 + cid,
         nl = TRUE),
      prior = c(prior(normal(1, 0.1), class = b, coef = cid1, nlpar = a),
                prior(normal(1, 0.1), class = b, coef = cid2, nlpar = a),
                prior(normal(0, 0.3), class = b, coef = cid1, nlpar = b),
                prior(normal(0, 0.3), class = b, coef = cid2, nlpar = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 8,
      file = "fits/b08.03")




library(gghighlight)

#get pointwise pareto and wais scores
criteria_influence <- function(model) {
  require(brms)
  
  model <- brms::add_criterion(model, criterion = c('loo', "waic"))
  tibble(pareto_k = model$criteria$loo$diagnostics$pareto_k,
         p_waic = model$criteria$waic$pointwise[, "p_waic"]) %>%
    rowid_to_column(var = "obs")
}


b8.3  <- add_criterion(b8.3, c("loo", "waic"))

criteria_influence(b8.3)


influ <- bind_rows(
  criteria_influence(b8.3) %>%
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


dd[145,3]
dd[27,3]


#switzerland is the othe routlier probably because it is economolyy successful because it is able to isolate itself from violence in Euorpe

b8h3_t <- 
  brm(data = dd, 
      family = student,
      bf(log_gdp_std ~ 0 + a + b * rugged_std_c, 
         a ~ 0 + cid, 
         b ~ 0 + cid,
         nu = 2,
         nl = TRUE),
      prior = c(prior(normal(1, 0.1), class = b, coef = cid1, nlpar = a),
                prior(normal(1, 0.1), class = b, coef = cid2, nlpar = a),
                prior(normal(0, 0.3), class = b, coef = cid1, nlpar = b),
                prior(normal(0, 0.3), class = b, coef = cid2, nlpar = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 8,
      file = "fits/b8h3_t")


b8h3_t <- add_criterion(b8h3_t, criterion = c("loo", "waic"))



criteria_influence(b8h3_t)


influ <- bind_rows(
  criteria_influence(b8h3_t) %>%
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





n_diff <- spread_draws(b8.3, b_b_cid1, b_b_cid2) %>% 
  mutate(diff = b_b_cid1 - b_b_cid2)

t_diff <- spread_draws(b8h3_t, b_b_cid1, b_b_cid2) %>% 
  mutate(diff = b_b_cid1 - b_b_cid2)

ggplot() +
  geom_density(data = n_diff, aes(x = diff, fill = "Normal"),
               color = NA, alpha = 0.6) +
  geom_density(data = t_diff, aes(x = diff, fill = "Student's *t*"),
               color = NA, alpha = 0.6) +
  scale_fill_manual(values = c("#009FB7", "#FED766")) +
  labs(x = "African &minus; Non-African", y = "Density", fill = NULL) +
  theme(legend.text = element_markdown())



# 8H4 ---------------------------------------------------------------------


data(nettle, package = "rethinking")


#Outcome variable
nettle$log.lang.per.cap <- log(nettle$num.lang / nettle$k.pop)


nettle <-
  nettle %>% 
  mutate(mean.grow_std = rethinking::standardize(mean.growing.season),
         sd.grow_cent = rethinking::standardize(sd.growing.season))


bh84a <- brm(log.lang.per.cap ~ 1 + mean.grow_std + sd.grow_cent,
            data = nettle, family = gaussian,
            prior =   c(prior(normal(0.25, 0.5), class = b, coef = mean.grow_std), 
                        prior(normal(0, 0.5), class = b, coef = sd.grow_cent), 
                        prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = "fits/bh84a")



summary(bh84a)


bh84b <- brm(log.lang.per.cap ~ 1 + mean.grow_std + sd.grow_cent + mean.grow_std:sd.grow_cent,
             data = nettle, family = gaussian,
             prior =   c(prior(normal(0.25, 0.5), class = b, coef = mean.grow_std), 
                         prior(normal(0, 0.5), class = b, coef = sd.grow_cent), 
                         prior(normal(0, 0.5), class = b, coef = "mean.grow_std:sd.grow_cent"), 
                         prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = "fits/bh84b")

summary(bh84b)



#Compare models


mcmc_plot(bh84a, variable = "^b_", regex = TRUE)
mcmc_plot(bh84b, variable = "^b_", regex = TRUE)


#8H2

bh84a <- add_criterion(bh84a, criterion = "waic") 
bh84b <- add_criterion(bh84b, criterion = "waic") 

#compare models
w <- loo_compare(bh84a, bh84b, criterion = "waic")
w



#Answer workflow
#1. Standardize everything




# 8H5 ---------------------------------------------------------------------


data(Wines2012, package = "rethinking")

d <- Wines2012
rm(Wines2012)

glimpse(d)

#standardize outcome variable
d <- d %>%
  as_tibble() %>%
  mutate(score_std = standardize(score),
         red = factor(flight, levels = c("white", "red")),
         wine_amer = factor(wine.amer),
         judge_amer = factor(judge.amer))



#I would imagine the wine would have a larger effect than the judges


#using 0 + syntax supresses the noraml intercept and gives a seperate intercept for each factor



bh85a <- brm(score_std ~ 0 + judge + wine,
             data = d, family = gaussian,
             prior =   c(prior(normal(0, 0.5), class = b), 
                         prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = "fits/bh85a")

summary(bh85a)




## answer key (when we have two intercepts we need to use non linear syntax)
b8h5 <- brm(bf(score_std ~ 0 + j + w,
               j ~ 0 + judge,
               w ~ 0 + wine,
               nl = TRUE),
            data = d, family = gaussian,
            prior = c(prior(normal(0, 0.5), nlpar = j),
                      prior(normal(0, 0.25), nlpar = w),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = "fits/b8h5")



## Plot the priors

ph85 <-
  update(b8h5,
         sample_prior = "only", #only take the prior
         iter = 2000, warmup = 1000, chains = 4, cores = 4,
         seed = 1234,
         file = "fits/ph85")




draws <-
  b8h5 %>%  #just have to swtich this out to see prior plot vs actual plot
  as_draws_df() %>% 
  as_tibble() %>% 
  select(-sigma, -lp__) %>% 
  pivot_longer(-c(.chain, .iteration, .draw, lprior), 
               names_to = c(NA, "type", "num"), 
               names_sep = "_",
               values_to = "value") 



draws %>%
  filter(type == "j") %>%
  mutate(num = factor(num)) %>% 
  select(num, value) %>%
  group_by(num) %>%
  median_hdci(.width = c(0.67, 0.89, 0.97)) %>% 
  ggplot(aes(y = fct_rev(num), x = value, xmin = .lower, xmax = .upper)) +
  geom_interval() +
  # scale_color_manual(values = ramp_blue(seq(0.9, 0.1, length.out = 3)),
  #                    limits = as.character(c(0.67, 0.89, 0.97))) +
  labs(y = NULL, x = "Parameter Value", color = "Interval")



#with many indicator variables this seems like a good way to veiw them
#they're might also be easier mthods to plot but this one seems fairly induitve to me




# -------------------------------------------------------------------------

# Now consider three features of the wines and judges:
# (1) flight: Whether the wine is red or white.
# (2) wine.amer: Indicator variable for American wines. 
# (3) judge.amer: Indicator variable for American judges.


data(Wines2012, package = "rethinking")

d <- Wines2012
rm(Wines2012)

glimpse(d)

#standardize outcome variable
d <- d %>%
  as_tibble() %>%
  mutate(score_std = standardize(score),
         red = factor(flight, levels = c("white", "red")),
         wineamer = factor(wine.amer),
         judgeamer = factor(judge.amer))


# rjammer
# rwamer
# wamerjamer

## answer key (when we have two intercepts we need to use non linear syntax)
b8h6 <- brm(bf(score_std ~ 0 + r + wamer + jamer,
               r ~ 0 + red,
               wamer ~ 0 + wineamer,
               jamer ~ 0 + judgeamer,
               
               nl = TRUE),
            data = d, family = gaussian,
            prior = c(prior(normal(0, 0.5), nlpar = r),
                      prior(normal(0, 0.25), nlpar = wamer),
                      prior(normal(0.25, 0.5), nlpar = jamer),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = "fits/b8h6")


#Prior justificaiton
#1. red white, no knowledge of direction, seems reasonable that 95% of data within -1 to 1 standard deviation
#2. whether the wine is american would have a smaller effect than the type of wine
#3 judges as we saw last time tend to have a larger effect 


#first plot priors
ph86 <-
  update(b8h6,
         sample_prior = "only", #only take the prior
         iter = 2000, warmup = 1000, chains = 4, cores = 4,
         seed = 1234,
         file = "fits/ph86")



draws <-
  b8h6 %>%  #just have to swtich this out to see prior plot vs actual plot
  as_draws_df() %>% 
  as_tibble() %>% 
  select(-sigma, -lp__) %>% 
  pivot_longer(-c(.chain, .iteration, .draw, lprior), 
               names_to = c(NA, "variable", "level"), 
               names_sep = "_",
               values_to = "value") 



draws %>%
  filter(variable == "jamer") %>%
  mutate(level = factor(level)) %>% 
  select(level, value) %>%
  group_by(level) %>%
  median_hdci(.width = c(0.67, 0.89, 0.97)) %>% 
  ggplot(aes(y = fct_rev(level), x = value, xmin = .lower, xmax = .upper)) +
  geom_interval() +
  # scale_color_manual(values = ramp_blue(seq(0.9, 0.1, length.out = 3)),
  #                    limits = as.character(c(0.67, 0.89, 0.97))) +
  labs(y = NULL, x = "Parameter Value", color = "Interval")







# Inteactions -------------------------------------------------------------

b8h7 <- brm(bf(score_std ~ 0 + r + wamer + jamer + r:jammer,
               r ~ 0 + red,
               wamer ~ 0 + wineamer,
               jamer ~ 0 + judgeamer,
               nl = TRUE),
            data = d, family = gaussian,
            prior = c(prior(normal(0, 0.5), nlpar = r),
                      prior(normal(0, 0.25), nlpar = wamer),
                      prior(normal(0.25, 0.5), nlpar = jamer),
                      prior(normal(0, 0.2), coef = 'r:jammer'),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = "fits/b8h7")



