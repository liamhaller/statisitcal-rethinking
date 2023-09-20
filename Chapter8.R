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


