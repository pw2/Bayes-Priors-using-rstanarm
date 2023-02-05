
## Bayesian priors for categorical variables using rstanarm

library(rstanarm)
library(tidyverse)
library(patchwork)

### Data -----------------------------------------------------------------
d <- mtcars %>%
  select(mpg, cyl, disp) %>%
  mutate(cyl = as.factor(cyl),
         cyl6 = ifelse(cyl == "6", 1, 0),
         cyl8 = ifelse(cyl == "8", 1, 0))

d %>% 
  head()

d %>%
  group_by(cyl) %>%
  summarize(avg = mean(mpg),
            SD = sd(mpg))

## Linear regression ------------------------------------------------------
fit_lm <- lm(mpg ~ cyl, data = d)
summary(fit_lm)


## Fit a Bayesian regression with rstanarm --------------------------------
## setting no prior info
stan_glm(mpg ~ cyl, data = d) %>%
  summary(digits = 3)
  
  
## Setting priors
ind_var_priors <- normal(location = c(0, 0), scale = c(10, 10))

fit_rstan <- stan_glm(mpg ~ cyl, 
                      prior = ind_var_priors,
                      prior_intercept = normal(15, 8),
                      prior_aux = cauchy(0, 3),
                      data = d)

# fit_rstan
summary(fit_rstan, digits = 3)

############################################################################################
#### Alternate approach to coding the priors -- dummy coding the categorical variables #####
############################################################################################

d2 <- d %>%
  mutate(cyl6 = ifelse(cyl == "6", 1, 0),
         cyl8 = ifelse(cyl == "8", 1, 0))

summary(lm(mpg ~ cyl6 + cyl8, data = d2))

stan_glm(mpg ~ cyl, 
         prior = ind_var_priors,
         prior_intercept = normal(15, 8),
         prior_aux = cauchy(0, 3),
         data = d2) %>%
  summary()

###########################################################################################
###########################################################################################

### Back to the original model
# posterior samples
post_rstan <- as.matrix(fit_rstan) %>%
  as.data.frame() %>%
  rename('cyl4' = '(Intercept)')

post_rstan %>%
  head()

mu.cyl4 <- post_rstan$cyl4
mu.cyl6 <- post_rstan$cyl4 + post_rstan$cyl6
mu.cyl8 <- post_rstan$cyl4 + post_rstan$cyl8

rstan_results <- data.frame(mu.cyl4, mu.cyl6, mu.cyl8) %>%
  pivot_longer(cols = everything())


rstan_plt <- rstan_results %>%
  left_join(
    
    d %>%
      group_by(cyl) %>%
      summarize(avg = mean(mpg)) %>%
      rename(name = cyl) %>%
      mutate(name = case_when(name == "4" ~ "mu.cyl4",
                              name == "6" ~ "mu.cyl6",
                              name == "8" ~ "mu.cyl8"))
    
  ) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(alpha = 0.4) +
  geom_vline(aes(xintercept = avg),
             color = "black",
             size = 1.2,
             linetype = "dashed") +
  facet_wrap(~name, scales = "free_x") +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", face = "bold")) +
  ggtitle("rstanarm")

rstan_plt

# summarize posteriors
mean(mu.cyl4)
sd(mu.cyl4)
qnorm(p = c(0.05, 0.95), mean = mean(mu.cyl4), sd = sd(mu.cyl4))
quantile(mu.cyl4, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

mean(mu.cyl6)
sd(mu.cyl6)
qnorm(p = c(0.05, 0.95), mean = mean(mu.cyl6), sd = sd(mu.cyl6))
quantile(mu.cyl6, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

mean(mu.cyl8)
sd(mu.cyl8)
qnorm(p = c(0.05, 0.95), mean = mean(mu.cyl8), sd = sd(mu.cyl8))
quantile(mu.cyl8, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

## Use wildly different priors ---------------------------------------------------------
ind_var_priors2 <- normal(location = c(0, 20), scale = c(10, 5))

fit_rstan2 <- stan_glm(mpg ~ cyl, 
                       prior = ind_var_priors2,
                       prior_intercept = normal(50, 2),
                       prior_aux = cauchy(0, 10),
                       data = d)

summary(fit_rstan2, digits = 3)

# posterior samples
post_rstan <- as.matrix(fit_rstan2) %>%
  as.data.frame() %>%
  rename('cyl4' = '(Intercept)')

post_rstan %>%
  head()

mu.cyl4 <- post_rstan$cyl4
mu.cyl6 <- post_rstan$cyl4 + post_rstan$cyl6
mu.cyl8 <- post_rstan$cyl4 + post_rstan$cyl8

rstan_results <- data.frame(mu.cyl4, mu.cyl6, mu.cyl8) %>%
  pivot_longer(cols = everything())


rstan_plt2 <- rstan_results %>%
  left_join(
    
    d %>%
      group_by(cyl) %>%
      summarize(avg = mean(mpg)) %>%
      rename(name = cyl) %>%
      mutate(name = case_when(name == "4" ~ "mu.cyl4",
                              name == "6" ~ "mu.cyl6",
                              name == "8" ~ "mu.cyl8"))
    
  ) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(alpha = 0.4) +
  geom_vline(aes(xintercept = avg),
             color = "black",
             size = 1.2,
             linetype = "dashed") +
  facet_wrap(~name, scales = "free_x") +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", face = "bold")) +
  ggtitle("rstanarm 2")

rstan_plt2


# summarize priors
mean(mu.cyl4)
sd(mu.cyl4)
qnorm(p = c(0.05, 0.95), mean = mean(mu.cyl4), sd = sd(mu.cyl4))
quantile(mu.cyl4, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

mean(mu.cyl6)
sd(mu.cyl6)
qnorm(p = c(0.05, 0.95), mean = mean(mu.cyl6), sd = sd(mu.cyl6))
quantile(mu.cyl6, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

mean(mu.cyl8)
sd(mu.cyl8)
qnorm(p = c(0.05, 0.95), mean = mean(mu.cyl8), sd = sd(mu.cyl8))
quantile(mu.cyl8, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

