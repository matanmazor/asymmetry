## Following A. Solomon Kurz' Bayesian power analysis tutorial:
## https://solomonkurz.netlify.app/post/bayesian-power-analysis-part-i/


## Load pacakges
library(tidyverse)
library(brms)
library(broom)

set.seed(1)
n <- 200

d <-
  tibble(
    # Six experiments
    exp = rep(c(1,2,3,4,5,6), each=n))%>% 
    group_by(exp) %>% 
     mutate(
       # the beta coefficient relating ROC and search asymmetries
       beta_ROC = rnorm(1,0.3,0.1),
       #ROC asymmetry is sampled from a beta distribution with alpha=2 and beta=2+bias
        bias = rnorm(1,0,0.5),
       # if bias>0, ROC_asym is closer to 1. If bias <0, closer to 0.
        ROC_asym = rbeta(n,2,2+bias),
        standardized_ROC_asym=(ROC_asym-mean(ROC_asym))/sd(ROC_asym),
       # searc_asym is sampled from a normal distribution centred around bias+beta_ROC*standardized ROC_asym
        search_asym = rnorm(n,mean=bias, sd=1)+ 
          beta_ROC*standardized_ROC_asym,
       # the correlation is not used anywhere, and is here only as a sanity check. 
        cor = cor(ROC_asym,search_asym)) 
fit <-
  brm(data = d,
      family = gaussian,
      search_asym ~ 1 + ROC_asym + (1+ROC_asym | exp), # mixed effects linear regression model predicting search asymmetry from ROC asymmetry as a function of experiment. 
      prior = c(prior(normal(0, 2), class = b),
                prior(student_t(3, 1, 1), class = sigma)),
      seed = 1,
      chains=3,
      iter=1000)



sim_d_and_fit <- function(seed, n) {
  
  set.seed(seed)
  
  d <-
    tibble(exp = rep(c(1,2,3,4,5,6), each=n))%>% 
    group_by(exp) %>% 
    mutate(beta_ROC = rnorm(1,0.3,0.1),
           bias = rnorm(1,0,0.5),
           ROC_asym = rbeta(n,2,2+bias),
           standardized_ROC_asym=(ROC_asym-mean(ROC_asym))/sd(ROC_asym), 
           search_asym = rnorm(n,mean=bias, sd=1)+ 
             beta_ROC*standardized_ROC_asym,
           cor = cor(ROC_asym,search_asym))  
  
  
  update(fit,
         newdata = d, 
         seed = seed) %>% 
    tidy(prob = .95) %>% 
    filter(term == "b_ROC_asym")
}

# how many simulations would you like?
n_sim <- 100


# here's the main event!
s <-
  tibble(seed = 1:n_sim) %>% 
  mutate(tidy = map(seed, sim_d_and_fit, n = n)) %>% 
  unnest(tidy)

s %>%
  ggplot(aes(x = seed, y = estimate, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = c(0.05, .5), color = "white") +
  geom_pointrange(fatten = 1/2) +
  labs(x = "seed (i.e., simulation index)",
       y = expression(beta[1]))

s %>%
  mutate(check = ifelse(lower > 0.05, 1, 0)) %>%
  summarise(power = mean(check))
