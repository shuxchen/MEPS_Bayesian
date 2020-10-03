
#re
get_prior(Y ~ competitor + P_b_prior_LOE + t_LOE + (1|ll),
          data = generic_price_train,
          threshold = "equidistant")

#fe
get_prior(Y ~ competitor + P_b_prior_LOE + t_LOE + as.factor(ll),
             data = generic_price_train,
             threshold = "equidistant")

#mixed
get_prior(Y ~ competitor + P_b_prior_LOE + t_LOE + (1 + competitor|ll),
          data = generic_price_train,
          threshold = "equidistant")

#0. Load data
setwd("~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian")
load("~/Dropbox/Advanced Method Project/Data/MEPS_summary_weighted.Rdata")

setwd("C:/Users/shuxian/repos/MEPS_Bayesian")
load("MEPS_summary_weighted.Rdata")

#1.1 Data construction
generic_price <- MEPS_summary_weighted %>%
  filter(!is.na(P_g),
         competitor > 0) %>%
  mutate(Y = log(P_g)) 

generic_price_id <- generic_price %>% 
  distinct(index) 

generic_price_id$ll <- seq.int(nrow(generic_price_id))

generic_price <- generic_price %>%
  left_join(generic_price_id) %>%
  select(Y, ll, competitor, P_b_prior_LOE, t_LOE, year, oral, inject, ATCA:ATCV)

generic_price %>% 
  filter(is.na(t_LOE)) %>%
  data.frame()

generic_price <- generic_price %>% 
  filter(!is.na(P_b_prior_LOE)) 

generic_price <- generic_price %>% 
  mutate(P_b_prior_LOE = log(P_b_prior_LOE)) 

#1.2 Split data
#1.2.1 count # by group: if 1, put in train only (for RE); for the rest, 5:5 in train and test
#1.2.2 sanity check: examine index in train and test; delete any index in test if not available in training as well
generic_price_n <- generic_price %>%
  group_by(ll) %>%
  count()

generic_price_n_multiple <- generic_price_n %>%
  filter(n > 1) %>%
  select(-n)

generic_price_n_one <- generic_price_n %>%
  filter(n == 1) %>%
  select(-n)

generic_price_multiple <- generic_price %>%
  inner_join(generic_price_n_multiple)

generic_price_one <- generic_price %>%
  inner_join(generic_price_n_one)

generic_price_train <- generic_price_multiple %>% 
  group_by(ll) %>% sample_frac(.5)

generic_price_test <- generic_price_multiple %>%
  anti_join(generic_price_train)

generic_price_train_id <- generic_price_train %>%
  distinct(ll)

generic_price_test <- generic_price_test %>%
  inner_join(generic_price_train_id)

generic_price_test_id <- generic_price_test %>%
  distinct(ll)

save(generic_price_train, file = "generic_price_train_brms.RData")
save(generic_price_test, file = "generic_price_test_brms.RData")

#3. Analysis
load("generic_price_train_brms.RData")
load("generic_price_test_brms.RData")

#3.1 RE
generic_price_train <- generic_price_train %>%
  select(1:5)

generic_price_test <- generic_price_test %>%
  select(1:5)

get_prior(Y ~ competitor + P_b_prior_LOE + t_LOE + (1 + competitor|ll),
          data = generic_price_train,
          threshold = "equidistant")

mod_re_default = brm(
  Y ~ competitor + P_b_prior_LOE + t_LOE + (1 + competitor|ll),
  data = generic_price_train,
  #family = 'bernoulli',
  #prior = set_prior('normal(0, 3)'),
  iter = 5000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.95) 
) 

fit_re_default = fitted(
  mod_re_default,
  newdata = generic_price_test,
  #re_formula = NA, # ignore random effects
  re_formula = NULL,
  summary = TRUE # mean and 95% CI
  )

get_CI <- function(data){
  colnames(data) = c('fit', 'se', 'lwr', 'upr')
  fit <- generic_price_test %>%
    cbind(data) %>%
    mutate(within_CI = ifelse(lwr <= Y & upr >= Y, 1, 0))
  return(fit)
}

fit_re_default_result <- get_CI(fit_re_default)

mean(fit_re_default_result$within_CI)


mod_re_informative = brm(
  Y ~ competitor + P_b_prior_LOE + t_LOE + (1 + competitor|ll),
  data = generic_price_train,
  prior = c(set_prior('normal(0, 100)', class = 'b', coef = 'intercept'),
            set_prior('normal(-0.075, 0.051)', class = 'b', coef = 'competitor'),
            set_prior('normal(0, 100)', class = 'b', coef = 'P_b_prior_LOE'),
            set_prior('normal(0, 100)', class = 'b', coef = 't_LOE'),
            set_prior('inv_gamma(0.01, 0.01)', class = 'sigma'),
            set_prior('cauchy(0, 10)', class = 'sd'),
            set_prior('cauchy(0, 2.5)', class = 'sd', coef = 'competitor', group = 'll')),
  iter = 5000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.95) 
) 

summary(mod_re_informative)
plot(mod_re_informative)

fit_re_informative = fitted(
  mod_re_informative,
  newdata = generic_price_test,
  #re_formula = NA, # ignore random effects
  re_formula = NULL,
  summary = TRUE # mean and 95% CI
)

fit_re_informative_result <- get_CI(fit_re_informative)

mean(fit_re_informative_result$within_CI)


mod_fe_default = brm(
  Y ~ competitor + P_b_prior_LOE + t_LOE + as.factor(ll),
  data = generic_price_train,
  #family = 'bernoulli',
  #prior = set_prior('normal(0, 3)'),
  iter = 5000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.95) 
) 

summary(mod_fe_default)
#plot(mod_fe_default)

fit = lm(Y ~ competitor + P_b_prior_LOE + t_LOE + as.factor(ll), data = generic_price_train)
summary(fit)