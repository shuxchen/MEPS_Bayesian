if (.Platform$OS.type == "unix"){
  
  setwd("~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian")
  load("~/Dropbox/Advanced Method Project/Data/MEPS_summary_weighted.Rdata")
  
} else {
  setwd("C:/Users/shuxian/repos/MEPS_Bayesian")
  load("MEPS_summary_weighted.Rdata")
}

#split training and validation
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
  select(Y, ll, competitor, P_b_prior_LOE, t_LOE, year) #, oral, inject, ATCA:ATCV

generic_price <- generic_price %>%
  left_join(generic_price_id) %>%
  select(Y, ll, competitor, P_b_prior_LOE, t_LOE, year, Trade_Name, oral, inject, ATCA:ATCV)


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

#random intercept
fit_generic_random_intercept_noninformative <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1|ll), 
                                                data = generic_price_train, family = gaussian(),
                                                set_prior("normal(-0.06, 0.02)", class = "b", coef = "competitor"))

fit_generic_random_intercept_noninformative <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + oral + inject + ATCA:ATCV + (1|ll), 
                                                   data = generic_price_train, family = gaussian(),
                                                   set_prior("normal(-0.06, 0.02)", class = "b", coef = "competitor"))


fit_generic_random_intercept_informative <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1|ll), 
                                                data = generic_price_train, family = gaussian(),
                                                set_prior("normal(-0.06, 0.02)", class = "b", coef = "competitor"))

get_prior(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1|ll), 
          data = generic_price_train, family = gaussian())

get_prior(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1|ll), 
          data = generic_price_train, family = gaussian(),
          set_prior("normal(-0.06, 0.02)", class = "b", coef = "competitor"))

summary(fit_generic_random_intercept_noninformative)
summary(fit_generic_random_intercept_informative)

#fit_generic_random_intercept_noninformative_prediction <- fitted(fit_generic_random_intercept_noninformative, newdata = generic_price_test, re_formula = NA)
fit_generic_random_intercept_noninformative_prediction <- fitted(fit_generic_random_intercept_noninformative, newdata = generic_price_test)

fit_generic_random_intercept_noninformative_prediction <- fit_generic_random_intercept_noninformative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_noninformative_prediction$CI_covered)

#random slope
fit_generic_random_slope_noninformative <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 + competitor|index), 
                                data = generic_price_train, family = gaussian(),
                                iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                control = list(adapt_delta = .975, max_treedepth = 20),
                                seed = 190831)

fit_generic_random_slope_informative <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 + competitor|ll), 
                                               data = generic_price_train, family = gaussian(),
                                               iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                               control = list(adapt_delta = .99, max_treedepth = 20),
                                               seed = 190831,
                                               set_prior("normal(-0.06, 0.02)", class = "b", coef = "competitor"),
                                            set_prior("igamma(0.01, 0.01)", class = "sigma"))

get_prior(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 + competitor|ll), 
          data = generic_price_train, family = gaussian())

summary(fit_generic_random_slope_noninformative)
summary(fit_generic_random_slope_informative)

fit_generic_random_slope_informative_prediction <- fitted(fit_generic_random_slope_informative, newdata = generic_price_test)

fit_generic_random_slope_informative_prediction <- fit_generic_random_slope_informative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_slope_informative_prediction$CI_covered)

