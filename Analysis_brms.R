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

#generic_price <- generic_price %>%
#  left_join(generic_price_id) %>%
#  select(Y, ll, competitor, P_b_prior_LOE, t_LOE, year) #, oral, inject, ATCA:ATCV

generic_price <- generic_price %>%
  left_join(generic_price_id) %>%
  select(Y, index, ll, competitor, P_b_prior_LOE, t_LOE, year, Trade_Name, oral, inject) #, ATCA:ATCV)


generic_price %>% 
  filter(is.na(t_LOE)) %>%
  data.frame()

generic_price <- generic_price %>% 
  filter(!is.na(P_b_prior_LOE)) 

generic_price <- generic_price %>% 
  mutate(P_b_prior_LOE = log(P_b_prior_LOE)) 

write.xlsx(generic_price, "MEPS_price.xlsx")

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
  inner_join(generic_price_n_one) %>%
  ungroup()

generic_price_train <- generic_price_multiple %>% 
  group_by(ll) %>% 
  sample_frac(.5) 

generic_price_test <- generic_price_multiple %>%
  anti_join(generic_price_train)

generic_price_train_id <- generic_price_train %>%
  distinct(ll)

generic_price_train <- generic_price_train %>%
  bind_rows(generic_price_one)

generic_price_test <- generic_price_test %>%
  inner_join(generic_price_train_id)

generic_price_test_id <- generic_price_test %>%
  distinct(ll)

save(generic_price_train, file = "generic_price_train_brms.RData")
save(generic_price_test, file = "generic_price_test_brms.RData")

load("generic_price_train_brms.Rdata")
load("generic_price_test_brms.Rdata")

### 1. random intercept
prior_noninformative <- c(set_prior("normal(0,10)", class = "b"))
prior_informative <- c(set_prior("normal(-0.08, 0.02)", class = "b", coef = "competitor"),
                       set_prior("normal(0,10)", class = "b"))
prior_bias_corrected <- c(set_prior("normal(-0.11, 0.07)", class = "b", coef = "competitor"),
                       set_prior("normal(0,10)", class = "b"))


fit_generic_random_intercept_noninformative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + (1|index), 
                                                data = generic_price_train, family = gaussian(),
                                                prior = prior_noninformative,
                                                iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                control = list(adapt_delta = .99, max_treedepth = 20))

fit_generic_random_intercept_noninformative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + oral + inject + (1|index), 
                                                   data = generic_price_train, family = gaussian(),
                                                   control = list(adapt_delta = .975, max_treedepth = 20),
                                                   iter = 6000, warmup = 1000, chains = 4, cores = 4)

fit_generic_random_intercept_noninformative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + oral + ATCA + ATCB + ATCC + ATCD + ATCG + ATCJ + ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + inject + (1|ll), 
                                                   data = generic_price_train, family = gaussian(),
                                                   control = list(adapt_delta = .975, max_treedepth = 20),
                                                   iter = 6000, warmup = 1000, chains = 4, cores = 4)

fit_generic_random_intercept_informative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + (1|index), 
                                                data = generic_price_train, family = gaussian(),
                                                prior = prior_informative,
                                                iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                control = list(adapt_delta = .99, max_treedepth = 20))

fit_generic_random_intercept_informative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + (1|index), 
                                                data = generic_price_train, family = gaussian(),
                                                prior = prior_bias_corrected,
                                                iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                control = list(adapt_delta = .99, max_treedepth = 20))


get_prior(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1|ll), 
          data = generic_price_train, family = gaussian())

get_prior(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1|ll), 
          data = generic_price_train, family = gaussian(),
          set_prior("normal(-0.08, 0.02)", class = "b", coef = "competitor"))

print(summary(fit_generic_random_intercept_noninformative),digits=5) 

print(summary(fit_generic_random_intercept_informative),digits=5) 

## prediction for noninformative
fit_generic_random_intercept_noninformative_prediction <- predict(fit_generic_random_intercept_noninformative, newdata = generic_price_test)

fit_generic_random_intercept_noninformative_prediction <- fit_generic_random_intercept_noninformative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_noninformative_prediction$CI_covered)

## prediction for informative
fit_generic_random_intercept_informative_prediction <- predict(fit_generic_random_intercept_informative, newdata = generic_price_test)

fit_generic_random_intercept_informative_prediction <- fit_generic_random_intercept_informative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_informative_prediction$CI_covered)

###2. random slope, N only
fit_generic_random_slope_noninformative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + (1 + competitor|ll), 
                                data = generic_price_train, family = gaussian(),
                                iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                control = list(adapt_delta = .995, max_treedepth = 20),
                                seed = 190831)

fit_generic_random_slope_informative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + (1 + competitor|ll), 
                                               data = generic_price_train, family = gaussian(),
                                               iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                               control = list(adapt_delta = .995, max_treedepth = 20),
                                               seed = 190831,
                                            set_prior("normal(0, 10)", class = "b"),
                                            set_prior("normal(-0.08, 0.02)", class = "b", coef = "competitor"),
                                            set_prior("igamma(0.01, 0.01)", class = "sigma"))

fit_generic_random_slope_informative_bias_correction <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + (1 + competitor|ll), 
                                            data = generic_price_train, family = gaussian(),
                                            iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                            control = list(adapt_delta = .995, max_treedepth = 20),
                                            seed = 190831,
                                            set_prior("normal(-0.11, 0.07)", class = "b", coef = "competitor"),
                                            set_prior("normal(0, 10)", class = "b"),
                                            set_prior("igamma(0.01, 0.01)", class = "sigma"))


get_prior(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 + competitor|ll), 
          data = generic_price_train, family = gaussian())

print(summary(fit_generic_random_slope_noninformative),digits=5) 

print(summary(fit_generic_random_slope_informative),digits=5) 

print(summary(fit_generic_random_slope_informative_bias_correction),digits=5) 

## Prediction, noninformative
fit_generic_random_slope_noninformative_prediction <- predict(fit_generic_random_slope_noninformative, newdata = generic_price_test)

fit_generic_random_slope_noninformative_prediction <- fit_generic_random_slope_noninformative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_slope_noninformative_prediction$CI_covered)

## Prediction, informative
fit_generic_random_slope_informative_prediction <- predict(fit_generic_random_slope_informative, newdata = generic_price_test)

fit_generic_random_slope_informative_prediction <- fit_generic_random_slope_informative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_slope_informative_prediction$CI_covered)

## Prediction, informative, bias corrected
fit_generic_random_slope_informative_bias_correction_prediction <- predict(fit_generic_random_slope_informative_bias_correction, newdata = generic_price_test)

fit_generic_random_slope_informative_bias_correction_prediction <- fit_generic_random_slope_informative_bias_correction_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_slope_informative_bias_correction_prediction$CI_covered)


##3. random slope, all
fit_generic_random_slope_all_noninformative <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 + competitor + t_LOE + log(P_b_prior_LOE)|index), 
                                               data = generic_price_train, family = gaussian(),
                                               iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                               control = list(adapt_delta = .99, max_treedepth = 20),
                                               seed = 190831)

fit_generic_random_slope_all_informative <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 + competitor + t_LOE + log(P_b_prior_LOE)|index), 
                                            data = generic_price_train, family = gaussian(),
                                            iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                            control = list(adapt_delta = .99, max_treedepth = 20),
                                            seed = 190831,
                                            set_prior("normal(0, 10)", class = "b"),
                                            set_prior("normal(-0.08, 0.02)", class = "b", coef = "competitor"),
                                            set_prior("igamma(0.01, 0.01)", class = "sigma"))

fit_generic_random_slope_all_informative <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 + competitor + t_LOE + log(P_b_prior_LOE)|index), 
                                                data = generic_price_train, family = gaussian(),
                                                iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                control = list(adapt_delta = .99, max_treedepth = 20),
                                                seed = 190831,
                                                set_prior("normal(-0.11, 0.07)", class = "b", coef = "competitor"),
                                                set_prior("normal(0, 10)", class = "b"),
                                                set_prior("igamma(0.01, 0.01)", class = "sigma"))

get_prior(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 + competitor|ll), 
          data = generic_price_train, family = gaussian())

print(summary(fit_generic_random_slope_all_noninformative),digits=5) 

print(summary(fit_generic_random_slope_all_informative),digits=5) 

## Prediction, noninformative
fit_generic_random_slope_all_noninformative_prediction <- predict(fit_generic_random_slope_all_noninformative, newdata = generic_price_test)

fit_generic_random_slope_all_noninformative_prediction <- fit_generic_random_slope_all_noninformative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_slope_all_noninformative_prediction$CI_covered)

## Prediction, informative
fit_generic_random_slope_all_informative_prediction <- predict(fit_generic_random_slope_all_informative, newdata = generic_price_test)

fit_generic_random_slope_all_informative_prediction <- fit_generic_random_slope_all_informative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_slope_all_informative_prediction$CI_covered)

##4. random intercept, molecule-route level
fit_generic_random_intercept_noninformative <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 |Trade_Name), 
                                                   data = generic_price_train, family = gaussian(),
                                                   iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                   control = list(adapt_delta = .99, max_treedepth = 20),
                                                   seed = 190831)

fit_generic_random_intercept_informative <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 |Trade_Name), 
                                                data = generic_price_train, family = gaussian(),
                                                iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                control = list(adapt_delta = .99, max_treedepth = 20),
                                                seed = 190831,
                                                set_prior("normal(0, 10)", class = "b"),
                                                set_prior("normal(-0.08, 0.02)", class = "b", coef = "competitor"),
                                                set_prior("igamma(0.01, 0.01)", class = "sigma"))

fit_generic_random_intercept_informative_bias_correction <- brm(Y ~ competitor + t_LOE + log(P_b_prior_LOE) + (1 |Trade_Name), 
                                                data = generic_price_train, family = gaussian(),
                                                iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                control = list(adapt_delta = .99, max_treedepth = 20),
                                                seed = 190831,
                                                set_prior("normal(-0.11, 0.07)", class = "b", coef = "competitor"),
                                                set_prior("normal(0, 10)", class = "b"),
                                                set_prior("igamma(0.01, 0.01)", class = "sigma"))

print(summary(fit_generic_random_intercept_noninformative),digits=5) 

print(summary(fit_generic_random_intercept_informative),digits=5) 

print(summary(fit_generic_random_intercept_informative_bias_correction),digits=5) 


## Prediction, noninformative
fit_generic_random_intercept_noninformative_prediction <- predict(fit_generic_random_intercept_noninformative, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_noninformative_prediction <- fit_generic_random_intercept_noninformative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_noninformative_prediction$CI_covered)

## Prediction, informative
fit_generic_random_intercept_informative_prediction <- predict(fit_generic_random_intercept_informative, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_informative_prediction <- fit_generic_random_intercept_informative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_informative_prediction$CI_covered)

## Prediction, informative, bias corrected
fit_generic_random_intercept_informative_bias_correction_prediction <- predict(fit_generic_random_intercept_informative_bias_correction, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_informative_bias_correction_prediction <- fit_generic_random_intercept_informative_bias_correction_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_informative_bias_correction_prediction$CI_covered)


##5. random intercept, with routes
fit_generic_random_intercept_route_noninformative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + oral + inject + (1 |ll), 
                                                   data = generic_price_train, family = gaussian(),
                                                   iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                   control = list(adapt_delta = .99, max_treedepth = 20),
                                                   seed = 190831)

fit_generic_random_intercept_route_informative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + oral + inject + (1 |ll), 
                                                data = generic_price_train, family = gaussian(),
                                                iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                control = list(adapt_delta = .99, max_treedepth = 20),
                                                seed = 190831,
                                                set_prior("normal(0, 10)", class = "b"),
                                                set_prior("normal(-0.08, 0.02)", class = "b", coef = "competitor"),
                                                set_prior("igamma(0.01, 0.01)", class = "sigma"))

fit_generic_random_intercept_route_informative_bias_correction <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + oral + inject + (1 |ll), 
                                                data = generic_price_train, family = gaussian(),
                                                iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                control = list(adapt_delta = .99, max_treedepth = 20),
                                                seed = 190831,
                                                set_prior("normal(-0.11, 0.07)", class = "b", coef = "competitor"),
                                                set_prior("normal(0, 10)", class = "b"),
                                                set_prior("igamma(0.01, 0.01)", class = "sigma"))

print(summary(fit_generic_random_intercept_route_noninformative),digits=5) 

print(summary(fit_generic_random_intercept_route_informative),digits=5) 

print(summary(fit_generic_random_intercept_route_informative_bias_correction),digits=5) 


## Prediction, noninformative
fit_generic_random_intercept_route_noninformative_prediction <- predict(fit_generic_random_intercept_route_noninformative, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_route_noninformative_prediction <- fit_generic_random_intercept_route_noninformative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_route_noninformative_prediction$CI_covered)

## Prediction, informative
fit_generic_random_intercept_route_informative_prediction <- predict(fit_generic_random_intercept_route_informative, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_route_informative_prediction <- fit_generic_random_intercept_route_informative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_route_informative_prediction$CI_covered)

## Prediction, informative, bias correction
fit_generic_random_intercept_route_informative_bias_correction_prediction <- predict(fit_generic_random_intercept_route_informative_bias_correction, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_route_informative_bias_correction_prediction <- fit_generic_random_intercept_route_informative_bias_correction_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_route_informative_bias_correction_prediction$CI_covered)

## 6. Random intercept, with routes and ATC
#first decide how to group ATC into small subgroups
MEPS_summary_weighted_id <- MEPS_summary_weighted %>%
  distinct(index)

df_inner <- df %>%
  inner_join(MEPS_summary_weighted_id, by = "index") %>%
  dplyr::select(index, ATCA:ATCV) %>%
  distinct()

df_inner <- df_inner %>%
  replace_with_na_all(condition = ~.x == 0)

df_inner <- df_inner %>%
  group_by(index) %>%
  fill(ATCA:ATCV) %>%
  fill(ATCA:ATCV, .direction = "up") %>%
  distinct()

df_inner[is.na(df_inner)] <- 0

# summarise ATCs
summary(df_inner)

df_inner <- df_inner %>%
  dplyr::select(-ATCH, -ATCP, -ATCV)

generic_price_train <- generic_price_train %>%
  left_join(df_inner, by = "index")

generic_price_test <- generic_price_test %>%
  left_join(df_inner, by = "index")

# fit models
fit_generic_random_intercept_route_ATC_noninformative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE  + ATCA + ATCB + ATCC + ATCD + ATCG + ATCJ + ATCL + ATCL + ATCM + ATCN + ATCR + ATCS + (1 |ll), 
                                                         data = generic_price_train, family = gaussian(),
                                                         iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                         control = list(adapt_delta = .99, max_treedepth = 20),
                                                         seed = 190831)

fit_generic_random_intercept_route_ATC_informative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + ATCA + ATCB + ATCC + ATCD + ATCG + ATCJ + ATCL + ATCL + ATCM + ATCN + ATCR + ATCS +  (1 |ll), 
                                                      data = generic_price_train, family = gaussian(),
                                                      iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                      control = list(adapt_delta = .99, max_treedepth = 20),
                                                      seed = 190831,
                                                      set_prior("normal(0, 10)", class = "b"),
                                                      set_prior("normal(-0.08, 0.02)", class = "b", coef = "competitor"),
                                                      set_prior("igamma(0.01, 0.01)", class = "sigma"))

fit_generic_random_intercept_route_ATC_informative_bias_correction <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + ATCA + ATCB + ATCC + ATCD + ATCG + ATCJ + ATCL + ATCL + ATCM + ATCN + ATCR + ATCS + (1 |ll), 
                                                                      data = generic_price_train, family = gaussian(),
                                                                      iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                                      control = list(adapt_delta = .99, max_treedepth = 20),
                                                                      seed = 190831,
                                                                      set_prior("normal(-0.11, 0.07)", class = "b", coef = "competitor"),
                                                                      set_prior("normal(0, 10)", class = "b"),
                                                                      set_prior("igamma(0.01, 0.01)", class = "sigma"))

print(summary(fit_generic_random_intercept_route_ATC_noninformative),digits=5) 

print(summary(fit_generic_random_intercept_route_ATC_informative),digits=5) 

print(summary(fit_generic_random_intercept_route_ATC_informative_bias_correction),digits=5) 


## Prediction, noninformative
fit_generic_random_intercept_route_ATC_noninformative_prediction <- predict(fit_generic_random_intercept_route_ATC_noninformative, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_route_ATC_noninformative_prediction <- fit_generic_random_intercept_route_ATC_noninformative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_route_ATC_noninformative_prediction$CI_covered)

## Prediction, informative
fit_generic_random_intercept_route_ATC_informative_prediction <- predict(fit_generic_random_intercept_route_ATC_informative, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_route_ATC_informative_prediction <- fit_generic_random_intercept_route_ATC_informative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_route_ATC_informative_prediction$CI_covered)

## Prediction, informative, bias correction
fit_generic_random_intercept_route_ATC_informative_bias_correction_prediction <- predict(fit_generic_random_intercept_route_ATC_informative_bias_correction, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_route_ATC_informative_bias_correction_prediction <- fit_generic_random_intercept_route_ATC_informative_bias_correction_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_route_ATC_informative_bias_correction_prediction$CI_covered)


## 7. Random intercept, with AG
df_AG <- df %>%
  inner_join(MEPS_summary_weighted_id, by = "index") %>%
  dplyr::select(index, AG) %>%
  distinct()

generic_price_train <- generic_price_train %>%
  left_join(df_AG, by = "index")

generic_price_test <- generic_price_test %>%
  left_join(df_AG, by = "index")

# fit models
fit_generic_random_intercept_AG_noninformative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE  + AG + (1 |ll), 
                                                             data = generic_price_train, family = gaussian(),
                                                             iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                             control = list(adapt_delta = .99, max_treedepth = 20),
                                                             seed = 190831)

fit_generic_random_intercept_AG_informative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + AG +  (1 |ll), 
                                                          data = generic_price_train, family = gaussian(),
                                                          iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                          control = list(adapt_delta = .99, max_treedepth = 20),
                                                          seed = 190831,
                                                          set_prior("normal(0, 10)", class = "b"),
                                                          set_prior("normal(-0.08, 0.02)", class = "b", coef = "competitor"),
                                                          set_prior("igamma(0.01, 0.01)", class = "sigma"))

fit_generic_random_intercept_AG_informative_bias_correction <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + AG + (1 |ll), 
                                                                          data = generic_price_train, family = gaussian(),
                                                                          iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                                          control = list(adapt_delta = .99, max_treedepth = 20),
                                                                          seed = 190831,
                                                                          set_prior("normal(-0.11, 0.07)", class = "b", coef = "competitor"),
                                                                          set_prior("normal(0, 10)", class = "b"),
                                                                          set_prior("igamma(0.01, 0.01)", class = "sigma"))

print(summary(fit_generic_random_intercept_AG_noninformative),digits=5) 

print(summary(fit_generic_random_intercept_AG_informative),digits=5) 

print(summary(fit_generic_random_intercept_AG_informative_bias_correction),digits=5) 


## Prediction, noninformative
fit_generic_random_intercept_AG_noninformative_prediction <- predict(fit_generic_random_intercept_AG_noninformative, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_AG_noninformative_prediction <- fit_generic_random_intercept_AG_noninformative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_AG_noninformative_prediction$CI_covered)

## Prediction, informative
fit_generic_random_intercept_AG_informative_prediction <- predict(fit_generic_random_intercept_AG_informative, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_AG_informative_prediction <- fit_generic_random_intercept_AG_informative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_AG_informative_prediction$CI_covered)

## Prediction, informative, bias correction
fit_generic_random_intercept_AG_informative_bias_correction_prediction <- predict(fit_generic_random_intercept_AG_informative_bias_correction, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_AG_informative_bias_correction_prediction <- fit_generic_random_intercept_AG_informative_bias_correction_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_AG_informative_bias_correction_prediction$CI_covered)


## 8. Random intercept, with PIV
MEPS_summary_weighted_PIV <- MEPS_summary_weighted %>%
  dplyr::select(index, PIV) %>%
  distinct()

generic_price_train <- generic_price_train %>%
  left_join(MEPS_summary_weighted_PIV, by = "index")

generic_price_test <- generic_price_test %>%
  left_join(MEPS_summary_weighted_PIV, by = "index")

# fit models
fit_generic_random_intercept_PIV_noninformative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE  + PIV + (1 |ll), 
                                                      data = generic_price_train, family = gaussian(),
                                                      iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                      control = list(adapt_delta = .99, max_treedepth = 20),
                                                      seed = 190831)

fit_generic_random_intercept_PIV_informative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + PIV +  (1 |ll), 
                                                   data = generic_price_train, family = gaussian(),
                                                   iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                   control = list(adapt_delta = .99, max_treedepth = 20),
                                                   seed = 190831,
                                                   set_prior("normal(0, 10)", class = "b"),
                                                   set_prior("normal(-0.08, 0.02)", class = "b", coef = "competitor"),
                                                   set_prior("igamma(0.01, 0.01)", class = "sigma"))

fit_generic_random_intercept_PIV_informative_bias_correction <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + PIV + (1 |ll), 
                                                                   data = generic_price_train, family = gaussian(),
                                                                   iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                                   control = list(adapt_delta = .99, max_treedepth = 20),
                                                                   seed = 190831,
                                                                   set_prior("normal(-0.11, 0.07)", class = "b", coef = "competitor"),
                                                                   set_prior("normal(0, 10)", class = "b"),
                                                                   set_prior("igamma(0.01, 0.01)", class = "sigma"))

print(summary(fit_generic_random_intercept_PIV_noninformative),digits=5) 

print(summary(fit_generic_random_intercept_PIV_informative),digits=5) 

print(summary(fit_generic_random_intercept_PIV_informative_bias_correction),digits=5) 


## Prediction, noninformative
fit_generic_random_intercept_PIV_noninformative_prediction <- predict(fit_generic_random_intercept_PIV_noninformative, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_PIV_noninformative_prediction <- fit_generic_random_intercept_PIV_noninformative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_PIV_noninformative_prediction$CI_covered)

## Prediction, informative
fit_generic_random_intercept_PIV_informative_prediction <- predict(fit_generic_random_intercept_PIV_informative, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_PIV_informative_prediction <- fit_generic_random_intercept_PIV_informative_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_PIV_informative_prediction$CI_covered)

## Prediction, informative, bias correction
fit_generic_random_intercept_PIV_informative_bias_correction_prediction <- predict(fit_generic_random_intercept_PIV_informative_bias_correction, newdata = generic_price_test, re_formula = ~ (1 | index))

fit_generic_random_intercept_PIV_informative_bias_correction_prediction <- fit_generic_random_intercept_PIV_informative_bias_correction_prediction %>%
  cbind(generic_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_random_intercept_PIV_informative_bias_correction_prediction$CI_covered)

