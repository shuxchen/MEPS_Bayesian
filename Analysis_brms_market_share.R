if (.Platform$OS.type == "unix"){
  
  setwd("~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian")
  load("~/Dropbox/Advanced Method Project/Data/MEPS_summary_weighted.Rdata")
  
} else {
  setwd("C:/Users/shuxian/repos/MEPS_Bayesian")
  load("MEPS_summary_weighted.Rdata")
}

#split training and validation
#1.1 Data construction
generic_share <- MEPS_summary_weighted %>%
  mutate(S_g = N_g/(N_g + N_b)) %>%
  filter(!is.na(P_g),
         !is.na(P_b),
         !is.na(S_g),
         competitor > 0) %>%
  rename(Y = S_g)

generic_share_id <- generic_share %>% 
  distinct(index) 

generic_share_id$ll <- seq.int(nrow(generic_share_id))

#generic_price <- generic_price %>%
#  left_join(generic_price_id) %>%
#  select(Y, ll, competitor, P_b_prior_LOE, t_LOE, year) #, oral, inject, ATCA:ATCV

generic_share <- generic_share %>%
  left_join(generic_share_id) %>%
  select(Y, index, ll, competitor, P_b_prior_LOE, t_LOE, year, Trade_Name, oral, inject, ATCA:ATCV)


generic_share %>% 
  filter(is.na(t_LOE)) %>%
  data.frame()

generic_share <- generic_share %>% 
  filter(!is.na(P_b_prior_LOE)) 

generic_share <- generic_share %>% 
  mutate(P_b_prior_LOE = log(P_b_prior_LOE)) 

write.xlsx(generic_share, "MEPS_generic_share.xlsx")

#1.2 Split data
#1.2.1 count # by group: if 1, put in train only (for RE); for the rest, 5:5 in train and test
#1.2.2 sanity check: examine index in train and test; delete any index in test if not available in training as well
generic_share_n <- generic_share %>%
  group_by(ll) %>%
  count()

generic_share_n_multiple <- generic_share_n %>%
  filter(n > 1) %>%
  select(-n)

generic_share_n_one <- generic_share_n %>%
  filter(n == 1) %>%
  select(-n)

generic_share_multiple <- generic_share %>%
  inner_join(generic_share_n_multiple)

generic_share_one <- generic_share %>%
  inner_join(generic_share_n_one)

generic_share_train <- generic_share_multiple %>% 
  group_by(ll) %>% sample_frac(.5)

generic_share_test <- generic_share_multiple %>%
  anti_join(generic_share_train)

generic_share_train_id <- generic_share_train %>%
  distinct(ll)

generic_share_test <- generic_share_test %>%
  inner_join(generic_share_train_id)

generic_share_test_id <- generic_share_test %>%
  distinct(ll)

save(generic_share_train, file = "generic_share_train_brms.RData")
save(generic_share_test, file = "generic_share_test_brms.RData")

load("generic_share_train_brms.Rdata")
load("generic_share_test_brms.Rdata")

###1. Random intercept
## Noninformative
fit_generic_share_random_intercept_noninformative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + (1|index), 
                                                   data = generic_share_train, family = gaussian(),
                                                   iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                   control = list(adapt_delta = .99, max_treedepth = 20))

print(summary(fit_generic_share_random_intercept_noninformative),digits=5) 

## prediction for noninformative
fit_generic_share_random_intercept_noninformative_prediction <- predict(fit_generic_share_random_intercept_noninformative, newdata = generic_share_test)

fit_generic_share_random_intercept_noninformative_prediction <- fit_generic_share_random_intercept_noninformative_prediction %>%
  cbind(generic_share_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_generic_share_random_intercept_noninformative_prediction$CI_covered)

