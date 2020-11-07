if (.Platform$OS.type == "unix"){
  
  setwd("~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian")
  load("~/Dropbox/Advanced Method Project/Data/MEPS_summary_weighted.Rdata")
  
} else {
  setwd("C:/Users/shuxian/repos/MEPS_Bayesian")
  load("MEPS_summary_weighted.Rdata")
}

#split training and validation
#1.1 Data construction
branded_price <- MEPS_summary_weighted %>%
  filter(!is.na(P_b_SSR),
         !is.na(P_b_SSR_prior_LOE),
         competitor > 0) %>%
  mutate(Y = log(P_b_SSR)) 

branded_price_id <- branded_price %>% 
  distinct(index) 

branded_price_id$ll <- seq.int(nrow(branded_price_id))

#generic_price <- generic_price %>%
#  left_join(generic_price_id) %>%
#  select(Y, ll, competitor, P_b_prior_LOE, t_LOE, year) #, oral, inject, ATCA:ATCV

branded_price <- branded_price %>%
  left_join(branded_price_id) %>%
  select(Y, index, ll, competitor, P_b_SSR_prior_LOE, t_LOE, year, Trade_Name, oral, inject)


branded_price %>% 
  filter(is.na(t_LOE)) %>%
  data.frame()

branded_price <- branded_price %>% 
  filter(!is.na(P_b_SSR_prior_LOE)) 

branded_price <- branded_price %>% 
  mutate(P_b_SSR_prior_LOE = log(P_b_SSR_prior_LOE)) %>%
  distinct()

write.xlsx(branded_price, "MEPS_branded_price.xlsx")

#1.2 Split data
#1.2.1 count # by group: if 1, put in train only (for RE); for the rest, 5:5 in train and test
#1.2.2 sanity check: examine index in train and test; delete any index in test if not available in training as well
branded_price_n <- branded_price %>%
  group_by(ll) %>%
  count()

branded_price_n_multiple <- branded_price_n %>%
  filter(n > 1) %>%
  select(-n)

branded_price_n_one <- branded_price_n %>%
  filter(n == 1) %>%
  select(-n)

branded_price_multiple <- branded_price %>%
  inner_join(branded_price_n_multiple)

branded_price_one <- branded_price %>%
  inner_join(branded_price_n_one)

branded_price_train <- branded_price_multiple %>% 
  group_by(ll) %>% sample_frac(.5)

branded_price_test <- branded_price_multiple %>%
  anti_join(branded_price_train)

branded_price_train_id <- branded_price_train %>%
  distinct(ll)

branded_price_test <- branded_price_test %>%
  inner_join(branded_price_train_id)

branded_price_test_id <- branded_price_test %>%
  distinct(ll)


###1. Random intercept
## Noninformative
fit_branded_random_intercept_noninformative <- brm(Y ~ competitor +  (1|index), 
                                                   data = branded_price_train, family = gaussian(),
                                                   iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                   control = list(adapt_delta = .99, max_treedepth = 20))

print(summary(fit_branded_random_intercept_noninformative),digits=5) 

## prediction for noninformative
fit_branded_random_intercept_noninformative_prediction <- predict(fit_branded_random_intercept_noninformative, newdata = branded_price_test)

fit_branded_random_intercept_noninformative_prediction <- fit_branded_random_intercept_noninformative_prediction %>%
  cbind(branded_price_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_branded_random_intercept_noninformative_prediction$CI_covered)
