if (.Platform$OS.type == "unix"){
  
  setwd("~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian")
  load("~/Dropbox/Advanced Method Project/Data/MEPS_summary_weighted.Rdata")
  
} else {
  setwd("C:/Users/shuxian/repos/MEPS_Bayesian")
  load("MEPS_summary_weighted.Rdata")
}

#split training and validation
#1.1 Data construction
total_utilization <- MEPS_summary_weighted %>%
  mutate(total = (N_g + N_b)) %>%
  filter(!is.na(P_g),
         !is.na(P_b),
         competitor > 0) %>%
  mutate(Y = log(total))

total_utilization_id <- total_utilization %>% 
  distinct(index) 

total_utilization_id$ll <- seq.int(nrow(total_utilization_id))

#generic_price <- generic_price %>%
#  left_join(generic_price_id) %>%
#  select(Y, ll, competitor, P_b_prior_LOE, t_LOE, year) #, oral, inject, ATCA:ATCV

total_utilization <- total_utilization %>%
  left_join(total_utilization_id) %>%
  select(Y, index, ll, competitor, P_b_prior_LOE, t_LOE, year, Trade_Name, oral, inject, ATCA:ATCV)


total_utilization %>% 
  filter(is.na(t_LOE)) %>%
  data.frame()

total_utilization <- total_utilization %>% 
  filter(!is.na(P_b_prior_LOE)) 

total_utilization <- total_utilization %>% 
  mutate(P_b_prior_LOE = log(P_b_prior_LOE)) 

write.xlsx(total_utilization, "MEPS_total_utilization.xlsx")

#1.2 Split data
#1.2.1 count # by group: if 1, put in train only (for RE); for the rest, 5:5 in train and test
#1.2.2 sanity check: examine index in train and test; delete any index in test if not available in training as well
total_utilization_n <- total_utilization %>%
  group_by(ll) %>%
  count()

total_utilization_n_multiple <- total_utilization_n %>%
  filter(n > 1) %>%
  select(-n)

total_utilization_n_one <- total_utilization_n %>%
  filter(n == 1) %>%
  select(-n)

total_utilization_multiple <- total_utilization %>%
  inner_join(total_utilization_n_multiple)

total_utilization_one <- total_utilization %>%
  inner_join(total_utilization_n_one)

total_utilization_train <- total_utilization_multiple %>% 
  group_by(ll) %>% sample_frac(.5)

total_utilization_test <- total_utilization_multiple %>%
  anti_join(total_utilization_train)

total_utilization_train_id <- total_utilization_train %>%
  distinct(ll)

total_utilization_test <- total_utilization_test %>%
  inner_join(total_utilization_train_id)

total_utilization_test_id <- total_utilization_test %>%
  distinct(ll)

save(total_utilization_train, file = "total_utilization_train_brms.RData")
save(total_utilization_test, file = "total_utilization_test_brms.RData")

load("total_utilization_train_brms.Rdata")
load("total_utilization_test_brms.Rdata")

###1. Random intercept
## Noninformative
fit_total_utilization_random_intercept_noninformative <- brm(Y ~ competitor + t_LOE + P_b_prior_LOE + (1|index), 
                                                         data = total_utilization_train, family = gaussian(),
                                                         iter = 6000, warmup = 1000, chains = 4, cores = 4,
                                                         control = list(adapt_delta = .99, max_treedepth = 20))

print(summary(fit_total_utilization_random_intercept_noninformative),digits=5) 

## prediction for noninformative
fit_total_utilization_random_intercept_noninformative_prediction <- predict(fit_total_utilization_random_intercept_noninformative, newdata = total_utilization_test)

fit_total_utilization_random_intercept_noninformative_prediction <- fit_total_utilization_random_intercept_noninformative_prediction %>%
  cbind(total_utilization_test[1]) %>%
  mutate(CI_covered = ifelse(`Q2.5` <= Y & `Q97.5` >= Y, 1, 0))

mean(fit_total_utilization_random_intercept_noninformative_prediction$CI_covered)