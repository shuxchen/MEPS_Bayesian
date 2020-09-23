load("MEPS_summary_weighted.RData")

fit0 <- lm(log(P_b) ~ competitor, data = MEPS_summary_weighted)
summary(fit0)

MEPS_summary_weighted_noNA <- MEPS_summary_weighted %>%
  filter(!is.na(P_b))

fit0 <- lm(log(P_b) ~ competitor, data = MEPS_summary_weighted_noNA)
summary(fit0)

MEPS_summary_weighted_noNA$res <- fit0$residuals
ggplot(data = MEPS_summary_weighted_noNA, aes(y = res, x = competitor)) + 
  geom_point(col = 'blue') + 
  geom_abline(slope = 0)

fit1 <- lm(log(P_b) ~ competitor + P_b_prior_LOE + t_LOE, data = MEPS_summary_weighted_noNA)
summary(fit1)

fit2 <- gee(log(P_b) ~ competitor + P_b_prior_LOE + t_LOE, data = MEPS_summary_weighted, id = index, family = gaussian, corstr = "exchangeable")
summary(fit2)

MEPS_summary_weighted_noNA <- MEPS_summary_weighted_noNA %>%
  filter(!is.na(P_b_prior_LOE))

MEPS_summary_weighted_noNA$res <- fit1$residuals
MEPS_summary_weighted_noNA$y_hat <- fit1$fitted.values

ggplot(data = MEPS_summary_weighted_noNA, aes(y = res, x = y_hat)) + 
  geom_point(col = 'blue') + 
  geom_abline(slope = 0)

fit3 <- lm(log(P_b) ~ I2 + I3 + I4 + P_b_prior_LOE + t_LOE, data = MEPS_summary_weighted)
summary(fit3)




setwd("C:/Users/shuxian/repos/MEPS_Bayesian")
load("MEPS_summary_weighted.Rdata")

#1.1 Data construction
branded_price <- MEPS_summary_weighted %>%
  filter(!is.na(P_b)) %>%
  mutate(Y = log(P_b)) 

branded_price_id <- branded_price %>% 
  distinct(index) 

branded_price_id$ll <- seq.int(nrow(branded_price_id))

branded_price <- branded_price %>%
  left_join(branded_price_id) %>%
  mutate(intercept = 1) %>%
  select(Y, ll, intercept, competitor, P_b_prior_LOE, t_LOE, year, oral, inject, ATCA:ATCV)

branded_price %>% 
  filter(is.na(t_LOE)) %>%
  data.frame()

branded_price <- branded_price %>% 
  filter(!is.na(P_b_prior_LOE)) 

branded_price <- branded_price %>% 
  mutate(P_b_prior_LOE = log(P_b_prior_LOE)) %>%
  filter(competitor >0)

#1.2 Split data
#spec = c(train = .45, test = .15, validate = .4)

spec = c(train = .6, test = .4)


n_index <- branded_price %>%
  distinct(ll) %>%
  count()

n_index <- n_index$n

list <- seq(1, n_index, by = 1) %>%
  data.frame()

g = sample(cut(
  seq(nrow(list)), 
  nrow(list)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(list, g)

branded_price_with_t <- branded_price
branded_price <- branded_price_with_t %>%
  select(-year)
branded_price <- branded_price_with_t %>%
  mutate(year = year - 2012)
branded_price <- branded_price %>%
  select(1:6)


branded_price_train <- branded_price %>%
  inner_join(res$train, by = c("ll" = "."))
branded_price_test <- branded_price %>%
  inner_join(res$test, by = c("ll" = "."))
branded_price_validate <- branded_price %>%
  inner_join(res$validate, by = c("ll" = "."))

#re-assign index 
assign_id <- function(df){
  df_id <- df %>% 
    distinct(ll) 
  
  df_id$index <- seq.int(nrow(df_id))
  
  df <- df %>%
    left_join(df_id) %>%
    select(-ll) %>%
    rename(ll = index) %>%
    arrange(Y, ll, intercept, competitor)
}

# split data
branded_price_train <- assign_id(branded_price_train) 
branded_price_test <- assign_id(branded_price_test)
branded_price_validate <- assign_id(branded_price_validate)

save(branded_price_train, file = "branded_price_train.RData")
save(branded_price_test, file = "branded_price_test.RData")

load("branded_price_train.RData")
load("branded_price_test.RData")

#branded_price_train <- branded_price_train %>%
#  select(-P_b_prior_LOE, -competitor, -t_LOE)

#branded_price_test <- branded_price_test %>%
#  select(-P_b_prior_LOE, -competitor, -t_LOE)

set.seed(123)
N <- nrow(branded_price_train)
K <- ncol(branded_price_train) - 2
L <- length(unique(branded_price_train$ll))

N_test <- nrow(branded_price_test)
L_test <- length(unique(branded_price_test$ll))

stan.dat_branded_price_test_noninformative <- list(N = N, 
                                                   K = K,
                                                   L = L, 
                                                   y = branded_price_train$Y, 
                                                   ll = branded_price_train$ll, 
                                                   x = branded_price_train[, 2:(K+1)],
                                                   informative_coefficient = 0,
                                                   N_test = N_test, 
                                                   L_test = L_test, 
                                                   #y_test = branded_price_test$Y, 
                                                   ll_test = branded_price_test$ll, 
                                                   x_test = branded_price_test[, 2:(K+1)])


fit2 <- stan(
  file = "model_predict_noncentered_branded_test.stan",  # Stan program
  data = stan.dat_branded_price_test_noninformative,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 20)
)

plot(fit2, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)
print(fit2)

log_lik_2 <- extract_log_lik(fit2)
loo_2 <- loo(log_lik_2)
print(loo_2)

waic_2 <- waic(log_lik_2)

set.seed(1999)
y_test <- branded_price_test$Y
y_test_rep_noninformative <- extract(fit2)[["y_pred_test"]]
samp100 <- sample(nrow(y_test_rep_noninformative), 1000)
ppc_dens_overlay(y_test, y_test_rep_noninformative[samp100, ]) +
  ylim(0,0.6) +
  xlim(-2, 12)

quants <- c(0.025, 0.05, 0.975, 0.95)
y_test_rep_noninformative_summary <- apply(y_test_rep_noninformative, 2 , quantile , probs = quants , na.rm = TRUE )

y_test_pred_noninformative <- as.data.frame(t(y_test_rep_noninformative_summary))

y_test_pred_noninformative <- cbind(y_test_pred_noninformative, branded_price_test$Y)

y_test_pred_noninformative <- y_test_pred_noninformative %>%
  rename(Y = `branded_price_test$Y`) %>%
  mutate(CI_covered_95 = ifelse(Y >= `2.5%` & Y <= `97.5%`, 1, 0),
         CI_covered_90 = ifelse(Y >= `5%` & Y <= `95%`, 1, 0))

mean(y_test_pred_noninformative$CI_covered_95)
mean(y_test_pred_noninformative$CI_covered_90)

MSE_test_noninformative <- 0
for (i in 1:N_test){
  MSE_test_noninformative = MSE_test_noninformative + (mean(y_test_rep_noninformative[,i]) - branded_price_test$Y[i])^2
}
MSE_test_noninformative <- MSE_test_noninformative/N_test


stan.dat_branded_price_test_informative <- list(N = N, 
                                                K = K,
                                                L = L, 
                                                y = branded_price_train$Y, 
                                                ll = branded_price_train$ll, 
                                                x = branded_price_train[, 2:(K+1)],
                                                informative_coefficient = 1,
                                                N_test = N_test, 
                                                L_test = L_test, 
                                                #y_test = branded_price_test$Y, 
                                                ll_test = branded_price_test$ll, 
                                                x_test = branded_price_test[, 2:(K+1)])


fit3 <- stan(
  file = "model_predict_noncentered_branded_test.stan",  # Stan program
  data = stan.dat_branded_price_test_informative,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 20)
)

plot(fit3, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)
print(fit3)

log_lik_3 <- extract_log_lik(fit3)
loo_3 <- loo(log_lik_3)
print(loo_3)

waic_3 <- waic(log_lik_3)

loo_diff <- loo_compare(loo_2, loo_3)
print(loo_diff)

waic_diff <- loo_compare(waic_2, waic_3)
print(waic_diff)

y_test_rep_informative <- extract(fit3)[["y_pred_test"]]
samp100 <- sample(nrow(y_test_rep_informative), 1000)
ppc_dens_overlay(y_test, y_test_rep_informative[samp100, ]) +
  ylim(0,0.6) +
  xlim(-2, 12)

y_test_rep_informative_summary <- apply(y_test_rep_informative, 2 , quantile , probs = quants , na.rm = TRUE )

y_test_pred_informative <- as.data.frame(t(y_test_rep_informative_summary))

y_test_pred_informative <- cbind(y_test_pred_informative, branded_price_test$Y)

y_test_pred_informative <- y_test_pred_informative %>%
  rename(Y = `branded_price_test$Y`) %>%
  mutate(CI_covered_95 = ifelse(Y >= `2.5%` & Y <= `97.5%`, 1, 0),
         CI_covered_90 = ifelse(Y >= `5%` & Y <= `95%`, 1, 0))

mean(y_test_pred_informative$CI_covered_95)
mean(y_test_pred_informative$CI_covered_90)


MSE_test_informative <- 0
for (i in 1:N_test){
  MSE_test_informative = MSE_test_informative + (mean(y_test_rep_informative[,i]) - branded_price_test$Y[i])^2
}
MSE_test_informative <- MSE_test_informative/N_test

