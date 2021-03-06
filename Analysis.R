
if (.Platform$OS.type == "unix"){
  
  setwd("~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian")
  load("~/Dropbox/Advanced Method Project/Data/MEPS_summary_weighted.Rdata")
  
} else {
  setwd("C:/Users/shuxian/repos/MEPS_Bayesian")
  load("MEPS_summary_weighted.Rdata")
}



save(MEPS_summary_weighted, file = "MEPS_summary_weighted.Rdata")

##1. Generic price 
#log(generic price) ~ N 

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
  mutate(intercept = 1) %>%
  select(Y, ll, intercept, competitor, P_b_prior_LOE, t_LOE, year, oral, inject, ATCA:ATCV)

generic_price %>% 
  filter(is.na(t_LOE)) %>%
  data.frame()

generic_price <- generic_price %>% 
  filter(!is.na(P_b_prior_LOE)) 

generic_price <- generic_price %>% 
  mutate(P_b_prior_LOE = log(P_b_prior_LOE)) 

#1.2 Split data
#spec = c(train = .45, test = .15, validate = .4)

spec = c(train = .6, test = .4)


n_index <- generic_price %>%
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

generic_price_with_t <- generic_price
generic_price <- generic_price_with_t %>%
  select(-year)
generic_price <- generic_price_with_t %>%
  mutate(year = year - 2012)
generic_price <- generic_price %>%
  select(1:6)

#generic_price <- generic_price_with_t %>%
#  select(1:5, t_LOE) 

#generic_price <- generic_price_with_t %>%
#  select(1:5, year) %>%
#  mutate(year = year - 2012)

#generic_price <- generic_price_with_t %>%
#  select(1:5, oral, inject) 

generic_price_train <- generic_price %>%
  inner_join(res$train, by = c("ll" = "."))
generic_price_test <- generic_price %>%
  inner_join(res$test, by = c("ll" = "."))
generic_price_validate <- generic_price %>%
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
generic_price_train <- assign_id(generic_price_train) 
generic_price_test <- assign_id(generic_price_test)
generic_price_validate <- assign_id(generic_price_validate)

save(generic_price_train, file = "generic_price_train.RData")
save(generic_price_test, file = "generic_price_test.RData")



#1.3 Stan model 
set.seed(123)
N <- nrow(generic_price_train)
K <- ncol(generic_price_train) - 2
L <- length(unique(generic_price_train$ll))

stan.dat_generic_price_train <- list(N = N, 
                 K = K,
                 L = L, 
                 y = generic_price_train$Y, 
                 ll = generic_price_train$ll, 
                 x = generic_price_train[, 2:(K+1)])

fit0 <- stan(
  file = "model.stan",  # Stan program
  data = stan.dat_generic_price_train,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  #cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

fit0 <- stan(
  file = "model_log_lik.stan",  # Stan program
  data = stan.dat_generic_price_train,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  #cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

plot(fit0, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)

print(fit0)

log_lik_0 <- extract_log_lik(fit0)
waic_0 <- waic(log_lik_0)
waic_0

fit1 <- stan(
  file = "model_log_lik.stan",  # Stan program
  data = stan.dat_generic_price_train,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  #cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

log_lik_1 <- extract_log_lik(fit1)
waic_1 <- waic(log_lik_1)
waic_1

waic_diff <- loo_compare(waic_0, waic_1)
waic_diff

N_test <- nrow(generic_price_test)
L_test <- length(unique(generic_price_test$ll))

stan.dat_generic_price_test <- list(N = N, 
                                    K = K,
                                    L = L, 
                                    y = generic_price_train$Y, 
                                    ll = generic_price_train$ll, 
                                    x = generic_price_train[, 2:(K+1)],
                                    N_test = N_test, 
                                    L_test = L_test, 
                                    #y_test = generic_price_test$Y, 
                                    ll_test = generic_price_test$ll, 
                                    x_test = generic_price_test[, 2:(K+1)])

fit1 <- stan(
  file = "model_predict.stan",  # Stan program
  data = stan.dat_generic_price_test,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

plot(fit1, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)

fit2 <- stan(
  file = "model_predict_log_lik_noncentered.stan",  # Stan program
  data = stan.dat_generic_price_test,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 7000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  #refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

pairs(fit2, pars = c("mu[1]", "mu[2]", "mu[3]", "omega[1]", "omega[2]", "omega[3]","sigma", "lp__"))


plot(fit2, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)

fit3 <- stan(
  file = "model_predict_noncentered.stan",  # Stan program
  data = stan.dat_generic_price_test,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 7000,           # total number of iterations per chain
  #cores = 4,              # number of cores (could use one per chain)
  refresh = 0#,            # no progress shown
  #control = list(adapt_delta = 0.999, max_treedepth = 15)
)

print(fit3)
fit3_results <- extract(fit3)
fit3_results$y_test
fit3_results$y_test[,1]
fit3_results$beta_pred[, , 1]
fit3_results$beta_pred[, , 2]

median(fit3_results$beta_pred[, , 2])
quantile(fit3_results$beta_pred[, , 2], probs = c(.025, .975))

median(fit3_results$y_test[,1])
quantile(fit3_results$y_test[,1], probs = c(.025, .975))

median(fit3_results$y_test[,2])
quantile(fit3_results$y_test[,2], probs = c(.025, .975))

median(fit3_results$y_test[,207])
quantile(fit3_results$y_test[,207], probs = c(.025, .975))


fit4 <- stan(
  file = "model_predict_noncentered_mu.stan",  # Stan program
  data = stan.dat_generic_price_test,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 7000,           # total number of iterations per chain
  #cores = 4,              # number of cores (could use one per chain)
  refresh = 0#,            # no progress shown
  #control = list(adapt_delta = 0.999, max_treedepth = 15)
)
print(fit4)
fit4_results <- extract(fit4)

median(fit4_results$beta_pred[, , 2])
quantile(fit4_results$beta_pred[, , 2], probs = c(.025, .975))

mean(fit4_results$y_test[,1])
quantile(fit4_results$y_test[,1], probs = c(.025, .975))

median(fit4_results$y_test[,2])
quantile(fit4_results$y_test[,2], probs = c(.025, .975))

mean(fit4_results$y_test[,207])
quantile(fit4_results$y_test[,207], probs = c(.025, .975))

mean(fit4_results$y_test[,206])
quantile(fit4_results$y_test[,206], probs = c(.025, .975))








log_lik_1 <- extract_log_lik(fit2)
waic_1 <- waic(log_lik_1)
waic_1

log_lik_2 <- extract_log_lik(fit2)
waic_2 <- waic(log_lik_2)
waic_2

log_lik_3 <- extract_log_lik(fit2)
waic_3 <- waic(log_lik_3)
waic_3

log_lik_4 <- extract_log_lik(fit2)
waic_4 <- waic(log_lik_4)
waic_4

waic_diff <- loo_compare(waic_1, waic_2, waic_3, waic_4)
waic_diff


loo_1 <- loo(log_lik_1)
loo_2 <- loo(log_lik_2)
loo_3 <- loo(log_lik_3)
loo_diff <- loo_compare(loo_1, loo_2, loo_3)
loo_diff

print(loo_1)
test <- lm(Y ~ competitor, data = generic_price_train)
summary(test)

fit0_result <- extract(fit0)
beta0 <- median(fit0_result$mu[,1])
beta1 <- median(fit0_result$mu[,2])

generic_price_test <- generic_price_test %>%
  mutate(Y_hat = beta0 + beta1*competitor,
         residual = Y - Y_hat,
         error_sq = (beta0 + beta1*competitor - Y)^2)

generic_price_train <- generic_price_train %>%
  mutate(Y_hat = beta0 + beta1*competitor,
         residual = Y - Y_hat,
         error_sq = (beta0 + beta1*competitor - Y)^2)

MSE <- mean(generic_price_test$error_sq)
#MSE_0 = 0.782782
#beta0 = 5.32
#beta1 = -0.07

##plot: predicted vs. actual
ggplot(generic_price_test, aes(x = Y, y = Y_hat)) +  
  geom_point()

ggplot(generic_price_test, aes(x = competitor, y = Y)) +
  geom_segment(aes(xend = competitor, yend = Y_hat)) +
  geom_point() +
  geom_point(aes(y = Y_hat), shape = 1) 

ggplot(generic_price_test, aes(x = competitor, y = Y)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = competitor, yend = Y_hat), alpha = .2) +
  geom_point(aes(color = residual)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  #   guides(color = FALSE) +  # Color legend removed
  #geom_point(aes(color = abs(residual))) + # Color mapped to abs(residuals)
  #scale_color_continuous(low = "black", high = "red") +  # Colors to use here  
  geom_point(aes(y = Y_hat), shape = 1) +
  theme_bw()

beta0_0 <- beta0
beta1_0 <- beta1
fit_0 <- fit0
MSE_0 <- MSE

beta0_1 <- beta0
beta1_1 <- beta1
fit_1 <- fit0
MSE_1 <- MSE

beta0_2 <- beta0
beta1_2 <- beta1
fit_2 <- fit0
MSE_2 <- MSE
