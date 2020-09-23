load("generic_price_cat_train.RData")
load("generic_price_cat_test.RData")


# Stan model 
set.seed(123)
N <- nrow(generic_price_cat_train)
K <- ncol(generic_price_cat_train) - 2
L <- length(unique(generic_price_cat_train$ll))
N_test <- nrow(generic_price_cat_test)
L_test <- length(unique(generic_price_cat_test$ll))

# with noninformative prior
stan.dat_generic_price_cat_train <- list(N = N, 
                                     K = K,
                                     L = L, 
                                     y = generic_price_cat_train$Y, 
                                     ll = generic_price_cat_train$ll, 
                                     x = generic_price_cat_train[, 2:(K+1)],
                                     informative_coefficient = 0)

fit0 <- stan(
  file = "model_cat_predict_noncentered_rep.stan",  # Stan program
  data = stan.dat_generic_price_cat_train,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 18)
)

posterior_fit0 <- as.array(fit0, pars = c("mu[1]", "mu[2]", "mu[3]", "mu[4]", "mu[5]", "mu[6]", "omega[1]", "omega[2]", "omega[3]", "omega[4]", "omega[5]", "omega[6]","sigma", "lp__"))

np_fit0 <- nuts_params(fit0)
head(np_fit0)

color_scheme_set("darkgray")
mcmc_parcoord(posterior_fit0, np = np_fit0)

plot(fit0, plotfun = "trace", pars = c("mu[2]", "mu[3]", "mu[4]"), inc_warmup = TRUE)

print(fit0)

set.seed(1856)
y <- generic_price_cat_train$Y
y_rep <- extract(fit0)[["y_pred"]]
samp200 <- sample(nrow(y_rep), 200)
ppc_dens_overlay(y, y_rep[samp200, ]) +
  ylim(0,0.6)  

ppc_intervals(
  y = y,
  yrep = y_rep,
  x =  generic_price_cat_train$I4_N,
  prob = 0.95
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "95% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by # of competitors (4+)"
  ) 

ppc_intervals(
  y = y,
  yrep = y_rep,
  x =  generic_price_cat_train$I2,
  prob = 0.95
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "95% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by whether second entrant"
  ) 

ppc_intervals(
  y = y,
  yrep = y_rep,
  x =  generic_price_cat_train$I3,
  prob = 0.95
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "95% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by whether third entrant"
  ) 

ppc_stat(y, y_rep, stat = 'median')
ppc_stat(y, y_rep, stat = 'mean')

# with informative prior

stan.dat_generic_price_cat_train <- list(N = N, 
                                         K = K,
                                         L = L, 
                                         y = generic_price_cat_train$Y, 
                                         ll = generic_price_cat_train$ll, 
                                         x = generic_price_cat_train[, 2:(K+1)],
                                         informative_coefficient = 1)

fit1 <- stan(
  file = "model_cat_predict_noncentered_rep.stan",  # Stan program
  data = stan.dat_generic_price_cat_train,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 18)
)

plot(fit1, plotfun = "trace", pars = c("mu[2]", "mu[3]", "mu[4]"), inc_warmup = TRUE)

print(fit1)

y_rep_1 <- extract(fit1)[["y_pred"]]
samp200 <- sample(nrow(y_rep_1), 200)
ppc_dens_overlay(y, y_rep_1[samp200, ]) +
  ylim(0,0.6)

ppc_intervals(
  y = y,
  yrep = y_rep_1,
  x =  generic_price_cat_train$I4_N,
  prob = 0.95
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "95% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by # of competitors (4+)"
  ) 

ppc_intervals(
  y = y,
  yrep = y_rep_1,
  x =  generic_price_cat_train$I2,
  prob = 0.95
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "95% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by whether second entrant"
  )

ppc_intervals(
  y = y,
  yrep = y_rep_1,
  x =  generic_price_cat_train$I3,
  prob = 0.95
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "95% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by whether third entrant"
  )


stan.dat_generic_price_cat_test_noninformative <- list(N = N, 
                                                   K = K,
                                                   L = L, 
                                                   y = generic_price_cat_train$Y, 
                                                   ll = generic_price_cat_train$ll, 
                                                   x = generic_price_cat_train[, 2:(K+1)],
                                                   informative_coefficient = 0,
                                                   N_test = N_test, 
                                                   L_test = L_test, 
                                                   #y_test = generic_price_test$Y, 
                                                   ll_test = generic_price_cat_test$ll, 
                                                   x_test = generic_price_cat_test[, 2:(K+1)])

fit2 <- stan(
  file = "model_cat_predict_noncentered_test.stan",  # Stan program
  data = stan.dat_generic_price_cat_test_noninformative,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 18)
)

y_test <- exp(generic_price_cat_test$Y)

quants <- c(0.025, 0.05, 0.975, 0.95)
y_test_cat_noninformative <- extract(fit2)[["y_pred_test"]]

y_test_cat_noninformative_summary <- apply(y_test_cat_noninformative, 2 , quantile , probs = quants , na.rm = TRUE )

y_test_cat_pred_noninformative <- as.data.frame(t(y_test_cat_noninformative_summary))

y_test_cat_pred_noninformative <- cbind(y_test_cat_pred_noninformative, exp(generic_price_cat_test$Y))

y_test_cat_pred_noninformative <- y_test_cat_pred_noninformative %>%
  rename(Y = `exp(generic_price_cat_test$Y)`) %>%
  mutate(CI_covered_95 = ifelse(Y >= `2.5%` & Y <= `97.5%`, 1, 0),
         CI_covered_90 = ifelse(Y >= `5%` & Y <= `95%`, 1, 0))

mean(y_test_cat_pred_noninformative$CI_covered_95)
mean(y_test_cat_pred_noninformative$CI_covered_90)

MSE_test_cat_noninformative <- 0
for (i in 1:N_test){
  MSE_test_cat_noninformative = MSE_test_cat_noninformative + (mean(y_test_cat_noninformative[,i]) - exp(generic_price_cat_test$Y[i]))^2
}
MSE_test_cat_noninformative <- MSE_test_cat_noninformative/N_test


median(y_test_cat_noninformative)
print(fit2, "y_pred_test", probs=c(.025,.975))


samp200 <- sample(nrow(y_test_cat_noninformative), 1000)
ppc_dens_overlay(y_test, y_test_cat_noninformative[samp200, ]) +
  ylim(0,0.7)

log_lik_2 <- extract_log_lik(fit2)
loo_2 <- loo(log_lik_2)
print(loo_2)

waic_2 <- waic(log_lik_2)




stan.dat_generic_price_cat_test_informative <- list(N = N, 
                                                       K = K,
                                                       L = L, 
                                                       y = generic_price_cat_train$Y, 
                                                       ll = generic_price_cat_train$ll, 
                                                       x = generic_price_cat_train[, 2:(K+1)],
                                                       informative_coefficient = 1,
                                                       N_test = N_test, 
                                                       L_test = L_test, 
                                                       #y_test = generic_price_test$Y, 
                                                       ll_test = generic_price_cat_test$ll, 
                                                       x_test = generic_price_cat_test[, 2:(K+1)])

fit3 <- stan(
  file = "model_cat_predict_noncentered_test.stan",  # Stan program
  data = stan.dat_generic_price_cat_test_informative,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 18)
)

y_test_cat_informative <- extract(fit3)[["y_pred_test"]]

y_test_cat_informative_summary <- apply(y_test_cat_informative, 2 , quantile , probs = quants , na.rm = TRUE )

y_test_cat_pred_informative <- as.data.frame(t(y_test_cat_informative_summary))

y_test_cat_pred_informative <- cbind(y_test_cat_pred_informative, exp(generic_price_cat_test$Y))

y_test_cat_pred_informative <- y_test_cat_pred_informative %>%
  rename(Y = `exp(generic_price_cat_test$Y)`) %>%
  mutate(CI_covered_95 = ifelse(Y >= `2.5%` & Y <= `97.5%`, 1, 0),
         CI_covered_90 = ifelse(Y >= `5%` & Y <= `95%`, 1, 0))

mean(y_test_cat_pred_informative$CI_covered_95)
mean(y_test_cat_pred_informative$CI_covered_90)

MSE_test_cat_informative <- 0
for (i in 1:N_test){
  MSE_test_cat_informative = MSE_test_cat_informative + (mean(y_test_cat_informative[,i]) - exp(generic_price_cat_test$Y[i]))^2
}
MSE_test_cat_informative <- MSE_test_cat_informative/N_test

posterior_interval(fit3)

print(fit3, "y_pred_test", probs=c(.025,.975))


samp200 <- sample(nrow(y_test_cat_informative), 1000)
ppc_dens_overlay(y_test, y_test_cat_informative[samp200, ]) +
  ylim(0,0.7)

log_lik_3 <- extract_log_lik(fit3)
loo_3 <- loo(log_lik_3)
print(loo_3)

waic_3 <- waic(log_lik_3)

loo_diff <- loo_compare(loo_2, loo_3)
print(loo_diff)

waic_diff <- loo_compare(waic_2, waic_3)
print(waic_diff)


# do not use the hierarchical structure to get predicted test result 
fit4 <- stan(
  file = "model_cat_predict_noncentered_test_pooled.stan",  # Stan program
  data = stan.dat_generic_price_cat_test_informative,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 18)
)
