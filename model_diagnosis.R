if (.Platform$OS.type == "unix"){
  setwd("~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian")
  source("Aim1/MEPS_Bayesian/load_packages.R")

} else {
  setwd("C:\\Users\\shuxian\\repos\\MEPS_Bayesian")
  source("load_packages.R")
}

load("generic_price_train.Rdata")
load("generic_price_test.Rdata")



set.seed(123)
N <- nrow(generic_price_train)
K <- ncol(generic_price_train) - 2
L <- length(unique(generic_price_train$ll))

stan.dat_generic_price_train <- list(N = N, 
                                     K = K,
                                     L = L, 
                                     y = generic_price_train$Y, 
                                     ll = generic_price_train$ll, 
                                     x = generic_price_train[, 2:(K+1)],
                                     informative_coefficient = 1)

fit0 <- stan(
  file = "model_predict_noncentered_rep.stan",  # Stan program
  data = stan.dat_generic_price_train,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  thin = 5,
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 20)
)

plot(fit0, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)
print(fit0)

mcmc_scatter(
  as.matrix(fit0),
  pars = c("mu[2]", "sigma"), 
  np = nuts_params(fit0), 
  np_style = scatter_style_np(div_color = "green", div_alpha = 0.8)
)

pairs(fit0, pars = c("mu[1]", "mu[2]", "mu[3]", "mu[4]", "omega[1]", "omega[2]", "omega[3]", "omega[4]", "sigma"))

mcmc_parcoord(fit0, pars = c("mu[1]", "mu[2]", "mu[3]", "mu[4]", "omega[1]", "omega[2]", "omega[3]", "omega[4]", "sigma"))

plot(fit0, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)


set.seed(1856)
y <- generic_price_train$Y
y_rep <- extract(fit0)[["y_pred"]]
samp200 <- sample(nrow(y_rep), 200)
ppc_dens_overlay(y, y_rep[samp200, ])  

ppc_stat(y, y_rep, stat = 'median')
ppc_stat(y, y_rep, stat = 'mean')

ppc_intervals(
  y = y,
  yrep = y_rep,
  x =  generic_price_train$competitor,
  prob = 0.95
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "95% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by # of competitors"
  ) 

log_lik_0 <- extract_log_lik(fit0)
waic_0 <- waic(log_lik_0)
waic_0

stan.dat_generic_price_train_noninformative <- list(N = N, 
                                     K = K,
                                     L = L, 
                                     y = generic_price_train$Y, 
                                     ll = generic_price_train$ll, 
                                     x = generic_price_train[, 2:(K+1)],
                                     informative_coefficient = 0)

fit1 <- stan(
  file = "model_predict_noncentered_rep.stan",  # Stan program
  data = stan.dat_generic_price_train_noninformative,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  thin = 5,
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 20)
)

plot(fit1, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)
print(fit1)


set.seed(1856)
y <- generic_price_train$Y
y_rep_1 <- extract(fit1)[["y_pred"]]
samp200_1 <- sample(nrow(y_rep_1), 200)
ppc_dens_overlay(y, y_rep_1[samp200_1, ])  

ppc_intervals(
  y = y,
  yrep = y_rep_1,
  x =  generic_price_train$competitor,
  prob = 0.95
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "95% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by # of competitors"
  ) 

ppc_stat(y, y_rep_1, stat = 'median')
ppc_stat(y, y_rep_1, stat = 'mean')

log_lik_1 <- extract_log_lik(fit1)
waic_1 <- waic(log_lik_1)
waic_1

waic_diff <- loo_compare(waic_0, waic_1)
waic_diff



N_test <- nrow(generic_price_test)
L_test <- length(unique(generic_price_test$ll))

stan.dat_generic_price_test_noninformative <- list(N = N, 
                                    K = K,
                                    L = L, 
                                    y = generic_price_train$Y, 
                                    ll = generic_price_train$ll, 
                                    x = generic_price_train[, 2:(K+1)],
                                    informative_coefficient = 0,
                                    N_test = N_test, 
                                    L_test = L_test, 
                                    #y_test = generic_price_test$Y, 
                                    ll_test = generic_price_test$ll, 
                                    x_test = generic_price_test[, 2:(K+1)])


fit2 <- stan(
  file = "model_predict_noncentered_test.stan",  # Stan program
  data = stan.dat_generic_price_test_noninformative,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 20)
)

plot(fit2, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)
print(fit2)

ratios2 <- neff_ratio(fit2)
print(ratios2)

set.seed(1999)
y_test <- generic_price_test$Y
y_test_rep_noninformative <- extract(fit2)[["y_pred_test"]]
samp100 <- sample(nrow(y_test_rep), 1000)
ppc_dens_overlay(y_test, y_test_rep_noninformative[samp100, ])  

ppc_intervals(
  y = y_test,
  yrep = y_test_rep,
  x =  generic_price_test$competitor,
  prob = 0.95
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "95% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by # of competitors"
  ) 

ppc_stat(y_test, y_test_rep, stat = 'median')
ppc_stat(y_test, y_test_rep, stat = 'mean')

quants <- c(0.025,0.975)
y_test_rep_noninformative_summary <- apply(y_test_rep_noninformative, 2 , quantile , probs = quants , na.rm = TRUE )

y_test_pred_noninformative <- as.data.frame(t(y_test_rep_noninformative_summary))

y_test_pred_noninformative <- cbind(y_test_pred_noninformative, generic_price_test$Y)

y_test_pred_noninformative <- y_test_pred_noninformative %>%
  rename(Y = `generic_price_test$Y`) %>%
  mutate(CI_covered = ifelse(Y >= `2.5%` & Y <= `97.5%`, 1, 0))

mean(y_test_pred_noninformative$CI_covered)

stan.dat_generic_price_test_informative <- list(N = N, 
                                                   K = K,
                                                   L = L, 
                                                   y = generic_price_train$Y, 
                                                   ll = generic_price_train$ll, 
                                                   x = generic_price_train[, 2:(K+1)],
                                                   informative_coefficient = 1,
                                                   N_test = N_test, 
                                                   L_test = L_test, 
                                                   #y_test = generic_price_test$Y, 
                                                   ll_test = generic_price_test$ll, 
                                                   x_test = generic_price_test[, 2:(K+1)])


fit3 <- stan(
  file = "model_predict_noncentered_test.stan",  # Stan program
  data = stan.dat_generic_price_test_informative,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 20)
)

plot(fit3, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)
print(fit3)

ratios3 <- neff_ratio(fit3)
print(ratios3)

set.seed(1856)
y_test_rep_informative <- extract(fit3)[["y_pred_test"]]
samp100 <- sample(nrow(y_test_rep_informative), 1000)
ppc_dens_overlay(y_test, y_test_rep_informative[samp100, ])  

quants <- c(0.025,0.975)
y_test_rep_informative_summary <- apply(y_test_rep_informative, 2 , quantile , probs = quants , na.rm = TRUE )

y_test_pred_informative <- as.data.frame(t(y_test_rep_informative_summary))

y_test_pred_informative <- cbind(y_test_pred_informative, generic_price_test$Y)

y_test_pred_informative <- y_test_pred_informative %>%
  rename(Y = `generic_price_test$Y`) %>%
  mutate(CI_covered = ifelse(Y >= `2.5%` & Y <= `97.5%`, 1, 0))

mean(y_test_pred_informative$CI_covered)

ppc_intervals(
  y = y_test,
  yrep = y_test_rep_informative,
  x =  generic_price_test$competitor,
  prob = 0.95
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "95% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by # of competitors"
  ) 

ppc_stat(y_test, y_test_rep_informative, stat = 'median')
ppc_stat(y_test, y_test_rep_informative, stat = 'mean')



fit2 <- stan(
  file = "model_predict_noncentered_student_t.stan",  # Stan program
  data = stan.dat_generic_price_train,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 1,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.85, max_treedepth = 15)
)
plot(fit2, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)
print(fit2)

y_rep_2 <- extract(fit2)[["y_pred"]]
samp200_2 <- sample(nrow(y_rep_2), 200)
ppc_dens_overlay(y, y_rep_2[samp200_2, ])  
