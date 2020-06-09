
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
    title = "50% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by # of competitors"
  ) 

fit1 <- stan(
  file = "model_predict_noncentered_rep.stan",  # Stan program
  data = stan.dat_generic_price_train,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,           # total number of iterations per chain
  cores = 1,              # number of cores (could use one per chain)
  refresh = 1000,            # no progress shown
  control = list(adapt_delta = 0.9, max_treedepth = 20)
)
plot(fit1, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)
print(fit1)

ppc_intervals(
  y = y,
  yrep = y_rep_1,
  x =  generic_price_train$competitor,
  prob = 0.5
) +
  labs(
    x = "N",
    y = "log(P_g)",
    title = "50% posterior predictive intervals \nvs observed log generic price",
    subtitle = "by # of competitors"
  ) 

set.seed(1856)
y <- generic_price_train$Y
y_rep_1 <- extract(fit1)[["y_pred"]]
samp200_1 <- sample(nrow(y_rep_1), 200)
ppc_dens_overlay(y, y_rep_1[samp200_1, ])  

ppc_stat(y, y_rep_1, stat = 'median')
ppc_stat(y, y_rep_1, stat = 'mean')


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

