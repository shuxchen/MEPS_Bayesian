setwd("C:/Users/shuxian/repos/MEPS_Bayesian")

load("generic_price_train.Rdata")
load("generic_price_test.Rdata")

generic_price_train <- generic_price_train[c(1,2,4,5,3,6)]
generic_price_test <- generic_price_test[c(1,2,4,5,3,6)]

set.seed(123)
N <- nrow(generic_price_train)
K <- ncol(generic_price_train) - 2
L <- length(unique(generic_price_train$ll))

N_test <- nrow(generic_price_test)
L_test <- length(unique(generic_price_test$ll))

stan.dat_generic_price_test_informative <- list(N = N, 
                                                K = K,
                                                L = L, 
                                                y = generic_price_train$Y, 
                                                ll = generic_price_train$ll, 
                                                x = generic_price_train[, 2:K],
                                                x_N = generic_price_train$competitor,
                                                informative_coefficient = 1,
                                                N_test = N_test, 
                                                L_test = L_test, 
                                                #y_test = generic_price_test$Y, 
                                                ll_test = generic_price_test$ll, 
                                                x_test = generic_price_test[, 2:K],
                                                x_N_test = generic_price_test$competitor)


fit <- stan(
  file = "model_predict_noncentered_test_updated.stan",  # Stan program
  data = stan.dat_generic_price_test_informative,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,           # total number of iterations per chain
  #cores = 4,              # number of cores (could use one per chain)
  refresh = 1000#,            # no progress shown
  #control = list(adapt_delta = 0.9, max_treedepth = 20)
)

plot(fit, plotfun = "trace", pars = c("mu_N"), inc_warmup = TRUE)
print(fit)



set.seed(1856)

quants <- c(0.025, 0.05, 0.975, 0.95)
y_test_rep_informative_summary <- apply(y_test_rep_informative, 2 , quantile , probs = quants , na.rm = TRUE )

y_test_pred_informative <- as.data.frame(t(y_test_rep_informative_summary))

y_test_pred_informative <- cbind(y_test_pred_informative, generic_price_test$Y)

y_test_pred_informative <- y_test_pred_informative %>%
  rename(Y = `generic_price_test$Y`) %>%
  mutate(CI_covered_95 = ifelse(Y >= `2.5%` & Y <= `97.5%`, 1, 0),
         CI_covered_90 = ifelse(Y >= `5%` & Y <= `95%`, 1, 0))

mean(y_test_pred_informative$CI_covered_95)
mean(y_test_pred_informative$CI_covered_90)
