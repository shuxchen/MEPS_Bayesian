setwd("~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian")

load("~/Dropbox/Advanced Method Project/Data/MEPS_summary_weighted.Rdata")

load("MEPS_summary_weighted.Rdata")
save(MEPS_summary_weighted, file = "MEPS_summary_weighted.Rdata")
##1. Generic price 
#log(generic price) ~ N 

#1.1 Data construction
generic_price <- MEPS_summary_weighted %>%
  filter(!is.na(P_g)) %>%
  mutate(Y = log(P_g)) 

generic_price_id <- generic_price %>% 
  distinct(index) 

generic_price_id$ll <- seq.int(nrow(generic_price_id))

generic_price <- generic_price %>%
  left_join(generic_price_id) %>%
  mutate(intercept = 1) %>%
  select(Y, ll, intercept, competitor, t_LOE, year, oral, inject, ATCA:ATCV)

generic_price %>% 
  filter(is.na(t_LOE)) %>%
  data.frame()

#generic_price <- generic_price %>% 
#  filter(!is.na(t_LOE)) 

#1.2 Split data
spec = c(train = .45, test = .15, validate = .4)

spec = c(train = .6, test = .4)


n_index <- generic_price %>%
  distinct(ll) %>%
  count()

list <- seq(1, 130, by = 1) %>%
  data.frame()

g = sample(cut(
  seq(nrow(list)), 
  nrow(list)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(list, g)

generic_price_all <- generic_price

generic_price <- generic_price_all %>%
  select(1:5)

generic_price <- generic_price_with_t %>%
  select(-year)
generic_price <- generic_price %>%
  mutate(year = year - 2012)
generic_price <- generic_price %>%
  select(-t_LOE, -year)

generic_price <- na.omit(generic_price)

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


generic_price_train <- assign_id(generic_price_train) 
generic_price_test <- assign_id(generic_price_test)
generic_price_validate <- assign_id(generic_price_validate)



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
  chains = 4,             # number of Markov chains
  warmup = 5000,          # number of warmup iterations per chain
  iter = 15000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.9999, max_treedepth = 15)
)

plot(fit0, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)

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
                                    #ll_test = generic_price_test$ll, 
                                    x_test = generic_price_test[, 2:(K+1)])

fit1 <- stan(
  file = "model_predict.stan",  # Stan program
  data = stan.dat_generic_price_test,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 5000,          # number of warmup iterations per chain
  iter = 15000,           # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.9999, max_treedepth = 15)
)

plot(fit1, plotfun = "trace", pars = c("mu[2]"), inc_warmup = TRUE)

ext_fit1 <- extract(fit1)
apply(ext_fit1$y_test, 2, median)
mean(apply(ext_fit1$y_test, 2, median) == y_test)


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

beta0_3 <- beta0
beta1_3 <- beta1
fit_3 <- fit0
MSE_3 <- MSE



param.sample <- as.data.frame(fit0)  

generic_price_train$Nbar <- mean(generic_price_train$N)

# posterior predicted values for rat weight using the new data frame (data2):
result.sample<- t(apply(param.sample, 1, function(x) x["mu_alpha"] + x["mu_beta"]*(data2$xbar - data2$x) ))

# summary
result.summary <- apply(result.sample, 2, mean) 
result.summary.std <- apply(result.sample, 2, sd) 

