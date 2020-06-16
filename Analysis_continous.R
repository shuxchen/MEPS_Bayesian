setwd("C:/Users/shuxian/repos/MEPS_Bayesian")
load("MEPS_summary_weighted.Rdata")

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



#1.3 Stan model 
set.seed(123)
N <- nrow(generic_price_train)
K <- ncol(generic_price_train) - 2
L <- length(unique(generic_price_train$ll))


N_test <- nrow(generic_price_test)
L_test <- length(unique(generic_price_test$ll))

stan.dat_generic_price_test <- list(N = N, 
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
