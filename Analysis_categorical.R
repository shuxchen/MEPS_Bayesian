setwd("C:/Users/shuxian/repos/MEPS_Bayesian")
load("MEPS_summary_weighted.Rdata")

MEPS_summary_weighted <- MEPS_summary_weighted %>%
  mutate(I2 = ifelse(competitor == 2, 1, 0),
         I3 = ifelse(competitor == 3, 1, 0),
         I4 = ifelse(competitor >= 4, 1, 0),
         #competitor4 = competitor - 3,
         competitor4 = competitor,
         I4_N = I4*competitor4)

generic_price_cat <- MEPS_summary_weighted %>%
  filter(!is.na(P_g)) %>%
  mutate(Y = log(P_g)) 

generic_price_cat_id <- generic_price_cat %>% 
  distinct(index) 

generic_price_cat_id$ll <- seq.int(nrow(generic_price_cat_id))

generic_price_cat <- generic_price_cat %>%
  left_join(generic_price_cat_id) %>%
  mutate(intercept = 1) %>%
  select(Y, ll, intercept, I2, I3, I4_N, P_b_prior_LOE, t_LOE, year, oral, inject, ATCA:ATCV)

generic_price_cat %>% 
  filter(is.na(t_LOE)) %>%
  data.frame()

generic_price_cat <- generic_price_cat %>% 
  filter(!is.na(P_b_prior_LOE)) 

generic_price_cat <- generic_price_cat %>% 
  mutate(P_b_prior_LOE = log(P_b_prior_LOE)) 

fit <- lm(Y ~ I2 + I3 + I4_N + t_LOE + P_b_prior_LOE, data = generic_price_cat)
summary(fit)

spec = c(train = .6, test = .4)


n_index <- generic_price_cat %>%
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

generic_price_cat <- generic_price_cat %>%
  select(1:8)

generic_price_cat_train <- generic_price_cat %>%
  inner_join(res$train, by = c("ll" = "."))
generic_price_cat_test <- generic_price_cat %>%
  inner_join(res$test, by = c("ll" = "."))

#re-assign index 
assign_id <- function(df){
  df_id <- df %>% 
    distinct(ll) 
  
  df_id$index <- seq.int(nrow(df_id))
  
  df <- df %>%
    left_join(df_id) %>%
    select(-ll) %>%
    rename(ll = index)
}

# split data
generic_price_cat_train <- assign_id(generic_price_cat_train)
generic_price_cat_test <- assign_id(generic_price_cat_test)

# Stan model 
set.seed(123)
N <- nrow(generic_price_cat_train)
K <- ncol(generic_price_cat_train) - 2
L <- length(unique(generic_price_cat_train$ll))

stan.dat_generic_price_cat_train <- list(N = N, 
                                     K = K,
                                     L = L, 
                                     y = generic_price_cat_train$Y, 
                                     ll = generic_price_cat_train$ll, 
                                     x = generic_price_cat_train[, 2:(K+1)])

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


