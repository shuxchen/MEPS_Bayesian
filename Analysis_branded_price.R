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

