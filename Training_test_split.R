setwd("~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian")

load("~/Dropbox/Advanced Method Project/Data/MEPS_summary_weighted.Rdata")

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
  select(Y, ll, intercept, competitor, P_b_prior_LOE, t_LOE, year, oral, inject, ATCA:ATCV)

generic_price %>% 
  filter(is.na(t_LOE)) %>%
  data.frame()

#generic_price <- generic_price %>% 
#  filter(!is.na(t_LOE)) 

#1.2 Split data
spec = c(train = .45, test = .15, validate = .4)

#spec = c(train = .6, validate = .4)


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

generic_price_all <- generic_price %>%
  mutate(year = year - 2012)