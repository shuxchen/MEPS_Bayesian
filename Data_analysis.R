#naive analysis 
#get price ratio:
price_ratio <- MEPS_summary_weighted %>%
  select(P_ratio, competitor) 

price_ratio <- na.omit(price_ratio)

price_ratio <- price_ratio %>%
  rename(N = competitor) %>%
  mutate(P_ratio = 100*P_ratio) %>%
  group_by(N) %>%
  summarise(y = mean(P_ratio)) %>%
  mutate(group = "MEPS")

save(price_ratio, file = "price_ratio.Rdata")

#use mean branded price before entry as base price?
base_price <- MEPS_summary_weighted %>%
  filter(competitor == 0) %>%
  filter(!is.na(P_b)) %>%
  group_by(index) %>%
  summarise(P_base = mean(P_b))
  
price_ratio_base <- MEPS_summary_weighted %>%
  select(P_g, competitor, index) %>%
  left_join(base_price) %>%
  mutate(P_ratio_base = 100*P_g/P_base) %>%
  select(P_ratio_base, competitor)

price_ratio_base <- na.omit(price_ratio_base)

price_ratio_base <- price_ratio_base %>%
  rename(N = competitor) %>%
  group_by(N) %>%
  summarise(y = mean(P_ratio_base)) %>%
  mutate(group = "MEPS_price_ratio_base")

save(price_ratio_base, file = "price_ratio_base.Rdata")

#use branded price prior to entry (last year) as base price
price_ratio_prior_LOE <- MEPS_summary_weighted %>%
  mutate(P_ratio_prior_LOE = 100*P_g/P_b_prior_LOE) %>%
  select(P_ratio_prior_LOE, competitor)

price_ratio_prior_LOE <- na.omit(price_ratio_prior_LOE)

price_ratio_prior_LOE <- price_ratio_prior_LOE %>%
  rename(N = competitor) %>%
  group_by(N) %>%
  summarise(y = mean(P_ratio_prior_LOE)) %>%
  mutate(group = "MEPS_price_ratio_LOE")

save(price_ratio_prior_LOE, file = "price_ratio_prior_LOE.Rdata")


##OLS
fit1 <- lm(log(P_g) ~ competitor, data = MEPS_summary)
summary(fit1)

fit2 <- lm(log(P_b) ~ competitor, data = MEPS_summary)
summary(fit2)

fit3 <- lm(log(P_g) ~ competitor + P_b, data = MEPS_summary)
summary(fit3)

fit4 <- lm(log(P_g) ~ competitor + t_LOE + P_b, data = MEPS_summary_weighted)
summary(fit4)

fit5 <- lm(log(P_g) ~ as.factor(competitor) + t_LOE + P_b, data = MEPS_summary_weighted)
summary(fit5)

test <- MEPS_summary_weighted %>%
  mutate(competitor_cat = case_when(competitor == 1 ~ "1",
                                    competitor == 2 | competitor == 3 ~ "2-3",
                                    competitor == 4 | competitor == 5 | competitor == 6 ~ "4-6",
                                    competitor == 7 | competitor == 8 | competitor == 9 ~ "7-9",
                                    TRUE ~ "above 10"))

fit6 <- lm(log(P_g) ~ competitor_cat, data = test)
summary(fit6)

fit7 <- lm(log(P_g) ~ competitor_cat + t_LOE + P_b, data = test)
summary(fit7)

#2SLS

#GEE