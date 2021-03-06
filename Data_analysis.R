load("MEPS_summary_weighted.RData")

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
MEPS_summary_weighted_noNA <- MEPS_summary_weighted %>%
  filter(!is.na(P_g))
fit1 <- lm(log(P_g) ~ competitor, data = MEPS_summary_weighted_noNA)
summary(fit1)

MEPS_summary_weighted_noNA$res <- fit1$residuals
ggplot(data = MEPS_summary_weighted_noNA, aes(y = res, x = competitor)) + 
  geom_point(col = 'blue') + 
  geom_abline(slope = 0)

var.func <- lm(res^2 ~ competitor, data = MEPS_summary_weighted_noNA)
summary(var.func)

bptest(fit1)

log(var(MEPS_summary_weighted_noNA$P_g))
var(log(MEPS_summary_weighted_noNA$P_g))


fit3 <- lm(log(P_g) ~ competitor + P_b, data = MEPS_summary)
summary(fit3)

MEPS_summary_weighted_noNA <- MEPS_summary_weighted %>%
  filter(!is.na(P_g), !is.na(P_b_prior_LOE))

fit4 <- lm(log(P_g) ~ competitor + t_LOE + log(P_b_prior_LOE), data = MEPS_summary_weighted_noNA)
summary(fit4)

MEPS_summary_weighted_noNA$res <- fit4$residuals
MEPS_summary_weighted_noNA$y_hat <- fit4$fitted.values

ggplot(data = MEPS_summary_weighted_noNA, aes(y = res, x = y_hat)) + 
  geom_point(col = 'blue') + 
  geom_abline(slope = 0)


bptest(fit4)



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

MEPS_summary_weighted <- MEPS_summary_weighted %>%
  mutate(I2 = ifelse(competitor == 2, 1, 0),
         I3 = ifelse(competitor == 3, 1, 0),
         I4 = ifelse(competitor >= 4, 1, 0),
         #competitor4 = competitor - 3,
         competitor4 = competitor,
         I4_N = I4*competitor4)


fit8 <- lm(log(P_g) ~ I2 + I3 + I4_N + t_LOE, data = MEPS_summary_weighted)
summary(fit8)

fit9 <- lm(log(P_g) ~ I2 + I3 + I4_N + t_LOE + P_b_prior_LOE, data = MEPS_summary_weighted)
summary(fit9)

fit10 <- lm(log(P_g) ~ I2 + I3 + I4 + t_LOE + P_b_prior_LOE, data = MEPS_summary_weighted)
summary(fit10)

fit11 <- lm(log(P_g) ~ I2 + I3 + I4 + t_LOE, data = MEPS_summary_weighted)
summary(fit11)

fit12 <- lm(log(P_g) ~ competitor + t_LOE, data = MEPS_summary_weighted)
summary(fit12)

# count % of different entrant
n_entrant <- MEPS_summary_weighted %>%
  filter(competitor != 0) %>% 
  group_by(competitor) %>%
  summarise(count = n()) 

n_entrant <- n_entrant %>%
  group_by(competitor) %>%
  summarise(count = sum(count)) %>%
  mutate(p = count/sum(count)) 

N4 <- n_entrant %>%
  filter(competitor >= 4) %>%
  mutate(sum = competitor*count) %>%
  summarise(total_competitor = sum(sum),
            total = sum(count)) %>%
  mutate(N4 = total_competitor/total)
  

#2SLS

#GEE