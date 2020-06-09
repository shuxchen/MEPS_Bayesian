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


