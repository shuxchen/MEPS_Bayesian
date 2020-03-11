#naive analysis 
#OLS
fit1 <- lm(log(P_g) ~ competitor, data = MEPS_summary)
summary(fit1)

fit2 <- lm(log(P_b) ~ competitor, data = MEPS_summary)
summary(fit2)

fit3 <- lm(log(P_g) ~ competitor + P_b, data = MEPS_summary)
summary(fit3)

fit4 <- lm(log(P_g) ~ competitor + t_LOE + P_b, data = MEPS_summary)
summary(fit4)

#2SLS

#GEE