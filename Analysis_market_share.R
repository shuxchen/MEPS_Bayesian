load("MEPS_summary_weighted.RData")

MEPS_summary_weighted <- MEPS_summary_weighted %>%
  mutate(s_g = N_g/(N_b + N_g))

fit0 <- lm(s_g ~ competitor, data = MEPS_summary_weighted)
summary(fit0)

fit1 <- glm(s_g ~ competitor, data = MEPS_summary_weighted, family = binomial)
summary(fit1)
m <- margins(fit1)
m

fit2 <- glm(s_g ~ competitor + t_LOE, data = MEPS_summary_weighted, family = binomial)
summary(fit2)
m2 <- margins(fit2)
plot(m2)

MEPS_summary_weighted <- MEPS_summary_weighted %>%
  mutate(competitor_cat = case_when(competitor == 1 ~ "1",
                                    competitor == 2 | competitor == 3 ~ "2-3",
                                    competitor == 4 | competitor == 5 | competitor == 6 ~ "4-6",
                                    competitor == 7 | competitor == 8 | competitor == 9 ~ "7-9",
                                    TRUE ~ "above 10"))


fit3 <- glm(s_g ~ competitor_cat + t_LOE, data = MEPS_summary_weighted, family = binomial)
summary(fit3)
m3 <- margins(fit3)
plot(m3)

MEPS_summary_weighted <- MEPS_summary_weighted %>%
  mutate(I2 = ifelse(competitor == 2, 1, 0),
         I3 = ifelse(competitor == 3, 1, 0),
         I4 = ifelse(competitor >= 4, 1, 0))

fit4 <- glm(s_g ~ I2 + I3 + I4 + t_LOE, data = MEPS_summary_weighted, family = binomial)
summary(fit4)
m4 <- margins(fit4)
plot(m4)
