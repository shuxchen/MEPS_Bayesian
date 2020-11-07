SSR_net <- read_excel("SSRHealthDatasets/NetSales.xlsx")
SSR_NDC <- read_excel("SSRHealthDatasets/NDCcrosslist.xlsx")
SSR_discount <- read_excel("SSRHealthDatasets/Discountrates_products.xlsx")
load("NDC_branded.Rdata")
load("MEPS_summary_weighted.Rdata")


SSR_discount <- SSR_discount %>%
  rename(Type = `...4`)

SSR_discount_WAC <- SSR_discount %>%
  filter(Type == "Total WAC-net discount %")

#SSR_discount_WAC_price <- SSR_NDC %>%
#  left_join(SSR_discount_WAC, on = "product") %>%
#  rename(NDC = `Ndc 11`) 

#SSR_discount_WAC_price$NDC = as.character(SSR_discount_WAC_price$NDC)
#SSR_discount_WAC_price$NDC <- str_pad(SSR_discount_WAC_price$NDC, 11, pad = "0")

SSR_price <- SSR_NDC %>%
  left_join(SSR_net, by = c("Product", "Class")) %>%
  rename(NDC = `Ndc 11`) 

SSR_price$NDC = as.character(SSR_price$NDC)
SSR_price$NDC <- str_pad(SSR_price$NDC, 11, pad = "0")

SSR_price <- SSR_price %>%
  left_join(SSR_discount_WAC, by = c("Product", "Class"))

SSR_price <- SSR_price %>%
  mutate(p_2007 = (`2007Q1.x`/(1-`2007Q1.y`) + `2007Q2.x`/(1-`2007Q2.y`) + `2007Q3.x`/(1-`2007Q3.y`) + `2007Q4.x`/(1-`2007Q4.y`))/4,
         p_2008 = (`2008Q1.x`/(1-`2008Q1.y`) + `2008Q2.x`/(1-`2008Q2.y`) + `2008Q3.x`/(1-`2008Q3.y`) + `2008Q4.x`/(1-`2008Q4.y`))/4,
         p_2009 = (`2009Q1.x`/(1-`2009Q1.y`) + `2009Q2.x`/(1-`2009Q2.y`) + `2009Q3.x`/(1-`2009Q3.y`) + `2009Q4.x`/(1-`2009Q4.y`))/4,
         p_2010 = (`2010Q1.x`/(1-`2010Q1.y`) + `2010Q2.x`/(1-`2010Q2.y`) + `2010Q3.x`/(1-`2010Q3.y`) + `2010Q4.x`/(1-`2010Q4.y`))/4,
         p_2011 = (`2011Q1.x`/(1-`2011Q1.y`) + `2011Q2.x`/(1-`2011Q2.y`) + `2011Q3.x`/(1-`2011Q3.y`) + `2011Q4.x`/(1-`2011Q4.y`))/4,
         p_2012 = (`2012Q1.x`/(1-`2012Q1.y`) + `2012Q2.x`/(1-`2012Q2.y`) + `2012Q3.x`/(1-`2012Q3.y`) + `2012Q4.x`/(1-`2012Q4.y`))/4,
         p_2013 = (`2013Q1.x`/(1-`2013Q1.y`) + `2013Q2.x`/(1-`2013Q2.y`) + `2013Q3.x`/(1-`2013Q3.y`) + `2013Q4.x`/(1-`2013Q4.y`))/4,
         p_2014 = (`2014Q1.x`/(1-`2014Q1.y`) + `2014Q2.x`/(1-`2014Q2.y`) + `2014Q3.x`/(1-`2014Q3.y`) + `2014Q4.x`/(1-`2014Q4.y`))/4,
         p_2015 = (`2015Q1.x`/(1-`2015Q1.y`) + `2015Q2.x`/(1-`2015Q2.y`) + `2015Q3.x`/(1-`2015Q3.y`) + `2015Q4.x`/(1-`2015Q4.y`))/4,
         p_2016 = (`2016Q1.x`/(1-`2016Q1.y`) + `2016Q2.x`/(1-`2016Q2.y`) + `2016Q3.x`/(1-`2016Q3.y`) + `2016Q4.x`/(1-`2016Q4.y`))/4,
         p_2017 = (`2017Q1.x`/(1-`2017Q1.y`) + `2017Q2.x`/(1-`2017Q2.y`) + `2017Q3.x`/(1-`2017Q3.y`) + `2017Q4.x`/(1-`2017Q4.y`))/4,
         p_2018 = (`2018Q1.x`/(1-`2018Q1.y`) + `2018Q2.x`/(1-`2018Q2.y`) + `2018Q3.x`/(1-`2018Q3.y`) + `2018Q4.x`/(1-`2018Q4.y`))/4) %>%
  dplyr::select(NDC, p_2007, p_2008, p_2009, p_2010, p_2011, p_2012, p_2013, p_2014, p_2015, p_2016, p_2017, p_2018)

# 1. get branded price for all NDC_branded
NDC_branded <- NDC_branded %>%
  left_join(SSR_price, by = c("NDC11" = "NDC"))

NDC_branded_SSR <- NDC_branded %>%
  dplyr::select(index, p_2012, p_2013, p_2014, p_2015, p_2016, p_2017, p_2018) %>%
  group_by(index) %>%
  summarise(`2012` = mean(p_2012, na.rm = T, na.action = na.pass),
            `2013` = mean(p_2013, na.rm = T, na.action = na.pass),
            `2014` = mean(p_2014, na.rm = T, na.action = na.pass),
            `2015` = mean(p_2015, na.rm = T, na.action = na.pass),
            `2016` = mean(p_2016, na.rm = T, na.action = na.pass),
            `2017` = mean(p_2017, na.rm = T, na.action = na.pass),
            `2018` = mean(p_2018, na.rm = T, na.action = na.pass)) %>%
  ungroup()

NDC_branded_SSR_long <- gather(NDC_branded_SSR, year, P_b_SSR, `2012`:`2018`, factor_key=TRUE)

#interpolate:
NDC_branded_SSR_long <- NDC_branded_SSR_long %>%
  group_by(index) %>%
  mutate(P_b_SSR = na.approx(P_b_SSR, na.rm=FALSE)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year) + 2011)

save(NDC_branded_SSR_long, file = "NDC_branded_SSR.Rdata")

MEPS_summary_weighted <- MEPS_summary_weighted %>%
  left_join(NDC_branded_SSR_long, by = c("index", "year"))

P_b_SSR_prior_LOE <- MEPS_summary_weighted %>%
  filter(t_LOE == -1) %>%
  dplyr::select(index, P_b_SSR) %>%
  rename(P_b_SSR_prior_LOE = P_b_SSR)

MEPS_summary_weighted <- MEPS_summary_weighted %>% 
  left_join(P_b_SSR_prior_LOE, by = "index")

save(MEPS_summary_weighted, file = "MEPS_summary_weighted.Rdata")
write.xlsx(MEPS_summary_weighted, "MEPS_summary_weighted.xlsx")

save(MEPS_summary_weighted, file = "~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian/MEPS_summary_weighted.Rdata")
write.xlsx(MEPS_summary_weighted, "~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian/MEPS_summary_weighted.xlsx")

test <- MEPS_summary_weighted %>%
  select(index, year, P_b, P_b_SSR, t_LOE)


P_g_by_n <-  MEPS_summary_weighted %>%
  mutate(competitor = ifelse(competitor <= 10, competitor, 11)) %>%
  filter(!is.na(P_g)) %>%
  group_by(competitor) %>%
  summarise(mean = mean(P_g)) %>%
  mutate(price = "generic")

P_b_by_n <-  MEPS_summary_weighted %>%
  mutate(competitor = ifelse(competitor <= 10, competitor, 11)) %>%
  filter(!is.na(P_b)) %>%
  group_by(competitor) %>%
  summarise(mean = mean(P_b)) %>%
  mutate(price = "branded")

P_b_SSR_by_n <-  MEPS_summary_weighted %>%
  mutate(competitor = ifelse(competitor <= 10, competitor, 11)) %>%
  filter(!is.na(P_b_SSR)) %>%
  group_by(competitor) %>%
  summarise(mean = mean(P_b_SSR)) %>%
  mutate(price = "branded_SSR")

P_by_n <- P_g_by_n %>%
  rbind(P_b_by_n) %>%
  rbind(P_b_SSR_by_n) %>%
  filter(competitor > 0)

ggplot(data = P_by_n, aes(x=competitor, y=mean, group=price, color=price)) +
  geom_line() +
  ggtitle("Average price with respect to the number of generics") +
  ylab("Price") +
  ylim(c(0, 500))

P_g_by_t_LOE <-  MEPS_summary_weighted %>%
  filter(!is.na(P_g)) %>%
  group_by(t_LOE) %>%
  summarise(mean = mean(P_g)) 

P_b_by_t_LOE <-  MEPS_summary_weighted %>%
  filter(!is.na(P_b)) %>%
  group_by(t_LOE) %>%
  summarise(mean = mean(P_b))
