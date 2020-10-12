SSR_net <- read_excel("SSRHealthDatasets/NetSales.xlsx")
SSR_NDC <- read_excel("SSRHealthDatasets/NDCcrosslist.xlsx")

SSR_price <- SSR_NDC %>%
  left_join(SSR_net, on = "product") %>%
  rename(NDC = `Ndc 11`) 

SSR_price$NDC = as.character(SSR_price$NDC)
SSR_price$NDC <- str_pad(SSR_price$NDC, 11, pad = "0")

 
SSR_price <- SSR_price %>%
  mutate(p_2007 = (`2007Q1` + `2007Q2` + `2007Q3` + `2007Q4`)/4,
         p_2008 = (`2008Q1` + `2008Q2` + `2008Q3` + `2008Q4`)/4,
         p_2009 = (`2009Q1` + `2009Q2` + `2009Q3` + `2009Q4`)/4,
         p_2010 = (`2010Q1` + `2010Q2` + `2010Q3` + `2010Q4`)/4,
         p_2011 = (`2011Q1` + `2011Q2` + `2011Q3` + `2011Q4`)/4,
         p_2012 = (`2012Q1` + `2012Q2` + `2012Q3` + `2012Q4`)/4,
         p_2013 = (`2013Q1` + `2013Q2` + `2013Q3` + `2013Q4`)/4,
         p_2014 = (`2014Q1` + `2014Q2` + `2014Q3` + `2014Q4`)/4,
         p_2015 = (`2015Q1` + `2015Q2` + `2015Q3` + `2015Q4`)/4,
         p_2016 = (`2016Q1` + `2016Q2` + `2016Q3` + `2016Q4`)/4,
         p_2017 = (`2017Q1` + `2017Q2` + `2017Q3` + `2017Q4`)/4,
         p_2018 = (`2018Q1` + `2018Q2` + `2018Q3` + `2018Q4`)/4) %>%
  dplyr::select(NDC, p_2007, p_2008, p_2009, p_2010, p_2011, p_2012, p_2013, p_2014, p_2015, p_2016, p_2017, p_2018)

# 1. get branded price for all NDC_branded
NDC_branded <- NDC_branded %>%
  left_join(SSR_price, on = "NDC")

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
