load("NDC.Rdata")
load("genericPIV.RData")
load("genericnoPIV.RData")
load("all.RData")
load("NDC_historical.Rdata")
load("NDC_excluded.Rdata")
load("NDC_no_historical.Rdata")


# 1. OB count
df %>%
  distinct(index) %>%
  count()

genericPIV_origin %>%
  distinct(index) %>%
  count()

genericnoPIV_origin %>%
  distinct(index) %>%
  count()

df <- genericPIV_origin %>%
  rbind(genericnoPIV_origin)

# 2.1 OB --> NDC count (combining NDC and NDC historical)
df_id <- df %>% 
  distinct(index)

df_included <- df %>%
  inner_join(df_id) %>%
  select(index, Appl_No, Product_No, Strength, Appl_Type, Approval_Date) 

# get index for molecules without multiple strengths  
df_Appl_No <- df_included %>%
  group_by(Appl_No) %>%
  distinct(Product_No) %>%
  count()

df_one_strength <- df_Appl_No %>%
  filter(n == 1) 

df_one_strength <- df_one_strength %>%
  left_join(df_included, on = 'Appl_No') %>%
  select(-n)

df_multiple_strengths <- df_included %>%
  anti_join(df_one_strength, on = 'index')

NDC_df_one_strength <- merge(NDC, df_one_strength, by=c("Appl_No"))

NDC_df_multiple_strengths <- merge(NDC, df_multiple_strengths, by=c("Appl_No", "Strength"))

NDC_df <- NDC_df_one_strength %>%
  bind_rows(NDC_df_multiple_strengths)

NDC_df_historical <- merge(NDC_historical, df_included, by=c("Appl_No", "Product_No")) %>%
  dplyr::select(index, NDC9)

NDC_df_combined <- NDC_df %>%
  dplyr::select(index, NDC9) %>%
  rbind(NDC_df_historical)

NDC_df_no_historical_one_strength <- merge(NDC_no_historical, df_one_strength, by=c("Appl_No"))

NDC_df_no_historical_multiple_strengths <- merge(NDC_no_historical, df_multiple_strengths, by=c("Appl_No", "Strength"))

NDC_df_no_historical <- NDC_df_no_historical_one_strength %>%
  bind_rows(NDC_df_no_historical_multiple_strengths)


#NDC_df <- NDC_generic %>%
#  rbind(NDC_branded) %>%
#  rename(NDC9 = NDC) %>%
#  dplyr::select(index, NDC9)

NDC_df %>%
  distinct(index) %>%
  count()

genericPIV_id <- genericPIV_origin %>%
  distinct(index)

genericnoPIV_id <- genericnoPIV_origin %>%
  distinct(index)

NDC_df %>%
  distinct(index) %>%
  inner_join(genericPIV_id) %>%
  count()

NDC_df %>%
  distinct(index) %>%
  inner_join(genericnoPIV_id) %>%
  count()

nrow(NDC_df)

NDC_df_combined %>%
  distinct(index) %>%
  inner_join(genericPIV_id) %>%
  count()

NDC_df_combined %>%
  distinct(index) %>%
  inner_join(genericnoPIV_id) %>%
  count()

nrow(NDC_df_combined)


# 3. NDC to MEPS count
MEPS_NDC <- function(MEPS, time){
  data <- MEPS %>%
    filter(!is.na(RXNDC9)) %>%
    select(RXNDC9, RXNAME, RXFLG) %>%
    distinct(RXNDC9, RXNAME, RXFLG) %>%
    mutate(year = time)
  return(data)
}

MEPS_NDC_new <- function(MEPS, time){
  data <- MEPS %>%
    filter(!is.na(RXNDC9)) %>%
    select(RXNDC9, RXNAME, `NDC IMPUTATION SOURCE ON PC DONOR REC`) %>%
    rename(RXFLG = `NDC IMPUTATION SOURCE ON PC DONOR REC`) %>%
    distinct(RXNDC9, RXNAME, RXFLG) %>%
    mutate(year = time) 
  return(data)
}

MEPS2006_NDC <- MEPS_NDC_new(MEPS2006, "2006")
MEPS2007_NDC <- MEPS_NDC(MEPS2007, "2007")
MEPS2008_NDC <- MEPS_NDC(MEPS2008, "2008")
MEPS2009_NDC <- MEPS_NDC(MEPS2009, "2009")
MEPS2010_NDC <- MEPS_NDC(MEPS2010, "2010")
MEPS2011_NDC <- MEPS_NDC(MEPS2011, "2011")
MEPS2012_NDC <- MEPS_NDC(MEPS2012, "2012")
MEPS2013_NDC <- MEPS_NDC(MEPS2013, "2013")
MEPS2014_NDC <- MEPS_NDC(MEPS2014, "2014")
MEPS2015_NDC <- MEPS_NDC(MEPS2015, "2015")
MEPS2016_NDC <- MEPS_NDC_new(MEPS2016, "2016")
MEPS2017_NDC <- MEPS_NDC_new(MEPS2017, "2017")

MEPS_all_NDC <- MEPS2006_NDC %>%
  bind_rows(MEPS2007_NDC) %>%
  bind_rows(MEPS2008_NDC) %>%
  bind_rows(MEPS2009_NDC) %>%
  bind_rows(MEPS2010_NDC) %>%
  bind_rows(MEPS2011_NDC) %>%
  bind_rows(MEPS2012_NDC) %>%
  bind_rows(MEPS2013_NDC) %>%
  bind_rows(MEPS2014_NDC) %>%
  bind_rows(MEPS2015_NDC) %>%
  bind_rows(MEPS2016_NDC) %>%
  bind_rows(MEPS2017_NDC) %>%
  distinct(RXNDC9, RXNAME, RXFLG, year) %>%
  mutate(imputed = ifelse(RXFLG == "1 NO IMPUTATION", 0, 1))

save(MEPS_all_NDC, file = "MEPS_all_NDC.Rdata")

n_imputed <- MEPS_all_NDC %>% 
  group_by(imputed, year) %>%
  count() %>%
  group_by(year) %>%
  mutate(percent = n/sum(n)) %>%
  filter(imputed == 1)

#3.1 MEPS to original NDC 
MEPS_NDC_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_antijoin %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_notmatched <- MEPS_NDC_antijoin %>%
  group_by(year) %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_innerjoin <- MEPS_all_NDC %>%
  inner_join(NDC, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_innerjoin %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_matched <- MEPS_NDC_innerjoin %>%
  group_by(year) %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_matched <- MEPS_NDC_matched %>%
  rename(n_match = n) %>%
  left_join(MEPS_NDC_notmatched, by = "year") %>%
  mutate(p = n_match / (n + n_match))

#3.2 MEPS to NDC (original + combined)

NDC_historical_code <- NDC_historical %>%
  dplyr::select(NDC9)

NDC_combined <- NDC %>%
  dplyr::select(NDC9) %>%
  rbind(NDC_historical_code) %>%
  distinct()

MEPS_NDC_combined_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC_combined, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_combined_antijoin %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_combined_notmatched <- MEPS_NDC_combined_antijoin %>%
  group_by(year) %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_combined_innerjoin <- MEPS_all_NDC %>%
  inner_join(NDC_combined, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_combined_innerjoin %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_combined_matched <- MEPS_NDC_combined_innerjoin %>%
  group_by(year) %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_combined_matched <- MEPS_NDC_combined_matched %>%
  rename(n_match = n) %>%
  left_join(MEPS_NDC_combined_notmatched, by = "year") %>%
  mutate(p = n_match / (n + n_match))


# 3.3 NDC from FDA (no historical data)
MEPS_NDC_no_historical_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC_no_historical, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_no_historical_antijoin %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_no_historical_notmatched <- MEPS_NDC_no_historical_antijoin %>%
  group_by(year) %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_no_historical_innerjoin <- MEPS_all_NDC %>%
  inner_join(NDC_no_historical, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_no_historical_innerjoin %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_no_historical_matched <- MEPS_NDC_no_historical_innerjoin %>%
  group_by(year) %>%
  #distinct(RXNAME) %>%
  count()

MEPS_NDC_no_historical_matched <- MEPS_NDC_no_historical_matched %>%
  rename(n_match = n) %>%
  left_join(MEPS_NDC_no_historical_notmatched, by = "year") %>%
  mutate(p = n_match / (n + n_match))

MEPS_NDC_matched_p <- MEPS_NDC_matched %>%
  dplyr::select(year, p) %>%
  mutate(NDC_data = "current + historical")

MEPS_NDC_combined_matched_p <- MEPS_NDC_combined_matched %>%
  dplyr::select(year, p) %>%
  mutate(NDC_data = "current + historical + NBER")

MEPS_NDC_no_historical_matched_p <- MEPS_NDC_no_historical_matched %>%
  dplyr::select(year, p) %>%
  mutate(NDC_data = "current")

MEPS_NDC_matched_p <- MEPS_NDC_matched_p %>%
  rbind(MEPS_NDC_combined_matched_p) %>%
  rbind(MEPS_NDC_no_historical_matched_p) %>%
  mutate(p = p*100)

ggplot(data = MEPS_NDC_matched_p, aes(x=year, y=p, group=NDC_data, color=NDC_data)) +
  geom_line() +
  ggtitle("% of MEPS observations that can be matched using NDC over time") +
  ylab("%") +
  ylim(0, 100)

## 4. NDC-OB to MEPS count
#4.1 MEPS to original NDC-OB
MEPS_NDC_OB_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC_df, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_OB_antijoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_OB_notmatched <- MEPS_NDC_OB_antijoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_OB_innerjoin <- MEPS_all_NDC %>%
  inner_join(NDC_df, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_OB_innerjoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_OB_matched <- MEPS_NDC_OB_innerjoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_OB_matched <- MEPS_NDC_OB_matched %>%
  rename(n_match = n) %>%
  left_join(MEPS_NDC_OB_notmatched, by = "year") %>%
  mutate(p = n_match / (n + n_match))