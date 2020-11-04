load("NDC.Rdata")
load("genericPIV.RData")
load("genericnoPIV.RData")
load("all.RData")
load("NDC_historical.Rdata")

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

# 2.1 OB --> NDC count
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

#2.2 OB --> NDC historical

NDC_df_historical <- merge(NDC_historical, df_included, by=c("Appl_No", "Product_No"))

NDC_df_historical %>%
  distinct(index) %>%
  count()

NDC_df_historical <- NDC_df_historical %>%
  dplyr::select(index, Appl_No, Product_No, NDC9, NDC11)

NDC_df_combined <- NDC_df %>%
  dplyr::select(index, Appl_No, Product_No, NDC9, NDC11) %>%
  rbind(NDC_df_historical) %>%
  distinct()

NDC_df_combined %>%
  distinct(index) %>%
  count()

NDC_df_combined %>%
  distinct(index) %>%
  inner_join(genericPIV_id) %>%
  count()

NDC_df_combined %>%
  distinct(index) %>%
  inner_join(genericnoPIV_id) %>%
  count()

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

n_imputed <- MEPS_all_NDC %>% 
  group_by(imputed, year) %>%
  count() %>%
  group_by(year) %>%
  mutate(percent = n/sum(n)) %>%
  filter(imputed == 1)
  
#original OB-NDC
NDC_df_MEPS <- NDC_df %>%
  inner_join(MEPS_all_NDC, by = c("NDC9" = "RXNDC9"))

NDC_df_MEPS %>%
  distinct(index) %>%
  count()

NDC_df_MEPS %>%
  distinct(index) %>%
  inner_join(genericPIV_id) %>%
  count()

NDC_df_MEPS %>%
  distinct(index) %>%
  inner_join(genericnoPIV_id) %>%
  count()

# OB-NDC with historical NDC 
NDC_df_combined_MEPS <- NDC_df_combined %>%
  inner_join(MEPS_all_NDC, by = c("NDC9" = "RXNDC9"))

NDC_df_combined_MEPS %>%
  distinct(index) %>%
  count()

NDC_df_combined_MEPS %>%
  distinct(index) %>%
  inner_join(genericPIV_id) %>%
  count()

NDC_df_combined_MEPS %>%
  distinct(index) %>%
  inner_join(genericnoPIV_id) %>%
  count()


MEPS_summary_weighted %>%
  filter(!is.na(P_b) & !is.na(P_g)) %>%
  distinct(index) %>%
  inner_join(genericPIV_id) %>%
  count()

MEPS_summary_weighted %>%
  filter(!is.na(P_b) & !is.na(P_g)) %>%
  distinct(index) %>%
  inner_join(genericnoPIV_id) %>%
  count()

NDC_df_MEPS %>%
  distinct(RXNAME) %>%
  count()

MEPS_MEPS_NDC_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC_df, by = c("RXNDC9" = "NDC9"))

MEPS_MEPS_NDC_antijoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_MEPS_NDC_innerjoin <- MEPS_all_NDC %>%
  inner_join(NDC_df, by = c("RXNDC9" = "NDC9"))

MEPS_MEPS_NDC_innerjoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_antijoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_notmatched <- MEPS_NDC_antijoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_innerjoin <- MEPS_all_NDC %>%
  inner_join(NDC, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_innerjoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_matched <- MEPS_NDC_innerjoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_matched <- MEPS_NDC_matched %>%
  rename(n_match = n) %>%
  left_join(MEPS_NDC_notmatched, by = "year") %>%
  mutate(p = n_match / (n + n_match))

# MEPS NDC-OB
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

MEPS_NDC_OB_matched_p <- MEPS_NDC_OB_matched %>%
  dplyr::select(year, p) %>%
  mutate(group = "OB-NDC")

MEPS_NDC_matched_p <- MEPS_NDC_matched %>%
  dplyr::select(year, p) %>%
  mutate(group = "NDC")

MEPS_NDC_matched_p <- MEPS_NDC_matched_p %>%
  rbind(MEPS_NDC_OB_matched_p) %>%
  mutate(p = p*100)

ggplot(data = MEPS_NDC_matched_p, aes(x=year, y=p, group=group, color=group)) +
  geom_line() +
  ggtitle("% of MEPS drugs that can be matched using NDC over time") +
  ylab("%") +
  ylim(0, 100)

# MEPS vs. OB-NDC with historical NDC
MEPS_NDC_OB_combined_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC_df_combined, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_OB_combined_antijoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_OB_combined_notmatched <- MEPS_NDC_OB_combined_antijoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_OB_combined_innerjoin <- MEPS_all_NDC %>%
  inner_join(NDC_df_combined, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_OB_combined_innerjoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_OB_combined_matched <- MEPS_NDC_OB_combined_innerjoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_OB_combined_matched <- MEPS_NDC_OB_combined_matched %>%
  rename(n_match = n) %>%
  left_join(MEPS_NDC_OB_combined_notmatched, by = "year") %>%
  mutate(p = n_match / (n + n_match))

MEPS_NDC_OB_combined_matched_p <- MEPS_NDC_OB_combined_matched %>%
  dplyr::select(year, p) %>%
  mutate(group = "OB-NDC")

MEPS_NDC_combined_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC_historical, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_combined_antijoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_combined_notmatched <- MEPS_NDC_combined_antijoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_combined_innerjoin <- MEPS_all_NDC %>%
  inner_join(NDC_historical, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_combined_innerjoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_combined_matched <- MEPS_NDC_combined_innerjoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_combined_matched <- MEPS_NDC_combined_matched %>%
  rename(n_match = n) %>%
  left_join(MEPS_NDC_notmatched, by = "year") %>%
  mutate(p = n_match / (n + n_match))


MEPS_NDC_combined_matched_p <- MEPS_NDC_combined_matched %>%
  dplyr::select(year, p) %>%
  mutate(group = "NDC")

MEPS_NDC_combined_matched_p <- MEPS_NDC_combined_matched_p %>%
  rbind(MEPS_NDC_OB_combined_matched_p) %>%
  mutate(p = p*100)

ggplot(data = MEPS_NDC_combined_matched_p, aes(x=year, y=p, group=group, color=group)) +
  geom_line() +
  ggtitle("% of MEPS drugs that can be matched using NDC over time") +
  ylab("%") +
  ylim(0, 100)

#consider whether imputed ones have worse quality
MEPS_NDC_notmatched_imputed <- MEPS_NDC_antijoin %>%
  group_by(year, imputed) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_matched_imputed <- MEPS_NDC_innerjoin %>%
  group_by(year, imputed) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_matched_imputed <- MEPS_NDC_matched_imputed %>%
  rename(n_match = n) %>%
  left_join(MEPS_NDC_notmatched_imputed, on = c("year", "imputed")) %>%
  mutate(p = n_match / (n + n_match))

MEPS_NDC_matched_imputed <- MEPS_NDC_matched_imputed %>%
  mutate(p = 100* p)

ggplot(data = MEPS_NDC_matched_imputed, aes(x=year, y=p, group=as.factor(imputed), color=as.factor(imputed))) +
  geom_line() +
  ggtitle("% of MEPS drugs that can be matched using NDC (imputed or not)") +
  ylab("%") +
  ylim(0, 100)

## use 9 digits only
MEPS_all_NDC$RXNDC11 <- MEPS_all_NDC$RXNDC9
MEPS_all_NDC$RXNDC9 <- str_sub(MEPS_all_NDC$RXNDC11, -11, -3)

NDC_df$NDC9 <- str_sub(NDC_df$NDC, -11, -3)
NDC$NDC9 <- str_sub(NDC$NDC, -11, -3)

MEPS_NDC_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_antijoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_notmatched <- MEPS_NDC_antijoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()


MEPS_NDC_innerjoin <- MEPS_all_NDC %>%
  inner_join(NDC, by = c("RXNDC9" = "NDC9"))

MEPS_NDC_innerjoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_matched <- MEPS_NDC_innerjoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_matched <- MEPS_NDC_matched %>%
  rename(n_match = n) %>%
  left_join(MEPS_NDC_notmatched, by = "year") %>%
  mutate(p = n_match / (n + n_match))

## use NDC and NDC_unfinished
NDC_code <- NDC %>%
  dplyr::select(NDC)

NDC_unfinished_code <- NDC_unfinished %>%
  dplyr::select(NDCPACKAGECODE)

NDC_unfinished_code$NDC <- ifelse(str_sub(NDC_unfinished_code$NDCPACKAGECODE, 5, 5) == "-", paste0("0", NDC_unfinished_code$NDCPACKAGECODE), ifelse(str_sub(NDC_unfinished_code$NDCPACKAGECODE, 10, 10) == "-", sub( '(?<=.{6})', '0', NDC_unfinished_code$NDCPACKAGECODE, perl = T), sub( '(?<=.{11})', '0', NDC_unfinished_code$NDCPACKAGECODE, perl = T)))
NDC_unfinished_code$NDC <- gsub("-", "", NDC_unfinished_code$NDC)

NDC_unfinished_code <- NDC_unfinished_code %>%
  dplyr::select(NDC)

NDC_unfinished_excluded_code <- NDC_unfinished_excluded %>%
  dplyr::select(NDCPACKAGECODE)

NDC_unfinished_excluded_code$NDC <- ifelse(str_sub(NDC_unfinished_excluded_code$NDCPACKAGECODE, 5, 5) == "-", paste0("0", NDC_unfinished_excluded_code$NDCPACKAGECODE), ifelse(str_sub(NDC_unfinished_excluded_code$NDCPACKAGECODE, 10, 10) == "-", sub( '(?<=.{6})', '0', NDC_unfinished_excluded_code$NDCPACKAGECODE, perl = T), sub( '(?<=.{11})', '0', NDC_unfinished_excluded_code$NDCPACKAGECODE, perl = T)))
NDC_unfinished_excluded_code$NDC <- gsub("-", "", NDC_unfinished_excluded_code$NDC)

NDC_unfinished_excluded_code <- NDC_unfinished_excluded_code %>%
  dplyr::select(NDC)

NDC_code_all <- NDC_code %>%
  bind_rows(NDC_unfinished_code) %>%
  bind_rows(NDC_unfinished_excluded_code) %>%
  distinct(NDC)
  
MEPS_NDC_all_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC_code_all, by = c("RXNDC11" = "NDC"))

MEPS_NDC_all_antijoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_all_notmatched <- MEPS_NDC_all_antijoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()


MEPS_NDC_all_innerjoin <- MEPS_all_NDC %>%
  inner_join(NDC_code_all, by = c("RXNDC11" = "NDC"))

MEPS_NDC_all_innerjoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_all_matched <- MEPS_NDC_all_innerjoin %>%
  group_by(year) %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_all_matched <- MEPS_NDC_all_matched %>%
  rename(n_match = n) %>%
  left_join(MEPS_NDC_all_notmatched, by = "year") %>%
  mutate(p = n_match / (n + n_match))


MEPS2017 %>%
  dplyr::select(`MEDICINE NAME (IMPUTED)`, RXNDC11, `QUANTITY OF RX/PRESCR MED (IMPUTED)`, price_total) %>%
  head(5)
