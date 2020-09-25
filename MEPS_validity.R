
# 1. OB count
df %>%
  distinct(index) %>%
  count()

# 2. OB --> NDC count
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

# 3. NDC to MEPS count
MEPS_NDC <- function(MEPS){
  data <- MEPS %>%
    filter(!is.na(RXNDC9)) %>%
    select(RXNDC9, RXNAME) %>%
    distinct(RXNDC9, RXNAME)
  return(data)
}

MEPS2006_NDC <- MEPS_NDC(MEPS2006)
MEPS2007_NDC <- MEPS_NDC(MEPS2007)
MEPS2008_NDC <- MEPS_NDC(MEPS2008)
MEPS2009_NDC <- MEPS_NDC(MEPS2009)
MEPS2010_NDC <- MEPS_NDC(MEPS2010)
MEPS2011_NDC <- MEPS_NDC(MEPS2011)
MEPS2012_NDC <- MEPS_NDC(MEPS2012)
MEPS2013_NDC <- MEPS_NDC(MEPS2013)
MEPS2014_NDC <- MEPS_NDC(MEPS2014)
MEPS2015_NDC <- MEPS_NDC(MEPS2015)
MEPS2016_NDC <- MEPS_NDC(MEPS2016)
MEPS2017_NDC <- MEPS_NDC(MEPS2017)

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
  distinct(RXNDC9, RXNAME)

NDC_df_MEPS <- NDC_df %>%
  inner_join(MEPS_all_NDC, by = c("NDC" = "RXNDC9"))

NDC_df_MEPS %>%
  distinct(index) %>%
  count()

NDC_df_MEPS %>%
  distinct(RXNAME) %>%
  count()

MEPS_MEPS_NDC_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC_df, by = c("RXNDC9" = "NDC"))

MEPS_MEPS_NDC_antijoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_MEPS_NDC_innerjoin <- MEPS_all_NDC %>%
  left_join(NDC_df, by = c("RXNDC9" = "NDC"))

MEPS_MEPS_NDC_innerjoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_antijoin <- MEPS_all_NDC %>%
  anti_join(NDC, by = c("RXNDC9" = "NDC"))

MEPS_NDC_antijoin %>%
  distinct(RXNAME) %>%
  count()

MEPS_NDC_innerjoin <- MEPS_all_NDC %>%
  left_join(NDC, by = c("RXNDC9" = "NDC"))

MEPS_NDC_innerjoin %>%
  distinct(RXNAME) %>%
  count()