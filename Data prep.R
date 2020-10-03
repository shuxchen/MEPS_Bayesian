if (.Platform$OS.type == "unix"){
  setwd("~/Dropbox/Advanced Method Project/Data")
  source("Aim1/MEPS_Bayesian/load_packages.R")
  source("Aim1/MEPS_Bayesian/NDC_strength_clean.R")
  
} else {
  setwd("C:\\Users\\shuxian\\repos\\MEPS_Bayesian")
  source("load_packages.R")
  source("NDC_strength_clean.R")
}

load("all.RData")
load("genericPIV.RData")
load("genericnoPIV.RData")
CPI <- read_delim("CPI_CU_Summaries.txt",
                     "\t", 
                     escape_double = FALSE, 
                     trim_ws = TRUE)

genericPIV <- genericPIV_origin
genericnoPIV <- genericnoPIV_origin

#1. Start with no PIV
genericnoPIV_id <- genericnoPIV %>% 
  distinct(index)

index_time_noPIV <- genericnoPIV %>%
  group_by(index) %>%
  summarize(index_time = min(date, patent, earliestexclusivity, na.rm = TRUE)) 

genericnoPIV_included <- genericnoPIV %>%
  inner_join(genericnoPIV_id) %>%
  left_join(index_time_noPIV) %>%
  mutate(year_LOE = year(index_time)) %>%
  select(index, Appl_No, Product_No, Strength, Appl_Type, approveyear, Approval_Date, year_LOE) 

# get index for molecules without multiple strengths  
genericnoPIV_Appl_No <- genericnoPIV_included %>%
  group_by(Appl_No) %>%
  distinct(Product_No) %>%
  count()

genericnoPIV_one_strength <- genericnoPIV_Appl_No %>%
  filter(n == 1) 

genericnoPIV_one_strength <- genericnoPIV_one_strength %>%
  left_join(genericnoPIV_included, on = 'Appl_No') %>%
  select(-n)

genericnoPIV_multiple_strengths <- genericnoPIV_included %>%
  anti_join(genericnoPIV_one_strength, on = 'index')

#get branded drug info
brandednoPIV <- df %>% 
  right_join(genericnoPIV_id) %>%
  filter(Appl_Type =="N")

name_b_noPIV <- droplevels(brandednoPIV)$Trade_Name

#Merge OB data and NDC data
NDC_noPIV_one_strength <- merge(NDC, genericnoPIV_one_strength, by=c("Appl_No"))

NDC_noPIV_one_strength %>%
  distinct(index) %>%
  count()

NDC_noPIV_multiple_strengths <- merge(NDC, genericnoPIV_multiple_strengths, by=c("Appl_No", "Strength"))

NDC_noPIV_multiple_strengths %>%
  distinct(index) %>%
  count()

NDC_noPIV <- NDC_noPIV_one_strength %>%
  bind_rows(NDC_noPIV_multiple_strengths)

save(NDC_noPIV, file = "NDC_noPIV.Rdata")

#2. PIV
genericPIV_id <- genericPIV %>% 
  distinct(index)

index_time_PIV <- genericPIV %>%
  group_by(index) %>%
  summarize(index_time = min(date, na.rm = TRUE)) 

genericPIV_included <- genericPIV %>%
  inner_join(genericPIV_id) %>%
  left_join(index_time_PIV) %>%
  mutate(year_LOE = year(index_time)) %>%
  select(index, Appl_No, Product_No, Strength, Appl_Type, approveyear, Approval_Date, year_LOE) 

# get index for molecules without multiple strengths 
genericPIV_Appl_No <- genericPIV_included %>%
  group_by(Appl_No) %>%
  distinct(Product_No) %>%
  count()

genericPIV_one_strength <- genericPIV_Appl_No %>%
  filter(n == 1) 

genericPIV_one_strength <- genericPIV_one_strength %>%
  left_join(genericPIV_included, on = 'Appl_No') %>%
  select(-n)

genericPIV_multiple_strengths <- genericPIV_included %>%
  anti_join(genericPIV_one_strength, on = 'index')


#get branded drug info
brandedPIV <- df %>% 
  right_join(genericPIV_id) %>%
  filter(Appl_Type =="N")

name_b_PIV <- droplevels(brandedPIV)$Trade_Name

#Merge OB data and NDC data
NDC_PIV_one_strength <- merge(NDC, genericPIV_one_strength, by=c("Appl_No"))

NDC_PIV_one_strength %>%
  distinct(index) %>%
  count()

NDC_PIV_multiple_strengths <- merge(NDC, genericPIV_multiple_strengths, by=c("Appl_No", "Strength"))

NDC_PIV_multiple_strengths %>%
  distinct(index) %>%
  count()

NDC_PIV <- NDC_PIV_one_strength %>%
  bind_rows(NDC_PIV_multiple_strengths)

save(NDC_PIV, file = "NDC_PIV.Rdata")

#check NDC code format
test <- NDC_noPIV[, c("Appl_No", "PRODUCTNDC", "NDC")] 

#Merge PC and noPC generics
NDC_generic <- NDC_noPIV %>%
  rbind(NDC_PIV)

#3. Branded
#for each index, getting branded drug NDC
branded_id <- NDC_PIV %>%
  rbind(NDC_noPIV) %>%
  distinct(index)

branded_included <- df %>%
  filter(Appl_Type == "N") %>%
  inner_join(branded_id) %>%
  select(index, Appl_No, Product_No, Strength, Appl_Type, Approval_Date) 

# get index for molecules without multiple strengths 
branded_Appl_No <- branded_included %>%
  group_by(Appl_No) %>%
  distinct(Product_No) %>%
  count()

branded_one_strength <- branded_Appl_No %>%
  filter(n == 1) 

branded_one_strength <- branded_one_strength %>%
  left_join(branded_included, on = 'Appl_No') %>%
  select(-n)

branded_multiple_strengths <- branded_included %>%
  anti_join(branded_one_strength, on = 'index')

#Merge data and NDC data

NDC_branded_one_strength <- merge(NDC, branded_one_strength, by=c("Appl_No"))

NDC_branded_one_strength %>%
  distinct(index) %>%
  count()

NDC_branded_multiple_strengths <- merge(NDC, branded_multiple_strengths, by=c("Appl_No", "Strength"))

NDC_branded_multiple_strengths %>%
  distinct(index) %>%
  count()

NDC_branded <- NDC_branded_one_strength %>%
  bind_rows(NDC_branded_multiple_strengths)

NDC_branded_id <- NDC_branded %>%
  distinct(index)

NDC_generic_id <- NDC_generic %>%
  distinct(index)

#only keep branded that shares index with any generic (have generic competition)
NDC_branded <- NDC_branded %>%
  inner_join(NDC_generic_id)

n_NDC_branded_updated <- NDC_branded %>%
  distinct(index) %>%
  count()

NDC_generic <- NDC_generic %>%
  inner_join(NDC_branded_id)

n_NDC_generic_updated <- NDC_generic %>%
  distinct(index) %>%
  count()

#(reason for total branded index # < generic index #: some generic with PC doesn't have branded drug info)

#consistent index for branded/generic (that can be matched to NDC code)
index_included <- NDC_generic %>%
  distinct(index) 

index <- index_included$index

genericnoPIV_included$approveyear <- as.numeric(genericnoPIV_included$approveyear)
 
generic_included <- genericPIV_included %>%
  rbind(genericnoPIV_included) %>%
  inner_join(index_included)

branded_included <- branded_included %>%
  inner_join(index_included) %>%
  distinct()

###Create table for MEPS info (competitior, N_b, N_g, P_b, P_g)
MEPS <- map_dfr(index, function(id){
  year <- c(2007:2017)
  competitor <- rep(0, 11)
  
  generic_included <- generic_included %>%
    filter(index == id)
  
  for (j in 2007:2017){
    for (i in 1:nrow(generic_included)){
      if(generic_included$approveyear[i] == j){
        competitor[j-2006]= competitor[j-2006] + 1
      }
    }
    if(j<2017){
      competitor[(j-2005):11]=competitor[j-2006]
    }
  }
  
  output <- data.frame(year, competitor)
  output <- output %>%
    mutate(index = id) %>%
    select(index, year, competitor)
  
  return(output)
})



###MEPS
####################################################################################
##Load MEPS 2006 data
####################################################################################
MEPS2006 <- read_excel("MEPS/2006/H102A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2006$RXNDC9 <- str_sub(MEPS2006$`NATIONAL DRUG CODE (IMPUTED)`, -11, -3)
MEPS2006$RXNDC9 <- MEPS2006$`NATIONAL DRUG CODE (IMPUTED)`
#Exclude "()" in names
MEPS2006$RXNAME <- sapply(strsplit(MEPS2006$`MEDICATION NAME (IMPUTED)`, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2006 <- MEPS2006 %>%
  mutate(price_total = as.numeric(`SUM OF PAYMENTS RXSF06X-RXOU06X(IMPUTED)`),
         price_OOP = `AMOUNT PAID, SELF OR FAMILY (IMPUTED)`,
         price_Medicare = `AMOUNT PAID, MEDICARE (IMPUTED)`,
         price_Medicaid = `AMOUNT PAID, MEDICAID (IMPUTED)`,
         price_private = `AMOUNT PAID, PRIVATE INSURANCE (IMPUTED)`
  ) %>%
  rename(weight = `FINAL PERSON LEVEL WEIGHT, 2006`)

MEPS2006 %>%
  filter(is.na(RXNDC9)) %>%
  count()

MEPS2006 %>%
  filter(RXNAME == '-9') %>%
  count()

####################################################################################
##Load MEPS 2007 data
####################################################################################
MEPS2007 <- read_excel("MEPS/2007/H110A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2007$RXNDC9 <- str_sub(MEPS2007$RXNDC, -11, -3)
MEPS2007$RXNDC9 <- MEPS2007$RXNDC

#Exclude "()" in names
MEPS2007$RXNAME <- sapply(strsplit(MEPS2007$RXNAME, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2007 <- MEPS2007 %>%
  mutate(price_total = as.numeric(RXSF07X) + as.numeric(RXMR07X) + as.numeric(RXMD07X) + as.numeric(RXPV07X) + as.numeric(RXVA07X) + as.numeric(RXTR07X) + as.numeric(RXOF07X) + as.numeric(RXSL07X) + as.numeric(RXWC07X) + as.numeric(RXOT07X) + as.numeric(RXOR07X) + as.numeric(RXOU07X),
         price_OOP = RXSF07X,
         price_Medicare = RXMR07X,
         price_Medicaid = RXMD07X,
         price_private = RXPV07X
  ) %>%
  rename(weight = PERWT07F)

MEPS2007_design <- svydesign(id       = ~ VARPSU,
                              strata  = ~ VARSTR,
                              weights = ~ PERWT07F,
                              nest    = TRUE,
                              data    = MEPS2007)

summary(MEPS2007_design)

test <- MEPS2007 %>% 
  filter(RXNDC9 == "504740400")

test_design <- svydesign(id       = ~ VARPSU,
                             strata  = ~ VARSTR,
                             weights = ~ PERWT07F,
                             nest    = TRUE,
                             data    = test)

svymean(~ RXQUANTY, test_design, na.rm = TRUE)


####################################################################################
##Load MEPS 2008 data
####################################################################################
MEPS2008 <- read_excel("MEPS/2008/H118A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2008$RXNDC9 <- str_sub(MEPS2008$RXNDC, -11, -3)
MEPS2008$RXNDC9 <- MEPS2008$RXNDC

#Exclude "()" in names
MEPS2008$RXNAME <- sapply(strsplit(MEPS2008$RXNAME, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2008 <- MEPS2008 %>%
  mutate(price_total = as.numeric(RXSF08X) + as.numeric(RXMR08X) + as.numeric(RXMD08X) + as.numeric(RXPV08X) + as.numeric(RXVA08X) + as.numeric(RXTR08X) + as.numeric(RXOF08X) + as.numeric(RXSL08X) + as.numeric(RXWC08X) + as.numeric(RXOT08X) + as.numeric(RXOR08X) + as.numeric(RXOU08X),
         price_OOP = RXSF08X,
         price_Medicare = RXMR08X,
         price_Medicaid = RXMD08X,
         price_private = RXPV08X
  ) %>%
  rename(weight = PERWT08F)

####################################################################################
##Load MEPS 2009 data
####################################################################################
MEPS2009 <- read_excel("MEPS/2009/H126A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2009$RXNDC9 <- str_sub(MEPS2009$RXNDC, -11, -3)
MEPS2009$RXNDC9 <- MEPS2009$RXNDC

#Exclude "()" in names
MEPS2009$RXNAME <- sapply(strsplit(MEPS2009$RXNAME, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2009 <- MEPS2009 %>%
  mutate(price_total = as.numeric(RXSF09X) + as.numeric(RXMR09X) + as.numeric(RXMD09X) + as.numeric(RXPV09X) + as.numeric(RXVA09X) + as.numeric(RXTR09X) + as.numeric(RXOF09X) + as.numeric(RXSL09X) + as.numeric(RXWC09X) + as.numeric(RXOT09X) + as.numeric(RXOR09X) + as.numeric(RXOU09X),
         price_OOP = RXSF09X,
         price_Medicare = RXMR09X,
         price_Medicaid = RXMD09X,
         price_private = RXPV09X
  ) %>%
  rename(weight = PERWT09F)

####################################################################################
##Load MEPS 2010 data
####################################################################################
MEPS2010 <- read_excel("MEPS/2010/H135A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2010$RXNDC9 <- str_sub(MEPS2010$RXNDC, -11, -3)
MEPS2010$RXNDC9 <- MEPS2010$RXNDC

#Exclude "()" in names
MEPS2010$RXNAME <- sapply(strsplit(MEPS2010$RXNAME, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2010 <- MEPS2010 %>%
  mutate(price_total = as.numeric(RXSF10X) + as.numeric(RXMR10X) + as.numeric(RXMD10X) + as.numeric(RXPV10X) + as.numeric(RXVA10X) + as.numeric(RXTR10X) + as.numeric(RXOF10X) + as.numeric(RXSL10X) + as.numeric(RXWC10X) + as.numeric(RXOT10X) + as.numeric(RXOR10X) + as.numeric(RXOU10X),
         price_OOP = RXSF10X,
         price_Medicare = RXMR10X,
         price_Medicaid = RXMD10X,
         price_private = RXPV10X
  ) %>%
  rename(weight = PERWT10F)

####################################################################################
##Load MEPS 2011 data
####################################################################################
MEPS2011 <- read_excel("MEPS/2011/H144A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2011$RXNDC9 <- str_sub(MEPS2011$RXNDC, -11, -3)
MEPS2011$RXNDC9 <- MEPS2011$RXNDC

#Exclude "()" in names
MEPS2011$RXNAME <- sapply(strsplit(MEPS2011$RXNAME, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2011 <- MEPS2011 %>%
  mutate(price_total = as.numeric(RXSF11X) + as.numeric(RXMR11X) + as.numeric(RXMD11X) + as.numeric(RXPV11X) + as.numeric(RXVA11X) + as.numeric(RXTR11X) + as.numeric(RXOF11X) + as.numeric(RXSL11X) + as.numeric(RXWC11X) + as.numeric(RXOT11X) + as.numeric(RXOR11X) + as.numeric(RXOU11X),
         price_OOP = RXSF11X,
         price_Medicare = RXMR11X,
         price_Medicaid = RXMD11X,
         price_private = RXPV11X
  ) %>%
  rename(weight = PERWT11F)

####################################################################################
##Load MEPS 2012 data
####################################################################################
MEPS2012 <- read_excel("MEPS/2012/H152A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2012$RXNDC9 <- str_sub(MEPS2012$RXNDC, -11, -3)
MEPS2012$RXNDC9 <- MEPS2012$RXNDC

#Exclude "()" in names
MEPS2012$RXNAME <- sapply(strsplit(MEPS2012$RXNAME, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2012 <- MEPS2012 %>%
  mutate(price_total = as.numeric(RXSF12X) + as.numeric(RXMR12X) + as.numeric(RXMD12X) + as.numeric(RXPV12X) + as.numeric(RXVA12X) + as.numeric(RXTR12X) + as.numeric(RXOF12X) + as.numeric(RXSL12X) + as.numeric(RXWC12X) + as.numeric(RXOT12X) + as.numeric(RXOR12X) + as.numeric(RXOU12X),
         price_OOP = RXSF12X,
         price_Medicare = RXMR12X,
         price_Medicaid = RXMD12X,
         price_private = RXPV12X
  ) %>%
  rename(weight = PERWT12F)

####################################################################################
##Load MEPS 2013 data
####################################################################################
MEPS2013 <- read_excel("MEPS/2013/H160A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2013$RXNDC9 <- str_sub(MEPS2013$RXNDC, -11, -3)
MEPS2013$RXNDC9 <- MEPS2013$RXNDC

#Exclude "()" in names
MEPS2013$RXNAME <- sapply(strsplit(MEPS2013$RXNAME, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2013 <- MEPS2013 %>%
  mutate(price_total = as.numeric(RXSF13X) + as.numeric(RXMR13X) + as.numeric(RXMD13X) + as.numeric(RXPV13X) + as.numeric(RXVA13X) + as.numeric(RXTR13X) + as.numeric(RXOF13X) + as.numeric(RXSL13X) + as.numeric(RXWC13X) + as.numeric(RXOT13X) + as.numeric(RXOR13X) + as.numeric(RXOU13X),
         price_OOP = RXSF13X,
         price_Medicare = RXMR13X,
         price_Medicaid = RXMD13X,
         price_private = RXPV13X
  ) %>%
  rename(weight = PERWT13F)

####################################################################################
##Load MEPS 2014 data
####################################################################################
MEPS2014 <- read_excel("MEPS/2014/H168A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2014$RXNDC9 <- str_sub(MEPS2014$RXNDC, -11, -3)
MEPS2014$RXNDC9 <- MEPS2014$RXNDC

#Exclude "()" in names
MEPS2014$RXNAME <- sapply(strsplit(MEPS2014$RXNAME, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2014 <- MEPS2014 %>%
  mutate(price_total = as.numeric(RXSF14X) + as.numeric(RXMR14X) + as.numeric(RXMD14X) + as.numeric(RXPV14X) + as.numeric(RXVA14X) + as.numeric(RXTR14X) + as.numeric(RXOF14X) + as.numeric(RXSL14X) + as.numeric(RXWC14X) + as.numeric(RXOT14X) + as.numeric(RXOR14X) + as.numeric(RXOU14X),
         price_OOP = RXSF14X,
         price_Medicare = RXMR14X,
         price_Medicaid = RXMD14X,
         price_private = RXPV14X
  ) %>%
  rename(weight = PERWT14F)

####################################################################################
##Load MEPS 2015 data
####################################################################################
MEPS2015 <- read_excel("MEPS/2015/H178A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2015$RXNDC9 <- str_sub(MEPS2015$RXNDC, -11, -3)
MEPS2015$RXNDC9 <- MEPS2015$RXNDC

#Exclude "()" in names
MEPS2015$RXNAME <- sapply(strsplit(MEPS2015$RXNAME, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2015 <- MEPS2015 %>%
  mutate(price_total = as.numeric(RXSF15X) + as.numeric(RXMR15X) + as.numeric(RXMD15X) + as.numeric(RXPV15X) + as.numeric(RXVA15X) + as.numeric(RXTR15X) + as.numeric(RXOF15X) + as.numeric(RXSL15X) + as.numeric(RXWC15X) + as.numeric(RXOT15X) + as.numeric(RXOR15X) + as.numeric(RXOU15X),
         price_OOP = RXSF15X,
         price_Medicare = RXMR15X,
         price_Medicaid = RXMD15X,
         price_private = RXPV15X
  ) %>%
  rename(weight = PERWT15F)

####################################################################################
##Load MEPS 2016 data
####################################################################################
MEPS2016 <- read_excel("MEPS/2016/H188A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2016$RXNDC9 <- str_sub(MEPS2016$`NDC (IMPUTED)`, -11, -3)
MEPS2016$RXNDC9 <- MEPS2016$`NDC (IMPUTED)`

#Exclude "()" in names
MEPS2016$RXNAME <- sapply(strsplit(MEPS2016$`MEDICINE NAME (IMPUTED)`, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2016 <- MEPS2016 %>%
  mutate(price_total = as.numeric(`SUM OF PAYMENTS RXSF16X-RXOU16X(IMPUTED)`),
         price_OOP = `AMOUNT PAID, SELF OR FAMILY (IMPUTED)`,
         price_Medicare = `AMOUNT PAID, MEDICARE (IMPUTED)`,
         price_Medicaid = `AMOUNT PAID, MEDICAID (IMPUTED)`,
         price_private = `AMOUNT PAID, PRIVATE INSURANCE (IMPUTED)`
  ) %>%
  rename(weight = `EXPENDITURE FILE PERSON WEIGHT, 2016`)

####################################################################################
##Load MEPS 2017 data
####################################################################################
MEPS2017 <- read_excel("MEPS/2017/H197A.xlsx")
#Create NDC code (9-digit, product NDC)
#MEPS2017$RXNDC9 <- str_sub(MEPS2017$`NDC (IMPUTED)`, -11, -3)
MEPS2017$RXNDC9 <- MEPS2017$`NDC (IMPUTED)`

#Exclude "()" in names
MEPS2017$RXNAME <- sapply(strsplit(MEPS2017$`MEDICINE NAME (IMPUTED)`, split=' (', fixed=TRUE), function(x) (x[1]))

MEPS2017 <- MEPS2017 %>%
  mutate(price_total = as.numeric(`SUM OF PAYMENTS RXSF17X-RXOU17X(IMPUTED)`),
         price_OOP = `AMOUNT PAID, SELF OR FAMILY (IMPUTED)`,
         price_Medicare = `AMOUNT PAID, MEDICARE (IMPUTED)`,
         price_Medicaid = `AMOUNT PAID, MEDICAID (IMPUTED)`,
         price_private = `AMOUNT PAID, PRIVATE INSURANCE (IMPUTED)`
  ) %>%
  rename(weight = `EXPENDITURE FILE PERSON WEIGHT, 2017`)

#Filter branded drug only, filtering by NDC code (not drug name)
#calculate total # of branded drug and generic drug for each index in 2007

getMEPSannual <- function(MEPS, index){
  map_dfr(index, function(id){
    data_branded <- NDC_branded %>%
      filter(index == id) %>%
      select(NDC) 
    
    data_generic <- NDC_generic %>%
      filter(index == id) %>%
      select(NDC) 
    
    NDC_branded_list <- data_branded$NDC

    NDC_generic_list <- data_generic$NDC
    
    branded <- map_dfr(NDC_branded_list, function(x){
      data <- MEPS %>% 
        filter(RXNDC9 == x)
      return(data)
    })
    
    generic <- map_dfr(NDC_generic_list, function(x){
      data <- MEPS %>% 
        filter(RXNDC9 == x)
      return(data)
    })
    
    n_branded <- nrow(branded) %>%
      data.frame() %>%
      rename(N_b = ".") %>%
      mutate(index = id)
    
    n_generic <- nrow(generic) %>%
      data.frame() %>%
      rename(N_g = ".") %>%
      mutate(index = id)
    
    p_branded <- branded %>%
      summarise(P_b = mean(price_total),
                P_b_sd = sd(price_total)) %>%
      mutate(index = id)

    p_generic <- generic %>%
      summarise(P_g = mean(price_total),
                P_g_sd = sd(price_total)) %>%
      mutate(index = id)
    
    data <- n_branded %>%
      left_join(n_generic, by = "index") %>%
      left_join(p_branded, by = "index") %>%
      left_join(p_generic, by = "index") 
    
    return(data)
  })
}

getMEPSannual_weighted <- function(MEPS, index){
  map_dfr(index, function(id){
    data_branded <- NDC_branded %>%
      filter(index == id) %>%
      select(NDC) 
    
    data_generic <- NDC_generic %>%
      filter(index == id) %>%
      select(NDC) 
    
    NDC_branded_list <- data_branded$NDC
    
    NDC_generic_list <- data_generic$NDC
    
    branded <- map_dfr(NDC_branded_list, function(x){
      data <- MEPS %>% 
        filter(RXNDC9 == x)
      return(data)
    })
    
    generic <- map_dfr(NDC_generic_list, function(x){
      data <- MEPS %>% 
        filter(RXNDC9 == x)
      return(data)
    })
    
    total_weight_branded <- sum(branded$weight) 
    total_weight_generic <- sum(generic$weight) 
    
    n_branded <- branded %>%
      mutate(n_total_weight = 1*weight) %>%
      summarise(N_b = sum(n_total_weight)) %>%
      mutate(index = id)
    
    n_generic <- generic %>%
      mutate(n_total_weight = 1*weight) %>%
      summarise(N_g = sum(n_total_weight)) %>%
      mutate(index = id)
    
    p_branded <- branded %>%
      mutate(price_total_weight = price_total*weight) %>%
      summarise(P_b = sum(price_total_weight)/total_weight_branded) %>%
      mutate(index = id)
    
    p_generic <- generic %>%
      mutate(price_total_weight = price_total*weight) %>%
      summarise(P_g = sum(price_total_weight)/total_weight_generic) %>%
      mutate(index = id)
    
    data <- n_branded %>%
      left_join(n_generic, by = "index") %>%
      left_join(p_branded, by = "index") %>%
      left_join(p_generic, by = "index") 
    
    return(data)
  })
}

MEPS2007_summary <- getMEPSannual(MEPS = MEPS2007, index = index) %>% 
  mutate(year = 2007) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2008_summary <- getMEPSannual(MEPS = MEPS2008, index = index) %>% 
  mutate(year = 2008) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2009_summary <- getMEPSannual(MEPS = MEPS2009, index = index) %>% 
  mutate(year = 2009) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2010_summary <- getMEPSannual(MEPS = MEPS2010, index = index) %>% 
  mutate(year = 2010) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2011_summary <- getMEPSannual(MEPS = MEPS2011, index = index) %>% 
  mutate(year = 2011) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2012_summary <- getMEPSannual(MEPS = MEPS2012, index = index) %>% 
  mutate(year = 2012) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2013_summary <- getMEPSannual(MEPS = MEPS2013, index = index) %>% 
  mutate(year = 2013) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2014_summary <- getMEPSannual(MEPS = MEPS2014, index = index) %>% 
  mutate(year = 2014) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2015_summary <- getMEPSannual(MEPS = MEPS2015, index = index) %>% 
  mutate(year = 2015) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2016_summary <- getMEPSannual(MEPS = MEPS2016, index = index) %>% 
  mutate(year = 2016) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2017_summary <- getMEPSannual(MEPS = MEPS2017, index = index) %>% 
  mutate(year = 2017) %>%
  select(index, year, P_b, P_g, N_b, N_g)

#Weighted
MEPS2006_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2006, index = index) %>% 
  mutate(year = 2006) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2007_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2007, index = index) %>% 
  mutate(year = 2007) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2008_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2008, index = index) %>% 
  mutate(year = 2008) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2009_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2009, index = index) %>% 
  mutate(year = 2009) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2010_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2010, index = index) %>% 
  mutate(year = 2010) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2011_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2011, index = index) %>% 
  mutate(year = 2011) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2012_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2012, index = index) %>% 
  mutate(year = 2012) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2013_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2013, index = index) %>% 
  mutate(year = 2013) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2014_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2014, index = index) %>% 
  mutate(year = 2014) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2015_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2015, index = index) %>% 
  mutate(year = 2015) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2016_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2016, index = index) %>% 
  mutate(year = 2016) %>%
  select(index, year, P_b, P_g, N_b, N_g)

MEPS2017_summary_weighted <- getMEPSannual_weighted(MEPS = MEPS2017, index = index) %>% 
  mutate(year = 2017) %>%
  select(index, year, P_b, P_g, N_b, N_g)




MEPS_summary <- MEPS2007_summary %>%
  rbind(MEPS2008_summary) %>%
  rbind(MEPS2009_summary) %>%
  rbind(MEPS2010_summary) %>%
  rbind(MEPS2011_summary) %>%
  rbind(MEPS2012_summary) %>%
  rbind(MEPS2013_summary) %>%
  rbind(MEPS2014_summary) %>%
  rbind(MEPS2015_summary) %>%
  rbind(MEPS2016_summary) %>%
  rbind(MEPS2017_summary) 


MEPS_summary_weighted <- MEPS2007_summary_weighted %>%
  rbind(MEPS2008_summary_weighted) %>%
  rbind(MEPS2009_summary_weighted) %>%
  rbind(MEPS2010_summary_weighted) %>%
  rbind(MEPS2011_summary_weighted) %>%
  rbind(MEPS2012_summary_weighted) %>%
  rbind(MEPS2013_summary_weighted) %>%
  rbind(MEPS2014_summary_weighted) %>%
  rbind(MEPS2015_summary_weighted) %>%
  rbind(MEPS2016_summary_weighted) %>%
  rbind(MEPS2017_summary_weighted) 
  
#combine competitor info
MEPS_summary <- MEPS_summary %>%
  left_join(MEPS, by = c("index", "year")) %>%
  arrange(index)

MEPS_summary_weighted <- MEPS_summary_weighted %>%
  left_join(MEPS, by = c("index", "year")) %>%
  arrange(index)

####################################################################################
##Inflation-adjusted price
####################################################################################
CPI <- CPI %>% 
  filter(year >= 2007 & year <= 2017) %>%
  group_by(year) %>%
  summarise(value = mean(value))

#normalized by 2019 CPI 
value_2017 <- CPI$value[11]
CPI_normalized <- CPI %>% 
  mutate(value_normalized = value/value_2017)

CPI_list <- CPI_normalized$value_normalized
year_list <- CPI_normalized$year

MEPS_summary <- pmap_dfr(list(CPI_year = year_list, CPI_adjustment = CPI_list), function(CPI_year, CPI_adjustment){
  MEPS_summary <- MEPS_summary %>%
    filter(year == CPI_year) %>%
    mutate(P_b = P_b/CPI_adjustment,
           P_g = P_g/CPI_adjustment)
  return(MEPS_summary)
})

MEPS_summary_weighted <- pmap_dfr(list(CPI_year = year_list, CPI_adjustment = CPI_list), function(CPI_year, CPI_adjustment){
  MEPS_summary_weighted <- MEPS_summary_weighted %>%
    filter(year == CPI_year) %>%
    mutate(P_b = P_b/CPI_adjustment,
           P_g = P_g/CPI_adjustment)
  return(MEPS_summary_weighted)
})

MEPS_summary <- MEPS_summary %>%
  arrange(index) %>%
  mutate(P_ratio = P_g/P_b)

MEPS_summary_weighted <- MEPS_summary_weighted %>%
  arrange(index) %>%
  mutate(P_ratio = P_g/P_b)

####################################################################################
##Add time since LOE
####################################################################################
#for generic entry with PC: year of exclucivity (index date)
#for no PC: year of branded drug expiry (index date)
index_year <- generic_included %>%
  select(index, year_LOE) %>%
  distinct(index, year_LOE)

MEPS_summary <- MEPS_summary %>%
  left_join(index_year, by = "index") %>%
  mutate(t_LOE = year - year_LOE)  

MEPS_summary_weighted <- MEPS_summary_weighted %>%
  left_join(index_year, by = "index") %>%
  mutate(t_LOE = year - year_LOE) 

####################################################################################
##Market size prior to LOE
####################################################################################

get_market_size <- function(df){
  df <- df %>%
    mutate(market_size = P_b * N_b) %>%
    select(index, year, market_size)
  return(df)
}
market_size_2006 <- get_market_size(MEPS2006_summary_weighted)
market_size_2007 <- get_market_size(MEPS2007_summary_weighted)
market_size_2008 <- get_market_size(MEPS2008_summary_weighted)
market_size_2009 <- get_market_size(MEPS2009_summary_weighted)
market_size_2010 <- get_market_size(MEPS2010_summary_weighted)
market_size_2011 <- get_market_size(MEPS2011_summary_weighted)
market_size_2012 <- get_market_size(MEPS2012_summary_weighted)
market_size_2013 <- get_market_size(MEPS2013_summary_weighted)
market_size_2014 <- get_market_size(MEPS2014_summary_weighted)
market_size_2015 <- get_market_size(MEPS2015_summary_weighted)
market_size_2016 <- get_market_size(MEPS2016_summary_weighted)
market_size_2017 <- get_market_size(MEPS2017_summary_weighted)

market_size <- market_size_2006 %>%
  rbind(market_size_2007) %>%
  rbind(market_size_2008) %>%
  rbind(market_size_2009) %>%
  rbind(market_size_2010) %>%
  rbind(market_size_2011) %>%
  rbind(market_size_2012) %>%
  rbind(market_size_2013) %>%
  rbind(market_size_2014) %>%
  rbind(market_size_2015) %>%
  rbind(market_size_2016) %>%
  rbind(market_size_2017) 
  

index_year <- index_year %>%
  mutate(year_prior_LOE = year_LOE - 1) 

market_size_prior_LOE <- index_year %>%
  left_join(market_size, by = c("index", "year_prior_LOE" = "year")) %>%
  select(index, market_size)

MEPS_summary_weighted <- MEPS_summary_weighted %>% 
  left_join(market_size_prior_LOE, by = "index")

####################################################################################
##branded drug price prior to LOE
####################################################################################
get_branded_price_prior_LOE <- function(df){
  df <- df %>%
    select(index, year, P_b)
  return(df)
}
P_b_2006 <- get_branded_price_prior_LOE(MEPS2006_summary_weighted)
P_b_2007 <- get_branded_price_prior_LOE(MEPS2007_summary_weighted)
P_b_2008 <- get_branded_price_prior_LOE(MEPS2008_summary_weighted)
P_b_2009 <- get_branded_price_prior_LOE(MEPS2009_summary_weighted)
P_b_2010 <- get_branded_price_prior_LOE(MEPS2010_summary_weighted)
P_b_2011 <- get_branded_price_prior_LOE(MEPS2011_summary_weighted)
P_b_2012 <- get_branded_price_prior_LOE(MEPS2012_summary_weighted)
P_b_2013 <- get_branded_price_prior_LOE(MEPS2013_summary_weighted)
P_b_2014 <- get_branded_price_prior_LOE(MEPS2014_summary_weighted)
P_b_2015 <- get_branded_price_prior_LOE(MEPS2015_summary_weighted)
P_b_2016 <- get_branded_price_prior_LOE(MEPS2016_summary_weighted)
P_b_2017 <- get_branded_price_prior_LOE(MEPS2017_summary_weighted)

P_b_prior_LOE <- P_b_2006 %>%
  rbind(P_b_2007) %>%
  rbind(P_b_2008) %>%
  rbind(P_b_2009) %>%
  rbind(P_b_2010) %>%
  rbind(P_b_2011) %>%
  rbind(P_b_2012) %>%
  rbind(P_b_2013) %>%
  rbind(P_b_2014) %>%
  rbind(P_b_2015) %>%
  rbind(P_b_2016) %>%
  rbind(P_b_2017) 

P_b_prior_LOE <- index_year %>%
  left_join(P_b_prior_LOE, by = c("index", "year_prior_LOE" = "year")) %>%
  select(index, P_b) %>%
  rename(P_b_prior_LOE = P_b)

MEPS_summary_weighted <- MEPS_summary_weighted %>% 
  left_join(P_b_prior_LOE, by = "index")

####################################################################################
##Add therapeutic areas, route
####################################################################################
load("~/Dropbox/Advanced Method Project/Data/all.RData")

#keep index, oral, inject, ATC*
drug_info <- df %>%
  select(index, oral, inject, ATCA:ATCV) %>%
  distinct()

MEPS_summary_weighted <- MEPS_summary_weighted %>% 
  left_join(drug_info, by = "index")

save(MEPS_summary, file = "MEPS_summary.Rdata")
write.xlsx(MEPS_summary, "MEPS_summary.xlsx")

save(MEPS_summary_weighted, file = "MEPS_summary_weighted.Rdata")
write.xlsx(MEPS_summary_weighted, "MEPS_summary_weighted.xlsx")

save(MEPS_summary_weighted, file = "~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian/MEPS_summary_weighted.Rdata")
write.xlsx(MEPS_summary_weighted, "~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian/MEPS_summary_weighted.xlsx")



# Check mismatch of NDC code
index_all_PIV <- genericPIV %>%
  distinct(index)

index_all_noPIV <- genericnoPIV %>%
  distinct(index)

index_all <- index_all_PIV %>%
  bind_rows(index_all_noPIV) %>%
  arrange(index)

index_all <- index_all$index

