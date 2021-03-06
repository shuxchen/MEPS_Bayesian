#load NDC data
NDC <- read_excel("~/Dropbox/Advanced Method Project/Data/ndcxls_20180706/product.xlsx")
NDC_package <- read_excel("~/Dropbox/Advanced Method Project/Data/ndcxls_20180706/package.xlsx")
NDC_package <- NDC_package[, c(1, 3)]
NDC <- merge(NDC, NDC_package, by=c("PRODUCTID"), all.x=T)
NDC <- NDC[!duplicated(NDC),]

NDC_excluded <- read_excel("~/Dropbox/Advanced Method Project/Data/ndcxls_20180706/ndc_excluded/Products_excluded.xlsx")
NDC_excluded_package <- read_excel("~/Dropbox/Advanced Method Project/Data/ndcxls_20180706/ndc_excluded/Packages_excluded.xlsx")
NDC_excluded_package <- NDC_excluded_package[, c(1, 3)]
NDC_excluded <- merge(NDC_excluded, NDC_excluded_package, by=c("PRODUCTID"), all.x=T)
NDC_excluded <- NDC_excluded[!duplicated(NDC_excluded),]
NDC_excluded$NDC9 <- gsub("-", "", NDC_excluded$PRODUCTNDC)
save(NDC_excluded, file = "NDC_excluded.Rdata")


NDC_unfinished <- read_excel("~/Dropbox/Advanced Method Project/Data/ndcxls_20180706/ndc_unfinished/unfinished_product.xlsx")
NDC_unfinished_package <- read_excel("~/Dropbox/Advanced Method Project/Data/ndcxls_20180706/ndc_unfinished/unfinished_package.xlsx")
NDC_unfinished_package <- NDC_unfinished_package[, c(1, 3)]
NDC_unfinished <- merge(NDC_unfinished, NDC_unfinished_package, by=c("PRODUCTID"), all.x=T)
NDC_unfinished <- NDC_unfinished[!duplicated(NDC_unfinished),]

NDC_unfinished_excluded <- read_excel("~/Dropbox/Advanced Method Project/Data/ndcxls_20180706/ndc_unfinished_excluded/unfinished_products_excluded.xlsx")
NDC_unfinished_package_excluded <- read_excel("~/Dropbox/Advanced Method Project/Data/ndcxls_20180706/ndc_unfinished_excluded/unfinished_packages_excluded.xlsx")
NDC_unfinished_package_excluded <- NDC_unfinished_package_excluded[, c(1, 3)]
NDC_unfinished_excluded <- merge(NDC_unfinished_excluded, NDC_unfinished_package_excluded, by=c("PRODUCTID"), all.x=T)
NDC_unfinished_excluded <- NDC_unfinished_excluded[!duplicated(NDC_unfinished_excluded),]

NDC_2014 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/2014/product.csv")
NDC_2014_package <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/2014/package.csv")
NDC_2014_package <- NDC_2014_package[, c(1, 3)]
NDC_2014 <- merge(NDC_2014, NDC_2014_package, by=c("prodid"), all.x=T)
NDC_2014 <- NDC_2014[!duplicated(NDC_2014),]

NDC_2018 <- read_excel("~/Dropbox/Advanced Method Project/Data/NDC_historical/2018/2008to2018.xlsx")

NDC_2018 <- NDC_2018 %>%
  dplyr::select(
          PRODUCTTYPENAME,
          PROPRIETARYNAME,
          #PROPRIETARYNAMESUFFIX,
          NONPROPRIETARYNAME,
          DOSAGEFORMNAME,
          ROUTENAME,
          #STARTMARKETINGDATE,
          #ENDMARKETINGDATE,
          #MARKETINGCATEGORYNAME,
          APPLICATIONNUMBER,
          LABELERNAME,
          SUBSTANCENAME,
          ACTIVE_NUMERATOR_STRENGTH,
          ACTIVE_INGRED_UNIT,
          PHARM_CLASSES,
          DEASCHEDULE,
          #NDC_EXCLUDE_FLAG,
          #LISTING_RECORD_CERTIFIED_THROUGH,
          NDCPACKAGECODE) %>%
  mutate(PRODUCTNDC = NA)


NDC_2014 <- NDC_2014 %>%
  dplyr::select(-listing_record_cert, -stmarkdate, -endmarkdate) %>%
  rename(PRODUCTID=prodid,
         PRODUCTNDC=ndc,
         PRODUCTTYPENAME=prodtype,
         PROPRIETARYNAME=propname,
         PROPRIETARYNAMESUFFIX=propsuf,
         NONPROPRIETARYNAME=npropname,
         DOSAGEFORMNAME=dosename,
         ROUTENAME=routename,
         STARTMARKETINGDATE=stmarkdatestr,
         ENDMARKETINGDATE=endmarkdatestr,
         MARKETINGCATEGORYNAME=markname,
         APPLICATIONNUMBER=appnum,
         LABELERNAME=labelname,
         SUBSTANCENAME=subname,
         ACTIVE_NUMERATOR_STRENGTH=actnumstr,
         ACTIVE_INGRED_UNIT=actingunit,
         PHARM_CLASSES=pharmclas,
         DEASCHEDULE=deasched,
         NDC_EXCLUDE_FLAG=ndc_exclude_flag,
         LISTING_RECORD_CERTIFIED_THROUGH=listing_record_certstr,
         NDCPACKAGECODE=packcode) 

NDC <- NDC %>%
  rbind(NDC_2014) %>%
  dplyr::select(PRODUCTTYPENAME,
                PROPRIETARYNAME,
                #PROPRIETARYNAMESUFFIX,
                NONPROPRIETARYNAME,
                DOSAGEFORMNAME,
                ROUTENAME,
                #STARTMARKETINGDATE,
                #ENDMARKETINGDATE,
                #MARKETINGCATEGORYNAME,
                APPLICATIONNUMBER,
                LABELERNAME,
                SUBSTANCENAME,
                ACTIVE_NUMERATOR_STRENGTH,
                ACTIVE_INGRED_UNIT,
                PHARM_CLASSES,
                DEASCHEDULE,
                #NDC_EXCLUDE_FLAG,
                #LISTING_RECORD_CERTIFIED_THROUGH,
                NDCPACKAGECODE,
                PRODUCTNDC)

NDC <- NDC[!duplicated(NDC), ]

NDC <- NDC %>%
  rbind(NDC_2018) 

NDC$ID <- seq.int(nrow(NDC))

#NDC <- NDC %>%
#  dplyr::select(1:4, DOSAGEFORMNAME, STARTMARKETINGDATE, MARKETINGCATEGORYNAME, LABELERNAME, SUBSTANCENAME, ACTIVE_NUMERATOR_STRENGTH, ACTIVE_INGRED_UNIT, NDCPACKAGECODE, APPLICATIONNUMBER)

NDC_unfinished <- NDC_unfinished %>%
  dplyr::select(1:4, DOSAGEFORMNAME, STARTMARKETINGDATE, MARKETINGCATEGORYNAME, LABELERNAME, SUBSTANCENAME, ACTIVE_NUMERATOR_STRENGTH, ACTIVE_INGRED_UNIT, NDCPACKAGECODE)

#change Application number to number only (string)
NDC$Appl_No <- sapply(strsplit(NDC$APPLICATIONNUMBER, split='NDA', fixed=TRUE), function(x) (x[2]))

#make NDC code into the same format as in MEPS
# 09999-9999 if 9999-9999
# 99999-0999 if 99999-999
# 99999-9999

#NDC$NDC <- ifelse(nchar(NDC$PRODUCTNDC)==10, NDC$PRODUCTNDC, 
#                  ifelse(str_sub(NDC$PRODUCTNDC, -4, -4) == "-", sub( '(?<=.{6})', '0', NDC$PRODUCTNDC, perl=TRUE ), sub( '(?<=.{1})', '0', NDC$PRODUCTNDC, perl=TRUE)))

#make NDC code into the same format as in MEPS (11 digits)
# 09999-9999-99 if 9999-9999-99
# 99999-0999-99 if 99999-999-99 
# 99999-9999-09 if 99999-9999-9

NDC$NDC <- ifelse(str_sub(NDC$NDCPACKAGECODE, 5, 5) == "-", paste0("0", NDC$NDCPACKAGECODE), ifelse(str_sub(NDC$NDCPACKAGECODE, 10, 10) == "-", sub( '(?<=.{6})', '0', NDC$NDCPACKAGECODE, perl = T), sub( '(?<=.{11})', '0', NDC$NDCPACKAGECODE, perl = T)))

#Get rid of "-"
NDC$NDC <- gsub("-", "", NDC$NDC)

NDC$NDC11 <- NDC$NDC
NDC$NDC9 <- ifelse(str_sub(NDC$PRODUCTNDC, 5, 5) == "-", paste0("0", NDC$PRODUCTNDC), ifelse(nchar(NDC$PRODUCTNDC) == 9, sub( '(?<=.{6})', '0', NDC$PRODUCTNDC, perl = T), NDC$PRODUCTNDC))

NDC$NDC9 <- gsub("-", "", NDC$NDC9)

NDC <- NDC %>%
  mutate(NDC9_new = ifelse(is.na(NDC9), substr(NDC, start = 1, stop = 9), NDC9)) %>%
  dplyr::select(-NDC9) %>%
  rename(NDC9 = NDC9_new)


#Keep NDA, ANDA and Authorized Generics
#Did not consider OTC MONOGRAPH FINAL
#NDC<-NDC[( NDC$MARKETINGCATEGORYNAME=="NDA" | NDC$MARKETINGCATEGORYNAME=="ANDA" | NDC$MARKETINGCATEGORYNAME=="NDA AUTHORIZED GENERIC"), ]

#Exclude those that have been excluded
#NDC<-NDC[ NDC$NDC_EXCLUDE_FLAG=="N", ]

#Exclude KIT (no information on Strength)
NDC<-NDC[ !(NDC$DOSAGEFORMNAME=="KIT"), ]

#Get Appl_No
NDC$Appl_No<-str_sub(NDC$APPLICATIONNUMBER, -6, -1)

#Exclude biologics (Application number starting with BN)
NDC$Appl_type<-str_sub(NDC$APPLICATIONNUMBER, -9, -7)
NDC<-NDC[(! NDC$Appl_type=="BA" | ! NDC$Appl_type=="BN"), ]


#Make the strength consistent across two datasets
NDC<-NDC %>% mutate_at(vars(-ACTIVE_NUMERATOR_STRENGTH, -LABELERNAME), funs(toupper))

NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/1"] <- "MG"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/1; MG/1"] <- "MG; MG"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/1"] <- "GM"
#a.1 [IU]/G --> UNITS/GM; [IU]/ML --> UNITS/ML
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "[IU]/G"] <- " UNITS/GM"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "[IU]/ML"] <- " UNITS/ML"

#a.2 Other [IU] related
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "[IU]/.09ML"] <- " IU/SPRAY"

NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0069-0195"] <- "IU/0.2ML (12,500IU/ML)"
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0069-0196"] <- "IU/0.2ML (25,000IU/ML)"
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0069-0206"] <- "IU/0.3ML (25,000IU/ML)"
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0069-0220"] <- "IU/0.5ML (25,000IU/ML)"
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0069-0223"] <- "IU/0.6ML (25,000IU/ML)"
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0069-0228"] <- "IU/0.72ML (25,000IU/ML)"
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0069-0217"] <- "IU/ML (10,000IU/ML)"
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0069-0232"] <- "IU/ML (25,000IU/ML)"

#For the following one, small inconsistency in strength, changed to be consistent with OB
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0052-0313"] <- " IU/0.36ML"
NDC$ACTIVE_NUMERATOR_STRENGTH[NDC$PRODUCTNDC=="0052-0313"] <- 300
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0052-0316"] <- " IU/0.72ML"
NDC$ACTIVE_NUMERATOR_STRENGTH[NDC$PRODUCTNDC=="0052-0316"] <- 600
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0052-0326"] <- " IU/1.08ML"
NDC$ACTIVE_NUMERATOR_STRENGTH[NDC$PRODUCTNDC=="0052-0326"] <- 900

NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="50090-1016"] <- " UNITS/ML"
NDC$ACTIVE_NUMERATOR_STRENGTH[NDC$PRODUCTNDC=="50090-1016"] <- 600000
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="60793-131"] <- " UNITS/ML"
NDC$ACTIVE_NUMERATOR_STRENGTH[NDC$PRODUCTNDC=="60793-131"] <- 600000
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="60793-701"] <- " UNITS/ML"
NDC$ACTIVE_NUMERATOR_STRENGTH[NDC$PRODUCTNDC=="60793-701"] <- 600000


NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="021273"] <- "IU/0.5ML"

NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="0069-0195"] <- "IU/0.2ML (12,500IU/ML)"

NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="44087-1115"] <- " IU/0.5ML"
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="44087-1116"] <- " IU/0.75ML"
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="44087-1117"] <- " IU/1.5ML"

NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="020313"] <- " IU/SPRAY"
NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="076396"] <- " IU/SPRAY"

NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="065079"] <- " UNITS"
NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="060657"] <- " UNITS"
NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="065149"] <- " UNITS"
NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="090211"] <- " UNITS"

NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="45932-0026"] <- " UNITS"

NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="207322"] <- " UNITS"

NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="065448"] <- " UNITS"

NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="065116"] <- " UNITS"


#a.3 [USP'U] related
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="55154-9392"] <- " UNITS/ML"
NDC$ACTIVE_NUMERATOR_STRENGTH[NDC$PRODUCTNDC=="55154-9392"] <- 10000
NDC$ACTIVE_INGRED_UNIT[NDC$PRODUCTNDC=="63323-543"] <- " UNITS/ML"
NDC$ACTIVE_NUMERATOR_STRENGTH[NDC$PRODUCTNDC=="63323-543"] <- 10000

NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "[USP'U]/1"] <- " UNITS"

NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "[USP'U]/100ML"] <- " UNITS/100ML"

NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "[USP'U]/G"] <- " UNITS/GM"

NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "[USP'U]/ML"] <- " UNITS/ML"

#a.4 1/G
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "1/G"] <- " UNITS/GM"

#Some miscoded:
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "43101"] <- "MG"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "36892"] <- "MG"

#a.4 CI/1
NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="017243"] <- " CI/GENERATOR"
NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="202158"] <- " CI/GENERATOR"

#a.5 G/100G
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/100G"] <- "%"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/100ML"] <- "%"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/10G"] <- "GM"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/10ML"] <- "GM"

NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="66220-729"] <- "GM"
NDC$ACTIVE_INGRED_UNIT[NDC$Appl_No=="67414-729"] <- "GM"

NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/20ML"] <- "GM"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/30ML"] <- "GM"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/5.5G"] <- "GM"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/5.7G"] <- "GM"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/50ML"] <- "GM/50ML (50MG/ML)"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/60ML"] <- "GM/60ML"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "G/9G"] <- "GM"

#no match for "G/G","G/ML", "KG"

#a.6 ""L/..." delete all GAS
NDC<-NDC[ !(NDC$DOSAGEFORMNAME=="GAS"), ]

#a.7 "MCL/ML" cannot find fludeoxyglucose f 18 injection in OB

#a.8 "MEQ/1"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MEQ/1"] <- "MEQ"
#no "MEQ/1000ML", "MEQ/100ML", "MEQ/30ML", "MEQ/15ML", "MEQ/5ML"
#only one "MEQ/G", and not matching
#"MEQ/ML" remain the same

#a.10 more generic way: "MG;MG"
#add MG before ";", delete " ", and add "MG" in the end
doubleMG<-subset(NDC, ACTIVE_INGRED_UNIT=="MG; MG")
doubleMG$ACTIVE_NUMERATOR_STRENGTH<-gsub("; ", "MG;", doubleMG$ACTIVE_NUMERATOR_STRENGTH)
doubleMG$ACTIVE_NUMERATOR_STRENGTH<-paste0(doubleMG$ACTIVE_NUMERATOR_STRENGTH, "MG")
doubleMG$Strength2<-doubleMG$ACTIVE_NUMERATOR_STRENGTH

#Similar for "MG;MG;MG"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/1; MG/1; MG/1"] <- "MG; MG; MG"
tripleMG<-subset(NDC, ACTIVE_INGRED_UNIT=="MG; MG; MG")
tripleMG$ACTIVE_NUMERATOR_STRENGTH<-gsub("; ", "MG;", tripleMG$ACTIVE_NUMERATOR_STRENGTH)
tripleMG$ACTIVE_NUMERATOR_STRENGTH<-paste0(tripleMG$ACTIVE_NUMERATOR_STRENGTH, "MG")
tripleMG$Strength2<-tripleMG$ACTIVE_NUMERATOR_STRENGTH

#Similar for "MG;MG;MG;MG"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/1; MG/1; MG/1; MG/1"] <- "MG; MG; MG; MG"
quadMG<-subset(NDC, ACTIVE_INGRED_UNIT=="MG; MG; MG; MG")
quadMG$ACTIVE_NUMERATOR_STRENGTH<-gsub("; ", "MG;", quadMG$ACTIVE_NUMERATOR_STRENGTH)
quadMG$ACTIVE_NUMERATOR_STRENGTH<-paste0(quadMG$ACTIVE_NUMERATOR_STRENGTH, "MG")
quadMG$Strength2<-quadMG$ACTIVE_NUMERATOR_STRENGTH

#a.11 "MG/10ML"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/10ML"] <- "MG"

#a.12 "MG/17.5ML", "MG/20ML"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/17.5ML"] <- "MG"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/201"] <- "MG"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/20MG"] <- "MG"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/20ML"] <- "MG"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/250MG"] <- "MG"

#a.13 "MG/2ML"; "MG/301"; "MG/50ML"
NDC$ACTIVE_NUMERATOR_STRENGTH<-with(NDC, ifelse(ACTIVE_INGRED_UNIT == "MG/2ML", as.numeric(ACTIVE_NUMERATOR_STRENGTH)/2, ACTIVE_NUMERATOR_STRENGTH))
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/301"] <- "MG"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/40ML"] <- "MG"
NDC$ACTIVE_NUMERATOR_STRENGTH<-with(NDC, ifelse(ACTIVE_INGRED_UNIT == "MG/50ML", as.numeric(ACTIVE_NUMERATOR_STRENGTH)/50, ACTIVE_NUMERATOR_STRENGTH))

#a.14 "MG/5ML" keep the same
#Multiple "MG/5ML"
multiMG5ML<-subset(NDC, ACTIVE_INGRED_UNIT=="MG/5ML; MG/5ML" | ACTIVE_INGRED_UNIT=="MG/5ML; MG/5ML; MG/5ML")
multiMG5ML$ACTIVE_NUMERATOR_STRENGTH<-gsub("; ", "MG/5ML;", multiMG5ML$ACTIVE_NUMERATOR_STRENGTH)
multiMG5ML$ACTIVE_NUMERATOR_STRENGTH<-paste0(multiMG5ML$ACTIVE_NUMERATOR_STRENGTH, "MG/5ML")
multiMG5ML$Strength2<-multiMG5ML$ACTIVE_NUMERATOR_STRENGTH

NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/61"] <- "MG"

NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/D"] <- "MG/24HR"

NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/G"] <- "%"

NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/H"] <- "MG/HR"
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "MG/MG"] <- "MG"

#a.15  "MG/ML; MG/ML"
multiMGML<-subset(NDC, ACTIVE_INGRED_UNIT=="MG/ML; MG/ML" | ACTIVE_INGRED_UNIT=="MG/ML; MG/ML; MG/ML")
multiMGML$ACTIVE_NUMERATOR_STRENGTH<-gsub("; ", "MG/ML;", multiMGML$ACTIVE_NUMERATOR_STRENGTH)
multiMGML$ACTIVE_NUMERATOR_STRENGTH<-paste0(multiMGML$ACTIVE_NUMERATOR_STRENGTH, "MG/5ML")
multiMGML$Strength2<-multiMGML$ACTIVE_NUMERATOR_STRENGTH



NDC$ACTIVE_NUMERATOR_STRENGTH<-formatC(as.numeric(NDC$ACTIVE_NUMERATOR_STRENGTH), format="fg", big.mark = ",")
#NDC$ACTIVE_NUMERATOR_STRENGTH<-formatC(as.numeric(NDC$ACTIVE_NUMERATOR_STRENGTH), drop0trailing = TRUE)

NDC<-transform(NDC, Strength=paste(ACTIVE_NUMERATOR_STRENGTH, ACTIVE_INGRED_UNIT, sep="")) 

#b. Get subset of NDC and change UG to MG
NDC$ACTIVE_INGRED_UNIT[NDC$ACTIVE_INGRED_UNIT == "UG/1"] <- "UG"
#temp1<-NDC[NDC$ACTIVE_INGRED_UNIT=="UG", ]
temp1<-subset(NDC, ACTIVE_INGRED_UNIT=="UG" )
temp1$ACTIVE_NUMERATOR_STRENGTH<-as.numeric(temp1$ACTIVE_NUMERATOR_STRENGTH)
temp1 <- temp1[!is.na(temp1$ACTIVE_NUMERATOR_STRENGTH),]

temp1$ACTIVE_INGRED_UNIT[temp1$ACTIVE_NUMERATOR_STRENGTH>=50] <- "MG"
index <- temp1$ACTIVE_NUMERATOR_STRENGTH>=50
temp1$ACTIVE_NUMERATOR_STRENGTH[index] <- (temp1$ACTIVE_NUMERATOR_STRENGTH[index])/1000 
temp1$ACTIVE_INGRED_UNIT[index] <- "MCG"

temp1$ACTIVE_NUMERATOR_STRENGTH<-formatC(temp1$ACTIVE_NUMERATOR_STRENGTH, drop0trailing = TRUE)

#Change format to make the unit consistent within each Appl_No
temp1$ACTIVE_NUMERATOR_STRENGTH[ (temp1$Appl_No==010379 |temp1$Appl_No== 020746 | temp1$Appl_No==020393 | temp1$Appl_No==019389 | temp1$Appl_No==022051 | temp1$Appl_No==021433 | temp1$Appl_No==022051 | temp1$Appl_No==090097 | temp1$Appl_No==078949 | temp1$Appl_No==076187 | temp1$Appl_No==090326 | temp1$Appl_No==021730 | temp1$Appl_No==020393 | temp1$Appl_No==021395 | temp1$Appl_No==020394 | temp1$Appl_No==021527 | temp1$Appl_No==203108 |temp1$Appl_No==021342 | temp1$Appl_No==078949 |temp1$Appl_No==021924 | temp1$Appl_No==076025 |temp1$Appl_No==076103 |temp1$Appl_No==090097 | temp1$Appl_No==021342 |temp1$Appl_No==078949 | temp1$Appl_No==076103 |temp1$Appl_No==200295 |temp1$Appl_No==090097 |temp1$Appl_No==202097 |temp1$Appl_No==021433 |temp1$Appl_No==090326 | temp1$Appl_No==020746 |temp1$Appl_No==076187 |temp1$Appl_No==021342 | temp1$Appl_No==200295 |temp1$Appl_No==090097 |temp1$Appl_No==020911 | temp1$Appl_No==076187 | temp1$Appl_No==010379 | temp1$Appl_No==076187 | temp1$Appl_No==021301 | temp1$Appl_No==020831 | temp1$Appl_No==076103 | temp1$Appl_No==010379 |temp1$Appl_No==021395 | temp1$Appl_No==021527 | temp1$Appl_No==020393 | temp1$Appl_No==021730 | temp1$Appl_No==076025 | temp1$Appl_No==020911 |temp1$Appl_No==021433 | temp1$Appl_No==022051 | temp1$Appl_No==076187 | temp1$Appl_No==021342 | temp1$Appl_No==200295 | temp1$Appl_No==020911 | temp1$Appl_No==202813 | temp1$Appl_No==207921 | temp1$Appl_No==010379 |temp1$Appl_No==021210 | temp1$Appl_No==090097 | temp1$Appl_No==021730 | temp1$Appl_No==202129 | temp1$Appl_No==200295 | temp1$Appl_No==200295 | temp1$Appl_No==021342 | temp1$Appl_No==090097 | temp1$Appl_No==021402 | temp1$Appl_No==021342 | temp1$Appl_No==021730 | temp1$Appl_No==021433 | temp1$Appl_No==208010 | temp1$Appl_No==202129 | temp1$Appl_No==200295 | temp1$Appl_No==200295) & temp1$ACTIVE_INGRED_UNIT=="MCG"] <- as.numeric(temp1$ACTIVE_NUMERATOR_STRENGTH)/1000
temp1$ACTIVE_INGRED_UNIT[ (temp1$Appl_No==010379 |temp1$Appl_No== 020746 | temp1$Appl_No==020393 | temp1$Appl_No==019389 | temp1$Appl_No==022051 | temp1$Appl_No==021433 | temp1$Appl_No==022051 | temp1$Appl_No==090097 | temp1$Appl_No==078949 | temp1$Appl_No==076187 | temp1$Appl_No==090326 | temp1$Appl_No==021730 | temp1$Appl_No==020393 | temp1$Appl_No==021395 | temp1$Appl_No==020394 | temp1$Appl_No==021527 | temp1$Appl_No==203108 |temp1$Appl_No==021342 | temp1$Appl_No==078949 |temp1$Appl_No==021924 | temp1$Appl_No==076025 |temp1$Appl_No==076103 |temp1$Appl_No==090097 | temp1$Appl_No==021342 |temp1$Appl_No==078949 | temp1$Appl_No==076103 |temp1$Appl_No==200295 |temp1$Appl_No==090097 |temp1$Appl_No==202097 |temp1$Appl_No==021433 |temp1$Appl_No==090326 | temp1$Appl_No==020746 |temp1$Appl_No==076187 |temp1$Appl_No==021342 | temp1$Appl_No==200295 |temp1$Appl_No==090097 |temp1$Appl_No==020911 | temp1$Appl_No==076187 | temp1$Appl_No==010379 | temp1$Appl_No==076187 | temp1$Appl_No==021301 | temp1$Appl_No==020831 | temp1$Appl_No==076103 | temp1$Appl_No==010379 |temp1$Appl_No==021395 | temp1$Appl_No==021527 | temp1$Appl_No==020393 | temp1$Appl_No==021730 | temp1$Appl_No==076025 | temp1$Appl_No==020911 |temp1$Appl_No==021433 | temp1$Appl_No==022051 | temp1$Appl_No==076187 | temp1$Appl_No==021342 | temp1$Appl_No==200295 | temp1$Appl_No==020911 | temp1$Appl_No==202813 | temp1$Appl_No==207921 | temp1$Appl_No==010379 |temp1$Appl_No==021210 | temp1$Appl_No==090097 | temp1$Appl_No==021730 | temp1$Appl_No==202129 | temp1$Appl_No==200295 | temp1$Appl_No==200295 | temp1$Appl_No==021342 | temp1$Appl_No==090097 | temp1$Appl_No==021402 | temp1$Appl_No==021342 | temp1$Appl_No==021730 | temp1$Appl_No==021433 | temp1$Appl_No==208010 | temp1$Appl_No==202129 | temp1$Appl_No==200295 | temp1$Appl_No==200295) & temp1$ACTIVE_INGRED_UNIT=="MCG"] <- "MG"
temp1<-transform(temp1, Strength1=paste(ACTIVE_NUMERATOR_STRENGTH, ACTIVE_INGRED_UNIT, sep="")) 
temp1<-temp1[c("ID", "Strength1")]

NDC<- merge(NDC,temp1, by=c("ID"), all=TRUE)
NDC$Strengthnew <- ifelse(!is.na(NDC$Strength1), as.character(NDC$Strength1), as.character(NDC$Strength))
NDC <- NDC %>%
  dplyr::select(-Strength, - Strength1) %>%
  rename(Strength = Strengthnew)

#c. multiple: [IU];[IU]
NDC$Strength[NDC$PRODUCTNDC=="60793-600"] <- "300,000 UNITS/ML;300,000 UNITS/ML"
NDC$Strength[NDC$PRODUCTNDC=="60793-601"] <- "300,000 UNITS/ML;300,000 UNITS/ML"
NDC$Strength[NDC$PRODUCTNDC=="60793-602"] <- "900,000 UNITS/2ML;300,000 UNITS/2ML"
NDC$Strength[NDC$PRODUCTNDC=="60793-702"] <- "600,000 UNITS/ML"
NDC$Strength[NDC$PRODUCTNDC=="0338-1021"] <- "20,000 UNITS/ML"
NDC$Strength[NDC$PRODUCTNDC=="0338-1023"] <- "40,000 UNITS/ML"
NDC$Strength[NDC$PRODUCTNDC=="0338-1025"] <- "60,000 UNITS/ML"
NDC$Strength[NDC$PRODUCTNDC=="0169-2911"] <- "300 UNITS/3ML;10.8MG/3ML (100 UNITS/ML;3.6MG/ML)"

#d. multiple: [USP'U]
NDC$Strength[NDC$PRODUCTNDC=="0023-6110"] <- "42,000USP UNITS;10,000USP UNITS;32,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="0023-6111"] <- "63,000USP UNITS;15,000USP UNITS;47,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="0023-6112"] <- "84,000USP UNITS;20,000USP UNITS;63,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="0023-6113"] <- "14,000USP UNITS;3,000USP UNITS;10,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="0023-6114"] <- "168,000USP UNITS;40,000USP UNITS;126,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="0023-6115"] <- "24,000USP UNITS;5,000USP UNITS;17,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="0023-6116"] <- "105,000USP UNITS;25,000USP UNITS;79,000USP UNITS"

NDC$Strength[NDC$PRODUCTNDC=="0032-1203"] <- "15,000USP UNITS;3,000USP UNITS;9,500USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="0032-1206"] <- "30,000USP UNITS;6,000USP UNITS;19,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="0032-1212"] <- "60,000USP UNITS;12,000USP UNITS;38,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="0032-1224"] <- "120,000USP UNITS;24,000USP UNITS;76,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="0032-1216"] <- "180,000USP UNITS;36,000USP UNITS;114,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="17856-1206"] <- "30,000USP UNITS;6,000USP UNITS;19,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="17856-1212"] <- "60,000USP UNITS;12,000USP UNITS;38,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="17856-1224"] <- "120,000USP UNITS;24,000USP UNITS;76,000USP UNITS"

NDC$Strength[NDC$PRODUCTNDC=="42865-300"] <- "24,000USP UNITS;5,000USP UNITS;17,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="42865-302"] <- "63,000USP UNITS;15,000USP UNITS;47,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="42865-303"] <- "84,000USP UNITS;20,000USP UNITS;63,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="42865-304"] <- "14,000USP UNITS;3,000USP UNITS;10,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="42865-305"] <- "105,000USP UNITS;25,000USP UNITS;79,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="42865-306"] <- "42,000USP UNITS;10,000USP UNITS;32,000USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="42865-307"] <- "168,000USP UNITS;40,000USP UNITS;126,000USP UNITS"

NDC$Strength[NDC$PRODUCTNDC=="50458-341"] <- "17,500USP/ UNITS;4,200USP/ UNITS;10,000USP/ UNITS"
NDC$Strength[NDC$PRODUCTNDC=="50458-342"] <- "43,750USP/ UNITS;10,500USP/ UNITS;25,000USP/ UNITS"
NDC$Strength[NDC$PRODUCTNDC=="50458-343"] <- "70,000USP/ UNITS;16,800USP/ UNITS;40,000USP/ UNITS"
NDC$Strength[NDC$PRODUCTNDC=="50458-346"] <- "61,000USP/ UNITS;21,000USP/ UNITS;37,000USP/ UNITS"
NDC$Strength[NDC$PRODUCTNDC=="50458-347"] <- "10,850USP UNITS;2,600USP UNITS;6,200USP UNITS"

NDC$Strength[NDC$PRODUCTNDC=="58914-112"] <- "39,150USP UNITS;10,440USP UNITS;39,150USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="58914-117"] <- "78,300USP UNITS;20,880USP UNITS;78,300USP UNITS"

NDC$Strength[NDC$PRODUCTNDC=="59767-004"] <- "15,125USP UNITS;4,000USP UNITS;14,375USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="59767-008"] <- "30,250USP UNITS;8,000USP UNITS;28,750USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="59767-016"] <- "30,250USP UNITS;8,000USP UNITS;28,750USP UNITS"
NDC$Strength[NDC$PRODUCTNDC=="59767-024"] <- "90,750USP UNITS;24,000USP UNITS;86,250USP UNITS"

NDC$Strength[NDC$PRODUCTNDC=="0264-9567"] <- "4,000 UNITS/100ML"
NDC$Strength[NDC$PRODUCTNDC=="0264-9577"] <- "5,000 UNITS/100ML"
NDC$Strength[NDC$PRODUCTNDC=="0264-9587"] <- "10,000 UNITS/100ML"
NDC$PRODUCTTYPENAME[NDC$PRODUCTNDC=="0264-9567"] <- "HEPARIN SODIUM 25,000 UNITS IN DEXTROSE 5% IN PLASTIC CONTAINER"
NDC$PRODUCTTYPENAME[NDC$PRODUCTNDC=="0264-9577"] <- "HEPARIN SODIUM 25,000 UNITS IN DEXTROSE 5% IN PLASTIC CONTAINER"
NDC$PRODUCTTYPENAME[NDC$PRODUCTNDC=="0264-9587"] <- "HEPARIN SODIUM 25,000 UNITS IN DEXTROSE 5% IN PLASTIC CONTAINER"

NDC$Strength[NDC$PRODUCTNDC=="64950-230"] <- "100,000 UNITS/ML"
NDC$Strength[NDC$PRODUCTNDC=="55154-1578"] <- "100,000 UNITS/ML"
NDC$Strength[NDC$PRODUCTNDC=="68094-599"] <- "100,000 UNITS/ML"

NDC$Strength[NDC$Appl_No=="065022"] <- "500 UNITS/GM;10,000 UNITS/GM"
NDC$Strength[NDC$Appl_No=="064028"] <- "500 UNITS/GM;10,000 UNITS/GM"
NDC$Strength[NDC$Appl_No=="064046"] <- "500 UNITS/GM;10,000 UNITS/GM"

NDC$Strength[NDC$PRODUCTTYPENAME=="NYSTATIN AND TRIAMCINOLONE ACETONIDE"] <- "100,000 UNITS/GM;0.1%"

NDC$Strength[NDC$PRODUCTNDC=="0023-7824"] <- "10,000 UNITS/ML;1MG/ML"
NDC$Strength[NDC$PRODUCTTYPENAME=="POLYMYXIN B SULFATE AND TRIMETHOPRIM"] <- "10,000 UNITS/ML;1MG/ML"

NDC$Strength[NDC$PRODUCTNDC=="0168-0273"] <- "500 UNITS/GM;10,000 UNITS/GM"

#e.multiple G/100G, G/100ML
NDC$Strength[NDC$Appl_No=="085368"] <- "1%;1%"
NDC$Strength[NDC$Appl_No=="085870"] <- "2MEQ/ML"

NDC$Strength[NDC$PRODUCTNDC=="0409-3715"] <- "10GM"
NDC$Strength[NDC$Appl_No=="074588"] <- "2%"
NDC$Strength[NDC$PRODUCTNDC=="0703-3019"] <- "5GM/100ML (50MG/ML)"
NDC$Strength[NDC$PRODUCTNDC=="0781-3103"] <- "10GM"
#ANDA061490 and ANDA062527 seems to have duplicates (different time to enter the market)

NDC$Strength[NDC$PRODUCTNDC=="0472-0882"] <- "2%;1%"
NDC$Strength[NDC$PRODUCTNDC=="0338-1130"] <- "10% (10GM/100ML)"
NDC$Strength[NDC$PRODUCTNDC=="0338-1131"] <- "6% (6GM/100ML)"
NDC$Strength[NDC$Appl_No=="020512"] <- "15% (15GM/100ML)"

#e. multiple MG
#might have wrong order, so cannot match well
#need to merge doubleMG's strength info back in
NDC<-merge(NDC, doubleMG, all=TRUE)
NDC<-NDC%>%mutate(Strength = ifelse(is.na(Strength), Strength2, Strength))
NDC<-NDC %>%
  dplyr::select(-Strength2)
#delete replicate
NDC<-NDC[!duplicated(NDC[,c('PRODUCTNDC')]),]

#need to merge tripleMG's strength info back in
NDC<-merge(NDC, tripleMG, all=TRUE)
NDC<-NDC%>%mutate(Strength = ifelse(is.na(Strength), Strength2, Strength))
NDC<-NDC %>%
  dplyr::select(-Strength2)
#delete replicate
NDC<-NDC[!duplicated(NDC[,c('PRODUCTNDC')]),]

#need to merge quadMG's strength info back in
NDC<-merge(NDC, quadMG, all=TRUE)
NDC<-NDC%>%mutate(Strength = ifelse(is.na(Strength), Strength2, Strength))
NDC<-NDC %>%
  dplyr::select(-Strength2)
#delete replicate
NDC<-NDC[!duplicated(NDC[,c('PRODUCTNDC')]),]
#modify some with different order
NDC$Strength[NDC$PRODUCTNDC=="0603-2553"] <- "325MG;50MG;40MG;30MG"
NDC$Strength[NDC$PRODUCTNDC=="0722-6094"] <- "325MG;50MG;40MG;30MG"
NDC$Strength[NDC$PRODUCTNDC=="51991-074"] <- "325MG;50MG;40MG;30MG"
NDC$Strength[NDC$PRODUCTNDC=="53489-281"] <- "1MG"
NDC$Strength[NDC$PRODUCTNDC=="63629-2952"] <- "325MG;50MG;40MG;30MG"
NDC$Strength[NDC$PRODUCTNDC=="68151-2780"] <- "1MG"
NDC$Strength[NDC$PRODUCTNDC=="71335-0653"] <- "325MG;50MG;40MG;30MG"

NDC$Strength[NDC$PRODUCTNDC=="0555-9025"] <- "0.02MG;1MG"
NDC$Strength[NDC$PRODUCTNDC=="0555-9027"] <- "0.03MG;1.5MG"

NDC$Strength[NDC$PRODUCTNDC=="51285-127"] <- "0.03MG;1.5MG"
NDC$Strength[NDC$PRODUCTNDC=="51285-131"] <- "0.02MG;1MG"

NDC$Strength[NDC$PRODUCTNDC=="0113-0057"] <- "40MG/ML"
NDC$Strength[NDC$Appl_No=="075547"] <- "15MG"
NDC$Strength[NDC$PRODUCTNDC=="37012-035"] <- "2%"

NDC$Strength[NDC$PRODUCTNDC=="0338-0341"] <- "17.6MG/100ML;325.3MG/100ML;119.3MG/100ML;643MG/100ML"
NDC$Strength[NDC$PRODUCTNDC=="0037-6822"] <- "1%;1%"
NDC$Strength[NDC$PRODUCTNDC=="0037-6824"] <- "1%;1%"
NDC$Strength[NDC$PRODUCTNDC=="68220-142"] <- "1%;1%"

#mg/10ml
NDC$Strength[NDC$PRODUCTNDC=="0409-6509"] <- "5GM"
NDC$Strength[NDC$PRODUCTNDC=="51655-187"] <- "800MG;160MG"

#multiple mg/15ml
NDC$Strength[NDC$Appl_No=="040482"] <- "325MG/15ML;7.5MG/15ML"
NDC$Strength[NDC$Appl_No=="040387"] <- "325MG/15ML;50MG/15ML;40MG/15ML"

NDC$Strength[NDC$Appl_No=="075693"] <- "0.2%"
NDC$Strength[NDC$Appl_No=="075562"] <- "0.2%"

#"MG/2ML"
NDC$Strength[NDC$Appl_No=="076349"] <- "50MG"
#"MG/3ML"
NDC$Strength[NDC$Appl_No=="074880"] <- "0.083%"
NDC$Strength[NDC$PRODUCTNDC=="68094-756"] <- "10MG/ML"

#Multi "MG/5ML"
NDC<-merge(NDC, multiMG5ML, all=TRUE)
NDC<-NDC%>%mutate(Strength = ifelse(is.na(Strength), Strength2, Strength))
NDC<-NDC %>%
  dplyr::select(-Strength2)
#delete replicate
NDC<-NDC[!duplicated(NDC[,c('PRODUCTNDC')]),]


NDC$Strength[NDC$Appl_No=="050168"] <- "400 UNITS/GM;1%;3.5MG/GM;5,000 UNITS/GM"
NDC$Strength[NDC$Appl_No=="050065"] <- "0.1%;3.5MG/GM;10,000 UNITS/GM"
NDC$Strength[NDC$PRODUCTNDC=="61570-032"] <- "0.5%;3.5MG/GM;10,000 UNITS/GM"
NDC$Strength[NDC$Appl_No=="065088"] <- "400 UNITS/GM;3.5MG/GM;10,000 UNITS/GM"
NDC$Strength[NDC$Appl_No=="060764"] <- "400 UNITS/GM;3.5MG/GM;10,000 UNITS/GM"
NDC$Strength[NDC$PROPRIETARYNAME=="Neomycin and Polymyxin B Sulfates and Bacitracin Zinc"] <- "400 UNITS/GM;3.5MG/GM;10,000 UNITS/GM"
NDC$Strength[NDC$PROPRIETARYNAME=="Neomycin and Polymyxin B Sulfates, Bacitracin Zinc and Hydrocortisone"] <- "400 UNITS/GM;1%;3.5MG/GM;10,000 UNITS/GM"
NDC$Strength[NDC$Appl_No=="064063"] <- "0.1%;3.5MG BASE/GM;10,000 UNITS/GM"
NDC$Strength[NDC$Appl_No=="0050065"] <- "0.1%;3.5MG BASE/GM;10,000 UNITS/GM"

#mg/g; mg/g
NDC$Strength[NDC$PRODUCTNDC=="0023-0066"] <- "0.3%;0.6%"
NDC$Strength[NDC$PRODUCTNDC=="0023-0313"] <- "0.2%;10%"
NDC$Strength[NDC$PRODUCTNDC=="0065-0648"] <- "0.1%;0.3%"
NDC$Strength[NDC$PRODUCTNDC=="0085-0924"] <- "0.05%;1%"
NDC$Strength[NDC$PRODUCTNDC=="0115-1468"] <- "2.5%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="0145-2371"] <- "5%;1.2%"
NDC$Strength[NDC$PRODUCTNDC=="0168-0258"] <- "0.05%;1%"
NDC$Strength[NDC$PRODUCTNDC=="0168-0357"] <- "2.5%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="0187-3050"] <- "3.75%;1.2%"
NDC$Strength[NDC$PRODUCTNDC=="0187-5104"] <- "5%;1%"
NDC$Strength[NDC$PRODUCTNDC=="0187-5190"] <- "5%;1%"
NDC$Strength[NDC$PRODUCTNDC=="0299-5906"] <- "0.3%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="0299-5908"] <- "0.1%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="0374-5060"] <- "5%;3%"
NDC$Strength[NDC$PRODUCTNDC=="0472-0310"] <- "0.1%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="0472-0379"] <- "0.05%;1%"
NDC$Strength[NDC$PRODUCTNDC=="0472-1790"] <- "1.2%;0.025%"
NDC$Strength[NDC$PRODUCTNDC=="0591-2070"] <- "2.5%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="0781-7161"] <- "5%;1.2%"
NDC$Strength[NDC$PRODUCTNDC=="13548-132"] <- "2.5%;1.2%"
NDC$Strength[NDC$PRODUCTNDC=="16110-071"] <- "1.2%;0.025%"
NDC$Strength[NDC$PRODUCTNDC=="16714-496"] <- "0.05%;1%"
NDC$Strength[NDC$PRODUCTNDC=="33261-591"] <- "2.5%;2.5%"
NDC$Strength[NDC$Appl_No=="090979"] <- "5%;1.2%"
NDC$Strength[NDC$PRODUCTNDC=="50090-1558"] <- "0.05%;1%"
NDC$Strength[NDC$PRODUCTNDC=="50090-3121"] <- "0.05%;1%"
NDC$Strength[NDC$PRODUCTNDC=="50383-667"] <- "2.5%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="50436-0898"] <- "0.05%;1%"
NDC$Strength[NDC$PRODUCTNDC=="50436-4546"] <- "0.05%;1%"
NDC$Strength[NDC$PRODUCTNDC=="51672-1364"] <- "0.1%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="51672-1366"] <- "5%;1.2%"
NDC$Strength[NDC$PRODUCTNDC=="51672-4048"] <- "0.05%;1%"
NDC$Strength[NDC$PRODUCTNDC=="51672-5305"] <- "7%;7%"
NDC$Strength[NDC$PRODUCTNDC=="54766-716"] <- "1%;1%"
NDC$Strength[NDC$Appl_No=="085368"] <- "1%;1%"
NDC$Strength[NDC$PRODUCTNDC=="55700-061"] <- "0.05%;1%"
NDC$Strength[NDC$Appl_No=="075502"] <- "0.05%;1%"
NDC$Strength[NDC$Appl_No=="076290"] <- "2.5%;2.5%"
NDC$Strength[NDC$Appl_No=="202894"] <- "0.05%;1%"
NDC$Strength[NDC$Appl_No=="076002"] <- "0.05%;1%"
NDC$Strength[NDC$Appl_No=="019941"] <- "2.5%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="64980-328"] <- "5%;3%"
NDC$Strength[NDC$Appl_No=="021451"] <- "2.5%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="66993-869"] <- "0.1%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="66993-949"] <- "5%;1.2%"
NDC$Strength[NDC$PRODUCTNDC=="68682-132"] <- "2.5%;1.2%"
NDC$Strength[NDC$Appl_No=="076453"] <- "2.5%;2.5%"
NDC$Strength[NDC$Appl_No=="076320"] <- "2.5%;2.5%"
NDC$Strength[NDC$PRODUCTNDC=="71085-009"] <- "100,000/GM;0.1%"
NDC$Strength[NDC$Appl_No=="021717"] <- "7%;7%"
NDC$Strength[NDC$PRODUCTNDC=="99207-300"] <- "1.2%;0.025%"
NDC$Strength[NDC$Appl_No=="062938"] <- "0.1%;3.5MG/GM;10,000 UNITS/GM"
NDC$Strength[NDC$Appl_No=="0299-5950"] <- "0.01%;4%;0.05%"
NDC$Strength[NDC$Appl_No=="40076-002"] <- "0.25%;81.35%;15%"


NDC$Strength[NDC$PRODUCTNDC=="64693-002"] <- "800PPM"
NDC$Strength[NDC$Appl_No=="091691"] <- "2%"
NDC$Strength[NDC$PRODUCTNDC=="64693-002"] <- "800PPM"
NDC$Strength[NDC$Appl_No=="060707"] <- "40MG/ML;200,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="062423"] <- "1%;3.5MG/ML;10,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="060582"] <- "0.025MG/ML;1.75MG/ML;10,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="062664"] <- "40MG/ML;200,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="065006"] <- "10,000 UNITS/ML;1MG/ML"
NDC$Strength[NDC$Appl_No=="065106"] <- "40MG/ML;200,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="065108"] <- "40MG/ML;200,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="050023"] <- "1%;3.5MG/ML;10,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="064053"] <- "1%;3.5MG/ML;10,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="064047"] <- "0.025MG/ML;1.75MG/ML;10,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="064135"] <- "0.1%;3.5MG/ML;10,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="062341"] <- "0.1%;3.5MG/ML;10,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="062874"] <- "1%;3.5MG/ML;10,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="062488"] <- "1%;3.5MG/ML;10,000 UNITS/ML"
NDC$Strength[NDC$Appl_No=="087388"] <- "66%;10%"

#need to merge MG/ML's strength info back in
NDC<-merge(NDC, multiMGML, all=TRUE)
NDC<-NDC%>%mutate(Strength = ifelse(is.na(Strength), Strength2, Strength))
NDC<-NDC %>%
  dplyr::select(-Strength2)
#delete replicate
NDC<-NDC[!duplicated(NDC[,c('PRODUCTNDC')]),]

#mg/ml;mg/ml
NDC$Strength[NDC$Appl_No=="087388"] <- "66%;10%"
NDC$Strength[NDC$Appl_No=="050586"] <- "0.3% BASE;1%"
NDC$Strength[NDC$Appl_No=="021398"] <- "0.2%;0.5% BASE"
NDC$Strength[NDC$PRODUCTNDC=="0049-0024"] <- "10GM BASE/VIAL;5GM BASE/VIAL"

#Stopped at mg/ml;mg/ml (not finished)

save(NDC, file = "NDC.Rdata")

#300 UNITS/ML (300 UNITS/ML)

#need to change some with %

NDC_noAG<-NDC[( NDC$MARKETINGCATEGORYNAME=="NDA" | NDC$MARKETINGCATEGORYNAME=="ANDA"), ]




