
##20120602
applicant_20120602 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20120602/applicat.csv")
listings_20120602 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20120602/listings.csv")
packages_20120602 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20120602/packages.csv")

applicant_20120602 <- applicant_20120602 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20120602 <- packages_20120602 %>%
  left_join(applicant_20120602, by = "listing_seq_no") %>%
  left_join(listings_20120602, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20111020
applicant_20111020 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20111020/applicat.csv")
listings_20111020 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20111020/listings.csv")
packages_20111020 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20111020/packages.csv")

applicant_20111020 <- applicant_20111020 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20111020 <- packages_20111020 %>%
  left_join(applicant_20111020, by = "listing_seq_no") %>%
  left_join(listings_20111020, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20110611
applicant_20110611 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20110611/applicat.csv")
listings_20110611 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20110611/listings.csv")
packages_20110611 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20110611/packages.csv")

applicant_20110611 <- applicant_20110611 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20110611 <- packages_20110611 %>%
  left_join(applicant_20110611, by = "listing_seq_no") %>%
  left_join(listings_20110611, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20110103
applicant_20110103 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20110103/applicat.csv")
listings_20110103 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20110103/listings.csv")
packages_20110103 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20110103/packages.csv")

applicant_20110103 <- applicant_20110103 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20110103 <- packages_20110103 %>%
  left_join(applicant_20110103, by = "listing_seq_no") %>%
  left_join(listings_20110103, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20100701
applicant_20100701 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100701/applicat.csv")
listings_20100701 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100701/listings.csv")
packages_20100701 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100701/packages.csv")

applicant_20100701 <- applicant_20100701 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20100701 <- packages_20100701 %>%
  left_join(applicant_20100701, by = "listing_seq_no") %>%
  left_join(listings_20100701, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20100520
applicant_20100520 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100520/applicat.csv")
listings_20100520 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100520/listings.csv")
packages_20100520 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100520/packages.csv")

applicant_20100520 <- applicant_20100520 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20100520 <- packages_20100520 %>%
  left_join(applicant_20100520, by = "listing_seq_no") %>%
  left_join(listings_20100520, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20100402
applicant_20100402 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100402/applicat.csv")
listings_20100402 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100402/listings.csv")
packages_20100402 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100402/packages.csv")

applicant_20100402 <- applicant_20100402 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20100402 <- packages_20100402 %>%
  left_join(applicant_20100402, by = "listing_seq_no") %>%
  left_join(listings_20100402, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20100301
applicant_20100301 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100301/applicat.csv")
listings_20100301 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100301/listings.csv")
packages_20100301 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20100301/packages.csv")

applicant_20100301 <- applicant_20100301 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20100301 <- packages_20100301 %>%
  left_join(applicant_20100301, by = "listing_seq_no") %>%
  left_join(listings_20100301, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20081203
applicant_20081203 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20081203/applicat.csv")
listings_20081203 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20081203/listings.csv")
packages_20081203 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20081203/packages.csv")

applicant_20081203 <- applicant_20081203 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20081203 <- packages_20081203 %>%
  left_join(applicant_20081203, by = "listing_seq_no") %>%
  left_join(listings_20081203, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20080902
applicant_20080902 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20080902/applicat.csv")
listings_20080902 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20080902/listings.csv")
packages_20080902 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20080902/packages.csv")

applicant_20080902 <- applicant_20080902 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20080902 <- packages_20080902 %>%
  left_join(applicant_20080902, by = "listing_seq_no") %>%
  left_join(listings_20080902, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20070706
applicant_20070706 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20070706/applicat.csv")
listings_20070706 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20070706/listings.csv")
packages_20070706 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20070706/packages.csv")

applicant_20070706 <- applicant_20070706 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20070706 <- packages_20070706 %>%
  left_join(applicant_20070706, by = "listing_seq_no") %>%
  left_join(listings_20070706, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20070108
applicant_20070108 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20070108/applicat.csv")
listings_20070108 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20070108/listings.csv")
packages_20070108 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20070108/packages.csv")

applicant_20070108 <- applicant_20070108 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20070108 <- packages_20070108 %>%
  left_join(applicant_20070108, by = "listing_seq_no") %>%
  left_join(listings_20070108, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

##20060608
applicant_20060608 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20060608/applicat.csv")
listings_20060608 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20060608/listings.csv")
packages_20060608 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20060608/packages.csv")

applicant_20060608 <- applicant_20060608 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20060608 <- packages_20060608 %>%
  left_join(applicant_20060608, by = "listing_seq_no") %>%
  left_join(listings_20060608, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

test <- packages_20060608 %>%
  left_join(applicant_20060608, by = "listing_seq_no")

##20050630
applicant_20050630 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20050630/applicat.csv")
listings_20050630 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20050630/listings.csv")
packages_20050630 <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/20050630/packages.csv")

applicant_20050630 <- applicant_20050630 %>%
  filter(appl_no != "Other",
         appl_no != 0,
         appl_no != "BLA",
         prod_no != "000",
         prod_no != "UNK")

NDC_20050630 <- packages_20050630 %>%
  left_join(applicant_20050630, by = "listing_seq_no") %>%
  left_join(listings_20050630, by = "listing_seq_no") %>%
  dplyr::select(appl_no, prod_no, lblcode, prodcode, pkgcode) %>%
  distinct()

## Append all datasets
NDC_historical <- NDC_20070108 %>%
  rbind(NDC_20070706) %>%
  rbind(NDC_20080902) %>%
  rbind(NDC_20081203) %>%
  rbind(NDC_20100301) %>%
  rbind(NDC_20100402) %>%
  rbind(NDC_20100520) %>%
  rbind(NDC_20100701) %>%
  rbind(NDC_20110103) %>%
  rbind(NDC_20110611) %>%
  rbind(NDC_20111020) %>%
  rbind(NDC_20120602) %>%
  distinct()

NDC_historical_2 <- NDC_20050630 %>%
  rbind(NDC_20060608) 

## Cleaning
NDC_historical <- NDC_historical %>%
  filter(!is.na(appl_no),
         !is.na(prod_no),
         prod_no != "UN")

NDC_historical <- NDC_historical %>%
  rename(Appl_No = appl_no,
         Product_No = prod_no)

NDC_historical$Product_No = str_pad(NDC_historical$Product_No, 3, pad = "0")

NDC_historical$prodcode <- as.character(NDC_historical$prodcode)
NDC_historical$pkgcode <- as.character(NDC_historical$pkgcode)

NDC_historical$prodcode <- gsub("*", "0", NDC_historical$prodcode, fixed = T)
NDC_historical$pkgcode <- gsub("*", "0", NDC_historical$pkgcode, fixed = T)

NDC_historical$lblcode = str_pad(NDC_historical$lblcode, 5, pad = "0")

NDC_historical$Appl_No = str_pad(NDC_historical$Appl_No, 6, pad = "0")

NDC_historical <- NDC_historical %>%
  mutate(NDC9 = paste(lblcode, prodcode, sep=""),
         NDC11 = paste(NDC9, pkgcode, sep="")) %>%
  dplyr::select(-lblcode, -prodcode, -pkgcode)

NDC_historical_2$lblcode <- as.character(NDC_historical_2$lblcode)
NDC_historical_2$prodcode <- as.character(NDC_historical_2$prodcode)
NDC_historical_2$pkgcode <- as.character(NDC_historical_2$pkgcode)

NDC_historical_2$lblcode <- gsub(" ", "", NDC_historical_2$lblcode, fixed = T)
#NDC_historical_2$lblcode <- gsub(" ", "", NDC_historical_2$lblcode, fixed = T)
NDC_historical_2$prodcode <- gsub(" ", "", NDC_historical_2$prodcode, fixed = T)

NDC_historical_2$lblcode = str_pad(NDC_historical_2$lblcode, 5, pad = "0", side = "right")

NDC_historical_2$pkgcode = str_pad(NDC_historical_2$pkgcode, 2, pad = "0")

NDC_historical_2 <- NDC_historical_2 %>%
  mutate(NDC9 = paste(lblcode, prodcode, sep=""),
         NDC11 = paste(NDC9, pkgcode, sep="")) %>%
  dplyr::select(-lblcode, -prodcode, -pkgcode) %>%
  rename(Appl_No = appl_no,
         Product_No = prod_no)

NDC_historical <- NDC_historical %>%
  rbind(NDC_historical_2)

save(NDC_historical, file = "NDC_historical.Rdata")

## Add NDC crosswalk
#NDC_crosswalk <- read.csv("~/Dropbox/Advanced Method Project/Data/NDC_historical/ndc_crosswalk.csv")

#NDC_historical <- NDC_historical %>%
#  left_join(NDC_crosswalk, by = c("lblcode", "prodcode"))


