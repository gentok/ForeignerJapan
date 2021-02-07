#' ---
#' title: "SIFCCT Matching (All)"
#' author: "Fan Lu & Gento Kato"
#' date: "January 26, 2021"
#' ---
#' 
#' # Preparation 
#' 

## Clean Up Space
rm(list=ls())

## Set Working Directory (Automatically) ##
require(rstudioapi); require(rprojroot)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)

## Import Packages
library(designmatch) # Need GLPK Optimizer Installed. 
library(gurobi) # Need Gurobi Optimizer Installed.
library(fields)
require(ebal)
require(Matching)

## Matching Function
source(paste0(projdir,"/src/findmatch.R"))

#'
#' # Data Preparation
#'

## Import Data
d <- readRDS(paste0(projdir, "/data/sifcct_zip_latest_v5.rds"))
nrow(d)
dp <- readRDS(paste0(projdir, "/data/sifcct_zip_latest_panel_v5.rds"))
nrow(dp)

# Exclude Waves With No Foreigner Suffrage Question
d <- subset(d, !wave%in%c(1,23,24))
nrow(d)

## Knowledge Variable (Replaced)
dp$knowledge[dp$wave==2] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==2],dp$panelid[dp$wave==1])]
dp$knowledge[dp$wave==3] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==3],dp$panelid[dp$wave==1])]
dp$knowledge[dp$wave==4] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==4],dp$panelid[dp$wave==1])]
dp$knowledge[dp$wave==5] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==5],dp$panelid[dp$wave==1])]
dp$knowledge[dp$wave==6] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==6],dp$panelid[dp$wave==1])]
dp$knowledge[dp$wave==7] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==7],dp$panelid[dp$wave==1])]
dp$knowledge[dp$wave==8] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==8],dp$panelid[dp$wave==1])]
dp$knowledge[dp$wave==9] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==9],dp$panelid[dp$wave==1])]
dp$knowledge[dp$wave==10] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==10],dp$panelid[dp$wave==1])]
dp$knowledge[dp$wave==11] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==11],dp$panelid[dp$wave==1])]
dp$knowledge[dp$wave==12] <- dp$knowledge[dp$wave==1][match(dp$panelid[dp$wave==12],dp$panelid[dp$wave==1])]
## Knowledge Variable (Replaced)
dp$knowledge[dp$wave==14] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==14],dp$panelid[dp$wave==13])]
dp$knowledge[dp$wave==15] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==15],dp$panelid[dp$wave==13])]
dp$knowledge[dp$wave==16] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==16],dp$panelid[dp$wave==13])]
dp$knowledge[dp$wave==17] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==17],dp$panelid[dp$wave==13])]
dp$knowledge[dp$wave==18] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==18],dp$panelid[dp$wave==13])]
dp$knowledge[dp$wave==19] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==19],dp$panelid[dp$wave==13])]
dp$knowledge[dp$wave==20] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==20],dp$panelid[dp$wave==13])]
dp$knowledge[dp$wave==21] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==21],dp$panelid[dp$wave==13])]
dp$knowledge[dp$wave==22] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==22],dp$panelid[dp$wave==13])]
dp$knowledge[dp$wave==23] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==23],dp$panelid[dp$wave==13])]
dp$knowledge[dp$wave==24] <- dp$knowledge[dp$wave==13][match(dp$panelid[dp$wave==24],dp$panelid[dp$wave==13])]
dp <- subset(dp, wave%in%c(2,13))

## Combine data
d <- rbind(d, dp)
nrow(d)

## Subset Data by those who lives in the area at least since high school
d <- subset(d, age - lvlen >= 23)

## Treat Population 0 zips as NA
d$zip[which(d$c10_sreg_pop==0)] <- NA

# Exclude Missing Values
ds <- d[,c("id","foreignsuff","foreignsuff3","foreignsuff3x",
           "knowledge","polint","ideology","ldpdpjft",
           "familiarityFT_KOR","familiarityFT_CHN","familiarityFT_USA",
           # "evecon","evecon_verybad","evecon_bad","evecon_notbad","evecon_qtype",
           "income", # "employed",
           "female","male","edu","edu2","age","agecat","bornyr",
           "lvlen","lvpr",
           "zip_did","c10_sreg_foreignN","c10_sreg_pop",
           "c10_sreg_edu_ugsP","c10_sreg_edu_ugs","c10_sreg_edu_graduated",
           "didper","c10_mun_foreignN","c10_mun_pop",
           "c10_mun_edu_ugsP","c10_mun_edu_ugs","c10_mun_edu_graduated",
           "zip","c10_name_pref","c10_name_mun","c10_name_sreg",
           "zip_lat","zip_lon",
           "wave","panel")]
ds <- na.omit(ds)
nrow(ds)

## Binary Age Cohort (50s or over)
ds$age2 <- ifelse(ds$age >= 50, 1, 0)

## Small Region Foreigner Percent
sort(ds$c10_sreg_pop)
ds$c10_sreg_fper <- ds$c10_sreg_foreignN/ds$c10_sreg_pop*100

## Municipality Foreigner Percent
ds$c10_mun_fper <- ds$c10_mun_foreignN/ds$c10_mun_pop*100

## Check Foreigner Percent
plot(d$fper, d$c10_mun_foreignN/d$c10_mun_pop)
cor(d$fper, d$c10_mun_foreignN/d$c10_mun_pop, use="pairwise")
plot(ds$c10_mun_fper, ds$c10_sreg_fper)
cor(ds$c10_mun_fper, ds$c10_sreg_fper, use="pairwise")

# Education
table(ds$edu, ds$female)
table(ds$edu2,ds$female)

## After Senkaku/Takeshima Dummy
ds$after <- ifelse(ds$wave>=11,1,0)
table(ds$edu, ds$after)

## Age Category as Numeric Variable
ds$agecatn <- as.numeric(ds$agecat)
ds$young <- (ds$agecatn==1)*1
ds$mid <- (ds$agecatn==2)*1
ds$old <- (ds$agecatn==3)*1

## 
table(ds$female)
table(ds$edu2, ds$female)
table(ds$edu2, ds$female, ds$after, ds$zip_did)

ds$femalebefore <- ifelse(ds$female==1 & ds$after==0, 1, 0)
ds$femaleafter  <- ifelse(ds$female==1 & ds$after==1, 1, 0)
ds$malebefore <- ifelse(ds$male==1 & ds$after==0, 1, 0)
ds$maleafter  <- ifelse(ds$male==1 & ds$after==1, 1, 0)
table(ds$femalebefore)
table(ds$femaleafter)
table(ds$malebefore)
table(ds$maleafter)

ds$femalebeforedid0 <- ifelse(ds$female==1 & ds$after==0 & ds$zip_did==0, 1, 0)
ds$femalebeforedid1 <- ifelse(ds$female==1 & ds$after==0 & ds$zip_did==1, 1, 0)
ds$femaleafterdid0  <- ifelse(ds$female==1 & ds$after==1 & ds$zip_did==0, 1, 0)
ds$femaleafterdid1  <- ifelse(ds$female==1 & ds$after==1 & ds$zip_did==1, 1, 0)
ds$malebeforedid0 <- ifelse(ds$male==1 & ds$after==0 & ds$zip_did==0, 1, 0)
ds$malebeforedid1 <- ifelse(ds$male==1 & ds$after==0 & ds$zip_did==1, 1, 0)
ds$maleafterdid0  <- ifelse(ds$male==1 & ds$after==1 & ds$zip_did==0, 1, 0)
ds$maleafterdid1  <- ifelse(ds$male==1 & ds$after==1 & ds$zip_did==1, 1, 0)
table(ds$femalebeforedid0)
table(ds$femalebeforedid1)
table(ds$femaleafterdid0)
table(ds$femaleafterdid1)
table(ds$malebeforedid0)
table(ds$malebeforedid1)
table(ds$maleafterdid0)
table(ds$maleafterdid1)

table(ds$wave,ds$after)
ds$femaleafterdid1a  <- ifelse(ds$female==1 & ds$after==1 & ds$zip_did==1 & ds$wave <= 16, 1, 0)
ds$femaleafterdid1b  <- ifelse(ds$female==1 & ds$after==1 & ds$zip_did==1 & ds$wave > 16, 1, 0)
ds$maleafterdid1a  <- ifelse(ds$male==1 & ds$after==1 & ds$zip_did==1 & ds$wave <= 16, 1, 0)
ds$maleafterdid1b  <- ifelse(ds$male==1 & ds$after==1 & ds$zip_did==1 & ds$wave > 16, 1, 0)
table(ds$femaleafterdid1a)
table(ds$femaleafterdid1b)
table(ds$maleafterdid1a)
table(ds$maleafterdid1b)

## Standardized Covariates

ds$stdage <- (ds$age - mean(ds$age))/sd(ds$age)
hist(ds$stdage)
ds$stdbornyr <- (ds$bornyr - mean(ds$bornyr))/sd(ds$bornyr)
hist(ds$stdbornyr)

ds$stdlvlen <- (ds$lvlen - mean(ds$lvlen))/sd(ds$lvlen)
hist(ds$stdlvlen)
ds$stdlvpr <- (ds$lvpr - mean(ds$lvpr))/sd(ds$lvpr)
hist(ds$stdlvpr)

ds$stdfper <- (ds$c10_sreg_fper - mean(ds$c10_sreg_fper))/sd(ds$c10_sreg_fper)
table(is.na(ds$stdfper))
ds$stdfpersq <- (sqrt(ds$c10_sreg_fper) - mean(sqrt(ds$c10_sreg_fper)))/sd(sqrt(ds$c10_sreg_fper))
hist(ds$stdfpersq)
ds$stdfpop <- (ds$c10_sreg_foreignN - mean(ds$c10_sreg_foreignN))/sd(ds$c10_sreg_foreignN)
hist(ds$stdfpop)
ds$stdpop <- (ds$c10_sreg_pop - mean(ds$c10_sreg_pop))/sd(ds$c10_sreg_pop)
hist(ds$stdpop)
ds$stduper <- (ds$c10_sreg_edu_ugsP - mean(ds$c10_sreg_edu_ugsP))/sd(ds$c10_sreg_edu_ugsP)
hist(ds$stdfper)
ds$stdupop <- (ds$c10_sreg_edu_ugs - mean(ds$c10_sreg_edu_ugs))/sd(ds$c10_sreg_edu_ugs)
hist(ds$stdupop)
ds$stdgpop <- (ds$c10_sreg_edu_graduated - mean(ds$c10_sreg_edu_graduated))/sd(ds$c10_sreg_edu_graduated)
hist(ds$stdgpop)

ds$stdfper2 <- (ds$c10_mun_fper - mean(ds$c10_mun_fper))/sd(ds$c10_mun_fper)
hist(ds$stdfper)
ds$stdfpersq2 <- (sqrt(ds$c10_mun_fper) - mean(sqrt(ds$c10_mun_fper)))/sd(sqrt(ds$c10_mun_fper))
hist(ds$stdfpersq)
ds$stdfpop2 <- (ds$c10_mun_foreignN - mean(ds$c10_mun_foreignN))/sd(ds$c10_mun_foreignN)
hist(ds$stdfpop)
ds$stdpop2 <- (ds$c10_mun_pop - mean(ds$c10_mun_pop))/sd(ds$c10_mun_pop)
hist(ds$stdpop)
ds$stduper2 <- (ds$c10_mun_edu_ugsP - mean(ds$c10_mun_edu_ugsP))/sd(ds$c10_mun_edu_ugsP)
hist(ds$stduper)
ds$stdupop2 <- (ds$c10_mun_edu_ugs - mean(ds$c10_mun_edu_ugs))/sd(ds$c10_mun_edu_ugs)
hist(ds$stdupop)
ds$stdgpop2 <- (ds$c10_mun_edu_graduated - mean(ds$c10_mun_edu_graduated))/sd(ds$c10_mun_edu_graduated)
hist(ds$stdgpop)

ds$stddidper <- (ds$didper - mean(ds$didper))/sd(ds$didper)
hist(ds$stddidper)

ds$stdwave <- (ds$wave - mean(ds$wave))/sd(ds$wave)
hist(ds$stdwave)

#+ eval=FALSE
## Save Data
saveRDS(ds, paste0(projdir, "/data/sifcct_moved_unmatched_v5.rds"))

rm(d,dp)

##############################################
# Checking Balance of Data 
##############################################

fmbal = formula(edu2 ~ female + age + bornyr + lvlen + lvpr + 
                  c10_sreg_fper + I(sqrt(c10_sreg_fper)) + c10_sreg_foreignN + c10_sreg_pop + 
                  c10_sreg_edu_ugsP + c10_sreg_edu_ugs + c10_sreg_edu_graduated + 
                  c10_mun_fper + I(sqrt(c10_mun_fper)) + c10_mun_foreignN + c10_mun_pop + 
                  c10_mun_edu_ugsP + c10_mun_edu_ugs + c10_mun_edu_graduated + 
                  zip_did + didper + wave + after + panel)
vnbal = c("Gender","Age","Born Year","Livng Length","Living Proportion",
          "Foreigner Percentage (zip)", "Foreigner Percentage sqrt. (zip)", 
          "Foreigner Population (zip)", "Population (zip)",
          "University Percentage (zip)",  
          "University Population (zip)", "Graduated Population (zip)",
          "Foreigner Percentage (mun.)", "Foreigner Percentage sqrt. (mun.)", 
          "Foreigner Population (mun.)", "Population (mun.)",
          "University Percentage (mun.)",  
          "University Population (mun.)", "Graduated Population (mun.)",
          "DID Residence","DID Proportion","Wave","Aug. 2012 or After","Panel")

## Balance
bal_ds_unmatched <- findbalance(ds, fmbal, vnbal)
round(bal_ds_unmatched,3)[,1:7]

#+ eval=FALSE
## Save Balance Data
saveRDS(bal_ds_unmatched, 
        file = paste0(projdir,"/data/sifcct_moved_unmatched_balance_v5.rds"))

gc()
Sys.sleep(1)

#+

setcovs = c("stdage","stdlvpr","stdlvlen","stdwave")
settols = c(0.01, 0.01, 0.01, 0.01)
exactcovs = c("panel")
nearexactcovs = c("age")
nearexactdevs = c(3)

#+ eval=FALSE
##############################################
# Match 1
# Optimal subset matching: yes
# Penalties on the distance matrix: no, *distance matrix based on covariates*
##############################################

## Female 
ds_matched_1fb <- findmatch(ds, subset_var = "femalebefore", subset_val = 1,
                           treated_var = "edu2", treated_val = 1, 
                           dist_covs = setcovs, 
                           mom_covs = setcovs,
                           mom_tols = settols,
                           exact_covs = exactcovs, near_exact_covs =  nearexactcovs, near_exact_devs =  nearexactdevs)
saveRDS(ds_matched_1fb, file = paste0(projdir,"/data/sifcct_moved_matched_1fb_v5.rds"))

gc()
Sys.sleep(1)

ds_matched_1fa <- findmatch(ds, subset_var = "femaleafter", subset_val = 1,
                            treated_var = "edu2", treated_val = 1, 
                            dist_covs = setcovs, 
                            mom_covs = setcovs,
                            mom_tols = settols,
                            exact_covs = exactcovs, near_exact_covs =  nearexactcovs, near_exact_devs =  nearexactdevs)
saveRDS(ds_matched_1fa, file = paste0(projdir,"/data/sifcct_moved_matched_1fa_v5.rds"))

gc()
Sys.sleep(1)

## Male
ds_matched_1mb <- findmatch(ds, subset_var = "malebefore", subset_val = 1,
                             treated_var = "edu2", treated_val = 0, 
                             dist_covs = setcovs, 
                             mom_covs = setcovs,
                             mom_tols = settols,
                             exact_covs = exactcovs, near_exact_covs =  nearexactcovs, near_exact_devs =  nearexactdevs)
saveRDS(ds_matched_1mb, file = paste0(projdir,"/data/sifcct_moved_matched_1mb_v5.rds"))

gc()
Sys.sleep(1)

ds_matched_1ma <- findmatch(ds, subset_var = "maleafter", subset_val = 1,
                             treated_var = "edu2", treated_val = 0, 
                             dist_covs = setcovs, 
                             mom_covs = setcovs,
                             mom_tols = settols,
                             exact_covs = exactcovs, near_exact_covs =  nearexactcovs, near_exact_devs =  nearexactdevs)
saveRDS(ds_matched_1ma, file = paste0(projdir,"/data/sifcct_moved_matched_1ma_v5.rds"))

gc()
Sys.sleep(1)

#+
ds_matched_1 <- rbind(readRDS(paste0(projdir,"/data/sifcct_moved_matched_1fb_v5.rds")),
                      readRDS(paste0(projdir,"/data/sifcct_moved_matched_1fa_v5.rds")),
                      readRDS(paste0(projdir,"/data/sifcct_moved_matched_1mb_v5.rds")),
                      readRDS(paste0(projdir,"/data/sifcct_moved_matched_1ma_v5.rds")))
saveRDS(ds_matched_1, file = paste0(projdir,"/data/sifcct_moved_matched_1_all_v5.rds"))

table(ds$edu2)
table(ds_matched_1$edu2)
length(unique(ds_matched_1$id))
# N of Matched Found (Female)
paste(table(ds_matched_1[ds_matched_1$female==1,]$edu2)[2],
      table(ds[ds$female==1,]$edu2)[2],sep="/") 
# N of Matched Found (Male)
paste(table(ds_matched_1[ds_matched_1$female==0,]$edu2)[1],
      table(ds[ds$female==0,]$edu2)[1],sep="/") # N of Matched Found

## Balance
balf_ds_matched_1 <- findbalance(ds_matched_1[ds_matched_1$female==1,], fmbal, vnbal)
round(balf_ds_matched_1,3)[,1:7]
### Male
balm_ds_matched_1 <- findbalance(ds_matched_1[ds_matched_1$female==0,], fmbal, vnbal)
round(balm_ds_matched_1,3)[,1:7]

#+ eval=FALSE
## Save Balance Data
saveRDS(list(f = balf_ds_matched_1, m = balm_ds_matched_1), 
        file = paste0(projdir,"/data/sifcct_moved_matched_1_balance_v5.rds"))
rm(ds_matched_1)

gc()
Sys.sleep(1)

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/data_sifcct_3_matching_moved_v5.R', 'pdf_document', encoding = 'UTF-8')
# rmarkdown::render('./src/data_sifcct_3_matching_moved_v5.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$|\\.log$|\\.tex$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
