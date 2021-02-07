#' ---
#' title: "SIFCCT Recoding"
#' author: "Fan Lu & Gento Kato"
#' date: "Dec 18, 2020"
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

## Find Data Directory
datadir1 <- paste(projdir,"data/original/sifcct_mail.csv",sep="/")

## Import Original Data
library(readr)
do <- read_csv(datadir1, locale=locale(encoding="CP932"))
colnames(do)[colnames(do)=="回答郵便番号"] <- "zip"
colnames(do)[colnames(do)=="都道府県コード"] <- "prefcode"

## Library Psych Package
require(psych)

#'
#' # Data Manipulation
#'

# Initiate New Data Set
d <- data.frame(id = do$SampleNo, 
                wave = NA, 
                panel = 0,
                panelid = NA)

#'
#' ## DEPENDENT variables of (potential) interest
#' 
#' ### The local election suffrage should be granted to foreigners.
#' 
#' * Original: 1=Strongly agree 5=Strongly disagree 6=DK 7=NA
#' * Recoded: 0=Strongly disagree, 0.5=Neither/DK, 1=Strongly agree, Missing=NA

# Original Variable
tmp <- do$Q11_3 
table(tmp, useNA="always")
# Recoded Variable
d$foreignsuff <- ifelse(tmp==7, 2, ifelse(tmp==6, 2, 5 - as.numeric(tmp)))/4
table(d$foreignsuff, useNA="always")
d$foreignsuff3 <- ifelse(d$foreignsuff==0.5,1,ifelse(d$foreignsuff>0.5,3,2))
d$foreignsuff3 <- factor(d$foreignsuff3, labels=c("Neither","Disagree","Agree"))
table(d$foreignsuff3, useNA="always")
d$foreignsuff3x <- factor(d$foreignsuff3, levels=c("Disagree","Neither","Agree"))
table(d$foreignsuff3x, useNA="always")

#'
#' ### Increase in long-term resident foreigners (Not Asked)
#'

d$immigincrease <- NA
d$immigincrease3 <- NA
d$immigincrease3x <- NA

#'
#' ### Trustworthiness of Foreigners (Not Asked)
#' 
#' * Original: 1=Not trustworthy 7=trustworthy
#' * Recoded: 0-1 range, 1 is the most trustworthy
#' 

d$trust_old_sko <- d$trust_old_kor <- d$trust_old_chn <- 
  d$trust_new_sko <- d$trust_new_chn <- d$trust_new_bra <- 
  d$trust_new_phl <- d$trust_new_usa <- NA
d$trust_old <- d$trust_new <- NA

#'
#' ### Foreign friends/acquaintances in Japan. (Not Asked)
#' 
#' * Original: 1=1 or 2, 4=11 or more, 5=None, 6= Don't want to answer
#' * Recoded: 0=None, 1=Any Friend
#' * Recoded 2: 0=None, 1=1or2, 2=More
#' 

d$foreignfriend_jpn <- NA
d$foreignfriend_jpn2 <- NA

#'
#' ### Foreign friends/acquaintances outside of Japan. (Not Asked)
#' 
#' * Original: 1=1 or 2, 4=11 or more, 5=None, 6= Don't want to answer
#' * Recoded 1: 0=None, 1=Any Friend
#' * Recoded 2: 0=None, 1=1or2, 2=More
#' 

d$foreignfriend_out <- NA
d$foreignfriend_out2 <- NA

#'
#' ### Foreign relatives. (Not Asked)
#' 
#' * Original: 1=1 or 2, 4=11 or more, 5=None, 6= Don't want to answer
#' * Recoded: 0=None, 1=Any
#' * Recoded 2: 0=None, 1=1or2, 2=More
#' 

d$foreignfamily <- NA
d$foreignfamily2 <- NA

#'
#' ### Foreign Acquaintances (Not Asked)
#' 
#' * Recoded: 0=None, 1=Any
#' * Recoded 2: 0=None, 1=1or2 (for only one), 2=More
#'

d$foreignacqu <- NA
d$foreignacqu2 <- NA

#'
#' ### Familiarity with Foreign Countries
#'

tmp1 <- do$Q16_1
tmp1 <- ifelse(tmp1==999, 50, ifelse(tmp1==888, 50, tmp1))
barplot(table(tmp1, useNA="always"))
d$familiarityFT_USA <- tmp1/100

tmp2 <- do$Q16_2
tmp2 <- ifelse(tmp2==999, 50, ifelse(tmp2==888, 50, tmp2))
barplot(table(tmp2, useNA="always"))
d$familiarityFT_CHN <- tmp2/100

tmp3 <- do$Q16_3
tmp3 <- ifelse(tmp3==999, 50, ifelse(tmp3==888, 50, tmp3))
barplot(table(tmp3, useNA="always"))
d$familiarityFT_KOR <- tmp3/100

#'
#' ### Political Knowledge
#' 
#' * Recoded: Sum of correct answers from 6 factual questions 
#' (standardized in 0-1 range)
#'

# Original 
tmp1 <- do$Q19%in%4
table(tmp1, useNA="always")
tmp2 <- do$Q20%in%3
table(tmp2, useNA="always")
tmp3 <- do$Q21%in%2
table(tmp3, useNA="always")
tmp4 <- do$Q22%in%2
table(tmp4, useNA="always")
tmp5 <- do$Q23%in%3
table(tmp5, useNA="always")
tmp6 <- do$Q24%in%3
table(tmp6, useNA="always")
# Recoded
d$knowledge <- (tmp1 + tmp2 + tmp3 + tmp4 + tmp5 + tmp6)/6
table(d$knowledge, useNA="always")
# Cronbach's Alpha is 0.77
psych::alpha(cbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6))

#' 
#' ### Interest in politics. 
#' 
#' * Original: 1= interested 4= Not interested 5=DK 6= Don't want to answer
#' * Recoded: 0=Not interested to 1=Interested, Missing=DK/NA
#' 

# Original
tmp <- do$Q3
table(tmp, useNA="always") 
# Recoded
d$polint <- ifelse(tmp%in%c(5,6), 0, 4 - tmp)/3
table(d$polint, useNA="always")

#'
#' ### Interest in international issues facing Japan. 
#' 
#' * Original: 1= interested 4= Not interested 5=DK 6= Don't want to answer
#' * Recoded: 1=Not interested, 4=Interested, Missing=DK/NA
#' 

# Original
tmp <- do$Q4
table(tmp, useNA="always") 
# Recoded
d$intlint <- ifelse(tmp%in%c(5,6), 0, 4 - tmp)/3
table(d$intlint, useNA="always")

#'
#' ## PREDICTORS
#' 
#' ### Education (Ordinal)
#' 
#' * Original: 1= primary/junior-high school, 2=High School, 
#' 3=Junior College/Vocational School, 4= College/Grad School, 5= NA
#' * Recoded: 1= "<=SHS", 2="Junior College/Vocational School", 3=">=College" 
#' 

# Original
tmp <- do$F3
table(tmp, useNA="always")
# Recoded
d$edu <- ifelse(tmp==5, NA, ifelse(tmp==1, 1, tmp-1))
# Make it a Factor
d$edu <- factor(d$edu, labels = c("<=SHS",
                                  ">SHS & <College(4yr)",
                                  ">=College(4yr)"))
table(d$edu, useNA="always")

# Education Treatment 
d$edu2 <- ifelse(d$edu==">=College(4yr)",1,0)
d$edu2x <- d$edu2
d$edu2x[which(d$edu==">SHS & <College(4yr)")] <- NA
table(d$edu2, useNA="always")
table(d$edu2x, useNA="always")

#' 
#' ### Gender
#' 
#' * Original: 1=male 2=female 3=NA
#' * Recoded: 0=male, 1=female
#' 

# Original
tmp <- do$F1
table(tmp, useNA="always")
# Recoded
d$female <- ifelse(tmp==3, NA, tmp-1)
table(d$female, useNA="always")
d$male <- 1 - d$female

#'
#' ### Age
#'
#' * Original: 70=70 or over, 99=NA
#' * Recoded: NA into Missing
#' * Recoded (Categorical):

# Original
tmp <- do$F2
table(tmp, useNA="always")
# Recoded
d$age <- ifelse(tmp==99, NA, tmp)
table(d$age, useNA="always")

## Recoded Born Year (by Academic Year: April-March)
d$bornyr <- NA
d$bornyr <- 2012 - d$age

# Recoded Categorical
d$agecat <- NA
d$agecat[d$age >= 60] <- "Elder (>=60s)"
d$agecat[d$age >= 40 & d$age < 60] <- "Middle Aged (40-50s)"
d$agecat[d$age < 40] <- "Young (<=30s)"
## coerce new character variable into a factor variable
d$agecat <- factor(d$agecat, levels=c("Young (<=30s)",
                                      "Middle Aged (40-50s)",
                                      "Elder (>=60s)"))
table(d$agecat, useNA="always") 

#'
#' ### Marital Status
#'

tmp <- do$F5
table(tmp)

d$married <- ifelse(tmp==3,NA,ifelse(tmp==1,1,0))
table(d$married)

#'
#' ### Income
#'

# Original
tmp <- do$F4
table(tmp, useNA="always")
# Recoded
## Percentile Conversion Function
convper <- function(old.var,missing.val){
  r <- old.var
  r[r %in% missing.val] <- NA
  rt <- cumsum(table(r)/sum(table(r))) # Cumulative Percentile
  rt <- rt - diff(c(0,rt))/2 # Take Midpoints 
  r <- rt[match(r, names(rt))]
  return(r)
}
d$income <- convper(tmp, c(9,10))
table(d$income, useNA="always")

d$incomecat <- NA
d$incomecat[which(d$income<=0.33)] <- "Low"
d$incomecat[which(d$income>0.33 & d$income<=0.67)] <- "Middle"
d$incomecat[which(d$income>0.67)] <- "High"
d$incomecat[which(tmp%in%c(9,10))] <- "Missing"
d$incomecat <- factor(d$incomecat, levels=c("Low","Middle","High","Missing"))
table(d$incomecat, useNA="always") 

#'
#' ### Newspaper which is read the most (Not Asked)
#'

# Original
d$npmost <- NA 
d$npmost2 <- NA

#'
#' ### assessment of current life condition. 
#' 
#' Note: Question Wording is randomized among fresh respondents.
#' 
#' * Original: 1=good 5=bad, 6=DK, 7=NA
#' * Recoded: 0=bad, 0.5=Neither/DK, 1=good, NA=NA
#'

# Original: Combine All randomized responses
tmp <- do$Q7
table(tmp, useNA="always")
# Recoded
d$evlife <- ifelse(tmp==7, 2, ifelse(tmp==6, 2, 5 - tmp))/4
table(d$evlife, useNA="always")

d$evlife_verybad <- ifelse(d$evlife%in%0, 1, 0)
d$evlife_bad <- ifelse(d$evlife%in%0.25, 1, 0)
d$evlife_notbad <- ifelse(!d$evlife%in%c(0,0.25), 1, 0)

# Question Wording Type (just in case)
# 0 = assessment of current life condition
# 1 = assessment of the change in life condition from a month ago
d$evlife_qtype <- 0

#'
#' ### assessment of current Japanese economy. 
#' 
#' Note: Question Wording is randomized among fresh respondents.
#' 
#' * Original: 1=good 5=bad, 6=DK, 7=NA
#' * Recoded: 0=bad, 0.5=Neither/DK, 1=good, NA=NA
#'

# Original: Combine All randomized responses
tmp <- do$Q8
table(tmp, useNA="always")
# Recoded
d$evecon <- ifelse(tmp==7 | is.na(tmp), 2, ifelse(tmp==6, 2, 5 - tmp))/4
table(d$evecon, useNA="always")

d$evecon_verybad <- ifelse(d$evecon%in%0, 1, 0)
d$evecon_bad <- ifelse(d$evecon%in%0.25, 1, 0)
d$evecon_notbad <- ifelse(!d$evecon%in%c(0,0.25), 1, 0)

# Question Wording Type (just in case)
# 0 = assessment of current economy
# 1 = assessment of the change in economy from a month ago
d$evecon_qtype <- 0

#'
#' ### Internet Usage
#' 
#' Original: 1=less than 30min 7=about more than 5 hrs, 
#' 8=Not using Internet, 9=NA
#' Recoded: Standardized to 0-1 range. NA=NA

# Original
tmp <- do$F7
table(tmp, useNA="always")
# Recoded
d$netuse <- ifelse(tmp==9,NA,ifelse(tmp==8, 0, tmp))/7
table(d$netuse, useNA="always")

#'
#' ### party support 
#' 
#' Original (e.g., version "a"): 
#' 1=Democratic Party of Japan (DPJ), 
#' 2=Liberal Democratic Party (LDP),
#' 3=New Komeito (CGP), 
#' 4=Japanese Communist Party (JCP) 
#' 5= Social Democratic Party (SDP)
#' 6=Your Party (YP) 7=Other, 8=Don't support any 9=Don't want to answer 
#' 
#' Recoded (Categorical): 1=Mutoha(No Party), 2=DPJ, 3=LDP, 4=CGP(Komeito), 
#' 5=JCP, 6=SDP, 7=YP, 8=JRP (Japan Restoration Party), 
#' 9=Others, NA=NA
#'

# Original: Combine All Responses
tmp <- do$Q25 
table(tmp, useNA="always") # (Version "c")
# Original: Response Category Type
tmptype <- "f"
# Original: NA Locations
tmpNA <- rep(0, length(tmp))
tmpNA[which(tmptype=="f" & tmp==12)] <- 1
table(tmpNA, useNA="always")

# Recoded
## DPJ
d$dpj <- (tmp==1)*1
d$dpj[tmpNA==1] <- NA
table(d$dpj, useNA="always")
## LDP
d$ldp <- (tmp==2)*1
d$ldp[tmpNA==1] <- NA
table(d$ldp, useNA="always")
## CGP
d$cgp <- (tmp==3)*1
d$cgp[tmpNA==1] <- NA
table(d$cgp, useNA="always")
## JCP
d$jcp <- (tmp==4)*1
d$jcp[tmpNA==1] <- NA
table(d$jcp, useNA="always")
## SDP
d$sdp <- (tmp==5)*1
d$sdp[tmpNA==1] <- NA
table(d$sdp, useNA="always")
## YP
d$yp <- ifelse(tmptype%in%c("a","b","c","d"), (tmp==6)*1, (tmp==5)*1)
d$yp[tmpNA==1] <- NA
table(d$yp, useNA="always")
## JRP
d$jrp <- (tmp==9)*1
d$jrp[tmpNA==1] <- NA
table(d$jrp, useNA="always")
## PLP Seikatsu/Mirai (People's Life Party)
d$plp <- (tmp==8)*1
d$plp[tmpNA==1] <- NA
table(d$plp, useNA="always")
## other parties 
d$othp <- (tmp%in%c(6,10))*1 
d$othp[tmpNA==1] <- NA
table(d$othp, useNA="always")
## mutoha (No party support)
d$mutoha <- (tmp==11)*1 
d$mutoha[tmpNA==1] <- NA
table(d$mutoha, useNA="always")
## Categorical Party Variable
### Check that variables do not overlap
with(d, table(dpj+ldp+cgp+jcp+sdp+yp+jrp+plp+othp+mutoha, useNA="always"))
### Create variable
d$psup <- NA
d$psup[d$mutoha==1] <- "None"
d$psup[d$dpj==1] <- "DPJ"
d$psup[d$ldp==1] <- "LDP"
d$psup[d$cgp==1] <- "CGP(Komei)"
d$psup[d$jcp==1] <- "JCP"
d$psup[d$sdp==1] <- "SDP"
d$psup[d$yp==1] <- "YP"
d$psup[d$jrp==1] <- "JRP"
d$psup[d$plp==1] <- "Other"
d$psup[d$othp==1] <- "Other"
d$psup <- factor(d$psup,
                 levels=c("None","DPJ","LDP","CGP(Komei)",
                          "JCP","SDP","YP","JRP","Other"))
table(d$psup, useNA="always")

d$psup_original <- d$psup
d$psup <- ifelse(d$psup_original%in%c("DPJ","CGP(Komei)","JCP","SDP"),
                 "Left",ifelse(d$psup_original%in%c("LDP","YP","JRP"),
                               "Right",
                               ifelse(d$psup_original%in%c("None","Other"),"None/Other",NA)))
d$psup <- factor(d$psup,
                 levels=c("None/Other","Left","Right"))
table(d$psup, useNA="always")

d$left <- ifelse(d$psup%in%"Left",1,0)
d$right <- ifelse(d$psup%in%"Right",1,0)

#'
#' # LDP - DPJ Feeling Thermometer
#'

tmp <- do$Q6_2
table(tmp)
d$ldpft <- ifelse(tmp==999,0.5,ifelse(tmp==888,0.5,tmp/100))
summary(d$ldpft)

tmp <- do$Q6_1
table(tmp)
d$dpjft <- ifelse(tmp==999,0.5,ifelse(tmp==888,0.5,tmp/100))
summary(d$dpjft)

d$ldpdpjft = (d$ldpft - d$dpjft + 1)/2
summary(d$ldpdpjft)
hist(d$ldpdpjft)

#'
#' # Ideology
#'

tmp <- do$Q18
table(tmp)
d$ideology <- ifelse(tmp==11,0.5,ifelse(tmp==12,0.5,tmp/10))
table(d$ideology, useNA="always")

#'
#' # Stayed in Foreign Country
#'

tmp <- do$F9a
table(tmp)
d$stayforeign <- ifelse(tmp==2, 1, ifelse(tmp==1, 0, NA))
table(d$stayforeign, useNA="always")

#'
#' # Jobs
#'

## Industry
tmp <- do$F8b
table(tmp)

d$industry <- ifelse(tmp%in%c(17,18),NA,
                     ifelse(tmp==1,"Primary",
                            ifelse(tmp%in%c(2,3),"Secondary",
                                   ifelse(tmp%in%c(4,5,6,7,8,9),"Teritiary",
                                          "Quarternary"))))
d$industry <- factor(d$industry, levels=c("Primary","Secondary","Teritiary","Quarternary"))
table(d$industry)

d$industry2 <- ifelse(d$industry%in%c("Primary","Secondary"),"Primary/Secondary",as.character(d$industry))
d$industry2 <- factor(d$industry2, levels=c("Primary/Secondary","Teritiary","Quarternary"))
table(d$industry2)

## Working Status
tmp <- do$F8a
table(tmp)

d$workstat <- ifelse(tmp%in%c(10,11),NA,
                    ifelse(tmp%in%c(2,3),"Full-Time",
                           ifelse(tmp%in%c(4,5),"Part-Time",
                                  ifelse(tmp%in%c(1,6,7),"Self-Employed","Not Employed"))))
d$workstat <- factor(d$workstat, levels=c("Full-Time","Part-Time","Self-Employed","Not Employed"))
table(d$workstat)

d$employed <- ifelse(d$workstat=="Not Employed",0,1)
table(d$employed)

#'
#' # Exporting Residential Locations from Zip-Code
#'

# Zip Code
tmp <- gsub("-","",do$zip)
table(nchar(tmp))
tmp[which(nchar(tmp)!=7)] <- NA
tmp[which(tmp==9999999)] <- NA
d$zip <- tmp
table(!is.na(d$zip))

# Longitude, Latitude, Prefecture, and Municipality from Zip Code

# addloc <- readRDS(paste0(projdir,"/data/original/sifcct_address/addloc.rds"))
# 
# zip_ext <- unique(d$zip[!d$zip%in%addloc$zip])[-1]
# 
# library(RCurl)
# library(RJSONIO)
# library(pbapply)
# appid <- readLines("/home/gentok/Documents/yahoo_appid.txt")
# query_prefix <- paste0("https://map.yahooapis.jp/search/zip/V1/zipCodeSearch?appid=",appid,"&output=json&detail=full&query=")
# 
# # Make Query by 2500 addresses Each
# adddt_ext <- pblapply(zip_ext, function(k) fromJSON(getURL(paste0(query_prefix,k))))
# names(adddt_ext) <- zip_ext
# saveRDS(adddt_ext, "./data/original/sifcct_address/adddt_ext.rds")
# 
# addloc_ext <- data.frame(zip=names(adddt_ext),
#                      coord = sapply(adddt_ext, function(k) ifelse(is.null(k$Feature[[1]]$Geometry[2]),"NA,NA",
#                                                              k$Feature[[1]]$Geometry[2])),
#                      pref = sapply(adddt_ext, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[1]][1]),NA,
#                                                              k$Feature[[1]]$Property$AddressElement[[1]][1])),
#                      pref_kana = sapply(adddt_ext, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[1]][2]),NA,
#                                                             k$Feature[[1]]$Property$AddressElement[[1]][2])),
#                      muni = sapply(adddt_ext, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[2]][1]),NA,
#                                                              k$Feature[[1]]$Property$AddressElement[[2]][1])),
#                      muni_kana = sapply(adddt_ext, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[2]][2]),NA,
#                                                                   k$Feature[[1]]$Property$AddressElement[[2]][2])),
#                      stringsAsFactors = FALSE)
# library(stringr)
# coordtmp <- str_split(addloc_ext$coord,",")
# addloc_ext$lon <- as.numeric(sapply(coordtmp, function(k) k[1]))
# addloc_ext$lat <- as.numeric(sapply(coordtmp, function(k) k[2]))
# 
# saveRDS(addloc_ext, "./data/original/sifcct_address/addloc_ext.rds")
# rm(adddt_ext, addloc_ext)

addloc <- rbind(readRDS(paste0(projdir,"/data/original/sifcct_address/addloc.rds")),
                readRDS(paste0(projdir,"/data/original/sifcct_address/addloc_ext.rds")))
unique(d$zip[!d$zip%in%addloc$zip]) ## All Zip found

d$zip_lon <- d$zip_lat <- NA
d$zip_lon[which(!is.na(d$zip))] <- addloc$lon[match(d$zip[which(!is.na(d$zip))],addloc$zip)]
d$zip_lat[which(!is.na(d$zip))] <- addloc$lat[match(d$zip[which(!is.na(d$zip))],addloc$zip)]
d$zip_pref <- d$zip_pref_kana <- NA
d$zip_pref[which(!is.na(d$zip))] <- addloc$pref[match(d$zip[which(!is.na(d$zip))],addloc$zip)]
d$zip_pref_kana[which(!is.na(d$zip))] <- addloc$pref_kana[match(d$zip[which(!is.na(d$zip))],addloc$zip)]
d$zip_muni <- d$zip_muni_kana <- NA
d$zip_muni[which(!is.na(d$zip))] <- addloc$muni[match(d$zip[which(!is.na(d$zip))],addloc$zip)]
d$zip_muni_kana[which(!is.na(d$zip))] <- addloc$muni_kana[match(d$zip[which(!is.na(d$zip))],addloc$zip)]

#'
#' # Living Length
#'

tmp <- do$F10_2
table(tmp)
d$lvlen <- ifelse(tmp==999,NA,tmp) # Length of Living
d$lvpr <- d$lvlen/d$age # Proportion in Life Living in the Current Address
d$lvpr[d$lvpr>1] <- 1
table(d$lvpr)

#'
#' # Saving Data
#'

#+ eval=FALSE
saveRDS(d, paste0(projdir, "/data/mail_latest_v5.rds"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/data_mail_1_recode_v5.R', rmarkdown::pdf_document(latex_engine="xelatex", extra_dependencies = list(bookmark=NULL, xltxtra=NULL, zxjatype=NULL, zxjafont=c("ipa"))), encoding = 'UTF-8')
# rmarkdown::render('./src/data_mail_1_recode_v5.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))

