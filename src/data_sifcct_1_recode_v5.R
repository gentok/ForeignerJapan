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
datadir1 <- paste(projdir,"data/original/panel_wave1-12.csv",sep="/")
datadir2 <- paste(projdir,"data/original/panel_wave13-24.csv",sep="/")

## Import Original Data
library(readr)
do1 <- read_csv(datadir1, locale=locale(encoding="UTF-8"), 
                col_types=paste(rep("c",764),collapse=""))
do2 <- read_csv(datadir2, locale=locale(encoding="UTF-8"), 
                col_types=paste(rep("c",548),collapse=""))
# # Use Only Flesh Samples
# do1 <- subset(do1, panel==0)
# do2 <- subset(do2, panel==0)

## Library Psych Package
require(psych)


#'
#' # Data Manipulation
#'

# Initiate New Data Set
d <- data.frame(id = paste(c(rep(1,nrow(do1)),rep(2,nrow(do2))),
                           c(do1$caseid, do2$caseid),sep="_"), 
                wave = as.integer(c(do1$wave, do2$wave)), 
                panel = as.integer(c(do1$panel, do2$panel)),
                panelid = paste(c(rep(1,nrow(do1)),rep(2,nrow(do2))),
                                c(do1$panelid, do2$panelid),sep="_"))

# Wave Variable
table(as.numeric(d$wave), useNA="always")

#'
#' ## DEPENDENT variables of (potential) interest
#' 
#' ### The local election suffrage should be granted to foreigners.
#' 
#' * Original: 1=Strongly agree 5=Strongly disagree 6=DK 7=NA
#' * Recoded: 0=Strongly disagree, 0.5=Neither/DK, 1=Strongly agree, Missing=NA

# Original Variable
tmp <- c(do1$i58a3, do2$i58a3) 
table(tmp, d$wave%in%c(1,23,24), useNA="always") # Not asked in 1, 23, 24 waves
# Recoded Variable
d$foreignsuff <- ifelse(tmp==7, 2, ifelse(tmp==6, 2, 5 - as.numeric(tmp)))/4
table(d$foreignsuff, d$wave%in%c(1,23,24), useNA="always")
d$foreignsuff3 <- ifelse(d$foreignsuff==0.5,1,ifelse(d$foreignsuff>0.5,3,2))
d$foreignsuff3 <- factor(d$foreignsuff3, labels=c("Neither","Disagree","Agree"))
table(d$foreignsuff3, d$wave%in%c(1,23,24), useNA="always")
d$foreignsuff3x <- factor(d$foreignsuff3, levels=c("Disagree","Neither","Agree"))
table(d$foreignsuff3x, d$wave%in%c(1,23,24), useNA="always")

#'
#' ### Increase in long-term resident foreigners (Only in Wave 2)
#'

plot(table(do1$i66))

d$immigincrease <- NA
d$immigincrease[1:nrow(do1)] <- 
  ifelse(do1$i66==6,2,ifelse(do1$i66==7, 2, (5 - as.numeric(do1$i66))))/4
table(d$immigincrease, d$wave%in%c(2), useNA="always")
d$immigincrease3 <- ifelse(d$immigincrease==0.5,1,ifelse(d$immigincrease>0.5,3,2))
d$immigincrease3 <- factor(d$immigincrease3, labels=c("Neither","Disagree","Agree"))
table(d$immigincrease3, d$wave%in%c(2), useNA="always")
d$immigincrease3x <- factor(d$immigincrease3, levels=c("Disagree","Neither","Agree"))
table(d$immigincrease3x, d$wave%in%c(2), useNA="always")

#'
#' ### Trustworthiness of Foreigners (Only in Wave 2)
#' 
#' * Original: 1=Not trustworthy 7=trustworthy
#' * Recoded: 0-1 range, 1 is the most trustworthy
#' 

plot(table(do1$i68a1))
plot(table(do1$i68a2))
plot(table(do1$i68a3))
plot(table(do1$i68a4))
plot(table(do1$i68a5))
plot(table(do1$i68a6))
plot(table(do1$i68a7))
plot(table(do1$i68a8))

d$trust_old_sko <- d$trust_old_kor <- d$trust_old_chn <- 
  d$trust_new_sko <- d$trust_new_chn <- d$trust_new_bra <- 
  d$trust_new_phl <- d$trust_new_usa <- NA
d$trust_old_sko[1:nrow(do1)] <- old_sko <- 
  (ifelse(do1$i68a1==8,3,ifelse(do1$i68a1==9,3,as.numeric(do1$i68a1)))-1)/6 # SK Old Commer
d$trust_old_kor[1:nrow(do1)] <- old_kor <- 
  (ifelse(do1$i68a2==8,3,ifelse(do1$i68a2==9,3,as.numeric(do1$i68a2)))-1)/6 # Korean Peninsura Old Commer
d$trust_old_chn[1:nrow(do1)] <- old_chn <- 
  (ifelse(do1$i68a3==8,3,ifelse(do1$i68a3==9,3,as.numeric(do1$i68a3)))-1)/6 # CH Old Commer
d$trust_new_sko[1:nrow(do1)] <- new_sko <- 
  (ifelse(do1$i68a4==8,3,ifelse(do1$i68a4==9,3,as.numeric(do1$i68a4)))-1)/6 # SK New Commer
d$trust_new_chn[1:nrow(do1)] <- new_chn <- 
  (ifelse(do1$i68a5==8,3,ifelse(do1$i68a5==9,3,as.numeric(do1$i68a5)))-1)/6 # CH New Commer
d$trust_new_bra[1:nrow(do1)] <- new_bra <- 
  (ifelse(do1$i68a6==8,3,ifelse(do1$i68a6==9,3,as.numeric(do1$i68a6)))-1)/6 # Brazil New Commer
d$trust_new_phl[1:nrow(do1)] <- new_phl <- 
  (ifelse(do1$i68a7==8,3,ifelse(do1$i68a7==9,3,as.numeric(do1$i68a7)))-1)/6 # PHL New Commer
d$trust_new_usa[1:nrow(do1)] <- new_usa <- 
  (ifelse(do1$i68a8==8,3,ifelse(do1$i68a8==9,3,as.numeric(do1$i68a8)))-1)/6 # US New Commer

tmp <- cor(cbind(old_sko,old_kor,old_chn,new_sko,
                 new_chn,new_bra,new_phl,new_usa),use="pairwise") 
round(tmp,3)

# Cronbach's Alpha
psych::alpha(cbind(old_sko,old_kor,old_chn)) # Old Commers
psych::alpha(cbind(new_sko,new_chn,new_bra,new_phl,new_usa)) # New Commers

# Combine
d$trust_old <- d$trust_new <- NA
d$trust_old[1:nrow(do1)] <- (old_sko + old_kor + old_chn)/3
summary(d$trust_old)
d$trust_new[1:nrow(do1)] <- (new_sko + new_chn + new_bra + new_phl + new_usa)/5
summary(d$trust_new)

#'
#' ### Foreign friends/acquaintances in Japan. (Only in Wave 2)
#' 
#' * Original: 1=1 or 2, 4=11 or more, 5=None, 6= Don't want to answer
#' * Recoded: 0=None, 1=Any Friend
#' * Recoded 2: 0=None, 1=1or2, 2=More
#' 

# Original: Only in Nov 2011 (Wave 2) survey!
table(do1$i62a1, do1$wave)
tmp <- c(do1$i62a1, rep(NA, nrow(do2)))
# Recoded 1
d$foreignfriend_jpn <- ifelse(tmp==6, NA, ifelse(tmp==5, 0, 1))
table(d$foreignfriend_jpn, useNA="always")
# Recoded 2
d$foreignfriend_jpn2 <- ifelse(tmp==6, NA, ifelse(tmp==5, 0, 
                                                  ifelse(tmp%in%c(1), 1, 2)))
table(d$foreignfriend_jpn2, useNA="always")

#'
#' ### Foreign friends/acquaintances outside of Japan. (Only in Wave2)
#' 
#' * Original: 1=1 or 2, 4=11 or more, 5=None, 6= Don't want to answer
#' * Recoded 1: 0=None, 1=Any Friend
#' * Recoded 2: 0=None, 1=1or2, 2=More
#' 

# Original: Only in Nov 2011 (Wave 2) survey!
table(do1$i62a2, do1$wave) 
tmp <- c(do1$i62a2, rep(NA, nrow(do2)))
# Recoded 1
d$foreignfriend_out <- ifelse(tmp==6, NA, ifelse(tmp==5, 0, 1))
table(d$foreignfriend_out, useNA="always")
# Recoded 2
d$foreignfriend_out2 <- ifelse(tmp==6, NA, ifelse(tmp==5, 0, 
                                                  ifelse(tmp%in%c(1), 1, 2)))
table(d$foreignfriend_out2, useNA="always")

#'
#' ### Foreign relatives. (Only in Wave 2)
#' 
#' * Original: 1=1 or 2, 4=11 or more, 5=None, 6= Don't want to answer
#' * Recoded: 0=None, 1=Any
#' * Recoded 2: 0=None, 1=1or2, 2=More
#' 

# Original: Only in Nov 2011 (Wave 2) survey!
table(do1$i62a3)
tmp <- c(do1$i62a3, rep(NA, nrow(do2)))
# Recoded 1
d$foreignfamily <- ifelse(tmp==6, NA, ifelse(tmp==5, 0, 1))
table(d$foreignfamily, useNA="always")
# Recoded 2
d$foreignfamily2 <- ifelse(tmp==6, NA, ifelse(tmp==5, 0, 
                                              ifelse(tmp%in%c(1), 1, 2)))
table(d$foreignfamily2, useNA="always")

#'
#' ### Foreign Acquaintances (Only in Wave 2)
#' 
#' * Recoded: 0=None, 1=Any
#' * Recoded 2: 0=None, 1=1or2 (for only one), 2=More
#'

d$foreignacqu <- ifelse(d$foreignfriend_jpn + 
                          d$foreignfriend_out + 
                          d$foreignfamily > 0, 1, 0)
table(d$foreignacqu[d$wave==2], useNA="always")
d$foreignacqux <- d$foreignacqu
d$foreignacqux[which(d$wave==2 & is.na(d$foreignacqu))] <- 0
table(d$foreignacqux[d$wave==2], useNA="always")

d$foreignacqu2 <- ifelse(d$foreignfriend_jpn2 + 
                          d$foreignfriend_out2 + 
                          d$foreignfamily2 > 1, 2,
                         d$foreignfriend_jpn2 + 
                           d$foreignfriend_out2 + 
                           d$foreignfamily2)
table(d$foreignacqu2, useNA="always")

#'
#' ### Familiarity with Foreign Countries
#'

tmp1 <- as.numeric(c(do1$i14a1, do2$i14a1))
tmp1 <- ifelse(tmp1==999, 50, ifelse(tmp1==888, 50, tmp1))
barplot(table(tmp1, useNA="always"))
d$familiarityFT_USA <- tmp1/100

tmp2 <- as.numeric(c(do1$i14a2, do2$i14a2))
tmp2 <- ifelse(tmp2==999, 50, ifelse(tmp2==888, 50, tmp2))
barplot(table(tmp2, useNA="always"))
d$familiarityFT_CHN <- tmp2/100

tmp3 <- as.numeric(c(do1$i14a3, do2$i14a3))
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
tmp1 <- c(do1$i21, do2$i21)==4
table(tmp1, useNA="always")
tmp2 <- c(do1$i22, do2$i22)==3
table(tmp2, useNA="always")
tmp3 <- c(do1$i23, do2$i23)==2
table(tmp3, useNA="always")
tmp4 <- c(do1$i24, do2$i24)==2
table(tmp4, useNA="always")
tmp5 <- c(do1$i25, do2$i25)==3
table(tmp5, useNA="always")
tmp6 <- c(do1$i26, do2$i26)==3
table(tmp6, useNA="always")
# Recoded
d$knowledge <- (tmp1 + tmp2 + tmp3 + tmp4 + tmp5 + tmp6)/6
table(d$knowledge, d$panel, useNA="always")
# Cronbach's Alpha is 0.77
psych::alpha(cbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6))

#' 
#' ### Interest in politics. 
#' 
#' * Original: 1= interested 4= Not interested 5=DK 6= Don't want to answer
#' * Recoded: 0=Not interested to 1=Interested, Missing=DK/NA
#' 

# Original
tmp <- as.numeric(c(do1$i5, do2$i5))
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
tmp <- as.numeric(c(do1$i6, do2$i6))
table(tmp, useNA="always") 
# Recoded
d$intlint <- ifelse(tmp%in%c(5,6), 0, 4 - tmp)/3
table(d$intlint, useNA="always")

#'
#' ### Political Awareness (Factor Score)
#'

tmp <- psych::fa(cbind(d$knowledge,d$polint))
hist((tmp$scores), 10)
d$awareness <- tmp$scores

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
tmp <- as.numeric(c(do1$i40, do2$i40))
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

#' 
#' ### Gender
#' 
#' * Original: 1=male 2=female 3=NA
#' * Recoded: 0=male, 1=female
#' 

# Original
tmp <- as.numeric(c(do1$i38, do2$i38))
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
tmp <- as.numeric(c(do1$i39, do2$i39))
table(tmp, useNA="always")
# Recoded
d$age <- ifelse(tmp==99, NA, tmp)
table(d$age, useNA="always")

## Recoded Born Year (by Academic Year: April-March)
d$bornyr <- NA
d$bornyr[which(d$wave%in%seq(1,6))] <- 2011 - d$age[which(d$wave%in%seq(1,6))]
d$bornyr[which(d$wave%in%seq(7,18))] <- 2012 - d$age[which(d$wave%in%seq(7,18))]
d$bornyr[which(d$wave%in%seq(19,24))] <- 2013 - d$age[which(d$wave%in%seq(19,24))]

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

tmp <- c(do1$i43,do2$i43)
table(tmp)

d$married <- ifelse(tmp==3,NA,ifelse(tmp==1,1,0))
table(d$married)

#'
#' ### Income
#'

# Original
tmp <- as.numeric(c(do1$i41, do2$i41))
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
#' ### Newspaper which is read the most
#'

# Original
tmp <- as.numeric(c(do1$i3, do2$i3))
table(tmp, useNA="always")
tmpx <- as.numeric(c(do1$i2,do2$i2))
table(tmp, tmpx, useNA="always")
# Recoded
d$npmost <- ifelse(tmpx==1,0,ifelse(tmp==10, NA, ifelse(tmp%in%c(7,8,9), 7, tmp)))
d$npmost <- factor(d$npmost, labels=c("None","Yomiuri","Asahi","Mainichi","Nikkei",
                                      "Sankei","Regional","Others"))
table(d$npmost, useNA="always")
# Recoded 2
d$npmost2 <- ifelse(d$npmost=="None","None",
                    ifelse(d$npmost%in%c("Yomiuri","Sankei"),"Yomiuri/Sankei",
                           ifelse(d$npmost%in%c("Asahi","Mainichi"),"Asahi/Mainichi",
                                  ifelse(d$npmost%in%c("Nikkei"),"Nikkei",
                                         ifelse(d$npmost%in%c("Regional","Others"),"Regional/Others",NA)))))
d$npmost2 <- factor(d$npmost2, levels=c("None","Yomiuri/Sankei","Asahi/Mainichi","Nikkei",
                                      "Regional/Others"))
table(d$npmost2, useNA="always")

#'
#' ### assessment of current life condition. 
#' 
#' Note: Question Wording is randomized among fresh respondents.
#' 
#' * Original: 1=good 5=bad, 6=DK, 7=NA
#' * Recoded: 0=bad, 0.5=Neither/DK, 1=good, NA=NA
#'

# Original: Combine All randomized responses
tmp <- as.numeric(c(do1$i9f1, do2$i9f1))
tmp[is.na(tmp)] <- as.numeric(c(do1$i9f2, do2$i9f2))[is.na(tmp)]
tmp[is.na(tmp)] <- as.numeric(c(do1$i9p, do2$i9p))[is.na(tmp)]
table(tmp, useNA="always")
# Recoded
d$evlife <- ifelse(tmp==7, 2, ifelse(tmp==6, 2, 5 - tmp))/4
table(d$evlife, useNA="always")

d$evlife_verybad <- ifelse(d$evlife%in%0, 1, 0)
d$evlife_bad <- ifelse(d$evlife%in%0.25, 1, 0)
d$evlife_notbad <- ifelse(!d$evlife%in%c(0,0.25), 1, 0)

# Question Wording Type (just in case)
# 0 = assessment of current economy
# 1 = assessment of the change in economy from a month ago
d$evlife_qtype <- 1 - (!is.na(c(do1$i9f1, do2$i9f1)) | d$panel==1)*1
table(d$evlife_qtype, useNA="always")

#'
#' ### assessment of current Japanese economy. 
#' 
#' Note: Question Wording is randomized among fresh respondents.
#' 
#' * Original: 1=good 5=bad, 6=DK, 7=NA
#' * Recoded: 0=bad, 0.5=Neither/DK, 1=good, NA=NA
#'

# Original: Combine All randomized responses
tmp <- as.numeric(c(do1$i11f1, do2$i11f1))
tmp[is.na(tmp)] <- as.numeric(c(do1$i11f2, do2$i11f2))[is.na(tmp)]
tmp[is.na(tmp)] <- as.numeric(c(do1$i11p, do2$i11p))[is.na(tmp)]
table(tmp, useNA="always")
# Recoded
d$evecon <- ifelse(tmp==7, 2, ifelse(tmp==6, 2, 5 - tmp))/4
table(d$evecon, useNA="always")

d$evecon_verybad <- ifelse(d$evecon%in%0, 1, 0)
d$evecon_bad <- ifelse(d$evecon%in%0.25, 1, 0)
d$evecon_notbad <- ifelse(!d$evecon%in%c(0,0.25), 1, 0)

# Question Wording Type (just in case)
# 0 = assessment of current economy
# 1 = assessment of the change in economy from a month ago
d$evecon_qtype <- 1 - (!is.na(c(do1$i11f1, do2$i11f1)) | d$panel==1)*1
table(d$evecon_qtype, useNA="always")

#'
#' ### Internet Usage
#' 
#' Original: 1=less than 30min 7=about more than 5 hrs, 8=NA
#' Recoded: Standardized to 0-1 range. NA=NA

# Original
tmp <- as.numeric(c(do1$i45, do2$i45))
table(tmp, useNA="always")
# Recoded
d$netuse <- ifelse(tmp==8, NA, tmp-1)/6
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
tmp <- c(do1$i42a, rep(NA,nrow(do2))) 
table(tmp, d$wave, useNA="always") # from wave 1-10 (Version "a")
tmp[is.na(tmp)] <- c(do1$i42b, rep(NA,nrow(do2)))[is.na(tmp)]
table(tmp, d$wave, useNA="always") # wave 11 only (Version "b")
tmp[is.na(tmp)] <- c(do1$i42c, do2$i42c)[is.na(tmp)]
table(tmp, d$wave, useNA="always") # wave 12-14 (Version "c")
tmp[is.na(tmp)] <- c(rep(NA,nrow(do1)), do2$i42d)[is.na(tmp)]
table(tmp, d$wave, useNA="always") # wave 15 only (Version "d")
tmp[is.na(tmp)] <- c(rep(NA,nrow(do1)), do2$i42e)[is.na(tmp)]
table(tmp, d$wave, useNA="always") # wave 16-24 (Version "e")
# Original: Response Category Type
tmptype <- ifelse(d$wave%in%c(1:10),"a",
                  ifelse(d$wave%in%c(11),"b",
                         ifelse(d$wave%in%c(12:14),"c",
                                ifelse(d$wave%in%c(15),"d","e"))))
table(tmptype, useNA="always")
# Original: NA Locations
tmpNA <- rep(0, length(tmp))
tmpNA[which(tmptype=="a" & tmp==9)] <- 1
tmpNA[which(tmptype=="b" & tmp==10)] <- 1
tmpNA[which(tmptype=="c" & tmp==11)] <- 1
tmpNA[which(tmptype=="d" & tmp==15)] <- 1
tmpNA[which(tmptype=="e" & tmp==11)] <- 1
table(tmpNA, useNA="always")

# Recoded
## DPJ
d$dpj <- ifelse(tmptype%in%c("a","b","c","d"), (tmp==1)*1, (tmp==2)*1)
d$dpj[tmpNA==1] <- NA
table(d$dpj, useNA="always")
## LDP
d$ldp <- ifelse(tmptype%in%c("a","b","c","d"), (tmp==2)*1, (tmp==1)*1)
d$ldp[tmpNA==1] <- NA
table(d$ldp, useNA="always")
## CGP
d$cgp <- ifelse(tmptype%in%c("a","b","c"), (tmp==3)*1, (tmp==4)*1)
d$cgp[tmpNA==1] <- NA
table(d$cgp, useNA="always")
## JCP
d$jcp <- ifelse(tmptype%in%c("a","b","c"), (tmp==4)*1, 
                ifelse(tmptype=="d", (tmp==5)*1, (tmp==7)*1))
d$jcp[tmpNA==1] <- NA
table(d$jcp, useNA="always")
## SDP
d$sdp <- ifelse(tmptype%in%c("a","b","c"), (tmp==5)*1, (tmp==8)*1)
d$sdp[tmpNA==1] <- NA
table(d$sdp, useNA="always")
## YP
d$yp <- ifelse(tmptype%in%c("a","b","c","d"), (tmp==6)*1, (tmp==5)*1)
d$yp[tmpNA==1] <- NA
table(d$yp, useNA="always")
## JRP
d$jrp <- ifelse(tmptype%in%c("a","b"), 0, 
                ifelse(tmptype=="c", (tmp==8)*1, 
                       ifelse(tmptype=="d", (tmp==7)*1, (tmp==3)*1)))
d$jrp[tmpNA==1] <- NA
table(d$jrp, useNA="always")
## PLP Seikatsu/Mirai (People's Life Party)
d$plp <- ifelse(tmptype=="a", 0, 
                     ifelse(tmptype%in%c("b","c"), (tmp==7)*1, 
                            ifelse(tmptype=="d", (tmp==3)*1, (tmp==6)*1)))
d$plp[tmpNA==1] <- NA
table(d$plp, useNA="always")
## other parties 
d$othp <- ifelse(tmptype=="a", (tmp%in%c(7))*1, 
                 ifelse(tmptype=="b", (tmp%in%c(8))*1, 
                        ifelse(tmptype=="c", (tmp%in%c(9))*1,
                               ifelse(tmptype=="d", (tmp%in%c(9,10,11,12,13))*1,
                                      (tmp%in%c(9))*1)))) 
d$othp[tmpNA==1] <- NA
table(d$othp, useNA="always")
## mutoha (No party support)
d$mutoha <- ifelse(tmptype=="a", (tmp==8)*1, 
                 ifelse(tmptype=="b", (tmp==9)*1, 
                        ifelse(tmptype=="c", (tmp==10)*1,
                               ifelse(tmptype=="d", (tmp==14)*1,
                                      (tmp==10)*1)))) 
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

tmp <- as.numeric(c(do1$i8a2,do2$i8a2))
table(tmp)
d$ldpft <- ifelse(tmp==999,0.5,ifelse(tmp==888,0.5,tmp/100))
summary(d$ldpft)

tmp <- as.numeric(c(do1$i8a1,do2$i8a1))
table(tmp)
d$dpjft <- ifelse(tmp==999,0.5,ifelse(tmp==888,0.5,tmp/100))
summary(d$dpjft)

d$ldpdpjft = (d$ldpft - d$dpjft + 1)/2
summary(d$ldpdpjft)
hist(d$ldpdpjft)

#'
#' # Ideology
#'

tmp <- as.numeric(c(do1$i20,do2$i20))
table(tmp)
d$ideology <- ifelse(tmp==999,0.5,ifelse(tmp==99,0.5,tmp/10))
table(d$ideology, useNA="always")

#'
#' # Stayed in Foreign Country
#'

tmp <- as.numeric(c(do1$i46,do2$i46))
table(tmp)
d$stayforeign <- ifelse(tmp==2, 1, ifelse(tmp==1, 0, NA))
table(d$stayforeign, useNA="always")

#'
#' # Jobs
#'

## Industry
tmp <- as.numeric(c(do1$i51,do2$i51))
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
tmp <- as.numeric(c(do1$i50,do2$i50))
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
tmp <- c(do1$i54,do2$i54)
tmp[which(nchar(tmp)==5)] <- paste0("00",tmp[which(nchar(tmp)==5)])
tmp[which(nchar(tmp)==6)] <- paste0("0",tmp[which(nchar(tmp)==6)])
tmp[which(nchar(tmp)!=7)] <- NA
tmp[which(tmp==9999999)] <- NA
d$zip <- tmp

# ## Approximately 20000 unique addresses recovered.
# tmpzip1 <- c(do1$i54,do2$i54)
# tmpzip1[which(nchar(tmpzip1)!=7)] <- NA
# tmpzip1[which(tmpzip1==9999999)] <- NA
# zipvec <- unique(na.omit(tmpzip1))
# tmpzip2 <- c(do1$i54,do2$i54)
# tmpzip2[which(nchar(tmpzip2)!=6)] <- NA
# zipvec2 <- paste0("0",unique(na.omit(tmpzip2)))
# tmpzip3 <- c(do1$i54,do2$i54)
# tmpzip3[which(nchar(tmpzip3)!=5)] <- NA
# zipvec3 <- paste0("00",unique(na.omit(tmpzip3)))
# 
# library(RCurl)
# library(RJSONIO)
# library(pbapply)
# appid <- readLines("/home/gentok/Documents/yahoo_appid.txt")
# query_prefix <- paste0("https://map.yahooapis.jp/search/zip/V1/zipCodeSearch?appid=",appid,"&output=json&detail=full&query=")
# 
# # Make Query by 2500 addresses Each
# adddt1 <- pblapply(zipvec[1:2500], function(k) fromJSON(getURL(paste0(query_prefix,k))))
# saveRDS(adddt1, "./data/sifcct_address/adddt1.rds")
# adddt2 <- pblapply(zipvec[2501:5000], function(k) fromJSON(getURL(paste0(query_prefix,k))))
# saveRDS(adddt2, "./data/sifcct_address/adddt2.rds")
# adddt3 <- pblapply(zipvec[5001:7500], function(k) fromJSON(getURL(paste0(query_prefix,k))))
# saveRDS(adddt3, "./data/sifcct_address/adddt3.rds")
# adddt4 <- pblapply(zipvec[7501:10000], function(k) fromJSON(getURL(paste0(query_prefix,k))))
# saveRDS(adddt4, "./data/sifcct_address/adddt4.rds")
# adddt5 <- pblapply(zipvec[10001:12500], function(k) fromJSON(getURL(paste0(query_prefix,k))))
# saveRDS(adddt5, "./data/sifcct_address/adddt5.rds")
# adddt6 <- pblapply(zipvec[12501:15000], function(k) fromJSON(getURL(paste0(query_prefix,k))))
# saveRDS(adddt6, "./data/sifcct_address/adddt6.rds")
# adddt7 <- pblapply(zipvec[15001:17500], function(k) fromJSON(getURL(paste0(query_prefix,k))))
# saveRDS(adddt7, "./data/sifcct_address/adddt7.rds")
# adddt8 <- pblapply(zipvec[17501:length(zipvec)], function(k) fromJSON(getURL(paste0(query_prefix,k))))
# saveRDS(adddt8, "./data/sifcct_address/adddt8.rds")
# adddt9 <- pblapply(c(zipvec2,zipvec3), function(k) fromJSON(getURL(paste0(query_prefix,k))))
# saveRDS(adddt9, "./data/sifcct_address/adddt9.rds")
# 
# adddt1 <- readRDS("./data/original/sifcct_address/adddt1.rds")
# adddt2 <- readRDS("./data/original/sifcct_address/adddt2.rds")
# adddt3 <- readRDS("./data/original/sifcct_address/adddt3.rds")
# adddt4 <- readRDS("./data/original/sifcct_address/adddt4.rds")
# adddt5 <- readRDS("./data/original/sifcct_address/adddt5.rds")
# adddt6 <- readRDS("./data/original/sifcct_address/adddt6.rds")
# adddt7 <- readRDS("./data/original/sifcct_address/adddt7.rds")
# adddt8 <- readRDS("./data/original/sifcct_address/adddt8.rds")
# adddt9 <- readRDS("./data/original/sifcct_address/adddt9.rds")
# 
# ## Combine ALl
# adddt <- c(adddt1,adddt2,adddt3,adddt4,adddt5,adddt6,adddt7,adddt8,adddt9)
# names(adddt) <- c(zipvec,zipvec2,zipvec3)
# saveRDS(adddt, "./data/original/sifcct_address/adddt_all.rds")
# rm(adddt1,adddt2,adddt3,adddt4,adddt5,adddt6,adddt7,adddt8,adddt9)
# 
# addloc <- data.frame(zip=names(adddt),
#                      coord = sapply(adddt, function(k) ifelse(is.null(k$Feature[[1]]$Geometry[2]),"NA,NA",
#                                                              k$Feature[[1]]$Geometry[2])),
#                      pref = sapply(adddt, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[1]][1]),NA,
#                                                              k$Feature[[1]]$Property$AddressElement[[1]][1])),
#                      pref_kana = sapply(adddt, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[1]][2]),NA,
#                                                             k$Feature[[1]]$Property$AddressElement[[1]][2])),
#                      muni = sapply(adddt, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[2]][1]),NA,
#                                                              k$Feature[[1]]$Property$AddressElement[[2]][1])),
#                      muni_kana = sapply(adddt, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[2]][2]),NA,
#                                                                   k$Feature[[1]]$Property$AddressElement[[2]][2])),
#                      stringsAsFactors = FALSE)
# library(stringr)
# coordtmp <- str_split(addloc$coord,",")
# addloc$lon <- as.numeric(sapply(coordtmp, function(k) k[1]))
# addloc$lat <- as.numeric(sapply(coordtmp, function(k) k[2]))
# 
# saveRDS(addloc, "./data/original/sifcct_address/addloc.rds")
# rm(adddt, addloc)

# Longitude, Latitude, Prefecture, and Municipality from Zip Code
addloc <- readRDS(paste0(projdir,"/data/original/sifcct_address/addloc.rds"))
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

tmp <- as.numeric(c(do1$i55,do2$i55))
table(tmp)
d$lvlen <- ifelse(tmp==999,NA,tmp) # Length of Living
d$lvpr <- d$lvlen/d$age # Proportion in Life Living in the Current Address

#'
#' # Saving Data
#'

#+ eval=FALSE
saveRDS(d, paste0(projdir, "/data/sifcct_latest_v5.rds"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/data_sifcct_1_recode_v5.R', 'pdf_document', encoding = 'UTF-8')
# rmarkdown::render('./src/data_sifcct_1_recode_v5.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))

