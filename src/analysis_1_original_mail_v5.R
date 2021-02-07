#' ---
#' title: "Analysis 1: Main Analysis with Original and Mail-In Data"
#' author: "Fan Lu & Gento Kato"
#' date: "January 26, 2021"
#' ---
#' 
#' # Analytical Strategy
#' 
#' ## Variables
#' 
#' 
#' * Outcome: Foreigner Suffrage (min 0, max 1)
#' 
#' 
#' * Mediator 1: (Objective) Political Knowledge (min = 0, max = 1)
#' * Mediator 2: Ideology (min 0 = left/liberal, max 1 = right/conservative)
#' * Mediator 3: LDP - DPJ FT (min 0 = favor DPJ, max 1 = favor LDP)
#' * Mediator 4: Favorability of South Korea (min = 0, max = 1)     
#' * Mediator 5: Favorability of China (min = 0, max = 1)     
#' * Mediator 6: Favorability of USA (min = 0, max = 1)   
#' * Mediator 7: Income (percentile, min = 0, max = 1)  
#' 
#' 
#' * Independent Variable: University Education (0 = Junior College or Less, 1 = University or More)
#' 
#' 
#' * Moderator 1: Gender (0 = Female, 1 = Male), This means that all "base" coefficients are for female.
#' * Moderator 2: Age (by 10 years, centered at 20). Reasoning: Two trends may influence the role of 
#' university education. (1) There is an evident increase in number of university graduates over 
#' the years, especially among women. This trend may impies that university experience may be 
#' more gendered in the past than today. (2) There is a trend of "internationalization" in 
#' university education in recent days. Therefore, the diversifying and liberalizing effect of 
#' education may be stronger for younger generation.   
#' 
#' * Control 1: Percent in life residing locally. More locally-identified individuals may dislike outsiders more.
#' * Control 2: (ZIP level) Residing in densely inhabited district (DID)
#' * Control 3: (ZIP level) Percent of foreigners in neighborhood (transformed by square root)        
#' * Control 4: (ZIP level) Percent of university graduates in neighborhood (by 10 percent)
#' * Control 5: (Municipality level) Percent of residents residing in DID
#' * Control 6: (Municipality level) Percent of foreigners (transformed by square root)
#' * Control 7: (Municipality level) Percent of university graduates (by 10 percent)
#'
#'                
#' ## Subset Data
#' 
#' Analysis is conducted on the following subset. 
#' 
#' If age - years of local ZIP residence is 15 or smaller. 
#' 15 is the age of entering high school in Japan. 
#' Assuming that an individual is living in the local ZIP continuously, 
#' this condition implies that one spend significant time before college 
#' in the ZIP of current residence. This filters out the possibility that 
#' education changes attitudes through the movement in residence.
#' 
#' ## Modeling Strategy
#' 
#' All models are estimated by OLS. For outcome model, alternative model 
#' is estimated by the multinomial logit model, with 3 category DV (disagree, 
#' neither, agree), with disagree as a reference category.
#' 
#' ## Robustness Check (in this file)
#' 
#' SIFCCT has one special survey where they conducted a survey through mail. 
#' Mail survey contains identical set of variables as online survey. So I 
#' replicated the analysis with the mail survey. 
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

## Original Data
datadir1a <- paste0(projdir, "/data/sifcct_zip_latest_v5.rds")
datadir1b <- paste0(projdir, "/data/sifcct_zip_latest_panel_v5.rds")
datadir2 <- paste0(projdir, "/data/mail_zip_latest_v5.rds")

## packages
require(sandwich)
require(lmtest)
require(MASS)
# devtools::install_github("tidyverse/ggplot2") # Need development version (as of Dec 31, 2019)
require(ggplot2)
require(texreg)
require(mlogit)
require(Formula)

#'
#' # Import and clean data
#'

###################
## SIFCCT Online ##
###################

sifcct <- rbind(readRDS(datadir1a),readRDS(datadir1b))

## Knowledge Variable (Replaced)
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==2] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==2],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==3] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==3],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==4] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==4],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==5] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==5],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==6] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==6],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==7] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==7],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==8] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==8],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==9] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==9],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==10] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==10],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==11] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==11],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==12] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==1][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==12],sifcct$panelid[sifcct$panel==1 & sifcct$wave==1])]
## Knowledge Variable (Replaced)
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==14] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==14],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==15] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==15],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==16] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==16],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==17] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==17],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==18] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==18],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==19] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==19],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==20] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==20],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==21] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==21],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==22] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==22],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==23] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==23],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]
sifcct$knowledge[sifcct$panel==1 & sifcct$wave==24] <- sifcct$knowledge[sifcct$panel==1 & sifcct$wave==13][match(sifcct$panelid[sifcct$panel==1 & sifcct$wave==24],sifcct$panelid[sifcct$panel==1 & sifcct$wave==13])]

## Subset Waves
sifcct <- subset(sifcct, !wave%in%c(1,23,24) & !(panel==1 & wave%in%c(1,3:12,14:24)))
table(sifcct$wave,sifcct$panel)

## sreg with no population as NA
sifcct$c10_sreg_pop[which(sifcct$c10_sreg_pop==0)] <- NA 

## Income Missing Percentage (8.9%)
table(is.na(sifcct$income))/sum(table(is.na(sifcct$income)))

## Exclude Missing Values
sifcctx <- sifcct[,c("id","foreignsuff","foreignsuff3","foreignsuff3x",
           "knowledge","polint","ideology","ldpdpjft",
           "familiarityFT_KOR","familiarityFT_CHN","familiarityFT_USA",
           # "evecon","evecon_verybad","evecon_bad","evecon_notbad","evecon_qtype",
           "income", #"employed",
           "female","male","edu","edu2","age","agecat","bornyr",
           "lvlen","lvpr",
           "zip_did","c10_sreg_foreignN","c10_sreg_pop",
           "c10_sreg_edu_ugsP","c10_sreg_edu_ugs","c10_sreg_edu_graduated",
           "didper","c10_mun_foreignN","c10_mun_pop",
           "c10_mun_edu_ugsP","c10_mun_edu_ugs","c10_mun_edu_graduated",
           "zip","c10_name_pref","c10_name_mun","c10_name_sreg",
           "zip_lat","zip_lon",
           "wave","panel")]
sifcctx <- na.omit(sifcctx)
nrow(sifcctx)

## Add Income and fper
sifcctx$income <- sifcct$income[match(paste(sifcctx$id,sifcctx$wave),paste(sifcct$id,sifcct$wave))]
summary(sifcctx$income)
sifcctx$fper <- sifcct$fper[match(paste(sifcctx$id,sifcctx$wave),paste(sifcct$id,sifcct$wave))]
summary(sifcctx$fper)

## Replace Data
sifcct <- sifcctx
rm(sifcctx)

nrow(sifcct[which(sifcct$age - sifcct$lvlen<=15),])

#################
## SIFCCT Mail ##
#################

mail <- readRDS(datadir2)

## sreg with no population as NA
mail$c10_sreg_pop[which(mail$c10_sreg_pop==0)] <- NA 

## Exclude Missing Values
mailx <- mail[,c("id","foreignsuff","foreignsuff3","foreignsuff3x",
                     "knowledge","polint","ideology","ldpdpjft",
                     "familiarityFT_KOR","familiarityFT_CHN","familiarityFT_USA",
                     # "evecon","evecon_verybad","evecon_bad","evecon_notbad","evecon_qtype",
                     # "income","employed",
                     "female","male","edu","edu2","age","agecat","bornyr",
                     "lvlen","lvpr",
                     "zip_did","c10_sreg_foreignN","c10_sreg_pop",
                     "c10_sreg_edu_ugsP","c10_sreg_edu_ugs","c10_sreg_edu_graduated",
                     "didper","c10_mun_foreignN","c10_mun_pop",
                     "c10_mun_edu_ugsP","c10_mun_edu_ugs","c10_mun_edu_graduated",
                     "zip","c10_name_pref","c10_name_mun","c10_name_sreg",
                     "zip_lat","zip_lon")]
mailx <- na.omit(mailx)
nrow(mailx)

## Add Income & fper
mailx$income <- mail$income[match(paste(mailx$id),paste(mail$id))]
summary(mailx$income)
mailx$fper <- mail$fper[match(paste(mailx$id),paste(mail$id))]
summary(mailx$fper)

## Replace Data
mail <- mailx
rm(mailx)

#'
#' # Recoding Variables
#' 

## SIFCCT ##

## Binary Age Cohort (50s or over)
sifcct$age2 <- ifelse(sifcct$age >= 50, 1, 0)
sifcct$agex <- sifcct$age/10 - 4.5
## Small Region Foreiner Percent
sifcct$c10_sreg_fper <- sifcct$c10_sreg_foreignN/sifcct$c10_sreg_pop*100
## Municipality Foreigner Percent
sifcct$c10_mun_fper <- sifcct$c10_mun_foreignN/sifcct$c10_mun_pop*100
## Compare Census and Foreinger Registry Numbers
plot(sifcct$fper, sifcct$c10_mun_fper)
cor(sifcct$fper, sifcct$c10_mun_fper, use="pairwise")
plot(sifcct$c10_mun_fper, sifcct$c10_sreg_fper)
cor(sifcct$c10_mun_fper, sifcct$c10_sreg_fper, use="pairwise")

## MAIL ##

## Binary Age Cohort (50s or over)
mail$age2 <- ifelse(mail$age >= 50, 1, 0)
mail$agex <- mail$age/10 - 4.5
## Small Region Foreiner Percent
mail$c10_sreg_fper <- mail$c10_sreg_foreignN/mail$c10_sreg_pop*100
## Municipality Foreigner Percent
mail$c10_mun_fper <- mail$c10_mun_foreignN/mail$c10_mun_pop*100
## Compare Census and Foreinger Registry Numbers
plot(mail$fper, mail$c10_mun_fper)
cor(mail$fper, mail$c10_mun_fper, use="pairwise")
plot(mail$c10_mun_fper, mail$c10_sreg_fper)
cor(mail$c10_mun_fper, mail$c10_sreg_fper, use="pairwise")

## Formula (SIFCCT) ##

basemod0 <- formula(  ~ edu2*male*agex + lvpr +  
                        as.factor(wave)) # sifcct
basemodA <- formula(  ~ edu2*male*agex + lvpr +  
                        zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                        as.factor(wave)) # sifcct
basemodB <- formula(  ~ edu2*male*agex + lvpr +  
                        didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                        as.factor(wave)) # sifcct
basemodC <- formula(  ~ edu2*male*agex + lvpr +  
                        zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                        didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                        as.factor(wave)) # sifcct

## Formula (SIFCCT.mlogit) ##

outmod0.mlogit <- Formula(foreignsuff3x  ~ 0 | edu2*male*agex + lvpr +  
                            as.factor(wave)) # sifcct
outmodA.mlogit <- Formula(foreignsuff3x  ~ 0 | edu2*male*agex + lvpr +  
                            zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                            as.factor(wave)) # sifcct
outmodB.mlogit <- Formula(foreignsuff3x  ~ 0 | edu2*male*agex + lvpr +  
                            didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                            as.factor(wave)) # sifcct
outmodC.mlogit <- Formula(foreignsuff3x  ~ 0 | edu2*male*agex + lvpr +  
                            zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                            didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                            as.factor(wave)) # sifcct

## Formula (MAIL) ##

basemod0m <- formula(  ~ edu2*male*agex + lvpr) # sifcct
basemodAm <- formula(  ~ edu2*male*agex + lvpr +  
                         zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10)) # sifcct
basemodBm <- formula(  ~ edu2*male*agex + lvpr +  
                         didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10)) # sifcct
basemodCm <- formula(  ~ edu2*male*agex + lvpr +  
                         zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                         didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10)) # sifcct

## Formula (MAIL.mlogit) ##

outmod0m.mlogit <- Formula(foreignsuff3x  ~ 0 | edu2*male*agex + lvpr) # sifcct
outmodAm.mlogit <- Formula(foreignsuff3x  ~ 0 | edu2*male*agex + lvpr +  
                            zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10)) # sifcct
outmodBm.mlogit <- Formula(foreignsuff3x  ~ 0 | edu2*male*agex + lvpr +  
                            didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10)) # sifcct
outmodCm.mlogit <- Formula(foreignsuff3x  ~ 0 | edu2*male*agex + lvpr +  
                            zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                            didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10)) # sifcct

## Variable Names ##

vnmap <- list("edu2" = "University education",
              "edu2 (1)" = "University education",
              "female" = "Gender (female)",
              "male" = "Gender (male)",
              "age2" = "Age 50s or older",
              "agex" = "Age (by 10 years, centered at 45)",
              "edu2:female" = "University * Female",
              "edu2:male" = "University * Male",
              "edu2 (2)" = "University * Male",
              "edu2:age2" = "University * >=50s",
              "edu2:agex" = "University * Age",
              "edu2 (3)" = "University * Age",
              "edu2:female:age2" = "University * Female * >=50s",
              "edu2:male:age2" = "University * Male * >=50s",
              "edu2:female:agex" = "University * Female * Age",
              "edu2:male:agex" = "University * Male * Age",
              "edu2 (4)" = "University * Male * Age",
              "female:age2" = "Female * >=50s",
              "male:age2" = "Male * >=50s",
              "female:agex" = "Female * Age",
              "male:agex" = "Male * Age",
              "male (2)" = "Male * Age",
              "agecatMiddle Aged (40-50s)" = "Middle Aged (40-50s)",
              "agecatElder (>=60s)" = "Elder (>=60s)",
              "lvpr" = "% of Life Residing Locally (zip)",
              "zip_did" = "DID residence (zip)",
              "sqrt(c10_sreg_fper)" = "Foreigner % sqrt. (zip)",
              "c10_sreg_edu_ugsP" = "University % (zip)",
              "I(c10_sreg_edu_ugsP/10)" = "University % by 10% (zip)",
              "didper" = "DID proportion (mun.)",
              "sqrt(c10_mun_fper)" = "Foreigner % sqrt. (mun.)",
              "I(c10_mun_edu_ugsP/10)" = "University % by 10% (mun.)",
              "c10_mun_edu_ugsP" = "University % (mun.)")

#'
#' # SIFCCT: Outcome Model 
#'

## Living in Local ZIP since at least age 15 ##

smo_10 <- lm(update(foreignsuff ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smo_1A <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smo_1B <- lm(update(foreignsuff ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smo_1C <- lm(update(foreignsuff ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])

screenreg(list(smo_10,smo_1A,smo_1B,smo_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smo_10,vcov.=vcovHC(smo_10))[,2],
                             coeftest(smo_1A,vcov.=vcovHC(smo_1A))[,2],
                             coeftest(smo_1B,vcov.=vcovHC(smo_1B))[,2],
                             coeftest(smo_1C,vcov.=vcovHC(smo_1C))[,2]),
          override.pvalues = list(coeftest(smo_10,vcov.=vcovHC(smo_10))[,4],
                                  coeftest(smo_1A,vcov.=vcovHC(smo_1A))[,4],
                                  coeftest(smo_1B,vcov.=vcovHC(smo_1B))[,4],
                                  coeftest(smo_1C,vcov.=vcovHC(smo_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(smo_10,smo_1A,smo_1B,smo_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smo_10,vcov.=vcovHC(smo_10))[,2],
                             coeftest(smo_1A,vcov.=vcovHC(smo_1A))[,2],
                             coeftest(smo_1B,vcov.=vcovHC(smo_1B))[,2],
                             coeftest(smo_1C,vcov.=vcovHC(smo_1C))[,2]),
          override.pvalues = list(coeftest(smo_10,vcov.=vcovHC(smo_10))[,4],
                                  coeftest(smo_1A,vcov.=vcovHC(smo_1A))[,4],
                                  coeftest(smo_1B,vcov.=vcovHC(smo_1B))[,4],
                                  coeftest(smo_1C,vcov.=vcovHC(smo_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
          booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
          file = paste0(projdir,"/out/smo_1_tabular.tex"))

#'
#' # SIFCCT: Outcome Model 2
#'

## Living in Local ZIP since at least age 15 ##

require(nnet)
smo2_10 <- multinom(update(foreignsuff3x ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smo2_1A <- multinom(update(foreignsuff3x ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smo2_1B <- multinom(update(foreignsuff3x ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smo2_1C <- multinom(update(foreignsuff3x ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])

sifcct.mlogit <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen<=15),],
                       shape = "wide", choice = "foreignsuff3x")
# levels(sifcct.mlogit$idx$id2) <- c("Disagree","Neither","Agree")
smo2_10 <- mlogit(outmod0.mlogit, data=sifcct.mlogit, reflevel="Disagree")
smo2_1A <- mlogit(outmodA.mlogit, data=sifcct.mlogit, reflevel="Disagree")
smo2_1B <- mlogit(outmodB.mlogit, data=sifcct.mlogit, reflevel="Disagree")
smo2_1C <- mlogit(outmodC.mlogit, data=sifcct.mlogit, reflevel="Disagree")

screenreg(list(smo2_10,smo2_1A), digits = 4, # single.row = T,
          override.se = list(coeftest(smo2_10,vcov=sandwich)[grep(":Neither",names(coef(smo2_10))),2],
                             coeftest(smo2_10,vcov=sandwich)[grep(":Agree",names(coef(smo2_10))),2],
                             coeftest(smo2_1A,vcov=sandwich)[grep(":Neither",names(coef(smo2_1A))),2],
                             coeftest(smo2_1A,vcov=sandwich)[grep(":Agree",names(coef(smo2_1A))),2]),
          override.pvalues = list(coeftest(smo2_10,vcov=sandwich)[grep(":Neither",names(coef(smo2_10))),4],
                                  coeftest(smo2_10,vcov=sandwich)[grep(":Agree",names(coef(smo2_10))),4],
                                  coeftest(smo2_1A,vcov=sandwich)[grep(":Neither",names(coef(smo2_1A))),4],
                                  coeftest(smo2_1A,vcov=sandwich)[grep(":Agree",names(coef(smo2_1A))),4]),
          beside = T,
          custom.coef.map = vnmap,
          custom.model.names = c("Base: Agree","Base: Neither",
                                 "ZIP: Agree","ZIP: Neither"),
          # custom.model.names = c("Base: Neither","Base: Agree",
          #                        "ZIP: Neither","ZIP: Agree"),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+")
screenreg(list(smo2_1B,smo2_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smo2_1B,vcov=sandwich)[grep(":Neither",names(coef(smo2_1B))),2],
                             coeftest(smo2_1B,vcov=sandwich)[grep(":Agree",names(coef(smo2_1B))),2],
                             coeftest(smo2_1C,vcov=sandwich)[grep(":Neither",names(coef(smo2_1C))),2],
                             coeftest(smo2_1C,vcov=sandwich)[grep(":Agree",names(coef(smo2_1C))),2]),
          override.pvalues = list(coeftest(smo2_1B,vcov=sandwich)[grep(":Neither",names(coef(smo2_1B))),4],
                                  coeftest(smo2_1B,vcov=sandwich)[grep(":Agree",names(coef(smo2_1B))),4],
                                  coeftest(smo2_1C,vcov=sandwich)[grep(":Neither",names(coef(smo2_1C))),4],
                                  coeftest(smo2_1C,vcov=sandwich)[grep(":Agree",names(coef(smo2_1C))),4]),
          beside = T,
          # custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Agree","Mun.: Neither",
                                 "Full: Agree","Full: Neither"),
          # custom.model.names = c("Mun.: Neither","Mun.: Agree",
          #                        "Full: Neither","Full: Agree"),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+")

#+ include = FALSE
texreg(list(smo2_10,smo2_1A), digits = 4, # single.row = T,
          override.se = list(coeftest(smo2_10,vcov=sandwich)[grep(":Neither",names(coef(smo2_10))),2],
                             coeftest(smo2_10,vcov=sandwich)[grep(":Agree",names(coef(smo2_10))),2],
                             coeftest(smo2_1A,vcov=sandwich)[grep(":Neither",names(coef(smo2_1A))),2],
                             coeftest(smo2_1A,vcov=sandwich)[grep(":Agree",names(coef(smo2_1A))),2]),
          override.pvalues = list(coeftest(smo2_10,vcov=sandwich)[grep(":Neither",names(coef(smo2_10))),4],
                                  coeftest(smo2_10,vcov=sandwich)[grep(":Agree",names(coef(smo2_10))),4],
                                  coeftest(smo2_1A,vcov=sandwich)[grep(":Neither",names(coef(smo2_1A))),4],
                                  coeftest(smo2_1A,vcov=sandwich)[grep(":Agree",names(coef(smo2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger", 
          # custom.model.names = c("Base: Neither","Base: Agree",
          #                        "ZIP: Neither","ZIP: Agree"),
       custom.model.names = c("Base: Agree","Base: Neither",
                              "ZIP: Agree","ZIP: Neither"),
       custom.coef.map = vnmap,
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/smo2_1_1_tabular.tex"))
texreg(list(smo2_1B,smo2_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smo2_1B,vcov=sandwich)[grep(":Neither",names(coef(smo2_1B))),2],
                             coeftest(smo2_1B,vcov=sandwich)[grep(":Agree",names(coef(smo2_1B))),2],
                             coeftest(smo2_1C,vcov=sandwich)[grep(":Neither",names(coef(smo2_1C))),2],
                             coeftest(smo2_1C,vcov=sandwich)[grep(":Agree",names(coef(smo2_1C))),2]),
          override.pvalues = list(coeftest(smo2_1B,vcov=sandwich)[grep(":Neither",names(coef(smo2_1B))),4],
                                  coeftest(smo2_1B,vcov=sandwich)[grep(":Agree",names(coef(smo2_1B))),4],
                                  coeftest(smo2_1C,vcov=sandwich)[grep(":Neither",names(coef(smo2_1C))),4],
                                  coeftest(smo2_1C,vcov=sandwich)[grep(":Agree",names(coef(smo2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap,
          # custom.model.names = c("Mun.: Neither","Mun.: Agree",
          #                        "Full: Neither","Full: Agree"),
       custom.model.names = c("Mun.: Agree","Mun.: Neither",
                              "Full: Agree","Full: Neither"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/smo2_1_2_tabular.tex"))

#'
#' # SIFCCT: Mediator Models
#' 
#' ## Knowledge
#'

smm01_10 <- lm(update(knowledge ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm01_1A <- lm(update(knowledge ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm01_1B <- lm(update(knowledge ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm01_1C <- lm(update(knowledge ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(smm01_10,smm01_1A,smm01_1B,smm01_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm01_10,vcov.=vcovHC(smm01_10))[,2],
                             coeftest(smm01_1A,vcov.=vcovHC(smm01_1A))[,2],
                             coeftest(smm01_1B,vcov.=vcovHC(smm01_1B))[,2],
                             coeftest(smm01_1C,vcov.=vcovHC(smm01_1C))[,2]),
          override.pvalues = list(coeftest(smm01_10,vcov.=vcovHC(smm01_10))[,4],
                                  coeftest(smm01_1A,vcov.=vcovHC(smm01_1A))[,4],
                                  coeftest(smm01_1B,vcov.=vcovHC(smm01_1B))[,4],
                                  coeftest(smm01_1C,vcov.=vcovHC(smm01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(smm01_10,smm01_1A,smm01_1B,smm01_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm01_10,vcov.=vcovHC(smm01_10))[,2],
                             coeftest(smm01_1A,vcov.=vcovHC(smm01_1A))[,2],
                             coeftest(smm01_1B,vcov.=vcovHC(smm01_1B))[,2],
                             coeftest(smm01_1C,vcov.=vcovHC(smm01_1C))[,2]),
          override.pvalues = list(coeftest(smm01_10,vcov.=vcovHC(smm01_10))[,4],
                                  coeftest(smm01_1A,vcov.=vcovHC(smm01_1A))[,4],
                                  coeftest(smm01_1B,vcov.=vcovHC(smm01_1B))[,4],
                                  coeftest(smm01_1C,vcov.=vcovHC(smm01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/smm01_1_tabular.tex"))



#'
#' ## Ideology
#'

smm02_10 <- lm(update(ideology ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm02_1A <- lm(update(ideology ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm02_1B <- lm(update(ideology ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm02_1C <- lm(update(ideology ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(smm02_10,smm02_1A,smm02_1B,smm02_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm02_10,vcov.=vcovHC(smm02_10))[,2],
                             coeftest(smm02_1A,vcov.=vcovHC(smm02_1A))[,2],
                             coeftest(smm02_1B,vcov.=vcovHC(smm02_1B))[,2],
                             coeftest(smm02_1C,vcov.=vcovHC(smm02_1C))[,2]),
          override.pvalues = list(coeftest(smm02_10,vcov.=vcovHC(smm02_10))[,4],
                                  coeftest(smm02_1A,vcov.=vcovHC(smm02_1A))[,4],
                                  coeftest(smm02_1B,vcov.=vcovHC(smm02_1B))[,4],
                                  coeftest(smm02_1C,vcov.=vcovHC(smm02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(smm02_10,smm02_1A,smm02_1B,smm02_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm02_10,vcov.=vcovHC(smm02_10))[,2],
                             coeftest(smm02_1A,vcov.=vcovHC(smm02_1A))[,2],
                             coeftest(smm02_1B,vcov.=vcovHC(smm02_1B))[,2],
                             coeftest(smm02_1C,vcov.=vcovHC(smm02_1C))[,2]),
          override.pvalues = list(coeftest(smm02_10,vcov.=vcovHC(smm02_10))[,4],
                                  coeftest(smm02_1A,vcov.=vcovHC(smm02_1A))[,4],
                                  coeftest(smm02_1B,vcov.=vcovHC(smm02_1B))[,4],
                                  coeftest(smm02_1C,vcov.=vcovHC(smm02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/smm02_1_tabular.tex"))

#'
#' ## LDP - DPJ FT
#'

smm03_10 <- lm(update(ldpdpjft ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm03_1A <- lm(update(ldpdpjft ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm03_1B <- lm(update(ldpdpjft ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm03_1C <- lm(update(ldpdpjft ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(smm03_10,smm03_1A,smm03_1B,smm03_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm03_10,vcov.=vcovHC(smm03_10))[,2],
                             coeftest(smm03_1A,vcov.=vcovHC(smm03_1A))[,2],
                             coeftest(smm03_1B,vcov.=vcovHC(smm03_1B))[,2],
                             coeftest(smm03_1C,vcov.=vcovHC(smm03_1C))[,2]),
          override.pvalues = list(coeftest(smm03_10,vcov.=vcovHC(smm03_10))[,4],
                                  coeftest(smm03_1A,vcov.=vcovHC(smm03_1A))[,4],
                                  coeftest(smm03_1B,vcov.=vcovHC(smm03_1B))[,4],
                                  coeftest(smm03_1C,vcov.=vcovHC(smm03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(smm03_10,smm03_1A,smm03_1B,smm03_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm03_10,vcov.=vcovHC(smm03_10))[,2],
                             coeftest(smm03_1A,vcov.=vcovHC(smm03_1A))[,2],
                             coeftest(smm03_1B,vcov.=vcovHC(smm03_1B))[,2],
                             coeftest(smm03_1C,vcov.=vcovHC(smm03_1C))[,2]),
          override.pvalues = list(coeftest(smm03_10,vcov.=vcovHC(smm03_10))[,4],
                                  coeftest(smm03_1A,vcov.=vcovHC(smm03_1A))[,4],
                                  coeftest(smm03_1B,vcov.=vcovHC(smm03_1B))[,4],
                                  coeftest(smm03_1C,vcov.=vcovHC(smm03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
          booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
          file = paste0(projdir,"/out/smm03_1_tabular.tex"))


#'
#' ## Favorability of South Korea
#'

smm04_10 <- lm(update(familiarityFT_KOR ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm04_1A <- lm(update(familiarityFT_KOR ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm04_1B <- lm(update(familiarityFT_KOR ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm04_1C <- lm(update(familiarityFT_KOR ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(smm04_10,smm04_1A,smm04_1B,smm04_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm04_10,vcov.=vcovHC(smm04_10))[,2],
                             coeftest(smm04_1A,vcov.=vcovHC(smm04_1A))[,2],
                             coeftest(smm04_1B,vcov.=vcovHC(smm04_1B))[,2],
                             coeftest(smm04_1C,vcov.=vcovHC(smm04_1C))[,2]),
          override.pvalues = list(coeftest(smm04_10,vcov.=vcovHC(smm04_10))[,4],
                                  coeftest(smm04_1A,vcov.=vcovHC(smm04_1A))[,4],
                                  coeftest(smm04_1B,vcov.=vcovHC(smm04_1B))[,4],
                                  coeftest(smm04_1C,vcov.=vcovHC(smm04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(smm04_10,smm04_1A,smm04_1B,smm04_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm04_10,vcov.=vcovHC(smm04_10))[,2],
                             coeftest(smm04_1A,vcov.=vcovHC(smm04_1A))[,2],
                             coeftest(smm04_1B,vcov.=vcovHC(smm04_1B))[,2],
                             coeftest(smm04_1C,vcov.=vcovHC(smm04_1C))[,2]),
          override.pvalues = list(coeftest(smm04_10,vcov.=vcovHC(smm04_10))[,4],
                                  coeftest(smm04_1A,vcov.=vcovHC(smm04_1A))[,4],
                                  coeftest(smm04_1B,vcov.=vcovHC(smm04_1B))[,4],
                                  coeftest(smm04_1C,vcov.=vcovHC(smm04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
          booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
          file = paste0(projdir,"/out/smm04_1_tabular.tex"))

#'
#' ## Favorability of China
#'

smm05_10 <- lm(update(familiarityFT_CHN ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm05_1A <- lm(update(familiarityFT_CHN ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm05_1B <- lm(update(familiarityFT_CHN ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm05_1C <- lm(update(familiarityFT_CHN ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(smm05_10,smm05_1A,smm05_1B,smm05_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm05_10,vcov.=vcovHC(smm05_10))[,2],
                             coeftest(smm05_1A,vcov.=vcovHC(smm05_1A))[,2],
                             coeftest(smm05_1B,vcov.=vcovHC(smm05_1B))[,2],
                             coeftest(smm05_1C,vcov.=vcovHC(smm05_1C))[,2]),
          override.pvalues = list(coeftest(smm05_10,vcov.=vcovHC(smm05_10))[,4],
                                  coeftest(smm05_1A,vcov.=vcovHC(smm05_1A))[,4],
                                  coeftest(smm05_1B,vcov.=vcovHC(smm05_1B))[,4],
                                  coeftest(smm05_1C,vcov.=vcovHC(smm05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(smm05_10,smm05_1A,smm05_1B,smm05_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm05_10,vcov.=vcovHC(smm05_10))[,2],
                             coeftest(smm05_1A,vcov.=vcovHC(smm05_1A))[,2],
                             coeftest(smm05_1B,vcov.=vcovHC(smm05_1B))[,2],
                             coeftest(smm05_1C,vcov.=vcovHC(smm05_1C))[,2]),
          override.pvalues = list(coeftest(smm05_10,vcov.=vcovHC(smm05_10))[,4],
                                  coeftest(smm05_1A,vcov.=vcovHC(smm05_1A))[,4],
                                  coeftest(smm05_1B,vcov.=vcovHC(smm05_1B))[,4],
                                  coeftest(smm05_1C,vcov.=vcovHC(smm05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
          booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
          file = paste0(projdir,"/out/smm05_1_tabular.tex"))

#'
#' ## Favorability of USA
#'

smm06_10 <- lm(update(familiarityFT_USA ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm06_1A <- lm(update(familiarityFT_USA ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm06_1B <- lm(update(familiarityFT_USA ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm06_1C <- lm(update(familiarityFT_USA ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(smm06_10,smm06_1A,smm06_1B,smm06_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm06_10,vcov.=vcovHC(smm06_10))[,2],
                             coeftest(smm06_1A,vcov.=vcovHC(smm06_1A))[,2],
                             coeftest(smm06_1B,vcov.=vcovHC(smm06_1B))[,2],
                             coeftest(smm06_1C,vcov.=vcovHC(smm06_1C))[,2]),
          override.pvalues = list(coeftest(smm06_10,vcov.=vcovHC(smm06_10))[,4],
                                  coeftest(smm06_1A,vcov.=vcovHC(smm06_1A))[,4],
                                  coeftest(smm06_1B,vcov.=vcovHC(smm06_1B))[,4],
                                  coeftest(smm06_1C,vcov.=vcovHC(smm06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(smm06_10,smm06_1A,smm06_1B,smm06_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm06_10,vcov.=vcovHC(smm06_10))[,2],
                             coeftest(smm06_1A,vcov.=vcovHC(smm06_1A))[,2],
                             coeftest(smm06_1B,vcov.=vcovHC(smm06_1B))[,2],
                             coeftest(smm06_1C,vcov.=vcovHC(smm06_1C))[,2]),
          override.pvalues = list(coeftest(smm06_10,vcov.=vcovHC(smm06_10))[,4],
                                  coeftest(smm06_1A,vcov.=vcovHC(smm06_1A))[,4],
                                  coeftest(smm06_1B,vcov.=vcovHC(smm06_1B))[,4],
                                  coeftest(smm06_1C,vcov.=vcovHC(smm06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/smm06_1_tabular.tex"))


#' 
#' ## Income
#'

smm07_10 <- lm(update(income ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm07_1A <- lm(update(income ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm07_1B <- lm(update(income ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
smm07_1C <- lm(update(income ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(smm07_10,smm07_1A,smm07_1B,smm07_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm07_10,vcov.=vcovHC(smm07_10))[,2],
                             coeftest(smm07_1A,vcov.=vcovHC(smm07_1A))[,2],
                             coeftest(smm07_1B,vcov.=vcovHC(smm07_1B))[,2],
                             coeftest(smm07_1C,vcov.=vcovHC(smm07_1C))[,2]),
          override.pvalues = list(coeftest(smm07_10,vcov.=vcovHC(smm07_10))[,4],
                                  coeftest(smm07_1A,vcov.=vcovHC(smm07_1A))[,4],
                                  coeftest(smm07_1B,vcov.=vcovHC(smm07_1B))[,4],
                                  coeftest(smm07_1C,vcov.=vcovHC(smm07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(smm07_10,smm07_1A,smm07_1B,smm07_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(smm07_10,vcov.=vcovHC(smm07_10))[,2],
                             coeftest(smm07_1A,vcov.=vcovHC(smm07_1A))[,2],
                             coeftest(smm07_1B,vcov.=vcovHC(smm07_1B))[,2],
                             coeftest(smm07_1C,vcov.=vcovHC(smm07_1C))[,2]),
          override.pvalues = list(coeftest(smm07_10,vcov.=vcovHC(smm07_10))[,4],
                                  coeftest(smm07_1A,vcov.=vcovHC(smm07_1A))[,4],
                                  coeftest(smm07_1B,vcov.=vcovHC(smm07_1B))[,4],
                                  coeftest(smm07_1C,vcov.=vcovHC(smm07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/smm07_1_tabular.tex"))

#'
#' # MAIL: Outcome Model 
#'

## Living in Local ZIP since at least age 15 ##

mmo_10 <- lm(update(foreignsuff ~ ., basemod0m), data=mail[which(mail$age - mail$lvlen<=15),])
mmo_1A <- lm(update(foreignsuff ~ ., basemodAm), data=mail[which(mail$age - mail$lvlen<=15),])
mmo_1B <- lm(update(foreignsuff ~ ., basemodBm), data=mail[which(mail$age - mail$lvlen<=15),])
mmo_1C <- lm(update(foreignsuff ~ ., basemodCm), data=mail[which(mail$age - mail$lvlen<=15),])
screenreg(list(mmo_10,mmo_1A,mmo_1B,mmo_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmo_10,vcov.=vcovHC(mmo_10))[,2],
                             coeftest(mmo_1A,vcov.=vcovHC(mmo_1A))[,2],
                             coeftest(mmo_1B,vcov.=vcovHC(mmo_1B))[,2],
                             coeftest(mmo_1C,vcov.=vcovHC(mmo_1C))[,2]),
          override.pvalues = list(coeftest(mmo_10,vcov.=vcovHC(mmo_10))[,4],
                                  coeftest(mmo_1A,vcov.=vcovHC(mmo_1A))[,4],
                                  coeftest(mmo_1B,vcov.=vcovHC(mmo_1B))[,4],
                                  coeftest(mmo_1C,vcov.=vcovHC(mmo_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(mmo_10,mmo_1A,mmo_1B,mmo_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmo_10,vcov.=vcovHC(mmo_10))[,2],
                             coeftest(mmo_1A,vcov.=vcovHC(mmo_1A))[,2],
                             coeftest(mmo_1B,vcov.=vcovHC(mmo_1B))[,2],
                             coeftest(mmo_1C,vcov.=vcovHC(mmo_1C))[,2]),
          override.pvalues = list(coeftest(mmo_10,vcov.=vcovHC(mmo_10))[,4],
                                  coeftest(mmo_1A,vcov.=vcovHC(mmo_1A))[,4],
                                  coeftest(mmo_1B,vcov.=vcovHC(mmo_1B))[,4],
                                  coeftest(mmo_1C,vcov.=vcovHC(mmo_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/mmo_1_tabular.tex"))


#'
#' # MAIL: Outcome Model 2
#'

## Living in Local ZIP since at least age 15 ##

# require(nnet)
# mmo2_10 <- multinom(update(foreignsuff3x ~ ., basemod0), data=mail[which(mail$age - mail$lvlen<=15),])
# mmo2_1A <- multinom(update(foreignsuff3x ~ ., basemodA), data=mail[which(mail$age - mail$lvlen<=15),])
# mmo2_1B <- multinom(update(foreignsuff3x ~ ., basemodB), data=mail[which(mail$age - mail$lvlen<=15),])
# mmo2_1C <- multinom(update(foreignsuff3x ~ ., basemodC), data=mail[which(mail$age - mail$lvlen<=15),])

mail.mlogit <- dfidx(mail[which(mail$age - mail$lvlen<=15),],
                       shape = "wide", choice = "foreignsuff3x")
levels(mail.mlogit$idx$id2) <- c("Disagree","Neither","Agree")
mmo2_10 <- mlogit(outmod0m.mlogit, data=mail.mlogit, reflevel="Disagree")
mmo2_1A <- mlogit(outmodAm.mlogit, data=mail.mlogit, reflevel="Disagree")
mmo2_1B <- mlogit(outmodBm.mlogit, data=mail.mlogit, reflevel="Disagree")
mmo2_1C <- mlogit(outmodCm.mlogit, data=mail.mlogit, reflevel="Disagree")

screenreg(list(mmo2_10,mmo2_1A), digits = 4, # single.row = T,
          override.se = list(coeftest(mmo2_10,vcov=sandwich)[grep(":Neither",names(coef(mmo2_10))),2],
                             coeftest(mmo2_10,vcov=sandwich)[grep(":Agree",names(coef(mmo2_10))),2],
                             coeftest(mmo2_1A,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1A))),2],
                             coeftest(mmo2_1A,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1A))),2]),
          override.pvalues = list(coeftest(mmo2_10,vcov=sandwich)[grep(":Neither",names(coef(mmo2_10))),4],
                                  coeftest(mmo2_10,vcov=sandwich)[grep(":Agree",names(coef(mmo2_10))),4],
                                  coeftest(mmo2_1A,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1A))),4],
                                  coeftest(mmo2_1A,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+", 
          custom.model.names = c("Base: Neither","Base: Agree",
                                 "ZIP: Neither","ZIP: Agree"),
          custom.coef.map = vnmap)
screenreg(list(mmo2_1B,mmo2_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmo2_1B,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1B))),2],
                             coeftest(mmo2_1B,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1B))),2],
                             coeftest(mmo2_1C,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1C))),2],
                             coeftest(mmo2_1C,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1C))),2]),
          override.pvalues = list(coeftest(mmo2_1B,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1B))),4],
                                  coeftest(mmo2_1B,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1B))),4],
                                  coeftest(mmo2_1C,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1C))),4],
                                  coeftest(mmo2_1C,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Neither","Mun.: Agree",
                                 "Full: Neither","Full: Agree"))

#+ include = FALSE
texreg(list(mmo2_10,mmo2_1A), digits = 4, # single.row = T,
          override.se = list(coeftest(mmo2_10,vcov=sandwich)[grep(":Neither",names(coef(mmo2_10))),2],
                             coeftest(mmo2_10,vcov=sandwich)[grep(":Agree",names(coef(mmo2_10))),2],
                             coeftest(mmo2_1A,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1A))),2],
                             coeftest(mmo2_1A,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1A))),2]),
          override.pvalues = list(coeftest(mmo2_10,vcov=sandwich)[grep(":Neither",names(coef(mmo2_10))),4],
                                  coeftest(mmo2_10,vcov=sandwich)[grep(":Agree",names(coef(mmo2_10))),4],
                                  coeftest(mmo2_1A,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1A))),4],
                                  coeftest(mmo2_1A,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger", 
          custom.model.names = c("Base: Neither","Base: Agree",
                                 "ZIP: Neither","ZIP: Agree"),
          custom.coef.map = vnmap,
          booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
          file = paste0(projdir,"/out/mmo2_1_1_tabular.tex"))
texreg(list(mmo2_1B,mmo2_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmo2_1B,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1B))),2],
                             coeftest(mmo2_1B,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1B))),2],
                             coeftest(mmo2_1C,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1C))),2],
                             coeftest(mmo2_1C,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1C))),2]),
          override.pvalues = list(coeftest(mmo2_1B,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1B))),4],
                                  coeftest(mmo2_1B,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1B))),4],
                                  coeftest(mmo2_1C,vcov=sandwich)[grep(":Neither",names(coef(mmo2_1C))),4],
                                  coeftest(mmo2_1C,vcov=sandwich)[grep(":Agree",names(coef(mmo2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Neither","Mun.: Agree",
                                 "Full: Neither","Full: Agree"),
          booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
          file = paste0(projdir,"/out/mmo2_1_2_tabular.tex"))

#'
#' # MAIL: Mediator Models
#' 
#' ## Knowledge
#'

mmm01_10 <- lm(update(knowledge ~ ., basemod0m), data=mail[which(mail$age - mail$lvlen<=15),])
mmm01_1A <- lm(update(knowledge ~ ., basemodAm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm01_1B <- lm(update(knowledge ~ ., basemodBm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm01_1C <- lm(update(knowledge ~ ., basemodCm), data=mail[which(mail$age - mail$lvlen<=15),])
screenreg(list(mmm01_10,mmm01_1A,mmm01_1B,mmm01_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm01_10,vcov.=vcovHC(mmm01_10))[,2],
                             coeftest(mmm01_1A,vcov.=vcovHC(mmm01_1A))[,2],
                             coeftest(mmm01_1B,vcov.=vcovHC(mmm01_1B))[,2],
                             coeftest(mmm01_1C,vcov.=vcovHC(mmm01_1C))[,2]),
          override.pvalues = list(coeftest(mmm01_10,vcov.=vcovHC(mmm01_10))[,4],
                                  coeftest(mmm01_1A,vcov.=vcovHC(mmm01_1A))[,4],
                                  coeftest(mmm01_1B,vcov.=vcovHC(mmm01_1B))[,4],
                                  coeftest(mmm01_1C,vcov.=vcovHC(mmm01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(mmm01_10,mmm01_1A,mmm01_1B,mmm01_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm01_10,vcov.=vcovHC(mmm01_10))[,2],
                             coeftest(mmm01_1A,vcov.=vcovHC(mmm01_1A))[,2],
                             coeftest(mmm01_1B,vcov.=vcovHC(mmm01_1B))[,2],
                             coeftest(mmm01_1C,vcov.=vcovHC(mmm01_1C))[,2]),
          override.pvalues = list(coeftest(mmm01_10,vcov.=vcovHC(mmm01_10))[,4],
                                  coeftest(mmm01_1A,vcov.=vcovHC(mmm01_1A))[,4],
                                  coeftest(mmm01_1B,vcov.=vcovHC(mmm01_1B))[,4],
                                  coeftest(mmm01_1C,vcov.=vcovHC(mmm01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/mmm01_1_tabular.tex"))

#'
#' ## Ideology
#'

mmm02_10 <- lm(update(ideology ~ ., basemod0m), data=mail[which(mail$age - mail$lvlen<=15),])
mmm02_1A <- lm(update(ideology ~ ., basemodAm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm02_1B <- lm(update(ideology ~ ., basemodBm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm02_1C <- lm(update(ideology ~ ., basemodCm), data=mail[which(mail$age - mail$lvlen<=15),])
screenreg(list(mmm02_10,mmm02_1A,mmm02_1B,mmm02_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm02_10,vcov.=vcovHC(mmm02_10))[,2],
                             coeftest(mmm02_1A,vcov.=vcovHC(mmm02_1A))[,2],
                             coeftest(mmm02_1B,vcov.=vcovHC(mmm02_1B))[,2],
                             coeftest(mmm02_1C,vcov.=vcovHC(mmm02_1C))[,2]),
          override.pvalues = list(coeftest(mmm02_10,vcov.=vcovHC(mmm02_10))[,4],
                                  coeftest(mmm02_1A,vcov.=vcovHC(mmm02_1A))[,4],
                                  coeftest(mmm02_1B,vcov.=vcovHC(mmm02_1B))[,4],
                                  coeftest(mmm02_1C,vcov.=vcovHC(mmm02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(mmm02_10,mmm02_1A,mmm02_1B,mmm02_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm02_10,vcov.=vcovHC(mmm02_10))[,2],
                             coeftest(mmm02_1A,vcov.=vcovHC(mmm02_1A))[,2],
                             coeftest(mmm02_1B,vcov.=vcovHC(mmm02_1B))[,2],
                             coeftest(mmm02_1C,vcov.=vcovHC(mmm02_1C))[,2]),
          override.pvalues = list(coeftest(mmm02_10,vcov.=vcovHC(mmm02_10))[,4],
                                  coeftest(mmm02_1A,vcov.=vcovHC(mmm02_1A))[,4],
                                  coeftest(mmm02_1B,vcov.=vcovHC(mmm02_1B))[,4],
                                  coeftest(mmm02_1C,vcov.=vcovHC(mmm02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
          booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
          file = paste0(projdir,"/out/mmm02_1_tabular.tex"))

#'
#' ## LDP - DPJ FT
#'

mmm03_10 <- lm(update(ldpdpjft ~ ., basemod0m), data=mail[which(mail$age - mail$lvlen<=15),])
mmm03_1A <- lm(update(ldpdpjft ~ ., basemodAm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm03_1B <- lm(update(ldpdpjft ~ ., basemodBm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm03_1C <- lm(update(ldpdpjft ~ ., basemodCm), data=mail[which(mail$age - mail$lvlen<=15),])
screenreg(list(mmm03_10,mmm03_1A,mmm03_1B,mmm03_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm03_10,vcov.=vcovHC(mmm03_10))[,2],
                             coeftest(mmm03_1A,vcov.=vcovHC(mmm03_1A))[,2],
                             coeftest(mmm03_1B,vcov.=vcovHC(mmm03_1B))[,2],
                             coeftest(mmm03_1C,vcov.=vcovHC(mmm03_1C))[,2]),
          override.pvalues = list(coeftest(mmm03_10,vcov.=vcovHC(mmm03_10))[,4],
                                  coeftest(mmm03_1A,vcov.=vcovHC(mmm03_1A))[,4],
                                  coeftest(mmm03_1B,vcov.=vcovHC(mmm03_1B))[,4],
                                  coeftest(mmm03_1C,vcov.=vcovHC(mmm03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(mmm03_10,mmm03_1A,mmm03_1B,mmm03_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm03_10,vcov.=vcovHC(mmm03_10))[,2],
                             coeftest(mmm03_1A,vcov.=vcovHC(mmm03_1A))[,2],
                             coeftest(mmm03_1B,vcov.=vcovHC(mmm03_1B))[,2],
                             coeftest(mmm03_1C,vcov.=vcovHC(mmm03_1C))[,2]),
          override.pvalues = list(coeftest(mmm03_10,vcov.=vcovHC(mmm03_10))[,4],
                                  coeftest(mmm03_1A,vcov.=vcovHC(mmm03_1A))[,4],
                                  coeftest(mmm03_1B,vcov.=vcovHC(mmm03_1B))[,4],
                                  coeftest(mmm03_1C,vcov.=vcovHC(mmm03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/mmm03_1_tabular.tex"))

#'
#' ## Favorability of South Korea
#'

mmm04_10 <- lm(update(familiarityFT_KOR ~ ., basemod0m), data=mail[which(mail$age - mail$lvlen<=15),])
mmm04_1A <- lm(update(familiarityFT_KOR ~ ., basemodAm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm04_1B <- lm(update(familiarityFT_KOR ~ ., basemodBm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm04_1C <- lm(update(familiarityFT_KOR ~ ., basemodCm), data=mail[which(mail$age - mail$lvlen<=15),])
screenreg(list(mmm04_10,mmm04_1A,mmm04_1B,mmm04_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm04_10,vcov.=vcovHC(mmm04_10))[,2],
                             coeftest(mmm04_1A,vcov.=vcovHC(mmm04_1A))[,2],
                             coeftest(mmm04_1B,vcov.=vcovHC(mmm04_1B))[,2],
                             coeftest(mmm04_1C,vcov.=vcovHC(mmm04_1C))[,2]),
          override.pvalues = list(coeftest(mmm04_10,vcov.=vcovHC(mmm04_10))[,4],
                                  coeftest(mmm04_1A,vcov.=vcovHC(mmm04_1A))[,4],
                                  coeftest(mmm04_1B,vcov.=vcovHC(mmm04_1B))[,4],
                                  coeftest(mmm04_1C,vcov.=vcovHC(mmm04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(mmm04_10,mmm04_1A,mmm04_1B,mmm04_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm04_10,vcov.=vcovHC(mmm04_10))[,2],
                             coeftest(mmm04_1A,vcov.=vcovHC(mmm04_1A))[,2],
                             coeftest(mmm04_1B,vcov.=vcovHC(mmm04_1B))[,2],
                             coeftest(mmm04_1C,vcov.=vcovHC(mmm04_1C))[,2]),
          override.pvalues = list(coeftest(mmm04_10,vcov.=vcovHC(mmm04_10))[,4],
                                  coeftest(mmm04_1A,vcov.=vcovHC(mmm04_1A))[,4],
                                  coeftest(mmm04_1B,vcov.=vcovHC(mmm04_1B))[,4],
                                  coeftest(mmm04_1C,vcov.=vcovHC(mmm04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
          booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
          file = paste0(projdir,"/out/mmm04_1_tabular.tex"))

#'
#' ## Favorability of China
#'

mmm05_10 <- lm(update(familiarityFT_CHN ~ ., basemod0m), data=mail[which(mail$age - mail$lvlen<=15),])
mmm05_1A <- lm(update(familiarityFT_CHN ~ ., basemodAm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm05_1B <- lm(update(familiarityFT_CHN ~ ., basemodBm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm05_1C <- lm(update(familiarityFT_CHN ~ ., basemodCm), data=mail[which(mail$age - mail$lvlen<=15),])
screenreg(list(mmm05_10,mmm05_1A,mmm05_1B,mmm05_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm05_10,vcov.=vcovHC(mmm05_10))[,2],
                             coeftest(mmm05_1A,vcov.=vcovHC(mmm05_1A))[,2],
                             coeftest(mmm05_1B,vcov.=vcovHC(mmm05_1B))[,2],
                             coeftest(mmm05_1C,vcov.=vcovHC(mmm05_1C))[,2]),
          override.pvalues = list(coeftest(mmm05_10,vcov.=vcovHC(mmm05_10))[,4],
                                  coeftest(mmm05_1A,vcov.=vcovHC(mmm05_1A))[,4],
                                  coeftest(mmm05_1B,vcov.=vcovHC(mmm05_1B))[,4],
                                  coeftest(mmm05_1C,vcov.=vcovHC(mmm05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(mmm05_10,mmm05_1A,mmm05_1B,mmm05_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm05_10,vcov.=vcovHC(mmm05_10))[,2],
                             coeftest(mmm05_1A,vcov.=vcovHC(mmm05_1A))[,2],
                             coeftest(mmm05_1B,vcov.=vcovHC(mmm05_1B))[,2],
                             coeftest(mmm05_1C,vcov.=vcovHC(mmm05_1C))[,2]),
          override.pvalues = list(coeftest(mmm05_10,vcov.=vcovHC(mmm05_10))[,4],
                                  coeftest(mmm05_1A,vcov.=vcovHC(mmm05_1A))[,4],
                                  coeftest(mmm05_1B,vcov.=vcovHC(mmm05_1B))[,4],
                                  coeftest(mmm05_1C,vcov.=vcovHC(mmm05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
          booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
          file = paste0(projdir,"/out/mmm05_1_tabular.tex"))


#'
#' ## Favorability of USA
#'

mmm06_10 <- lm(update(familiarityFT_USA ~ ., basemod0m), data=mail[which(mail$age - mail$lvlen<=15),])
mmm06_1A <- lm(update(familiarityFT_USA ~ ., basemodAm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm06_1B <- lm(update(familiarityFT_USA ~ ., basemodBm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm06_1C <- lm(update(familiarityFT_USA ~ ., basemodCm), data=mail[which(mail$age - mail$lvlen<=15),])
screenreg(list(mmm06_10,mmm06_1A,mmm06_1B,mmm06_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm06_10,vcov.=vcovHC(mmm06_10))[,2],
                             coeftest(mmm06_1A,vcov.=vcovHC(mmm06_1A))[,2],
                             coeftest(mmm06_1B,vcov.=vcovHC(mmm06_1B))[,2],
                             coeftest(mmm06_1C,vcov.=vcovHC(mmm06_1C))[,2]),
          override.pvalues = list(coeftest(mmm06_10,vcov.=vcovHC(mmm06_10))[,4],
                                  coeftest(mmm06_1A,vcov.=vcovHC(mmm06_1A))[,4],
                                  coeftest(mmm06_1B,vcov.=vcovHC(mmm06_1B))[,4],
                                  coeftest(mmm06_1C,vcov.=vcovHC(mmm06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(mmm06_10,mmm06_1A,mmm06_1B,mmm06_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm06_10,vcov.=vcovHC(mmm06_10))[,2],
                             coeftest(mmm06_1A,vcov.=vcovHC(mmm06_1A))[,2],
                             coeftest(mmm06_1B,vcov.=vcovHC(mmm06_1B))[,2],
                             coeftest(mmm06_1C,vcov.=vcovHC(mmm06_1C))[,2]),
          override.pvalues = list(coeftest(mmm06_10,vcov.=vcovHC(mmm06_10))[,4],
                                  coeftest(mmm06_1A,vcov.=vcovHC(mmm06_1A))[,4],
                                  coeftest(mmm06_1B,vcov.=vcovHC(mmm06_1B))[,4],
                                  coeftest(mmm06_1C,vcov.=vcovHC(mmm06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/mmm06_1_tabular.tex"))


#' 
#' ## Income
#'

mmm07_10 <- lm(update(income ~ ., basemod0m), data=mail[which(mail$age - mail$lvlen<=15),])
mmm07_1A <- lm(update(income ~ ., basemodAm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm07_1B <- lm(update(income ~ ., basemodBm), data=mail[which(mail$age - mail$lvlen<=15),])
mmm07_1C <- lm(update(income ~ ., basemodCm), data=mail[which(mail$age - mail$lvlen<=15),])
screenreg(list(mmm07_10,mmm07_1A,mmm07_1B,mmm07_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm07_10,vcov.=vcovHC(mmm07_10))[,2],
                             coeftest(mmm07_1A,vcov.=vcovHC(mmm07_1A))[,2],
                             coeftest(mmm07_1B,vcov.=vcovHC(mmm07_1B))[,2],
                             coeftest(mmm07_1C,vcov.=vcovHC(mmm07_1C))[,2]),
          override.pvalues = list(coeftest(mmm07_10,vcov.=vcovHC(mmm07_10))[,4],
                                  coeftest(mmm07_1A,vcov.=vcovHC(mmm07_1A))[,4],
                                  coeftest(mmm07_1B,vcov.=vcovHC(mmm07_1B))[,4],
                                  coeftest(mmm07_1C,vcov.=vcovHC(mmm07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(mmm07_10,mmm07_1A,mmm07_1B,mmm07_1C), digits = 4, # single.row = T,
          override.se = list(coeftest(mmm07_10,vcov.=vcovHC(mmm07_10))[,2],
                             coeftest(mmm07_1A,vcov.=vcovHC(mmm07_1A))[,2],
                             coeftest(mmm07_1B,vcov.=vcovHC(mmm07_1B))[,2],
                             coeftest(mmm07_1C,vcov.=vcovHC(mmm07_1C))[,2]),
          override.pvalues = list(coeftest(mmm07_10,vcov.=vcovHC(mmm07_10))[,4],
                                  coeftest(mmm07_1A,vcov.=vcovHC(mmm07_1A))[,4],
                                  coeftest(mmm07_1B,vcov.=vcovHC(mmm07_1B))[,4],
                                  coeftest(mmm07_1C,vcov.=vcovHC(mmm07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"),
          booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
          file = paste0(projdir,"/out/mmm07_1_tabular.tex"))

#'
#' # Plotting
#'

extout <- function(gender,ageset,sub=1) {
  
  if (gender=="Male") sifcct$gender <- sifcct$female
  if (gender=="Female") sifcct$gender <- sifcct$male
  sifcct$ageset <- (sifcct$age - ageset)/10
  
  if (sub==1) {
    modset <- lm(foreignsuff ~ edu2 * gender * ageset + lvpr + zip_did + sqrt(c10_sreg_fper) + 
                   I(c10_sreg_edu_ugsP/10) + didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                   as.factor(wave), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
    subname = "Stayed"
  } else {
    modset <- lm(foreignsuff ~ edu2 * gender * ageset + lvpr + as.factor(wave), 
                 data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
    subname = "Moved"
  }
  
  res <- c(gender,ageset,coef(modset)[2],
           coefci(modset, vcov.=vcovHC(modset), level = 0.95)[2,],
           coefci(modset, vcov.=vcovHC(modset), level = 0.90)[2,],
           coeftest(modset, vcov.=vcovHC(modset))[2,c(2,4)],
           subname)
  names(res) <- c("gender","age","est","lci95","uci95","lci90","uci90","se","p","lv")
  
  return(res)
  
}

outdt0 <- rbind(extout("Female",25,1),
                 extout("Female",35,1),
                 extout("Female",45,1),
                 extout("Female",55,1),
                 extout("Female",65,1),
                 extout("Male",25,1),
                 extout("Male",35,1),
                 extout("Male",45,1),
                 extout("Male",55,1),
                 extout("Male",65,1))
outdt0 <- as.data.frame(outdt0)
for(i in 2:9) outdt0[,i] <- as.numeric(outdt0[,i])
outdt0$gender <- factor(outdt0$gender, levels=unique(outdt0$gender))
summary(outdt0)

extout <- function(gender,ageset,sub=1) {
  
  if (gender=="Male") mail$gender <- mail$female
  if (gender=="Female") mail$gender <- mail$male
  mail$ageset <- (mail$age - ageset)/10
  
  if (sub==1) {
    modset <- lm(foreignsuff ~ edu2 * gender * ageset + lvpr + zip_did + sqrt(c10_sreg_fper) + 
                   I(c10_sreg_edu_ugsP/10) + didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10),
                 data=mail[which(mail$age - mail$lvlen<=15),])
    subname = "Stayed"
  } else {
    modset <- lm(foreignsuff ~ edu2 * gender * ageset + lvpr, 
                 data=mail[which(mail$age - mail$lvlen>=23),])
    subname = "Moved"
  }

  res <- c(gender,ageset,coef(modset)[2],
           coefci(modset, vcov.=vcovHC(modset), level = 0.95)[2,],
           coefci(modset, vcov.=vcovHC(modset), level = 0.90)[2,],
           coeftest(modset, vcov.=vcovHC(modset))[2,c(2,4)],
           subname)
  names(res) <- c("gender","age","est","lci95","uci95","lci90","uci90","se","p","lv")
  
  return(res)
  
}

outdtm <- rbind(extout("Female",25,1),
                 extout("Female",35,1),
                 extout("Female",45,1),
                 extout("Female",55,1),
                 extout("Female",65,1),
                 extout("Male",25,1),
                 extout("Male",35,1),
                 extout("Male",45,1),
                 extout("Male",55,1),
                 extout("Male",65,1))
outdtm <- as.data.frame(outdtm)
for(i in 2:9) outdtm[,i] <- as.numeric(outdtm[,i])
outdtm$gender <- factor(outdtm$gender, levels=unique(outdtm$gender))
summary(outdtm)

outdt0$data <- "Online"
outdtm$data <- "Mail-in"

visdt <- rbind(outdt0,outdtm)
visdt$data <- factor(visdt$data, levels=c("Online","Mail-in"))

visdt$pstar <- factor(ifelse(visdt$p>=.1,"n.s.",ifelse(visdt$p>=.05,"p<.1","p<.05")),
                       levels = c("p<.05","p<.1","n.s."))

saveRDS(subset(visdt, data=="Mail-in"), paste0(projdir, "/out/visdt_mail_ols.rds"))

require(ggplot2)
p <- ggplot(visdt, aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour="1",alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour="1",alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=lv, colour="1",alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ data) +
  scale_y_continuous(breaks = c(-0.1,-0.05,0.00,0.05)) + 
  scale_shape_discrete(name="Change in residece after university") +
  scale_color_manual(name="Change in residece after university",values=rep("black", 1)) +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). Outcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/mailineffectplot.png"),p,width=8,height=5)

## Multinomial Logit ##

extout <- function(gender,ageset,sub=1) {
  
  if (gender=="Male") sifcct$gender <- sifcct$female
  if (gender=="Female") sifcct$gender <- sifcct$male
  sifcct$ageset <- (sifcct$age - ageset)/10
  
  if (sub==1) {
    # modset <- multinom(foreignsuff3x ~ edu2 * gender * ageset + lvpr + zip_did + sqrt(c10_sreg_fper) + 
    #                      I(c10_sreg_edu_ugsP/10) + didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
    #                      as.factor(wave), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),],
    #                    Hess = TRUE)
    sifcct.mlogit.tmp <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen<=15),],
                           shape = "wide", choice = "foreignsuff3x")
    # levels(sifcct.mlogit.tmp$idx$id2) <- c("Disagree","Neither","Agree")
    modset <- mlogit(foreignsuff3x ~ 0 | edu2 * gender * ageset + lvpr + zip_did + sqrt(c10_sreg_fper) + 
                         I(c10_sreg_edu_ugsP/10) + didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                         as.factor(wave), data=sifcct.mlogit.tmp, reflevel="Disagree")
    subname = "Stayed"
  } else {
    # modset <- multinom(foreignsuff3x ~ edu2 * gender * ageset + lvpr + as.factor(wave), 
    #                    data=sifcct[which(sifcct$age - sifcct$lvlen>=23),],
    #                    Hess = TRUE)
    sifcct.mlogit.tmp <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen>=23),],
                           shape = "wide", choice = "foreignsuff3x")
    # levels(sifcct.mlogit.tmp$idx$id2) <- c("Disagree","Neither","Agree")
    modset <- mlogit(foreignsuff3x ~ 0 | edu2 * gender * ageset + lvpr + as.factor(wave), 
                       data=sifcct.mlogit.tmp, reflevel="Disagree")
    subname = "Moved"
  }

  # modres <- extract(modset)
  
  # res <- c(gender,ageset,modres@coef[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@coef[grep("^Agree: edu2$",modres@coef.names)] - qnorm(0.975)*modres@se[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@coef[grep("^Agree: edu2$",modres@coef.names)] + qnorm(0.975)*modres@se[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@coef[grep("^Agree: edu2$",modres@coef.names)] - qnorm(0.95)*modres@se[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@coef[grep("^Agree: edu2$",modres@coef.names)] + qnorm(0.95)*modres@se[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@se[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@pvalues[grep("^Agree: edu2$",modres@coef.names)],
  #          subname)
  res <- c(gender,ageset,coef(modset)[3],
           coefci(modset, vcov=sandwich, level = 0.95)[3,],
           coefci(modset, vcov=sandwich, level = 0.90)[3,],
           coeftest(modset, vcov=sandwich)[3,c(2,4)],
           subname)
  
  names(res) <- c("gender","age","est","lci95","uci95","lci90","uci90","se","p","lv")
  
  return(res)
  
}

outdt0 <- rbind(extout("Female",25,1),
                 extout("Female",35,1),
                 extout("Female",45,1),
                 extout("Female",55,1),
                 extout("Female",65,1),
                 extout("Male",25,1),
                 extout("Male",35,1),
                 extout("Male",45,1),
                 extout("Male",55,1),
                 extout("Male",65,1))
outdt0 <- as.data.frame(outdt0)
for(i in 2:9) outdt0[,i] <- as.numeric(outdt0[,i])
outdt0$gender <- factor(outdt0$gender, levels=unique(outdt0$gender))
summary(outdt0)

extout <- function(gender,ageset,sub=1) {
  
  if (gender=="Male") mail$gender <- mail$female
  if (gender=="Female") mail$gender <- mail$male
  mail$ageset <- (mail$age - ageset)/10
  
  if (sub==1) {
    # modset <- multinom(foreignsuff3x ~ edu2 * gender * ageset + lvpr + zip_did + sqrt(c10_sreg_fper) +
    #                      I(c10_sreg_edu_ugsP/10) + didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10),
    #                    data=mail[which(mail$age - mail$lvlen<=15),],
    #                    Hess = TRUE)
    mail.mlogit.tmp <- dfidx(mail[which(mail$age - mail$lvlen<=15),],
                               shape = "wide", choice = "foreignsuff3x")
    # levels(mail.mlogit.tmp$idx$id2) <- c("Disagree","Neither","Agree")
    modset <- mlogit(foreignsuff3x ~ 0 | edu2 * gender * ageset + lvpr + zip_did + sqrt(c10_sreg_fper) + 
                       I(c10_sreg_edu_ugsP/10) + didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10), 
                     data=mail.mlogit.tmp, reflevel="Disagree")
    subname = "Stayed"
  } else {
    # modset <- multinom(foreignsuff3x ~ edu2 * gender * ageset + lvpr, 
    #                    data=mail[which(mail$age - mail$lvlen>=23),],
    #                    Hess = TRUE)
    mail.mlogit.tmp <- dfidx(mail[which(mail$age - mail$lvlen>=23),],
                               shape = "wide", choice = "foreignsuff3x")
    # levels(mail.mlogit.tmp$idx$id2) <- c("Disagree","Neither","Agree")
    modset <- mlogit(foreignsuff3x ~ 0 | edu2 * gender * ageset + lvpr, 
                     data=mail.mlogit.tmp, reflevel="Disagree")
    subname = "Moved"
  }
  
  # modres <- extract(modset)
  
  # res <- c(gender,ageset,modres@coef[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@coef[grep("^Agree: edu2$",modres@coef.names)] - qnorm(0.975)*modres@se[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@coef[grep("^Agree: edu2$",modres@coef.names)] + qnorm(0.975)*modres@se[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@coef[grep("^Agree: edu2$",modres@coef.names)] - qnorm(0.95)*modres@se[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@coef[grep("^Agree: edu2$",modres@coef.names)] + qnorm(0.95)*modres@se[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@se[grep("^Agree: edu2$",modres@coef.names)],
  #          modres@pvalues[grep("^Agree: edu2$",modres@coef.names)],
  #          subname)
  res <- c(gender,ageset,coef(modset)[3],
           coefci(modset, vcov=sandwich, level = 0.95)[3,],
           coefci(modset, vcov=sandwich, level = 0.90)[3,],
           coeftest(modset, vcov=sandwich)[3,c(2,4)],
           subname)
  
  names(res) <- c("gender","age","est","lci95","uci95","lci90","uci90","se","p","lv")
  
  return(res)
  
}

outdtm <- rbind(extout("Female",25,1),
                 extout("Female",35,1),
                 extout("Female",45,1),
                 extout("Female",55,1),
                 extout("Female",65,1),
                 extout("Male",25,1),
                 extout("Male",35,1),
                 extout("Male",45,1),
                 extout("Male",55,1),
                 extout("Male",65,1))
outdtm <- as.data.frame(outdtm)
for(i in 2:9) outdtm[,i] <- as.numeric(outdtm[,i])
outdtm$gender <- factor(outdtm$gender, levels=unique(outdtm$gender))
summary(outdtm)

outdt0$data <- "Online"
outdtm$data <- "Mail-in"

visdt <- rbind(outdt0,outdtm)
visdt$data <- factor(visdt$data, levels=c("Online","Mail-in"))

visdt$pstar <- factor(ifelse(visdt$p>=.1,"n.s.",ifelse(visdt$p>=.05,"p<.1","p<.05")),
                       levels = c("p<.05","p<.1","n.s."))

saveRDS(subset(visdt, data=="Mail-in"), paste0(projdir, "/out/visdt_mail_multinom.rds"))

require(ggplot2)
p <- ggplot(visdt, aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour="1",alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour="1",alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=lv, colour="1",alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ data) +
  #scale_y_continuous(breaks = c(-0.1,-0.05,0.00,0.05)) + 
  scale_shape_discrete(name="Change in residece after university") +
  scale_color_manual(name="Change in residece after university",values=rep("black", 1)) +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). Outcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p


ggsave(paste0(projdir,"/out/mailineffectplot_multinom.png"),p,width=8,height=5)

#'
#' # Save Image
#'

#+ eval=FALSE
save.image(file=paste0(projdir,"/out/heavy/analysis_1_original_mail_v5.RData"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/analysis_1_original_mail_v5.R', rmarkdown::pdf_document(latex_engine="xelatex", extra_dependencies = list(bookmark=NULL, xltxtra=NULL, zxjatype=NULL, zxjafont=c("ipa"))), encoding = 'UTF-8')
# rmarkdown::render('./src/analysis_1_original_mail_v5.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
