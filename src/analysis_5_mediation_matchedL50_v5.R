#' ---
#' title: "Analysis 5: Mediation Analysis (Matched with Lambda = 50km)"
#' author: "Fan Lu & Gento Kato"
#' date: "January 26, 2020"
#' ---
#' 
#' # Preparation 

## Clean Up Space
rm(list=ls())

#+
## Set Working Directory (Automatically) ##
require(rstudioapi); require(rprojroot)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)

## Matched/Unmatched Data Locations
datadir0 <- paste0(projdir, "/data/sifcct_unmatched_v5.rds")
datadir1 <- paste0(projdir, "/data/sifcct_matched_1_all_v5.rds")
datadir2 <- paste0(projdir, "/data/sifcct_matched_2_all_v5.rds")
datadir3 <- paste0(projdir, "/data/sifcct_matched_3_all_v5.rds")
datadir4 <- paste0(projdir, "/data/sifcct_matched_4_all_v5.rds")
datadir5 <- paste0(projdir, "/data/sifcct_matched_5_all_v5.rds")


#+
## packages
require(sandwich)
require(lmtest)
require(MASS)
require(ggplot2)
require(texreg)
require(mediation)

##
vnmap <- list("edu2" = "University education",
              "female" = "Gender (female)",
              "male" = "Gender (male)",
              "age2" = "Age 50s or older",
              "agex" = "Age (by 10 years)",
              "knowledge" = "Political Knowledge",
              "ideology" = "Ideology",
              "ldpdpjft" = "LDP -DPJ Feeling Thermometer",
              "familiarityFT_KOR" = "South Korea Feeling Thermometer",
              "familiarityFT_CHN" = "China Feeling Thermometer",
              "familiarityFT_USA" = "United States Feeling Thermometer",
              "income" = "Income",
              "edu2:female" = "University * Female",
              "edu2:male" = "University * Male",
              "edu2:age2" = "University * >=50s",
              "edu2:agex" = "University * Age",
              "edu2:female:age2" = "University * Female * >=50s",
              "edu2:male:age2" = "University * Male * >=50s",
              "edu2:female:agex" = "University * Female * Age",
              "edu2:male:agex" = "University * Male * Age",
              "female:knowledge" = "Knowledge * Female",
              "male:knowledge" = "Knowledge * Male",
              "age2:knowledge" = "Knowledge * >=50s",
              "agex:knowledge" = "Knowledge * Age",
              "female:age2:knowledge" = "Knowledge * Female * >=50s",
              "male:age2:knowledge" = "Knowledge * Male * >=50s",
              "female:agex:knowledge" = "Knowledge * Female * Age",
              "male:agex:knowledge" = "Knowledge * Male * Age",
              "female:ideology" = "Ideology * Female",
              "male:ideology" = "Ideology * Male",
              "age2:ideology" = "Ideology * >=50s",
              "agex:ideology" = "Ideology * Age",
              "female:age2:ideology" = "Ideology * Female * >=50s",
              "male:age2:ideology" = "Ideology * Male * >=50s",
              "female:agex:ideology" = "Ideology * Female * Age",
              "male:agex:ideology" = "Ideology * Male * Age",
              "female:ldpdpjft" = "LDP - DPJ FT * Female",
              "male:ldpdpjft" = "LDP - DPJ FT * Male",
              "age2:ldpdpjft" = "LDP - DPJ FT * >=50s",
              "agex:ldpdpjft" = "LDP - DPJ FT * Age",
              "female:age2:ldpdpjft" = "LDP - DPJ FT * Female * >=50s",
              "male:age2:ldpdpjft" = "LDP - DPJ FT * Male * >=50s",
              "female:agex:ldpdpjft" = "LDP - DPJ FT * Female * Age",
              "male:agex:ldpdpjft" = "LDP - DPJ FT * Male * Age",
              "female:familiarityFT_KOR" = "South Korea FT * Female",
              "male:familiarityFT_KOR" = "South Korea FT * Male",
              "age2:familiarityFT_KOR" = "South Korea FT * >=50s",
              "agex:familiarityFT_KOR" = "South Korea FT * Age",
              "female:age2:familiarityFT_KOR" = "South Korea FT * Female * >=50s",
              "male:age2:familiarityFT_KOR" = "South Korea FT * Male * >=50s",
              "female:agex:familiarityFT_KOR" = "South Korea FT * Female * Age",
              "male:agex:familiarityFT_KOR" = "South Korea FT * Male * Age",
              "female:familiarityFT_CHN" = "China FT * Female",
              "male:familiarityFT_CHN" = "China FT * Male",
              "age2:familiarityFT_CHN" = "China FT * >=50s",
              "agex:familiarityFT_CHN" = "China FT * Age",
              "female:age2:familiarityFT_CHN" = "China FT * Female * >=50s",
              "male:age2:familiarityFT_CHN" = "China FT * Male * >=50s",
              "female:agex:familiarityFT_CHN" = "China FT * Female * Age",
              "male:agex:familiarityFT_CHN" = "China FT * Male * Age",
              "female:familiarityFT_USA" = "United States FT * Female",
              "male:familiarityFT_USA" = "United States FT * Male",
              "age2:familiarityFT_USA" = "United States FT * >=50s",
              "agex:familiarityFT_USA" = "United States FT * Age",
              "female:age2:familiarityFT_USA" = "United States FT * Female * >=50s",
              "male:age2:familiarityFT_USA" = "United States FT * Male * >=50s",
              "female:agex:familiarityFT_USA" = "United States FT * Female * Age",
              "male:agex:familiarityFT_USA" = "United States FT * Male * Age",
              "female:income" = "Income * Female",
              "male:income" = "Income * Male",
              "age2:income" = "Income * >=50s",
              "age:income" = "Income * Age",
              "female:age2:income" = "Income * Female * >=50s",
              "male:age2:income" = "Income * Male * >=50s",
              "female:agex:income" = "Income * Female * Age",
              "male:agex:income" = "Income * Male * Age",
              "female:age2" = "Female * >=50s",
              "male:age2" = "Male * >=50s",
              "female:agex" = "Female * Age",
              "male:agex" = "Male * Age",
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
#' # Models
#' 
#' ## SIFCCT (Matched with Lambda = 50km)
#'

#+ eval=FALSE

sifcct <- readRDS(datadir2)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)

#'
#' ### Knowledge
#'

#+ eval=FALSE

## Outcome Model 
s2mout01_1C <- lm(foreignsuff  ~ edu2*male*agex + knowledge*male*agex + lvpr +  
                    zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                    didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                    as.factor(wave), data=sifcct)

## Mediator Model
s2mm01_1C <- lm(knowledge  ~ edu2*male*agex + lvpr +  
                  zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                  didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                  as.factor(wave), data=sifcct)

## Table
screenreg(list(s2mm01_1C,s2mout01_1C), digits = 4, single.row = T,
          override.se = list(coeftest(s2mm01_1C,vcov.=vcovHC(s2mm01_1C))[,2],
                             coeftest(s2mout01_1C,vcov.=vcovHC(s2mout01_1C))[,2]),
          override.pvalues = list(coeftest(s2mm01_1C,vcov.=vcovHC(s2mm01_1C))[,4],
                                  coeftest(s2mout01_1C,vcov.=vcovHC(s2mout01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Mediator","Outcome"))

#+ include = FALSE, eval = FALSE
texreg(list(s2mm01_1C,s2mout01_1C), digits = 4, single.row = T,
       override.se = list(coeftest(s2mm01_1C,vcov.=vcovHC(s2mm01_1C))[,2],
                          coeftest(s2mout01_1C,vcov.=vcovHC(s2mout01_1C))[,2]),
       override.pvalues = list(coeftest(s2mm01_1C,vcov.=vcovHC(s2mm01_1C))[,4],
                               coeftest(s2mout01_1C,vcov.=vcovHC(s2mout01_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Mediator","Outcome"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2medout01_1_tabular.tex"))

## Causal Mediation Analysis
set.seed(2345)

## Female
medout01_f25 <- mediate(s2mm01_1C, s2mout01_1C, treat = "edu2", 
                        mediator = "knowledge", 
                        covariates = list(male = 0, agex = -2), robustSE = TRUE)
summary(medout01_f25)
medout01_f35 <- mediate(s2mm01_1C, s2mout01_1C, treat = "edu2", 
                        mediator = "knowledge", 
                        covariates = list(male = 0, agex = -1), robustSE = TRUE)
summary(medout01_f35)
medout01_f45 <- mediate(s2mm01_1C, s2mout01_1C, treat = "edu2", 
                        mediator = "knowledge", 
                        covariates = list(male = 0, agex = 0), robustSE = TRUE)
summary(medout01_f45)
medout01_f55 <- mediate(s2mm01_1C, s2mout01_1C, treat = "edu2", 
                        mediator = "knowledge", 
                        covariates = list(male = 0, agex = 1), robustSE = TRUE)
summary(medout01_f55)
medout01_f65 <- mediate(s2mm01_1C, s2mout01_1C, treat = "edu2", 
                        mediator = "knowledge", 
                        covariates = list(male = 0, agex = 2), robustSE = TRUE)
summary(medout01_f65)
## Male
medout01_m25 <- mediate(s2mm01_1C, s2mout01_1C, treat = "edu2", 
                        mediator = "knowledge", 
                        covariates = list(male = 1, agex = -2), robustSE = TRUE)
summary(medout01_m25)
medout01_m35 <- mediate(s2mm01_1C, s2mout01_1C, treat = "edu2", 
                        mediator = "knowledge", 
                        covariates = list(male = 1, agex = -1), robustSE = TRUE)
summary(medout01_m35)
medout01_m45 <- mediate(s2mm01_1C, s2mout01_1C, treat = "edu2", 
                        mediator = "knowledge", 
                        covariates = list(male = 1, agex = 0), robustSE = TRUE)
summary(medout01_m45)
medout01_m55 <- mediate(s2mm01_1C, s2mout01_1C, treat = "edu2", 
                        mediator = "knowledge", 
                        covariates = list(male = 1, agex = 1), robustSE = TRUE)
summary(medout01_m55)
medout01_m65 <- mediate(s2mm01_1C, s2mout01_1C, treat = "edu2", 
                        mediator = "knowledge", 
                        covariates = list(male = 1, agex = 2), robustSE = TRUE)
summary(medout01_m65)

#'
#' ### Ideology
#'

#+ eval=FALSE

## Outcome Model 
s2mout02_1C <- lm(foreignsuff  ~ edu2*male*agex + ideology*male*agex + lvpr +  
                    zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                    didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                    as.factor(wave), data=sifcct)

## Mediator Model
s2mm02_1C <- lm(ideology  ~ edu2*male*agex + lvpr +  
                  zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                  didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                  as.factor(wave), data=sifcct)

## Table
screenreg(list(s2mm02_1C,s2mout02_1C), digits = 4, single.row = T,
          override.se = list(coeftest(s2mm02_1C,vcov.=vcovHC(s2mm02_1C))[,2],
                             coeftest(s2mout02_1C,vcov.=vcovHC(s2mout02_1C))[,2]),
          override.pvalues = list(coeftest(s2mm02_1C,vcov.=vcovHC(s2mm02_1C))[,4],
                                  coeftest(s2mout02_1C,vcov.=vcovHC(s2mout02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Mediator","Outcome"))

#+ include = FALSE, eval = FALSE
texreg(list(s2mm02_1C,s2mout02_1C), digits = 4, single.row = T,
       override.se = list(coeftest(s2mm02_1C,vcov.=vcovHC(s2mm02_1C))[,2],
                          coeftest(s2mout02_1C,vcov.=vcovHC(s2mout02_1C))[,2]),
       override.pvalues = list(coeftest(s2mm02_1C,vcov.=vcovHC(s2mm02_1C))[,4],
                               coeftest(s2mout02_1C,vcov.=vcovHC(s2mout02_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Mediator","Outcome"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2medout02_1_tabular.tex"))

## Causal Mediation Analysis
set.seed(2345)

## Female
medout02_f25 <- mediate(s2mm02_1C, s2mout02_1C, treat = "edu2", 
                        mediator = "ideology", 
                        covariates = list(male = 0, agex = -2), robustSE = TRUE)
summary(medout02_f25)
medout02_f35 <- mediate(s2mm02_1C, s2mout02_1C, treat = "edu2", 
                        mediator = "ideology", 
                        covariates = list(male = 0, agex = -1), robustSE = TRUE)
summary(medout02_f35)
medout02_f45 <- mediate(s2mm02_1C, s2mout02_1C, treat = "edu2", 
                        mediator = "ideology", 
                        covariates = list(male = 0, agex = 0), robustSE = TRUE)
summary(medout02_f45)
medout02_f55 <- mediate(s2mm02_1C, s2mout02_1C, treat = "edu2", 
                        mediator = "ideology", 
                        covariates = list(male = 0, agex = 1), robustSE = TRUE)
summary(medout02_f55)
medout02_f65 <- mediate(s2mm02_1C, s2mout02_1C, treat = "edu2", 
                        mediator = "ideology", 
                        covariates = list(male = 0, agex = 2), robustSE = TRUE)
summary(medout02_f65)
## Male
medout02_m25 <- mediate(s2mm02_1C, s2mout02_1C, treat = "edu2", 
                        mediator = "ideology", 
                        covariates = list(male = 1, agex = -2), robustSE = TRUE)
summary(medout02_m25)
medout02_m35 <- mediate(s2mm02_1C, s2mout02_1C, treat = "edu2", 
                        mediator = "ideology", 
                        covariates = list(male = 1, agex = -1), robustSE = TRUE)
summary(medout02_m35)
medout02_m45 <- mediate(s2mm02_1C, s2mout02_1C, treat = "edu2", 
                        mediator = "ideology", 
                        covariates = list(male = 1, agex = 0), robustSE = TRUE)
summary(medout02_m45)
medout02_m55 <- mediate(s2mm02_1C, s2mout02_1C, treat = "edu2", 
                        mediator = "ideology", 
                        covariates = list(male = 1, agex = 1), robustSE = TRUE)
summary(medout02_m55)
medout02_m65 <- mediate(s2mm02_1C, s2mout02_1C, treat = "edu2", 
                        mediator = "ideology", 
                        covariates = list(male = 1, agex = 2), robustSE = TRUE)
summary(medout02_m65)

#'
#' ### LDP - DPJ FT
#'

#+ eval=FALSE

## Outcome Model 
s2mout03_1C <- lm(foreignsuff  ~ edu2*male*agex + ldpdpjft*male*agex + lvpr +  
                    zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                    didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                    as.factor(wave), data=sifcct)

## Mediator Model
s2mm03_1C <- lm(ldpdpjft ~ edu2*male*agex + lvpr +  
                  zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                  didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                  as.factor(wave), data=sifcct)

## Table
screenreg(list(s2mm03_1C,s2mout03_1C), digits = 4, single.row = T,
          override.se = list(coeftest(s2mm03_1C,vcov.=vcovHC(s2mm03_1C))[,2],
                             coeftest(s2mout03_1C,vcov.=vcovHC(s2mout03_1C))[,2]),
          override.pvalues = list(coeftest(s2mm03_1C,vcov.=vcovHC(s2mm03_1C))[,4],
                                  coeftest(s2mout03_1C,vcov.=vcovHC(s2mout03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Mediator","Outcome"))

#+ include = FALSE, eval = FALSE
texreg(list(s2mm03_1C,s2mout03_1C), digits = 4, single.row = T,
       override.se = list(coeftest(s2mm03_1C,vcov.=vcovHC(s2mm03_1C))[,2],
                          coeftest(s2mout03_1C,vcov.=vcovHC(s2mout03_1C))[,2]),
       override.pvalues = list(coeftest(s2mm03_1C,vcov.=vcovHC(s2mm03_1C))[,4],
                               coeftest(s2mout03_1C,vcov.=vcovHC(s2mout03_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Mediator","Outcome"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2medout03_1_tabular.tex"))

## Causal Mediation Analysis
set.seed(2345)

## Female
medout03_f25 <- mediate(s2mm03_1C, s2mout03_1C, treat = "edu2", 
                        mediator = "ldpdpjft", 
                        covariates = list(male = 0, agex = -2), robustSE = TRUE)
summary(medout03_f25)
medout03_f35 <- mediate(s2mm03_1C, s2mout03_1C, treat = "edu2", 
                        mediator = "ldpdpjft", 
                        covariates = list(male = 0, agex = -1), robustSE = TRUE)
summary(medout03_f35)
medout03_f45 <- mediate(s2mm03_1C, s2mout03_1C, treat = "edu2", 
                        mediator = "ldpdpjft", 
                        covariates = list(male = 0, agex = 0), robustSE = TRUE)
summary(medout03_f45)
medout03_f55 <- mediate(s2mm03_1C, s2mout03_1C, treat = "edu2", 
                        mediator = "ldpdpjft", 
                        covariates = list(male = 0, agex = 1), robustSE = TRUE)
summary(medout03_f55)
medout03_f65 <- mediate(s2mm03_1C, s2mout03_1C, treat = "edu2", 
                        mediator = "ldpdpjft", 
                        covariates = list(male = 0, agex = 2), robustSE = TRUE)
summary(medout03_f65)
## Male
medout03_m25 <- mediate(s2mm03_1C, s2mout03_1C, treat = "edu2", 
                        mediator = "ldpdpjft", 
                        covariates = list(male = 1, agex = -2), robustSE = TRUE)
summary(medout03_m25)
medout03_m35 <- mediate(s2mm03_1C, s2mout03_1C, treat = "edu2", 
                        mediator = "ldpdpjft", 
                        covariates = list(male = 1, agex = -1), robustSE = TRUE)
summary(medout03_m35)
medout03_m45 <- mediate(s2mm03_1C, s2mout03_1C, treat = "edu2", 
                        mediator = "ldpdpjft", 
                        covariates = list(male = 1, agex = 0), robustSE = TRUE)
summary(medout03_m45)
medout03_m55 <- mediate(s2mm03_1C, s2mout03_1C, treat = "edu2", 
                        mediator = "ldpdpjft", 
                        covariates = list(male = 1, agex = 1), robustSE = TRUE)
summary(medout03_m55)
medout03_m65 <- mediate(s2mm03_1C, s2mout03_1C, treat = "edu2", 
                        mediator = "ldpdpjft", 
                        covariates = list(male = 1, agex = 2), robustSE = TRUE)
summary(medout03_m65)

#'
#' ### Favorability of South Korea
#'

#+ eval=FALSE

## Outcome Model 
s2mout04_1C <- lm(foreignsuff  ~ edu2*male*agex + familiarityFT_KOR*male*agex + lvpr +  
                    zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                    didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                    as.factor(wave), data=sifcct)

## Mediator Model
s2mm04_1C <- lm(familiarityFT_KOR  ~ edu2*male*agex + lvpr +  
                  zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                  didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                  as.factor(wave), data=sifcct)

## Table
screenreg(list(s2mm04_1C,s2mout04_1C), digits = 4, single.row = T,
          override.se = list(coeftest(s2mm04_1C,vcov.=vcovHC(s2mm04_1C))[,2],
                             coeftest(s2mout04_1C,vcov.=vcovHC(s2mout04_1C))[,2]),
          override.pvalues = list(coeftest(s2mm04_1C,vcov.=vcovHC(s2mm04_1C))[,4],
                                  coeftest(s2mout04_1C,vcov.=vcovHC(s2mout04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Mediator","Outcome"))

#+ include = FALSE, eval = FALSE
texreg(list(s2mm04_1C,s2mout04_1C), digits = 4, single.row = T,
       override.se = list(coeftest(s2mm04_1C,vcov.=vcovHC(s2mm04_1C))[,2],
                          coeftest(s2mout04_1C,vcov.=vcovHC(s2mout04_1C))[,2]),
       override.pvalues = list(coeftest(s2mm04_1C,vcov.=vcovHC(s2mm04_1C))[,4],
                               coeftest(s2mout04_1C,vcov.=vcovHC(s2mout04_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Mediator","Outcome"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2medout04_1_tabular.tex"))

## Causal Mediation Analysis
set.seed(2345)

## Female
medout04_f25 <- mediate(s2mm04_1C, s2mout04_1C, treat = "edu2", 
                        mediator = "familiarityFT_KOR", 
                        covariates = list(male = 0, agex = -2), robustSE = TRUE)
summary(medout04_f25)
medout04_f35 <- mediate(s2mm04_1C, s2mout04_1C, treat = "edu2", 
                        mediator = "familiarityFT_KOR", 
                        covariates = list(male = 0, agex = -1), robustSE = TRUE)
summary(medout04_f35)
medout04_f45 <- mediate(s2mm04_1C, s2mout04_1C, treat = "edu2", 
                        mediator = "familiarityFT_KOR", 
                        covariates = list(male = 0, agex = 0), robustSE = TRUE)
summary(medout04_f45)
medout04_f55 <- mediate(s2mm04_1C, s2mout04_1C, treat = "edu2", 
                        mediator = "familiarityFT_KOR", 
                        covariates = list(male = 0, agex = 1), robustSE = TRUE)
summary(medout04_f55)
medout04_f65 <- mediate(s2mm04_1C, s2mout04_1C, treat = "edu2", 
                        mediator = "familiarityFT_KOR", 
                        covariates = list(male = 0, agex = 2), robustSE = TRUE)
summary(medout04_f65)
## Male
medout04_m25 <- mediate(s2mm04_1C, s2mout04_1C, treat = "edu2", 
                        mediator = "familiarityFT_KOR", 
                        covariates = list(male = 1, agex = -2), robustSE = TRUE)
summary(medout04_m25)
medout04_m35 <- mediate(s2mm04_1C, s2mout04_1C, treat = "edu2", 
                        mediator = "familiarityFT_KOR", 
                        covariates = list(male = 1, agex = -1), robustSE = TRUE)
summary(medout04_m35)
medout04_m45 <- mediate(s2mm04_1C, s2mout04_1C, treat = "edu2", 
                        mediator = "familiarityFT_KOR", 
                        covariates = list(male = 1, agex = 0), robustSE = TRUE)
summary(medout04_m45)
medout04_m55 <- mediate(s2mm04_1C, s2mout04_1C, treat = "edu2", 
                        mediator = "familiarityFT_KOR", 
                        covariates = list(male = 1, agex = 1), robustSE = TRUE)
summary(medout04_m55)
medout04_m65 <- mediate(s2mm04_1C, s2mout04_1C, treat = "edu2", 
                        mediator = "familiarityFT_KOR", 
                        covariates = list(male = 1, agex = 2), robustSE = TRUE)
summary(medout04_m65)

#'
#' ### Favorability of China
#'

#+ eval=FALSE

## Outcome Model 
s2mout05_1C <- lm(foreignsuff  ~ edu2*male*agex + familiarityFT_CHN*male*agex + lvpr +  
                    zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                    didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                    as.factor(wave), data=sifcct)

## Mediator Model
s2mm05_1C <- lm(familiarityFT_CHN  ~ edu2*male*agex + lvpr +  
                  zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                  didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                  as.factor(wave), data=sifcct)

## Table
screenreg(list(s2mm05_1C,s2mout05_1C), digits = 4, single.row = T,
          override.se = list(coeftest(s2mm05_1C,vcov.=vcovHC(s2mm05_1C))[,2],
                             coeftest(s2mout05_1C,vcov.=vcovHC(s2mout05_1C))[,2]),
          override.pvalues = list(coeftest(s2mm05_1C,vcov.=vcovHC(s2mm05_1C))[,4],
                                  coeftest(s2mout05_1C,vcov.=vcovHC(s2mout05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Mediator","Outcome"))

#+ include = FALSE, eval = FALSE
texreg(list(s2mm05_1C,s2mout05_1C), digits = 4, single.row = T,
       override.se = list(coeftest(s2mm05_1C,vcov.=vcovHC(s2mm05_1C))[,2],
                          coeftest(s2mout05_1C,vcov.=vcovHC(s2mout05_1C))[,2]),
       override.pvalues = list(coeftest(s2mm05_1C,vcov.=vcovHC(s2mm05_1C))[,4],
                               coeftest(s2mout05_1C,vcov.=vcovHC(s2mout05_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Mediator","Outcome"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2medout05_1_tabular.tex"))

## Causal Mediation Analysis
set.seed(2345)

## Female
medout05_f25 <- mediate(s2mm05_1C, s2mout05_1C, treat = "edu2", 
                        mediator = "familiarityFT_CHN", 
                        covariates = list(male = 0, agex = -2), robustSE = TRUE)
summary(medout05_f25)
medout05_f35 <- mediate(s2mm05_1C, s2mout05_1C, treat = "edu2", 
                        mediator = "familiarityFT_CHN", 
                        covariates = list(male = 0, agex = -1), robustSE = TRUE)
summary(medout05_f35)
medout05_f45 <- mediate(s2mm05_1C, s2mout05_1C, treat = "edu2", 
                        mediator = "familiarityFT_CHN", 
                        covariates = list(male = 0, agex = 0), robustSE = TRUE)
summary(medout05_f45)
medout05_f55 <- mediate(s2mm05_1C, s2mout05_1C, treat = "edu2", 
                        mediator = "familiarityFT_CHN", 
                        covariates = list(male = 0, agex = 1), robustSE = TRUE)
summary(medout05_f55)
medout05_f65 <- mediate(s2mm05_1C, s2mout05_1C, treat = "edu2", 
                        mediator = "familiarityFT_CHN", 
                        covariates = list(male = 0, agex = 2), robustSE = TRUE)
summary(medout05_f65)
## Male
medout05_m25 <- mediate(s2mm05_1C, s2mout05_1C, treat = "edu2", 
                        mediator = "familiarityFT_CHN", 
                        covariates = list(male = 1, agex = -2), robustSE = TRUE)
summary(medout05_m25)
medout05_m35 <- mediate(s2mm05_1C, s2mout05_1C, treat = "edu2", 
                        mediator = "familiarityFT_CHN", 
                        covariates = list(male = 1, agex = -1), robustSE = TRUE)
summary(medout05_m35)
medout05_m45 <- mediate(s2mm05_1C, s2mout05_1C, treat = "edu2", 
                        mediator = "familiarityFT_CHN", 
                        covariates = list(male = 1, agex = 0), robustSE = TRUE)
summary(medout05_m45)
medout05_m55 <- mediate(s2mm05_1C, s2mout05_1C, treat = "edu2", 
                        mediator = "familiarityFT_CHN", 
                        covariates = list(male = 1, agex = 1), robustSE = TRUE)
summary(medout05_m55)
medout05_m65 <- mediate(s2mm05_1C, s2mout05_1C, treat = "edu2", 
                        mediator = "familiarityFT_CHN", 
                        covariates = list(male = 1, agex = 2), robustSE = TRUE)
summary(medout05_m65)

#'
#' ### Favorability of United States
#'

#+ eval=FALSE

## Outcome Model 
s2mout06_1C <- lm(foreignsuff  ~ edu2*male*agex + familiarityFT_USA*male*agex + lvpr +  
                    zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                    didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                    as.factor(wave), data=sifcct)

## Mediator Model
s2mm06_1C <- lm(familiarityFT_USA  ~ edu2*male*agex + lvpr +  
                  zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                  didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                  as.factor(wave), data=sifcct)

## Table
screenreg(list(s2mm06_1C,s2mout06_1C), digits = 4, single.row = T,
          override.se = list(coeftest(s2mm06_1C,vcov.=vcovHC(s2mm06_1C))[,2],
                             coeftest(s2mout06_1C,vcov.=vcovHC(s2mout06_1C))[,2]),
          override.pvalues = list(coeftest(s2mm06_1C,vcov.=vcovHC(s2mm06_1C))[,4],
                                  coeftest(s2mout06_1C,vcov.=vcovHC(s2mout06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Mediator","Outcome"))

#+ include = FALSE, eval = FALSE
texreg(list(s2mm06_1C,s2mout06_1C), digits = 4, single.row = T,
       override.se = list(coeftest(s2mm06_1C,vcov.=vcovHC(s2mm06_1C))[,2],
                          coeftest(s2mout06_1C,vcov.=vcovHC(s2mout06_1C))[,2]),
       override.pvalues = list(coeftest(s2mm06_1C,vcov.=vcovHC(s2mm06_1C))[,4],
                               coeftest(s2mout06_1C,vcov.=vcovHC(s2mout06_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Mediator","Outcome"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2medout06_1_tabular.tex"))

## Causal Mediation Analysis
set.seed(2345)

## Female
medout06_f25 <- mediate(s2mm06_1C, s2mout06_1C, treat = "edu2", 
                        mediator = "familiarityFT_USA", 
                        covariates = list(male = 0, agex = -2), robustSE = TRUE)
summary(medout06_f25)
medout06_f35 <- mediate(s2mm06_1C, s2mout06_1C, treat = "edu2", 
                        mediator = "familiarityFT_USA", 
                        covariates = list(male = 0, agex = -1), robustSE = TRUE)
summary(medout06_f35)
medout06_f45 <- mediate(s2mm06_1C, s2mout06_1C, treat = "edu2", 
                        mediator = "familiarityFT_USA", 
                        covariates = list(male = 0, agex = 0), robustSE = TRUE)
summary(medout06_f45)
medout06_f55 <- mediate(s2mm06_1C, s2mout06_1C, treat = "edu2", 
                        mediator = "familiarityFT_USA", 
                        covariates = list(male = 0, agex = 1), robustSE = TRUE)
summary(medout06_f55)
medout06_f65 <- mediate(s2mm06_1C, s2mout06_1C, treat = "edu2", 
                        mediator = "familiarityFT_USA", 
                        covariates = list(male = 0, agex = 2), robustSE = TRUE)
summary(medout06_f65)
## Male
medout06_m25 <- mediate(s2mm06_1C, s2mout06_1C, treat = "edu2", 
                        mediator = "familiarityFT_USA", 
                        covariates = list(male = 1, agex = -2), robustSE = TRUE)
summary(medout06_m25)
medout06_m35 <- mediate(s2mm06_1C, s2mout06_1C, treat = "edu2", 
                        mediator = "familiarityFT_USA", 
                        covariates = list(male = 1, agex = -1), robustSE = TRUE)
summary(medout06_m35)
medout06_m45 <- mediate(s2mm06_1C, s2mout06_1C, treat = "edu2", 
                        mediator = "familiarityFT_USA", 
                        covariates = list(male = 1, agex = 0), robustSE = TRUE)
summary(medout06_m45)
medout06_m55 <- mediate(s2mm06_1C, s2mout06_1C, treat = "edu2", 
                        mediator = "familiarityFT_USA", 
                        covariates = list(male = 1, agex = 1), robustSE = TRUE)
summary(medout06_m55)
medout06_m65 <- mediate(s2mm06_1C, s2mout06_1C, treat = "edu2", 
                        mediator = "familiarityFT_USA", 
                        covariates = list(male = 1, agex = 2), robustSE = TRUE)
summary(medout06_m65)

#'
#' ### Income
#'

#+ eval=FALSE

## Outcome Model 
s2mout07_1C <- lm(foreignsuff  ~ edu2*male*agex + income*male*agex + lvpr +  
                    zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                    didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                    as.factor(wave), data=sifcct)

## Mediator Model
s2mm07_1C <- lm(income  ~ edu2*male*agex + lvpr +  
                  zip_did + sqrt(c10_sreg_fper) + I(c10_sreg_edu_ugsP/10) + 
                  didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                  as.factor(wave), data=sifcct)

## Table
screenreg(list(s2mm07_1C,s2mout07_1C), digits = 4, single.row = T,
          override.se = list(coeftest(s2mm07_1C,vcov.=vcovHC(s2mm07_1C))[,2],
                             coeftest(s2mout07_1C,vcov.=vcovHC(s2mout07_1C))[,2]),
          override.pvalues = list(coeftest(s2mm07_1C,vcov.=vcovHC(s2mm07_1C))[,4],
                                  coeftest(s2mout07_1C,vcov.=vcovHC(s2mout07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Mediator","Outcome"))

#+ include = FALSE, eval = FALSE
texreg(list(s2mm07_1C,s2mout07_1C), digits = 4, single.row = T,
       override.se = list(coeftest(s2mm07_1C,vcov.=vcovHC(s2mm07_1C))[,2],
                          coeftest(s2mout07_1C,vcov.=vcovHC(s2mout07_1C))[,2]),
       override.pvalues = list(coeftest(s2mm07_1C,vcov.=vcovHC(s2mm07_1C))[,4],
                               coeftest(s2mout07_1C,vcov.=vcovHC(s2mout07_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Mediator","Outcome"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2medout07_1_tabular.tex"))

## Causal Mediation Analysis
set.seed(2345)

## Female
medout07_f25 <- mediate(s2mm07_1C, s2mout07_1C, treat = "edu2", 
                        mediator = "income", 
                        covariates = list(male = 0, agex = -2), robustSE = TRUE)
summary(medout07_f25)
medout07_f35 <- mediate(s2mm07_1C, s2mout07_1C, treat = "edu2", 
                        mediator = "income", 
                        covariates = list(male = 0, agex = -1), robustSE = TRUE)
summary(medout07_f35)
medout07_f45 <- mediate(s2mm07_1C, s2mout07_1C, treat = "edu2", 
                        mediator = "income", 
                        covariates = list(male = 0, agex = 0), robustSE = TRUE)
summary(medout07_f45)
medout07_f55 <- mediate(s2mm07_1C, s2mout07_1C, treat = "edu2", 
                        mediator = "income", 
                        covariates = list(male = 0, agex = 1), robustSE = TRUE)
summary(medout07_f55)
medout07_f65 <- mediate(s2mm07_1C, s2mout07_1C, treat = "edu2", 
                        mediator = "income", 
                        covariates = list(male = 0, agex = 2), robustSE = TRUE)
summary(medout07_f65)
## Male
medout07_m25 <- mediate(s2mm07_1C, s2mout07_1C, treat = "edu2", 
                        mediator = "income", 
                        covariates = list(male = 1, agex = -2), robustSE = TRUE)
summary(medout07_m25)
medout07_m35 <- mediate(s2mm07_1C, s2mout07_1C, treat = "edu2", 
                        mediator = "income", 
                        covariates = list(male = 1, agex = -1), robustSE = TRUE)
summary(medout07_m35)
medout07_m45 <- mediate(s2mm07_1C, s2mout07_1C, treat = "edu2", 
                        mediator = "income", 
                        covariates = list(male = 1, agex = 0), robustSE = TRUE)
summary(medout07_m45)
medout07_m55 <- mediate(s2mm07_1C, s2mout07_1C, treat = "edu2", 
                        mediator = "income", 
                        covariates = list(male = 1, agex = 1), robustSE = TRUE)
summary(medout07_m55)
medout07_m65 <- mediate(s2mm07_1C, s2mout07_1C, treat = "edu2", 
                        mediator = "income", 
                        covariates = list(male = 1, agex = 2), robustSE = TRUE)
summary(medout07_m65)

## Save Image

#+ eval=FALSE
save.image(paste0(projdir,"/out/heavy/analysis_5_mediation_matchedL50_v5.RData"))
load(paste0(projdir,"/out/heavy/analysis_5_mediation_matchedL50_v5.RData"))

#'
#' # Coefficient Plot
#' 
#' ## Prepare Data
#'

#+ eval=FALSE
## Treatment to Mediator

extmed <- function(med,gender,ageset) {
  
  sifcct$med <- sifcct[,med]
  if (gender=="Male") sifcct$gender <- sifcct$female
  if (gender=="Female") sifcct$gender <- sifcct$male
  sifcct$ageset <- (sifcct$age - ageset)/10
  
  modset <- lm(med ~ edu2 * gender * ageset + lvpr + zip_did + sqrt(c10_sreg_fper) + 
                 I(c10_sreg_edu_ugsP/10) + didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                 as.factor(wave), data=sifcct)
  
  res <- c(med,gender,ageset,coef(modset)[2],
           coefci(modset, vcov.=vcovHC(modset), level = 0.95)[2,],
           coefci(modset, vcov.=vcovHC(modset), level = 0.90)[2,],
           coeftest(modset, vcov.=vcovHC(modset))[2,c(2,4)],
           "Treatment => Mediator")
  names(res) <- c("med","gender","age","est","lci95","uci95","lci90","uci90","se","p","mod")
  
  return(res)
  
}

meddt <- rbind(extmed("knowledge","Female",25),
               extmed("knowledge","Female",35),
               extmed("knowledge","Female",45),
               extmed("knowledge","Female",55),
               extmed("knowledge","Female",65),
               extmed("knowledge","Male",25),
               extmed("knowledge","Male",35),
               extmed("knowledge","Male",45),
               extmed("knowledge","Male",55),
               extmed("knowledge","Male",65),
               extmed("ideology","Female",25),
               extmed("ideology","Female",35),
               extmed("ideology","Female",45),
               extmed("ideology","Female",55),
               extmed("ideology","Female",65),
               extmed("ideology","Male",25),
               extmed("ideology","Male",35),
               extmed("ideology","Male",45),
               extmed("ideology","Male",55),
               extmed("ideology","Male",65),
               extmed("ldpdpjft","Female",25),
               extmed("ldpdpjft","Female",35),
               extmed("ldpdpjft","Female",45),
               extmed("ldpdpjft","Female",55),
               extmed("ldpdpjft","Female",65),
               extmed("ldpdpjft","Male",25),
               extmed("ldpdpjft","Male",35),
               extmed("ldpdpjft","Male",45),
               extmed("ldpdpjft","Male",55),
               extmed("ldpdpjft","Male",65),
               extmed("familiarityFT_KOR","Female",25),
               extmed("familiarityFT_KOR","Female",35),
               extmed("familiarityFT_KOR","Female",45),
               extmed("familiarityFT_KOR","Female",55),
               extmed("familiarityFT_KOR","Female",65),
               extmed("familiarityFT_KOR","Male",25),
               extmed("familiarityFT_KOR","Male",35),
               extmed("familiarityFT_KOR","Male",45),
               extmed("familiarityFT_KOR","Male",55),
               extmed("familiarityFT_KOR","Male",65),
               extmed("familiarityFT_CHN","Female",25),
               extmed("familiarityFT_CHN","Female",35),
               extmed("familiarityFT_CHN","Female",45),
               extmed("familiarityFT_CHN","Female",55),
               extmed("familiarityFT_CHN","Female",65),
               extmed("familiarityFT_CHN","Male",25),
               extmed("familiarityFT_CHN","Male",35),
               extmed("familiarityFT_CHN","Male",45),
               extmed("familiarityFT_CHN","Male",55),
               extmed("familiarityFT_CHN","Male",65),
               extmed("familiarityFT_USA","Female",25),
               extmed("familiarityFT_USA","Female",35),
               extmed("familiarityFT_USA","Female",45),
               extmed("familiarityFT_USA","Female",55),
               extmed("familiarityFT_USA","Female",65),
               extmed("familiarityFT_USA","Male",25),
               extmed("familiarityFT_USA","Male",35),
               extmed("familiarityFT_USA","Male",45),
               extmed("familiarityFT_USA","Male",55),
               extmed("familiarityFT_USA","Male",65),
               extmed("income","Female",25),
               extmed("income","Female",35),
               extmed("income","Female",45),
               extmed("income","Female",55),
               extmed("income","Female",65),
               extmed("income","Male",25),
               extmed("income","Male",35),
               extmed("income","Male",45),
               extmed("income","Male",55),
               extmed("income","Male",65))
meddt <- as.data.frame(meddt)
for(i in 3:10) meddt[,i] <- as.numeric(meddt[,i])
meddt$med <- factor(meddt$med, levels=unique(meddt$med))
meddt$gender <- factor(meddt$gender, levels=unique(meddt$gender))
summary(meddt)

## Mediator to Outcome

extout <- function(med,gender,ageset) {
  
  sifcct$med <- sifcct[,med]
  if (gender=="Male") sifcct$gender <- sifcct$female
  if (gender=="Female") sifcct$gender <- sifcct$male
  sifcct$ageset <- (sifcct$age - ageset)/10
  
  modset <- lm(foreignsuff ~ med * gender * ageset + edu2 * gender * ageset + lvpr + zip_did + sqrt(c10_sreg_fper) + 
                 I(c10_sreg_edu_ugsP/10) + didper + sqrt(c10_mun_fper) + I(c10_mun_edu_ugsP/10) + 
                 as.factor(wave), data=sifcct)
  
  res <- c(med,gender,ageset,coef(modset)[2],
           coefci(modset, vcov.=vcovHC(modset), level = 0.95)[2,],
           coefci(modset, vcov.=vcovHC(modset), level = 0.90)[2,],
           coeftest(modset, vcov.=vcovHC(modset))[2,c(2,4)],
           "Mediator => Outcome")
  names(res) <- c("med","gender","age","est","lci95","uci95","lci90","uci90","se","p","mod")
  
  return(res)
  
}

outdt <- rbind(extout("knowledge","Female",25),
               extout("knowledge","Female",35),
               extout("knowledge","Female",45),
               extout("knowledge","Female",55),
               extout("knowledge","Female",65),
               extout("knowledge","Male",25),
               extout("knowledge","Male",35),
               extout("knowledge","Male",45),
               extout("knowledge","Male",55),
               extout("knowledge","Male",65),
               extout("ideology","Female",25),
               extout("ideology","Female",35),
               extout("ideology","Female",45),
               extout("ideology","Female",55),
               extout("ideology","Female",65),
               extout("ideology","Male",25),
               extout("ideology","Male",35),
               extout("ideology","Male",45),
               extout("ideology","Male",55),
               extout("ideology","Male",65),
               extout("ldpdpjft","Female",25),
               extout("ldpdpjft","Female",35),
               extout("ldpdpjft","Female",45),
               extout("ldpdpjft","Female",55),
               extout("ldpdpjft","Female",65),
               extout("ldpdpjft","Male",25),
               extout("ldpdpjft","Male",35),
               extout("ldpdpjft","Male",45),
               extout("ldpdpjft","Male",55),
               extout("ldpdpjft","Male",65),
               extout("familiarityFT_KOR","Female",25),
               extout("familiarityFT_KOR","Female",35),
               extout("familiarityFT_KOR","Female",45),
               extout("familiarityFT_KOR","Female",55),
               extout("familiarityFT_KOR","Female",65),
               extout("familiarityFT_KOR","Male",25),
               extout("familiarityFT_KOR","Male",35),
               extout("familiarityFT_KOR","Male",45),
               extout("familiarityFT_KOR","Male",55),
               extout("familiarityFT_KOR","Male",65),
               extout("familiarityFT_CHN","Female",25),
               extout("familiarityFT_CHN","Female",35),
               extout("familiarityFT_CHN","Female",45),
               extout("familiarityFT_CHN","Female",55),
               extout("familiarityFT_CHN","Female",65),
               extout("familiarityFT_CHN","Male",25),
               extout("familiarityFT_CHN","Male",35),
               extout("familiarityFT_CHN","Male",45),
               extout("familiarityFT_CHN","Male",55),
               extout("familiarityFT_CHN","Male",65),
               extout("familiarityFT_USA","Female",25),
               extout("familiarityFT_USA","Female",35),
               extout("familiarityFT_USA","Female",45),
               extout("familiarityFT_USA","Female",55),
               extout("familiarityFT_USA","Female",65),
               extout("familiarityFT_USA","Male",25),
               extout("familiarityFT_USA","Male",35),
               extout("familiarityFT_USA","Male",45),
               extout("familiarityFT_USA","Male",55),
               extout("familiarityFT_USA","Male",65),
               extout("income","Female",25),
               extout("income","Female",35),
               extout("income","Female",45),
               extout("income","Female",55),
               extout("income","Female",65),
               extout("income","Male",25),
               extout("income","Male",35),
               extout("income","Male",45),
               extout("income","Male",55),
               extout("income","Male",65))
outdt <- as.data.frame(outdt)
for(i in 3:10) outdt[,i] <- as.numeric(outdt[,i])
outdt$med <- factor(outdt$med, levels=unique(outdt$med))
outdt$gender <- factor(outdt$gender, levels=unique(outdt$gender))
summary(outdt)

## Mediation 

extmedout <- function(med,gender,ageset,medout) {
  
  res1 <- c(med,gender,ageset,
            medout$d0,
            quantile(medout$d0.sims,probs=c(0.025,0.975,0.05,0.95)),
            NA,
            medout01_f25$d0.p,
            "Treat. => Med. => Out.\n(ACME)")
  names(res1) <- c("med","gender","age","est","lci95","uci95","lci90","uci90","se","p","mod")
  
  res2 <- c(med,gender,ageset,
            medout$z0,
            quantile(medout$z0.sims,probs=c(0.025,0.975,0.05,0.95)),
            NA,
            medout01_f25$z0.p,
            "Treatment => Outcome\n(ADE)")
  names(res1) <- c("med","gender","age","est","lci95","uci95","lci90","uci90","se","p","mod")
  
  return(rbind(res1,res2))
  
}

medoutdt <- rbind(extmedout("knowledge","Female",25,medout01_f25),
                  extmedout("knowledge","Female",35,medout01_f35),
                  extmedout("knowledge","Female",45,medout01_f45),
                  extmedout("knowledge","Female",55,medout01_f55),
                  extmedout("knowledge","Female",65,medout01_f65),
                  extmedout("knowledge","Male",25,medout01_m25),
                  extmedout("knowledge","Male",35,medout01_m35),
                  extmedout("knowledge","Male",45,medout01_m45),
                  extmedout("knowledge","Male",55,medout01_m55),
                  extmedout("knowledge","Male",65,medout01_m65),
                  extmedout("ideology","Female",25,medout02_f25),
                  extmedout("ideology","Female",35,medout02_f35),
                  extmedout("ideology","Female",45,medout02_f45),
                  extmedout("ideology","Female",55,medout02_f55),
                  extmedout("ideology","Female",65,medout02_f65),
                  extmedout("ideology","Male",25,medout02_m25),
                  extmedout("ideology","Male",35,medout02_m35),
                  extmedout("ideology","Male",45,medout02_m45),
                  extmedout("ideology","Male",55,medout02_m55),
                  extmedout("ideology","Male",65,medout02_m65),
                  extmedout("ldpdpjft","Female",25,medout03_f25),
                  extmedout("ldpdpjft","Female",35,medout03_f35),
                  extmedout("ldpdpjft","Female",45,medout03_f45),
                  extmedout("ldpdpjft","Female",55,medout03_f55),
                  extmedout("ldpdpjft","Female",65,medout03_f65),
                  extmedout("ldpdpjft","Male",25,medout03_m25),
                  extmedout("ldpdpjft","Male",35,medout03_m35),
                  extmedout("ldpdpjft","Male",45,medout03_m45),
                  extmedout("ldpdpjft","Male",55,medout03_m55),
                  extmedout("ldpdpjft","Male",65,medout03_m65),
                  extmedout("familiarityFT_KOR","Female",25,medout04_f25),
                  extmedout("familiarityFT_KOR","Female",35,medout04_f35),
                  extmedout("familiarityFT_KOR","Female",45,medout04_f45),
                  extmedout("familiarityFT_KOR","Female",55,medout04_f55),
                  extmedout("familiarityFT_KOR","Female",65,medout04_f65),
                  extmedout("familiarityFT_KOR","Male",25,medout04_m25),
                  extmedout("familiarityFT_KOR","Male",35,medout04_m35),
                  extmedout("familiarityFT_KOR","Male",45,medout04_m45),
                  extmedout("familiarityFT_KOR","Male",55,medout04_m55),
                  extmedout("familiarityFT_KOR","Male",65,medout04_m65),
                  extmedout("familiarityFT_CHN","Female",25,medout05_f25),
                  extmedout("familiarityFT_CHN","Female",35,medout05_f35),
                  extmedout("familiarityFT_CHN","Female",45,medout05_f45),
                  extmedout("familiarityFT_CHN","Female",55,medout05_f55),
                  extmedout("familiarityFT_CHN","Female",65,medout05_f65),
                  extmedout("familiarityFT_CHN","Male",25,medout05_m25),
                  extmedout("familiarityFT_CHN","Male",35,medout05_m35),
                  extmedout("familiarityFT_CHN","Male",45,medout05_m45),
                  extmedout("familiarityFT_CHN","Male",55,medout05_m55),
                  extmedout("familiarityFT_CHN","Male",65,medout05_m65),
                  extmedout("familiarityFT_USA","Female",25,medout06_f25),
                  extmedout("familiarityFT_USA","Female",35,medout06_f35),
                  extmedout("familiarityFT_USA","Female",45,medout06_f45),
                  extmedout("familiarityFT_USA","Female",55,medout06_f55),
                  extmedout("familiarityFT_USA","Female",65,medout06_f65),
                  extmedout("familiarityFT_USA","Male",25,medout06_m25),
                  extmedout("familiarityFT_USA","Male",35,medout06_m35),
                  extmedout("familiarityFT_USA","Male",45,medout06_m45),
                  extmedout("familiarityFT_USA","Male",55,medout06_m55),
                  extmedout("familiarityFT_USA","Male",65,medout06_m65),
                  extmedout("income","Female",25,medout07_f25),
                  extmedout("income","Female",35,medout07_f35),
                  extmedout("income","Female",45,medout07_f45),
                  extmedout("income","Female",55,medout07_f55),
                  extmedout("income","Female",65,medout07_f65),
                  extmedout("income","Male",25,medout07_m25),
                  extmedout("income","Male",35,medout07_m35),
                  extmedout("income","Male",45,medout07_m45),
                  extmedout("income","Male",55,medout07_m55),
                  extmedout("income","Male",65,medout07_m65))
medoutdt <- as.data.frame(medoutdt)
for(i in 3:10) medoutdt[,i] <- as.numeric(medoutdt[,i])
medoutdt$med <- factor(medoutdt$med, levels=unique(medoutdt$med))
medoutdt$gender <- factor(medoutdt$gender, levels=unique(medoutdt$gender))
summary(medoutdt)

## Combine All Data

coefdt <- rbind(meddt,outdt,medoutdt)
coefdt$mod <- factor(coefdt$mod, levels=unique(coefdt$mod))

coefdt$lambda <- "Matched with Lambda = 50km"
# coefdt$lambda <- rep(c("Unmatched", 
#                               "Matched without Distance Adj.",
#                               "Matched with Lambda = 350km", 
#                               "Matched with Lambda = 200km", 
#                               "Matched with Lambda = 100km", 
#                               "Matched with Lambda = 50km"), each=8)
# coefdt$lambda <- factor(coefdt$lambda, levels=unique(coefdt$lambda))
# 

coefdt$pstar <- factor(ifelse(coefdt$lci95>0|coefdt$uci95<0,"p<.05",
                              ifelse(coefdt$lci90>0|coefdt$uci90<0,"p<.1","n.s.")),
                       levels = c("p<.05","p<.1","n.s."))

## Save Data Temporarily
saveRDS(coefdt, paste0(projdir,"/out/medoutcoefdt_matchedL50_v5.rds"))

#+ echo=FALSE
coefdt <- readRDS(paste0(projdir,"/out/medoutcoefdt_matchedL50_v5.rds"))



#'
#' ## Plotting for knowledge
#'

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="knowledge",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: Political knowledge (rescaled to 0-1 with 1 being the most knowledgeable).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot_knowledge_matchedL50_v5.png"),p,width=8,height=5)

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="knowledge" & coefdt$mod!="Treatment => Outcome\n(ADE)",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: Political knowledge (rescaled to 0-1 with 1 being the most knowledgeable).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot2_knowledge_matchedL50_v5.png"),p,width=8,height=5)

#'
#' ## Plotting for ideology
#'

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="ideology",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: Political ideology (rescaled to 0-1 with 1 being the most conservative).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot_ideology_matchedL50_v5.png"),p,width=8,height=5)

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="ideology" & coefdt$mod!="Treatment => Outcome\n(ADE)",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: Political ideology (rescaled to 0-1 with 1 being the most conservative).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot2_ideology_matchedL50_v5.png"),p,width=8,height=5)

#'
#' ## Plotting for ldpdpjft
#'

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="ldpdpjft",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: LDP - DPJ Feeling Thermometer (rescaled to 0-1).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot_ldpdpjft_matchedL50_v5.png"),p,width=8,height=5)

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="ldpdpjft" & coefdt$mod!="Treatment => Outcome\n(ADE)",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: LDP - DPJ Feeling Thermometer (rescaled to 0-1).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot2_ldpdpjft_matchedL50_v5.png"),p,width=8,height=5)

#'
#' ## Plotting for South Korea Feeling Thermometer
#'

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="familiarityFT_KOR",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: South Korea Feeling Thermometer (rescaled to 0-1 with 1 being the most favorable).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot_familiarityFT_KOR_matchedL50_v5.png"),p,width=8,height=5)

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="familiarityFT_KOR" & coefdt$mod!="Treatment => Outcome\n(ADE)",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: South Korea Feeling Thermometer (rescaled to 0-1 with 1 being the most favorable).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot2_familiarityFT_KOR_matchedL50_v5.png"),p,width=8,height=5)

#'
#' ## Plotting for China Feeling Thermometer
#'

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="familiarityFT_CHN",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: China Feeling Thermometer (rescaled to 0-1 with 1 being the most favorable).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot_familiarityFT_CHN_matchedL50_v5.png"),p,width=8,height=5)

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="familiarityFT_CHN" & coefdt$mod!="Treatment => Outcome\n(ADE)",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: China Feeling Thermometer (rescaled to 0-1 with 1 being the most favorable).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot2_familiarityFT_CHN_matchedL50_v5.png"),p,width=8,height=5)

#'
#' ## Plotting for United States Feeling Thermometer
#'

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="familiarityFT_USA",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: United States Feeling Thermometer (rescaled to 0-1 with 1 being the most favorable).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot_familiarityFT_USA_matchedL50_v5.png"),p,width=8,height=5)

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="familiarityFT_USA" & coefdt$mod!="Treatment => Outcome\n(ADE)",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: United States Feeling Thermometer (rescaled to 0-1 with 1 being the most favorable).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot2_familiarityFT_USA_matchedL50_v5.png"),p,width=8,height=5)

#'
#' ## Plotting for Income
#'

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="income",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: Income (rescaled to 0-1 with 1 being the richest).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot_income_matchedL50_v5.png"),p,width=8,height=5)

require(ggplot2)
p <- ggplot(coefdt[coefdt$med=="income" & coefdt$mod!="Treatment => Outcome\n(ADE)",], aes(x=gender, y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=as.factor(age), alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=as.factor(age), alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=as.factor(age), colour=as.factor(age), alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Age") +
  scale_color_manual(name="Age",values=rep("black", 5)) +
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab(NULL) +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nMediatior: Income (rescaled to 0-1 with 1 being the richest).\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot2_income_matchedL50_v5.png"),p,width=8,height=5)

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/analysis_5_mediation_matchedL50_v5.R', rmarkdown::pdf_document(latex_engine="xelatex", extra_dependencies = list(bookmark=NULL, xltxtra=NULL, zxjatype=NULL, zxjafont=c("ipa"))), encoding = 'UTF-8')
# rmarkdown::render('./src/analysis_5_mediation_matchedL50_v5.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
