#' ---
#' title: "Analysis 2: Main Analysis with Unmatched and Matched Data"
#' author: "Fan Lu & Gento Kato"
#' date: "January 26, 2020"
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
originaldir1a <- paste0(projdir, "/data/sifcct_zip_latest_v5.rds")
originaldir1b <- paste0(projdir, "/data/sifcct_zip_latest_panel_v5.rds")
original <- rbind(readRDS(originaldir1a),readRDS(originaldir1b))

## Matched/Unmatched Data Locations
datadir0 <- paste0(projdir, "/data/sifcct_unmatched_v5.rds")
datadir1 <- paste0(projdir, "/data/sifcct_matched_1_all_v5.rds")
datadir2 <- paste0(projdir, "/data/sifcct_matched_2_all_v5.rds")
datadir3 <- paste0(projdir, "/data/sifcct_matched_3_all_v5.rds")
datadir4 <- paste0(projdir, "/data/sifcct_matched_4_all_v5.rds")
datadir5 <- paste0(projdir, "/data/sifcct_matched_5_all_v5.rds")

## packages
require(sandwich)
require(lmtest)
require(MASS)
# devtools::install_github("tidyverse/ggplot2") # Need development version (as of Dec 31, 2019)
library(ggplot2)
require(texreg)
# require(nnet)
require(mlogit)
require(dfidx)
require(Formula)

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

## Variable Names ##

vnmap <- list("edu2" = "University education",
              "edu2 (1)" = "University education",
              "female" = "Gender (female)",
              "male" = "Gender (male)",
              "male (1)" = "Gender (male)",
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
#' # With Unmatched Data
#'

sifcct <- readRDS(datadir0)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)

#'
#' ## Outcome Model 
#'

## Living in Local ZIP since at least age 15 ##

s0mo_10 <- lm(update(foreignsuff ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mo_1A <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mo_1B <- lm(update(foreignsuff ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mo_1C <- lm(update(foreignsuff ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s0mo_10,s0mo_1A,s0mo_1B,s0mo_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mo_10,vcov.=vcovHC(s0mo_10))[,2],
                             coeftest(s0mo_1A,vcov.=vcovHC(s0mo_1A))[,2],
                             coeftest(s0mo_1B,vcov.=vcovHC(s0mo_1B))[,2],
                             coeftest(s0mo_1C,vcov.=vcovHC(s0mo_1C))[,2]),
          override.pvalues = list(coeftest(s0mo_10,vcov.=vcovHC(s0mo_10))[,4],
                                  coeftest(s0mo_1A,vcov.=vcovHC(s0mo_1A))[,4],
                                  coeftest(s0mo_1B,vcov.=vcovHC(s0mo_1B))[,4],
                                  coeftest(s0mo_1C,vcov.=vcovHC(s0mo_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mo_10,s0mo_1A,s0mo_1B,s0mo_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mo_10,vcov.=vcovHC(s0mo_10))[,2],
                          coeftest(s0mo_1A,vcov.=vcovHC(s0mo_1A))[,2],
                          coeftest(s0mo_1B,vcov.=vcovHC(s0mo_1B))[,2],
                          coeftest(s0mo_1C,vcov.=vcovHC(s0mo_1C))[,2]),
       override.pvalues = list(coeftest(s0mo_10,vcov.=vcovHC(s0mo_10))[,4],
                               coeftest(s0mo_1A,vcov.=vcovHC(s0mo_1A))[,4],
                               coeftest(s0mo_1B,vcov.=vcovHC(s0mo_1B))[,4],
                               coeftest(s0mo_1C,vcov.=vcovHC(s0mo_1C))[,4]),
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mo_1_tabular.tex"))

#+
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

outdt0 <- rbind(extout("Female",25),
                extout("Female",35),
                extout("Female",45),
                extout("Female",55),
                extout("Female",65),
                extout("Male",25),
                extout("Male",35),
                extout("Male",45),
                extout("Male",55),
                extout("Male",65))
outdt0 <- as.data.frame(outdt0)
for(i in 2:9) outdt0[,i] <- as.numeric(outdt0[,i])
outdt0$gender <- factor(outdt0$gender, levels=unique(outdt0$gender))
summary(outdt0)

#'
#' ## Outcome Model 2
#'

## Living in Local ZIP since at least age 15 ##

# require(nnet)
# s0mo2_10 <- multinom(update(foreignsuff3x ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s0mo2_1A <- multinom(update(foreignsuff3x ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s0mo2_1B <- multinom(update(foreignsuff3x ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s0mo2_1C <- multinom(update(foreignsuff3x ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])

sifcct.mlogit <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen<=15),],
                       shape = "wide", choice = "foreignsuff3x")
# # levels(sifcct.mlogit$idx$id2) <- c("Disagree","Neither","Agree")
s0mo2_10 <- mlogit(outmod0.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s0mo2_1A <- mlogit(outmodA.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s0mo2_1B <- mlogit(outmodB.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s0mo2_1C <- mlogit(outmodC.mlogit, data=sifcct.mlogit, reflevel="Disagree")

screenreg(list(s0mo2_10,s0mo2_1A), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_10))),2],
                             coeftest(s0mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_10))),2],
                             coeftest(s0mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1A))),2],
                             coeftest(s0mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1A))),2]),
          override.pvalues = list(coeftest(s0mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_10))),4],
                                  coeftest(s0mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_10))),4],
                                  coeftest(s0mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1A))),4],
                                  coeftest(s0mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+", 
          custom.model.names = c("Base: Agree","Base: Neither",
                                 "ZIP: Agree","ZIP: Neither"),
          custom.coef.map = vnmap)
screenreg(list(s0mo2_1B,s0mo2_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1B))),2],
                             coeftest(s0mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1B))),2],
                             coeftest(s0mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1C))),2],
                             coeftest(s0mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1C))),2]),
          override.pvalues = list(coeftest(s0mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1B))),4],
                                  coeftest(s0mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1B))),4],
                                  coeftest(s0mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1C))),4],
                                  coeftest(s0mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Agree","Mun.: Neither",
                                 "Full: Agree","Full: Neither"))

#+ include = FALSE
texreg(list(s0mo2_10,s0mo2_1A), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_10))),2],
                          coeftest(s0mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_10))),2],
                          coeftest(s0mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1A))),2],
                          coeftest(s0mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1A))),2]),
       override.pvalues = list(coeftest(s0mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_10))),4],
                               coeftest(s0mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_10))),4],
                               coeftest(s0mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1A))),4],
                               coeftest(s0mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1A))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger", 
       custom.model.names = c("Base: Agree","Base: Neither",
                              "ZIP: Agree","ZIP: Neither"),
       custom.coef.map = vnmap,
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mo2_1_1_tabular.tex"))
texreg(list(s0mo2_1B,s0mo2_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1B))),2],
                          coeftest(s0mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1B))),2],
                          coeftest(s0mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1C))),2],
                          coeftest(s0mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1C))),2]),
       override.pvalues = list(coeftest(s0mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1B))),4],
                               coeftest(s0mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1B))),4],
                               coeftest(s0mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1C))),4],
                               coeftest(s0mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1C))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap,
       custom.model.names = c("Mun.: Agree","Mun.: Neither",
                              "Full: Agree","Full: Neither"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mo2_1_2_tabular.tex"))

#+
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
                       as.factor(wave), data=sifcct.mlogit.tmp, reflevel = "Disagree")
    subname = "Stayed"
  } else {
    # modset <- multinom(foreignsuff3x ~ edu2 * gender * ageset + lvpr + as.factor(wave), 
    #                    data=sifcct[which(sifcct$age - sifcct$lvlen>=23),],
    #                    Hess = TRUE)
    sifcct.mlogit.tmp <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen>=23),],
                               shape = "wide", choice = "foreignsuff3x")
    # levels(sifcct.mlogit.tmp$idx$id2) <- c("Disagree","Neither","Agree")
    modset <- mlogit(foreignsuff3x ~ 0 | edu2 * gender * ageset + lvpr + as.factor(wave), 
                     data=sifcct.mlogit.tmp, reflevel = "Disagree")
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

outdt0m <- rbind(extout("Female",25,1),
                extout("Female",35,1),
                extout("Female",45,1),
                extout("Female",55,1),
                extout("Female",65,1),
                extout("Male",25,1),
                extout("Male",35,1),
                extout("Male",45,1),
                extout("Male",55,1),
                extout("Male",65,1))
outdt0m <- as.data.frame(outdt0m)
for(i in 2:9) outdt0m[,i] <- as.numeric(outdt0m[,i])
outdt0m$gender <- factor(outdt0m$gender, levels=unique(outdt0m$gender))
summary(outdt0m)

#'
#' ## Mediator Models
#' 
#' ## Knowledge
#'

s0mm01_10 <- lm(update(knowledge ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm01_1A <- lm(update(knowledge ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm01_1B <- lm(update(knowledge ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm01_1C <- lm(update(knowledge ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s0mm01_10,s0mm01_1A,s0mm01_1B,s0mm01_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mm01_10,vcov.=vcovHC(s0mm01_10))[,2],
                             coeftest(s0mm01_1A,vcov.=vcovHC(s0mm01_1A))[,2],
                             coeftest(s0mm01_1B,vcov.=vcovHC(s0mm01_1B))[,2],
                             coeftest(s0mm01_1C,vcov.=vcovHC(s0mm01_1C))[,2]),
          override.pvalues = list(coeftest(s0mm01_10,vcov.=vcovHC(s0mm01_10))[,4],
                                  coeftest(s0mm01_1A,vcov.=vcovHC(s0mm01_1A))[,4],
                                  coeftest(s0mm01_1B,vcov.=vcovHC(s0mm01_1B))[,4],
                                  coeftest(s0mm01_1C,vcov.=vcovHC(s0mm01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mm01_10,s0mm01_1A,s0mm01_1B,s0mm01_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mm01_10,vcov.=vcovHC(s0mm01_10))[,2],
                          coeftest(s0mm01_1A,vcov.=vcovHC(s0mm01_1A))[,2],
                          coeftest(s0mm01_1B,vcov.=vcovHC(s0mm01_1B))[,2],
                          coeftest(s0mm01_1C,vcov.=vcovHC(s0mm01_1C))[,2]),
       override.pvalues = list(coeftest(s0mm01_10,vcov.=vcovHC(s0mm01_10))[,4],
                               coeftest(s0mm01_1A,vcov.=vcovHC(s0mm01_1A))[,4],
                               coeftest(s0mm01_1B,vcov.=vcovHC(s0mm01_1B))[,4],
                               coeftest(s0mm01_1C,vcov.=vcovHC(s0mm01_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mm01_1_tabular.tex"))



#'
#' ## Ideology
#'

s0mm02_10 <- lm(update(ideology ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm02_1A <- lm(update(ideology ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm02_1B <- lm(update(ideology ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm02_1C <- lm(update(ideology ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s0mm02_10,s0mm02_1A,s0mm02_1B,s0mm02_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mm02_10,vcov.=vcovHC(s0mm02_10))[,2],
                             coeftest(s0mm02_1A,vcov.=vcovHC(s0mm02_1A))[,2],
                             coeftest(s0mm02_1B,vcov.=vcovHC(s0mm02_1B))[,2],
                             coeftest(s0mm02_1C,vcov.=vcovHC(s0mm02_1C))[,2]),
          override.pvalues = list(coeftest(s0mm02_10,vcov.=vcovHC(s0mm02_10))[,4],
                                  coeftest(s0mm02_1A,vcov.=vcovHC(s0mm02_1A))[,4],
                                  coeftest(s0mm02_1B,vcov.=vcovHC(s0mm02_1B))[,4],
                                  coeftest(s0mm02_1C,vcov.=vcovHC(s0mm02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mm02_10,s0mm02_1A,s0mm02_1B,s0mm02_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mm02_10,vcov.=vcovHC(s0mm02_10))[,2],
                          coeftest(s0mm02_1A,vcov.=vcovHC(s0mm02_1A))[,2],
                          coeftest(s0mm02_1B,vcov.=vcovHC(s0mm02_1B))[,2],
                          coeftest(s0mm02_1C,vcov.=vcovHC(s0mm02_1C))[,2]),
       override.pvalues = list(coeftest(s0mm02_10,vcov.=vcovHC(s0mm02_10))[,4],
                               coeftest(s0mm02_1A,vcov.=vcovHC(s0mm02_1A))[,4],
                               coeftest(s0mm02_1B,vcov.=vcovHC(s0mm02_1B))[,4],
                               coeftest(s0mm02_1C,vcov.=vcovHC(s0mm02_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mm02_1_tabular.tex"))

#'
#' ## LDP - DPJ FT
#'

s0mm03_10 <- lm(update(ldpdpjft ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm03_1A <- lm(update(ldpdpjft ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm03_1B <- lm(update(ldpdpjft ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm03_1C <- lm(update(ldpdpjft ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s0mm03_10,s0mm03_1A,s0mm03_1B,s0mm03_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mm03_10,vcov.=vcovHC(s0mm03_10))[,2],
                             coeftest(s0mm03_1A,vcov.=vcovHC(s0mm03_1A))[,2],
                             coeftest(s0mm03_1B,vcov.=vcovHC(s0mm03_1B))[,2],
                             coeftest(s0mm03_1C,vcov.=vcovHC(s0mm03_1C))[,2]),
          override.pvalues = list(coeftest(s0mm03_10,vcov.=vcovHC(s0mm03_10))[,4],
                                  coeftest(s0mm03_1A,vcov.=vcovHC(s0mm03_1A))[,4],
                                  coeftest(s0mm03_1B,vcov.=vcovHC(s0mm03_1B))[,4],
                                  coeftest(s0mm03_1C,vcov.=vcovHC(s0mm03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mm03_10,s0mm03_1A,s0mm03_1B,s0mm03_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mm03_10,vcov.=vcovHC(s0mm03_10))[,2],
                          coeftest(s0mm03_1A,vcov.=vcovHC(s0mm03_1A))[,2],
                          coeftest(s0mm03_1B,vcov.=vcovHC(s0mm03_1B))[,2],
                          coeftest(s0mm03_1C,vcov.=vcovHC(s0mm03_1C))[,2]),
       override.pvalues = list(coeftest(s0mm03_10,vcov.=vcovHC(s0mm03_10))[,4],
                               coeftest(s0mm03_1A,vcov.=vcovHC(s0mm03_1A))[,4],
                               coeftest(s0mm03_1B,vcov.=vcovHC(s0mm03_1B))[,4],
                               coeftest(s0mm03_1C,vcov.=vcovHC(s0mm03_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mm03_1_tabular.tex"))


#'
#' ## Favorability of South Korea
#'

s0mm04_10 <- lm(update(familiarityFT_KOR ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm04_1A <- lm(update(familiarityFT_KOR ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm04_1B <- lm(update(familiarityFT_KOR ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm04_1C <- lm(update(familiarityFT_KOR ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s0mm04_10,s0mm04_1A,s0mm04_1B,s0mm04_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mm04_10,vcov.=vcovHC(s0mm04_10))[,2],
                             coeftest(s0mm04_1A,vcov.=vcovHC(s0mm04_1A))[,2],
                             coeftest(s0mm04_1B,vcov.=vcovHC(s0mm04_1B))[,2],
                             coeftest(s0mm04_1C,vcov.=vcovHC(s0mm04_1C))[,2]),
          override.pvalues = list(coeftest(s0mm04_10,vcov.=vcovHC(s0mm04_10))[,4],
                                  coeftest(s0mm04_1A,vcov.=vcovHC(s0mm04_1A))[,4],
                                  coeftest(s0mm04_1B,vcov.=vcovHC(s0mm04_1B))[,4],
                                  coeftest(s0mm04_1C,vcov.=vcovHC(s0mm04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mm04_10,s0mm04_1A,s0mm04_1B,s0mm04_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mm04_10,vcov.=vcovHC(s0mm04_10))[,2],
                          coeftest(s0mm04_1A,vcov.=vcovHC(s0mm04_1A))[,2],
                          coeftest(s0mm04_1B,vcov.=vcovHC(s0mm04_1B))[,2],
                          coeftest(s0mm04_1C,vcov.=vcovHC(s0mm04_1C))[,2]),
       override.pvalues = list(coeftest(s0mm04_10,vcov.=vcovHC(s0mm04_10))[,4],
                               coeftest(s0mm04_1A,vcov.=vcovHC(s0mm04_1A))[,4],
                               coeftest(s0mm04_1B,vcov.=vcovHC(s0mm04_1B))[,4],
                               coeftest(s0mm04_1C,vcov.=vcovHC(s0mm04_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mm04_1_tabular.tex"))

#'
#' ## Favorability of China
#'

s0mm05_10 <- lm(update(familiarityFT_CHN ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm05_1A <- lm(update(familiarityFT_CHN ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm05_1B <- lm(update(familiarityFT_CHN ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm05_1C <- lm(update(familiarityFT_CHN ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s0mm05_10,s0mm05_1A,s0mm05_1B,s0mm05_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mm05_10,vcov.=vcovHC(s0mm05_10))[,2],
                             coeftest(s0mm05_1A,vcov.=vcovHC(s0mm05_1A))[,2],
                             coeftest(s0mm05_1B,vcov.=vcovHC(s0mm05_1B))[,2],
                             coeftest(s0mm05_1C,vcov.=vcovHC(s0mm05_1C))[,2]),
          override.pvalues = list(coeftest(s0mm05_10,vcov.=vcovHC(s0mm05_10))[,4],
                                  coeftest(s0mm05_1A,vcov.=vcovHC(s0mm05_1A))[,4],
                                  coeftest(s0mm05_1B,vcov.=vcovHC(s0mm05_1B))[,4],
                                  coeftest(s0mm05_1C,vcov.=vcovHC(s0mm05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mm05_10,s0mm05_1A,s0mm05_1B,s0mm05_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mm05_10,vcov.=vcovHC(s0mm05_10))[,2],
                          coeftest(s0mm05_1A,vcov.=vcovHC(s0mm05_1A))[,2],
                          coeftest(s0mm05_1B,vcov.=vcovHC(s0mm05_1B))[,2],
                          coeftest(s0mm05_1C,vcov.=vcovHC(s0mm05_1C))[,2]),
       override.pvalues = list(coeftest(s0mm05_10,vcov.=vcovHC(s0mm05_10))[,4],
                               coeftest(s0mm05_1A,vcov.=vcovHC(s0mm05_1A))[,4],
                               coeftest(s0mm05_1B,vcov.=vcovHC(s0mm05_1B))[,4],
                               coeftest(s0mm05_1C,vcov.=vcovHC(s0mm05_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mm05_1_tabular.tex"))

#'
#' ## Favorability of USA
#'

s0mm06_10 <- lm(update(familiarityFT_USA ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm06_1A <- lm(update(familiarityFT_USA ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm06_1B <- lm(update(familiarityFT_USA ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm06_1C <- lm(update(familiarityFT_USA ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s0mm06_10,s0mm06_1A,s0mm06_1B,s0mm06_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mm06_10,vcov.=vcovHC(s0mm06_10))[,2],
                             coeftest(s0mm06_1A,vcov.=vcovHC(s0mm06_1A))[,2],
                             coeftest(s0mm06_1B,vcov.=vcovHC(s0mm06_1B))[,2],
                             coeftest(s0mm06_1C,vcov.=vcovHC(s0mm06_1C))[,2]),
          override.pvalues = list(coeftest(s0mm06_10,vcov.=vcovHC(s0mm06_10))[,4],
                                  coeftest(s0mm06_1A,vcov.=vcovHC(s0mm06_1A))[,4],
                                  coeftest(s0mm06_1B,vcov.=vcovHC(s0mm06_1B))[,4],
                                  coeftest(s0mm06_1C,vcov.=vcovHC(s0mm06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mm06_10,s0mm06_1A,s0mm06_1B,s0mm06_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mm06_10,vcov.=vcovHC(s0mm06_10))[,2],
                          coeftest(s0mm06_1A,vcov.=vcovHC(s0mm06_1A))[,2],
                          coeftest(s0mm06_1B,vcov.=vcovHC(s0mm06_1B))[,2],
                          coeftest(s0mm06_1C,vcov.=vcovHC(s0mm06_1C))[,2]),
       override.pvalues = list(coeftest(s0mm06_10,vcov.=vcovHC(s0mm06_10))[,4],
                               coeftest(s0mm06_1A,vcov.=vcovHC(s0mm06_1A))[,4],
                               coeftest(s0mm06_1B,vcov.=vcovHC(s0mm06_1B))[,4],
                               coeftest(s0mm06_1C,vcov.=vcovHC(s0mm06_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mm06_1_tabular.tex"))


#' 
#' ## Income
#'

s0mm07_10 <- lm(update(income ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm07_1A <- lm(update(income ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm07_1B <- lm(update(income ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s0mm07_1C <- lm(update(income ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s0mm07_10,s0mm07_1A,s0mm07_1B,s0mm07_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mm07_10,vcov.=vcovHC(s0mm07_10))[,2],
                             coeftest(s0mm07_1A,vcov.=vcovHC(s0mm07_1A))[,2],
                             coeftest(s0mm07_1B,vcov.=vcovHC(s0mm07_1B))[,2],
                             coeftest(s0mm07_1C,vcov.=vcovHC(s0mm07_1C))[,2]),
          override.pvalues = list(coeftest(s0mm07_10,vcov.=vcovHC(s0mm07_10))[,4],
                                  coeftest(s0mm07_1A,vcov.=vcovHC(s0mm07_1A))[,4],
                                  coeftest(s0mm07_1B,vcov.=vcovHC(s0mm07_1B))[,4],
                                  coeftest(s0mm07_1C,vcov.=vcovHC(s0mm07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mm07_10,s0mm07_1A,s0mm07_1B,s0mm07_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mm07_10,vcov.=vcovHC(s0mm07_10))[,2],
                          coeftest(s0mm07_1A,vcov.=vcovHC(s0mm07_1A))[,2],
                          coeftest(s0mm07_1B,vcov.=vcovHC(s0mm07_1B))[,2],
                          coeftest(s0mm07_1C,vcov.=vcovHC(s0mm07_1C))[,2]),
       override.pvalues = list(coeftest(s0mm07_10,vcov.=vcovHC(s0mm07_10))[,4],
                               coeftest(s0mm07_1A,vcov.=vcovHC(s0mm07_1A))[,4],
                               coeftest(s0mm07_1B,vcov.=vcovHC(s0mm07_1B))[,4],
                               coeftest(s0mm07_1C,vcov.=vcovHC(s0mm07_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mm07_1_tabular.tex"))

#'
#' # With Matched Data (Without Distance Adjustment)
#'

sifcct <- readRDS(datadir1)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)

#'
#' ## Outcome Model 
#'

## Living in Local ZIP since at least age 15 ##

s1mo_10 <- lm(update(foreignsuff ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mo_1A <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mo_1B <- lm(update(foreignsuff ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mo_1C <- lm(update(foreignsuff ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s1mo_10,s1mo_1A,s1mo_1B,s1mo_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mo_10,vcov.=vcovHC(s1mo_10))[,2],
                             coeftest(s1mo_1A,vcov.=vcovHC(s1mo_1A))[,2],
                             coeftest(s1mo_1B,vcov.=vcovHC(s1mo_1B))[,2],
                             coeftest(s1mo_1C,vcov.=vcovHC(s1mo_1C))[,2]),
          override.pvalues = list(coeftest(s1mo_10,vcov.=vcovHC(s1mo_10))[,4],
                                  coeftest(s1mo_1A,vcov.=vcovHC(s1mo_1A))[,4],
                                  coeftest(s1mo_1B,vcov.=vcovHC(s1mo_1B))[,4],
                                  coeftest(s1mo_1C,vcov.=vcovHC(s1mo_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mo_10,s1mo_1A,s1mo_1B,s1mo_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mo_10,vcov.=vcovHC(s1mo_10))[,2],
                          coeftest(s1mo_1A,vcov.=vcovHC(s1mo_1A))[,2],
                          coeftest(s1mo_1B,vcov.=vcovHC(s1mo_1B))[,2],
                          coeftest(s1mo_1C,vcov.=vcovHC(s1mo_1C))[,2]),
       override.pvalues = list(coeftest(s1mo_10,vcov.=vcovHC(s1mo_10))[,4],
                               coeftest(s1mo_1A,vcov.=vcovHC(s1mo_1A))[,4],
                               coeftest(s1mo_1B,vcov.=vcovHC(s1mo_1B))[,4],
                               coeftest(s1mo_1C,vcov.=vcovHC(s1mo_1C))[,4]),
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mo_1_tabular.tex"))

#+
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

outdt1 <- rbind(extout("Female",25),
                extout("Female",35),
                extout("Female",45),
                extout("Female",55),
                extout("Female",65),
                extout("Male",25),
                extout("Male",35),
                extout("Male",45),
                extout("Male",55),
                extout("Male",65))
outdt1 <- as.data.frame(outdt1)
for(i in 2:9) outdt1[,i] <- as.numeric(outdt1[,i])
outdt1$gender <- factor(outdt1$gender, levels=unique(outdt1$gender))
summary(outdt1)

#'
#' ## Outcome Model 2
#'

## Living in Local ZIP since at least age 15 ##

# require(nnet)
# s1mo2_10 <- multinom(update(foreignsuff3x ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s1mo2_1A <- multinom(update(foreignsuff3x ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s1mo2_1B <- multinom(update(foreignsuff3x ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s1mo2_1C <- multinom(update(foreignsuff3x ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])

sifcct.mlogit <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen<=15),],
                       shape = "wide", choice = "foreignsuff3x")
# # levels(sifcct.mlogit$idx$id2) <- c("Disagree","Neither","Agree")
s1mo2_10 <- mlogit(outmod0.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s1mo2_1A <- mlogit(outmodA.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s1mo2_1B <- mlogit(outmodB.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s1mo2_1C <- mlogit(outmodC.mlogit, data=sifcct.mlogit, reflevel="Disagree")

screenreg(list(s1mo2_10,s1mo2_1A), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_10))),2],
                             coeftest(s1mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_10))),2],
                             coeftest(s1mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1A))),2],
                             coeftest(s1mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1A))),2]),
          override.pvalues = list(coeftest(s1mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_10))),4],
                                  coeftest(s1mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_10))),4],
                                  coeftest(s1mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1A))),4],
                                  coeftest(s1mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+", 
          custom.model.names = c("Base: Agree","Base: Neither",
                                 "ZIP: Agree","ZIP: Neither"),
          custom.coef.map = vnmap)
screenreg(list(s1mo2_1B,s1mo2_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1B))),2],
                             coeftest(s1mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1B))),2],
                             coeftest(s1mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1C))),2],
                             coeftest(s1mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1C))),2]),
          override.pvalues = list(coeftest(s1mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1B))),4],
                                  coeftest(s1mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1B))),4],
                                  coeftest(s1mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1C))),4],
                                  coeftest(s1mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Agree","Mun.: Neither",
                                 "Full: Agree","Full: Neither"))

#+ include = FALSE
texreg(list(s1mo2_10,s1mo2_1A), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_10))),2],
                          coeftest(s1mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_10))),2],
                          coeftest(s1mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1A))),2],
                          coeftest(s1mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1A))),2]),
       override.pvalues = list(coeftest(s1mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_10))),4],
                               coeftest(s1mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_10))),4],
                               coeftest(s1mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1A))),4],
                               coeftest(s1mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1A))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger", 
       custom.model.names = c("Base: Agree","Base: Neither",
                              "ZIP: Agree","ZIP: Neither"),
       custom.coef.map = vnmap,
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mo2_1_1_tabular.tex"))
texreg(list(s1mo2_1B,s1mo2_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1B))),2],
                          coeftest(s1mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1B))),2],
                          coeftest(s1mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1C))),2],
                          coeftest(s1mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1C))),2]),
       override.pvalues = list(coeftest(s1mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1B))),4],
                               coeftest(s1mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1B))),4],
                               coeftest(s1mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s1mo2_1C))),4],
                               coeftest(s1mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s1mo2_1C))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap,
       custom.model.names = c("Mun.: Agree","Mun.: Neither",
                              "Full: Agree","Full: Neither"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mo2_1_2_tabular.tex"))

#+
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
                       as.factor(wave), data=sifcct.mlogit.tmp, reflevel = "Disagree")
    subname = "Stayed"
  } else {
    # modset <- multinom(foreignsuff3x ~ edu2 * gender * ageset + lvpr + as.factor(wave), 
    #                    data=sifcct[which(sifcct$age - sifcct$lvlen>=23),],
    #                    Hess = TRUE)
    sifcct.mlogit.tmp <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen>=23),],
                               shape = "wide", choice = "foreignsuff3x")
    # levels(sifcct.mlogit.tmp$idx$id2) <- c("Disagree","Neither","Agree")
    modset <- mlogit(foreignsuff3x ~ 0 | edu2 * gender * ageset + lvpr + as.factor(wave), 
                     data=sifcct.mlogit.tmp, reflevel = "Disagree")
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

outdt1m <- rbind(extout("Female",25,1),
                 extout("Female",35,1),
                 extout("Female",45,1),
                 extout("Female",55,1),
                 extout("Female",65,1),
                 extout("Male",25,1),
                 extout("Male",35,1),
                 extout("Male",45,1),
                 extout("Male",55,1),
                 extout("Male",65,1))
outdt1m <- as.data.frame(outdt1m)
for(i in 2:9) outdt1m[,i] <- as.numeric(outdt1m[,i])
outdt1m$gender <- factor(outdt1m$gender, levels=unique(outdt1m$gender))
summary(outdt1m)

#'
#' ## Mediator Models
#' 
#' ## Knowledge
#'

s1mm01_10 <- lm(update(knowledge ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm01_1A <- lm(update(knowledge ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm01_1B <- lm(update(knowledge ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm01_1C <- lm(update(knowledge ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s1mm01_10,s1mm01_1A,s1mm01_1B,s1mm01_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mm01_10,vcov.=vcovHC(s1mm01_10))[,2],
                             coeftest(s1mm01_1A,vcov.=vcovHC(s1mm01_1A))[,2],
                             coeftest(s1mm01_1B,vcov.=vcovHC(s1mm01_1B))[,2],
                             coeftest(s1mm01_1C,vcov.=vcovHC(s1mm01_1C))[,2]),
          override.pvalues = list(coeftest(s1mm01_10,vcov.=vcovHC(s1mm01_10))[,4],
                                  coeftest(s1mm01_1A,vcov.=vcovHC(s1mm01_1A))[,4],
                                  coeftest(s1mm01_1B,vcov.=vcovHC(s1mm01_1B))[,4],
                                  coeftest(s1mm01_1C,vcov.=vcovHC(s1mm01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mm01_10,s1mm01_1A,s1mm01_1B,s1mm01_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mm01_10,vcov.=vcovHC(s1mm01_10))[,2],
                          coeftest(s1mm01_1A,vcov.=vcovHC(s1mm01_1A))[,2],
                          coeftest(s1mm01_1B,vcov.=vcovHC(s1mm01_1B))[,2],
                          coeftest(s1mm01_1C,vcov.=vcovHC(s1mm01_1C))[,2]),
       override.pvalues = list(coeftest(s1mm01_10,vcov.=vcovHC(s1mm01_10))[,4],
                               coeftest(s1mm01_1A,vcov.=vcovHC(s1mm01_1A))[,4],
                               coeftest(s1mm01_1B,vcov.=vcovHC(s1mm01_1B))[,4],
                               coeftest(s1mm01_1C,vcov.=vcovHC(s1mm01_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mm01_1_tabular.tex"))



#'
#' ## Ideology
#'

s1mm02_10 <- lm(update(ideology ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm02_1A <- lm(update(ideology ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm02_1B <- lm(update(ideology ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm02_1C <- lm(update(ideology ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s1mm02_10,s1mm02_1A,s1mm02_1B,s1mm02_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mm02_10,vcov.=vcovHC(s1mm02_10))[,2],
                             coeftest(s1mm02_1A,vcov.=vcovHC(s1mm02_1A))[,2],
                             coeftest(s1mm02_1B,vcov.=vcovHC(s1mm02_1B))[,2],
                             coeftest(s1mm02_1C,vcov.=vcovHC(s1mm02_1C))[,2]),
          override.pvalues = list(coeftest(s1mm02_10,vcov.=vcovHC(s1mm02_10))[,4],
                                  coeftest(s1mm02_1A,vcov.=vcovHC(s1mm02_1A))[,4],
                                  coeftest(s1mm02_1B,vcov.=vcovHC(s1mm02_1B))[,4],
                                  coeftest(s1mm02_1C,vcov.=vcovHC(s1mm02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mm02_10,s1mm02_1A,s1mm02_1B,s1mm02_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mm02_10,vcov.=vcovHC(s1mm02_10))[,2],
                          coeftest(s1mm02_1A,vcov.=vcovHC(s1mm02_1A))[,2],
                          coeftest(s1mm02_1B,vcov.=vcovHC(s1mm02_1B))[,2],
                          coeftest(s1mm02_1C,vcov.=vcovHC(s1mm02_1C))[,2]),
       override.pvalues = list(coeftest(s1mm02_10,vcov.=vcovHC(s1mm02_10))[,4],
                               coeftest(s1mm02_1A,vcov.=vcovHC(s1mm02_1A))[,4],
                               coeftest(s1mm02_1B,vcov.=vcovHC(s1mm02_1B))[,4],
                               coeftest(s1mm02_1C,vcov.=vcovHC(s1mm02_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mm02_1_tabular.tex"))

#'
#' ## LDP - DPJ FT
#'

s1mm03_10 <- lm(update(ldpdpjft ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm03_1A <- lm(update(ldpdpjft ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm03_1B <- lm(update(ldpdpjft ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm03_1C <- lm(update(ldpdpjft ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s1mm03_10,s1mm03_1A,s1mm03_1B,s1mm03_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mm03_10,vcov.=vcovHC(s1mm03_10))[,2],
                             coeftest(s1mm03_1A,vcov.=vcovHC(s1mm03_1A))[,2],
                             coeftest(s1mm03_1B,vcov.=vcovHC(s1mm03_1B))[,2],
                             coeftest(s1mm03_1C,vcov.=vcovHC(s1mm03_1C))[,2]),
          override.pvalues = list(coeftest(s1mm03_10,vcov.=vcovHC(s1mm03_10))[,4],
                                  coeftest(s1mm03_1A,vcov.=vcovHC(s1mm03_1A))[,4],
                                  coeftest(s1mm03_1B,vcov.=vcovHC(s1mm03_1B))[,4],
                                  coeftest(s1mm03_1C,vcov.=vcovHC(s1mm03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mm03_10,s1mm03_1A,s1mm03_1B,s1mm03_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mm03_10,vcov.=vcovHC(s1mm03_10))[,2],
                          coeftest(s1mm03_1A,vcov.=vcovHC(s1mm03_1A))[,2],
                          coeftest(s1mm03_1B,vcov.=vcovHC(s1mm03_1B))[,2],
                          coeftest(s1mm03_1C,vcov.=vcovHC(s1mm03_1C))[,2]),
       override.pvalues = list(coeftest(s1mm03_10,vcov.=vcovHC(s1mm03_10))[,4],
                               coeftest(s1mm03_1A,vcov.=vcovHC(s1mm03_1A))[,4],
                               coeftest(s1mm03_1B,vcov.=vcovHC(s1mm03_1B))[,4],
                               coeftest(s1mm03_1C,vcov.=vcovHC(s1mm03_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mm03_1_tabular.tex"))


#'
#' ## Favorability of South Korea
#'

s1mm04_10 <- lm(update(familiarityFT_KOR ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm04_1A <- lm(update(familiarityFT_KOR ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm04_1B <- lm(update(familiarityFT_KOR ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm04_1C <- lm(update(familiarityFT_KOR ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s1mm04_10,s1mm04_1A,s1mm04_1B,s1mm04_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mm04_10,vcov.=vcovHC(s1mm04_10))[,2],
                             coeftest(s1mm04_1A,vcov.=vcovHC(s1mm04_1A))[,2],
                             coeftest(s1mm04_1B,vcov.=vcovHC(s1mm04_1B))[,2],
                             coeftest(s1mm04_1C,vcov.=vcovHC(s1mm04_1C))[,2]),
          override.pvalues = list(coeftest(s1mm04_10,vcov.=vcovHC(s1mm04_10))[,4],
                                  coeftest(s1mm04_1A,vcov.=vcovHC(s1mm04_1A))[,4],
                                  coeftest(s1mm04_1B,vcov.=vcovHC(s1mm04_1B))[,4],
                                  coeftest(s1mm04_1C,vcov.=vcovHC(s1mm04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mm04_10,s1mm04_1A,s1mm04_1B,s1mm04_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mm04_10,vcov.=vcovHC(s1mm04_10))[,2],
                          coeftest(s1mm04_1A,vcov.=vcovHC(s1mm04_1A))[,2],
                          coeftest(s1mm04_1B,vcov.=vcovHC(s1mm04_1B))[,2],
                          coeftest(s1mm04_1C,vcov.=vcovHC(s1mm04_1C))[,2]),
       override.pvalues = list(coeftest(s1mm04_10,vcov.=vcovHC(s1mm04_10))[,4],
                               coeftest(s1mm04_1A,vcov.=vcovHC(s1mm04_1A))[,4],
                               coeftest(s1mm04_1B,vcov.=vcovHC(s1mm04_1B))[,4],
                               coeftest(s1mm04_1C,vcov.=vcovHC(s1mm04_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mm04_1_tabular.tex"))

#'
#' ## Favorability of China
#'

s1mm05_10 <- lm(update(familiarityFT_CHN ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm05_1A <- lm(update(familiarityFT_CHN ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm05_1B <- lm(update(familiarityFT_CHN ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm05_1C <- lm(update(familiarityFT_CHN ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s1mm05_10,s1mm05_1A,s1mm05_1B,s1mm05_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mm05_10,vcov.=vcovHC(s1mm05_10))[,2],
                             coeftest(s1mm05_1A,vcov.=vcovHC(s1mm05_1A))[,2],
                             coeftest(s1mm05_1B,vcov.=vcovHC(s1mm05_1B))[,2],
                             coeftest(s1mm05_1C,vcov.=vcovHC(s1mm05_1C))[,2]),
          override.pvalues = list(coeftest(s1mm05_10,vcov.=vcovHC(s1mm05_10))[,4],
                                  coeftest(s1mm05_1A,vcov.=vcovHC(s1mm05_1A))[,4],
                                  coeftest(s1mm05_1B,vcov.=vcovHC(s1mm05_1B))[,4],
                                  coeftest(s1mm05_1C,vcov.=vcovHC(s1mm05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mm05_10,s1mm05_1A,s1mm05_1B,s1mm05_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mm05_10,vcov.=vcovHC(s1mm05_10))[,2],
                          coeftest(s1mm05_1A,vcov.=vcovHC(s1mm05_1A))[,2],
                          coeftest(s1mm05_1B,vcov.=vcovHC(s1mm05_1B))[,2],
                          coeftest(s1mm05_1C,vcov.=vcovHC(s1mm05_1C))[,2]),
       override.pvalues = list(coeftest(s1mm05_10,vcov.=vcovHC(s1mm05_10))[,4],
                               coeftest(s1mm05_1A,vcov.=vcovHC(s1mm05_1A))[,4],
                               coeftest(s1mm05_1B,vcov.=vcovHC(s1mm05_1B))[,4],
                               coeftest(s1mm05_1C,vcov.=vcovHC(s1mm05_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mm05_1_tabular.tex"))

#'
#' ## Favorability of USA
#'

s1mm06_10 <- lm(update(familiarityFT_USA ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm06_1A <- lm(update(familiarityFT_USA ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm06_1B <- lm(update(familiarityFT_USA ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm06_1C <- lm(update(familiarityFT_USA ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s1mm06_10,s1mm06_1A,s1mm06_1B,s1mm06_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mm06_10,vcov.=vcovHC(s1mm06_10))[,2],
                             coeftest(s1mm06_1A,vcov.=vcovHC(s1mm06_1A))[,2],
                             coeftest(s1mm06_1B,vcov.=vcovHC(s1mm06_1B))[,2],
                             coeftest(s1mm06_1C,vcov.=vcovHC(s1mm06_1C))[,2]),
          override.pvalues = list(coeftest(s1mm06_10,vcov.=vcovHC(s1mm06_10))[,4],
                                  coeftest(s1mm06_1A,vcov.=vcovHC(s1mm06_1A))[,4],
                                  coeftest(s1mm06_1B,vcov.=vcovHC(s1mm06_1B))[,4],
                                  coeftest(s1mm06_1C,vcov.=vcovHC(s1mm06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mm06_10,s1mm06_1A,s1mm06_1B,s1mm06_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mm06_10,vcov.=vcovHC(s1mm06_10))[,2],
                          coeftest(s1mm06_1A,vcov.=vcovHC(s1mm06_1A))[,2],
                          coeftest(s1mm06_1B,vcov.=vcovHC(s1mm06_1B))[,2],
                          coeftest(s1mm06_1C,vcov.=vcovHC(s1mm06_1C))[,2]),
       override.pvalues = list(coeftest(s1mm06_10,vcov.=vcovHC(s1mm06_10))[,4],
                               coeftest(s1mm06_1A,vcov.=vcovHC(s1mm06_1A))[,4],
                               coeftest(s1mm06_1B,vcov.=vcovHC(s1mm06_1B))[,4],
                               coeftest(s1mm06_1C,vcov.=vcovHC(s1mm06_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mm06_1_tabular.tex"))


#' 
#' ## Income
#'

s1mm07_10 <- lm(update(income ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm07_1A <- lm(update(income ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm07_1B <- lm(update(income ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s1mm07_1C <- lm(update(income ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s1mm07_10,s1mm07_1A,s1mm07_1B,s1mm07_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mm07_10,vcov.=vcovHC(s1mm07_10))[,2],
                             coeftest(s1mm07_1A,vcov.=vcovHC(s1mm07_1A))[,2],
                             coeftest(s1mm07_1B,vcov.=vcovHC(s1mm07_1B))[,2],
                             coeftest(s1mm07_1C,vcov.=vcovHC(s1mm07_1C))[,2]),
          override.pvalues = list(coeftest(s1mm07_10,vcov.=vcovHC(s1mm07_10))[,4],
                                  coeftest(s1mm07_1A,vcov.=vcovHC(s1mm07_1A))[,4],
                                  coeftest(s1mm07_1B,vcov.=vcovHC(s1mm07_1B))[,4],
                                  coeftest(s1mm07_1C,vcov.=vcovHC(s1mm07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mm07_10,s1mm07_1A,s1mm07_1B,s1mm07_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mm07_10,vcov.=vcovHC(s1mm07_10))[,2],
                          coeftest(s1mm07_1A,vcov.=vcovHC(s1mm07_1A))[,2],
                          coeftest(s1mm07_1B,vcov.=vcovHC(s1mm07_1B))[,2],
                          coeftest(s1mm07_1C,vcov.=vcovHC(s1mm07_1C))[,2]),
       override.pvalues = list(coeftest(s1mm07_10,vcov.=vcovHC(s1mm07_10))[,4],
                               coeftest(s1mm07_1A,vcov.=vcovHC(s1mm07_1A))[,4],
                               coeftest(s1mm07_1B,vcov.=vcovHC(s1mm07_1B))[,4],
                               coeftest(s1mm07_1C,vcov.=vcovHC(s1mm07_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mm07_1_tabular.tex"))

#'
#' # With Matched Data (Lambda = 50km)
#'

sifcct <- readRDS(datadir2)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)

#'
#' ## Outcome Model 
#'

## Living in Local ZIP since at least age 15 ##

s2mo_10 <- lm(update(foreignsuff ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mo_1A <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mo_1B <- lm(update(foreignsuff ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mo_1C <- lm(update(foreignsuff ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s2mo_10,s2mo_1A,s2mo_1B,s2mo_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s2mo_10,vcov.=vcovHC(s2mo_10))[,2],
                             coeftest(s2mo_1A,vcov.=vcovHC(s2mo_1A))[,2],
                             coeftest(s2mo_1B,vcov.=vcovHC(s2mo_1B))[,2],
                             coeftest(s2mo_1C,vcov.=vcovHC(s2mo_1C))[,2]),
          override.pvalues = list(coeftest(s2mo_10,vcov.=vcovHC(s2mo_10))[,4],
                                  coeftest(s2mo_1A,vcov.=vcovHC(s2mo_1A))[,4],
                                  coeftest(s2mo_1B,vcov.=vcovHC(s2mo_1B))[,4],
                                  coeftest(s2mo_1C,vcov.=vcovHC(s2mo_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s2mo_10,s2mo_1A,s2mo_1B,s2mo_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s2mo_10,vcov.=vcovHC(s2mo_10))[,2],
                          coeftest(s2mo_1A,vcov.=vcovHC(s2mo_1A))[,2],
                          coeftest(s2mo_1B,vcov.=vcovHC(s2mo_1B))[,2],
                          coeftest(s2mo_1C,vcov.=vcovHC(s2mo_1C))[,2]),
       override.pvalues = list(coeftest(s2mo_10,vcov.=vcovHC(s2mo_10))[,4],
                               coeftest(s2mo_1A,vcov.=vcovHC(s2mo_1A))[,4],
                               coeftest(s2mo_1B,vcov.=vcovHC(s2mo_1B))[,4],
                               coeftest(s2mo_1C,vcov.=vcovHC(s2mo_1C))[,4]),
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2mo_1_tabular.tex"))

#+
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

outdt2 <- rbind(extout("Female",25),
                extout("Female",35),
                extout("Female",45),
                extout("Female",55),
                extout("Female",65),
                extout("Male",25),
                extout("Male",35),
                extout("Male",45),
                extout("Male",55),
                extout("Male",65))
outdt2 <- as.data.frame(outdt2)
for(i in 2:9) outdt2[,i] <- as.numeric(outdt2[,i])
outdt2$gender <- factor(outdt2$gender, levels=unique(outdt2$gender))
summary(outdt2)

#'
#' ## Outcome Model 2
#'

## Living in Local ZIP since at least age 15 ##

# require(nnet)
# s2mo2_10 <- multinom(update(foreignsuff3x ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s2mo2_1A <- multinom(update(foreignsuff3x ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s2mo2_1B <- multinom(update(foreignsuff3x ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s2mo2_1C <- multinom(update(foreignsuff3x ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])

sifcct.mlogit <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen<=15),],
                       shape = "wide", choice = "foreignsuff3x")
# # levels(sifcct.mlogit$idx$id2) <- c("Disagree","Neither","Agree")
s2mo2_10 <- mlogit(outmod0.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s2mo2_1A <- mlogit(outmodA.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s2mo2_1B <- mlogit(outmodB.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s2mo2_1C <- mlogit(outmodC.mlogit, data=sifcct.mlogit, reflevel="Disagree")

screenreg(list(s2mo2_10,s2mo2_1A), digits = 4, #single.row = T,
          override.se = list(coeftest(s2mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_10))),2],
                             coeftest(s2mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_10))),2],
                             coeftest(s2mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1A))),2],
                             coeftest(s2mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1A))),2]),
          override.pvalues = list(coeftest(s2mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_10))),4],
                                  coeftest(s2mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_10))),4],
                                  coeftest(s2mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1A))),4],
                                  coeftest(s2mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+", 
          custom.model.names = c("Base: Agree","Base: Neither",
                                 "ZIP: Agree","ZIP: Neither"),
          custom.coef.map = vnmap)
screenreg(list(s2mo2_1B,s2mo2_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s2mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1B))),2],
                             coeftest(s2mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1B))),2],
                             coeftest(s2mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1C))),2],
                             coeftest(s2mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1C))),2]),
          override.pvalues = list(coeftest(s2mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1B))),4],
                                  coeftest(s2mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1B))),4],
                                  coeftest(s2mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1C))),4],
                                  coeftest(s2mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Agree","Mun.: Neither",
                                 "Full: Agree","Full: Neither"))

#+ include = FALSE
texreg(list(s2mo2_10,s2mo2_1A), digits = 4, #single.row = T,
       override.se = list(coeftest(s2mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_10))),2],
                          coeftest(s2mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_10))),2],
                          coeftest(s2mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1A))),2],
                          coeftest(s2mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1A))),2]),
       override.pvalues = list(coeftest(s2mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_10))),4],
                               coeftest(s2mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_10))),4],
                               coeftest(s2mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1A))),4],
                               coeftest(s2mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1A))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger", 
       custom.model.names = c("Base: Agree","Base: Neither",
                              "ZIP: Agree","ZIP: Neither"),
       custom.coef.map = vnmap,
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2mo2_1_1_tabular.tex"))
texreg(list(s2mo2_1B,s2mo2_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s2mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1B))),2],
                          coeftest(s2mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1B))),2],
                          coeftest(s2mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1C))),2],
                          coeftest(s2mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1C))),2]),
       override.pvalues = list(coeftest(s2mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1B))),4],
                               coeftest(s2mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1B))),4],
                               coeftest(s2mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s2mo2_1C))),4],
                               coeftest(s2mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s2mo2_1C))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap,
       custom.model.names = c("Mun.: Agree","Mun.: Neither",
                              "Full: Agree","Full: Neither"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2mo2_1_2_tabular.tex"))

#+
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
                       as.factor(wave), data=sifcct.mlogit.tmp, reflevel = "Disagree")
    subname = "Stayed"
  } else {
    # modset <- multinom(foreignsuff3x ~ edu2 * gender * ageset + lvpr + as.factor(wave), 
    #                    data=sifcct[which(sifcct$age - sifcct$lvlen>=23),],
    #                    Hess = TRUE)
    sifcct.mlogit.tmp <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen>=23),],
                               shape = "wide", choice = "foreignsuff3x")
    # levels(sifcct.mlogit.tmp$idx$id2) <- c("Disagree","Neither","Agree")
    modset <- mlogit(foreignsuff3x ~ 0 | edu2 * gender * ageset + lvpr + as.factor(wave), 
                     data=sifcct.mlogit.tmp, reflevel = "Disagree")
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

outdt2m <- rbind(extout("Female",25,1),
                 extout("Female",35,1),
                 extout("Female",45,1),
                 extout("Female",55,1),
                 extout("Female",65,1),
                 extout("Male",25,1),
                 extout("Male",35,1),
                 extout("Male",45,1),
                 extout("Male",55,1),
                 extout("Male",65,1))
outdt2m <- as.data.frame(outdt2m)
for(i in 2:9) outdt2m[,i] <- as.numeric(outdt2m[,i])
outdt2m$gender <- factor(outdt2m$gender, levels=unique(outdt2m$gender))
summary(outdt2m)

#'
#' ## Mediator Models
#' 
#' ## Knowledge
#'

s2mm01_10 <- lm(update(knowledge ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm01_1A <- lm(update(knowledge ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm01_1B <- lm(update(knowledge ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm01_1C <- lm(update(knowledge ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s2mm01_10,s2mm01_1A,s2mm01_1B,s2mm01_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s2mm01_10,vcov.=vcovHC(s2mm01_10))[,2],
                             coeftest(s2mm01_1A,vcov.=vcovHC(s2mm01_1A))[,2],
                             coeftest(s2mm01_1B,vcov.=vcovHC(s2mm01_1B))[,2],
                             coeftest(s2mm01_1C,vcov.=vcovHC(s2mm01_1C))[,2]),
          override.pvalues = list(coeftest(s2mm01_10,vcov.=vcovHC(s2mm01_10))[,4],
                                  coeftest(s2mm01_1A,vcov.=vcovHC(s2mm01_1A))[,4],
                                  coeftest(s2mm01_1B,vcov.=vcovHC(s2mm01_1B))[,4],
                                  coeftest(s2mm01_1C,vcov.=vcovHC(s2mm01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s2mm01_10,s2mm01_1A,s2mm01_1B,s2mm01_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s2mm01_10,vcov.=vcovHC(s2mm01_10))[,2],
                          coeftest(s2mm01_1A,vcov.=vcovHC(s2mm01_1A))[,2],
                          coeftest(s2mm01_1B,vcov.=vcovHC(s2mm01_1B))[,2],
                          coeftest(s2mm01_1C,vcov.=vcovHC(s2mm01_1C))[,2]),
       override.pvalues = list(coeftest(s2mm01_10,vcov.=vcovHC(s2mm01_10))[,4],
                               coeftest(s2mm01_1A,vcov.=vcovHC(s2mm01_1A))[,4],
                               coeftest(s2mm01_1B,vcov.=vcovHC(s2mm01_1B))[,4],
                               coeftest(s2mm01_1C,vcov.=vcovHC(s2mm01_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2mm01_1_tabular.tex"))



#'
#' ## Ideology
#'

s2mm02_10 <- lm(update(ideology ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm02_1A <- lm(update(ideology ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm02_1B <- lm(update(ideology ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm02_1C <- lm(update(ideology ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s2mm02_10,s2mm02_1A,s2mm02_1B,s2mm02_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s2mm02_10,vcov.=vcovHC(s2mm02_10))[,2],
                             coeftest(s2mm02_1A,vcov.=vcovHC(s2mm02_1A))[,2],
                             coeftest(s2mm02_1B,vcov.=vcovHC(s2mm02_1B))[,2],
                             coeftest(s2mm02_1C,vcov.=vcovHC(s2mm02_1C))[,2]),
          override.pvalues = list(coeftest(s2mm02_10,vcov.=vcovHC(s2mm02_10))[,4],
                                  coeftest(s2mm02_1A,vcov.=vcovHC(s2mm02_1A))[,4],
                                  coeftest(s2mm02_1B,vcov.=vcovHC(s2mm02_1B))[,4],
                                  coeftest(s2mm02_1C,vcov.=vcovHC(s2mm02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s2mm02_10,s2mm02_1A,s2mm02_1B,s2mm02_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s2mm02_10,vcov.=vcovHC(s2mm02_10))[,2],
                          coeftest(s2mm02_1A,vcov.=vcovHC(s2mm02_1A))[,2],
                          coeftest(s2mm02_1B,vcov.=vcovHC(s2mm02_1B))[,2],
                          coeftest(s2mm02_1C,vcov.=vcovHC(s2mm02_1C))[,2]),
       override.pvalues = list(coeftest(s2mm02_10,vcov.=vcovHC(s2mm02_10))[,4],
                               coeftest(s2mm02_1A,vcov.=vcovHC(s2mm02_1A))[,4],
                               coeftest(s2mm02_1B,vcov.=vcovHC(s2mm02_1B))[,4],
                               coeftest(s2mm02_1C,vcov.=vcovHC(s2mm02_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2mm02_1_tabular.tex"))

#'
#' ## LDP - DPJ FT
#'

s2mm03_10 <- lm(update(ldpdpjft ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm03_1A <- lm(update(ldpdpjft ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm03_1B <- lm(update(ldpdpjft ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm03_1C <- lm(update(ldpdpjft ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s2mm03_10,s2mm03_1A,s2mm03_1B,s2mm03_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s2mm03_10,vcov.=vcovHC(s2mm03_10))[,2],
                             coeftest(s2mm03_1A,vcov.=vcovHC(s2mm03_1A))[,2],
                             coeftest(s2mm03_1B,vcov.=vcovHC(s2mm03_1B))[,2],
                             coeftest(s2mm03_1C,vcov.=vcovHC(s2mm03_1C))[,2]),
          override.pvalues = list(coeftest(s2mm03_10,vcov.=vcovHC(s2mm03_10))[,4],
                                  coeftest(s2mm03_1A,vcov.=vcovHC(s2mm03_1A))[,4],
                                  coeftest(s2mm03_1B,vcov.=vcovHC(s2mm03_1B))[,4],
                                  coeftest(s2mm03_1C,vcov.=vcovHC(s2mm03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s2mm03_10,s2mm03_1A,s2mm03_1B,s2mm03_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s2mm03_10,vcov.=vcovHC(s2mm03_10))[,2],
                          coeftest(s2mm03_1A,vcov.=vcovHC(s2mm03_1A))[,2],
                          coeftest(s2mm03_1B,vcov.=vcovHC(s2mm03_1B))[,2],
                          coeftest(s2mm03_1C,vcov.=vcovHC(s2mm03_1C))[,2]),
       override.pvalues = list(coeftest(s2mm03_10,vcov.=vcovHC(s2mm03_10))[,4],
                               coeftest(s2mm03_1A,vcov.=vcovHC(s2mm03_1A))[,4],
                               coeftest(s2mm03_1B,vcov.=vcovHC(s2mm03_1B))[,4],
                               coeftest(s2mm03_1C,vcov.=vcovHC(s2mm03_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2mm03_1_tabular.tex"))


#'
#' ## Favorability of South Korea
#'

s2mm04_10 <- lm(update(familiarityFT_KOR ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm04_1A <- lm(update(familiarityFT_KOR ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm04_1B <- lm(update(familiarityFT_KOR ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm04_1C <- lm(update(familiarityFT_KOR ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s2mm04_10,s2mm04_1A,s2mm04_1B,s2mm04_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s2mm04_10,vcov.=vcovHC(s2mm04_10))[,2],
                             coeftest(s2mm04_1A,vcov.=vcovHC(s2mm04_1A))[,2],
                             coeftest(s2mm04_1B,vcov.=vcovHC(s2mm04_1B))[,2],
                             coeftest(s2mm04_1C,vcov.=vcovHC(s2mm04_1C))[,2]),
          override.pvalues = list(coeftest(s2mm04_10,vcov.=vcovHC(s2mm04_10))[,4],
                                  coeftest(s2mm04_1A,vcov.=vcovHC(s2mm04_1A))[,4],
                                  coeftest(s2mm04_1B,vcov.=vcovHC(s2mm04_1B))[,4],
                                  coeftest(s2mm04_1C,vcov.=vcovHC(s2mm04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s2mm04_10,s2mm04_1A,s2mm04_1B,s2mm04_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s2mm04_10,vcov.=vcovHC(s2mm04_10))[,2],
                          coeftest(s2mm04_1A,vcov.=vcovHC(s2mm04_1A))[,2],
                          coeftest(s2mm04_1B,vcov.=vcovHC(s2mm04_1B))[,2],
                          coeftest(s2mm04_1C,vcov.=vcovHC(s2mm04_1C))[,2]),
       override.pvalues = list(coeftest(s2mm04_10,vcov.=vcovHC(s2mm04_10))[,4],
                               coeftest(s2mm04_1A,vcov.=vcovHC(s2mm04_1A))[,4],
                               coeftest(s2mm04_1B,vcov.=vcovHC(s2mm04_1B))[,4],
                               coeftest(s2mm04_1C,vcov.=vcovHC(s2mm04_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2mm04_1_tabular.tex"))

#'
#' ## Favorability of China
#'

s2mm05_10 <- lm(update(familiarityFT_CHN ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm05_1A <- lm(update(familiarityFT_CHN ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm05_1B <- lm(update(familiarityFT_CHN ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm05_1C <- lm(update(familiarityFT_CHN ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s2mm05_10,s2mm05_1A,s2mm05_1B,s2mm05_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s2mm05_10,vcov.=vcovHC(s2mm05_10))[,2],
                             coeftest(s2mm05_1A,vcov.=vcovHC(s2mm05_1A))[,2],
                             coeftest(s2mm05_1B,vcov.=vcovHC(s2mm05_1B))[,2],
                             coeftest(s2mm05_1C,vcov.=vcovHC(s2mm05_1C))[,2]),
          override.pvalues = list(coeftest(s2mm05_10,vcov.=vcovHC(s2mm05_10))[,4],
                                  coeftest(s2mm05_1A,vcov.=vcovHC(s2mm05_1A))[,4],
                                  coeftest(s2mm05_1B,vcov.=vcovHC(s2mm05_1B))[,4],
                                  coeftest(s2mm05_1C,vcov.=vcovHC(s2mm05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s2mm05_10,s2mm05_1A,s2mm05_1B,s2mm05_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s2mm05_10,vcov.=vcovHC(s2mm05_10))[,2],
                          coeftest(s2mm05_1A,vcov.=vcovHC(s2mm05_1A))[,2],
                          coeftest(s2mm05_1B,vcov.=vcovHC(s2mm05_1B))[,2],
                          coeftest(s2mm05_1C,vcov.=vcovHC(s2mm05_1C))[,2]),
       override.pvalues = list(coeftest(s2mm05_10,vcov.=vcovHC(s2mm05_10))[,4],
                               coeftest(s2mm05_1A,vcov.=vcovHC(s2mm05_1A))[,4],
                               coeftest(s2mm05_1B,vcov.=vcovHC(s2mm05_1B))[,4],
                               coeftest(s2mm05_1C,vcov.=vcovHC(s2mm05_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2mm05_1_tabular.tex"))

#'
#' ## Favorability of USA
#'

s2mm06_10 <- lm(update(familiarityFT_USA ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm06_1A <- lm(update(familiarityFT_USA ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm06_1B <- lm(update(familiarityFT_USA ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm06_1C <- lm(update(familiarityFT_USA ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s2mm06_10,s2mm06_1A,s2mm06_1B,s2mm06_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s2mm06_10,vcov.=vcovHC(s2mm06_10))[,2],
                             coeftest(s2mm06_1A,vcov.=vcovHC(s2mm06_1A))[,2],
                             coeftest(s2mm06_1B,vcov.=vcovHC(s2mm06_1B))[,2],
                             coeftest(s2mm06_1C,vcov.=vcovHC(s2mm06_1C))[,2]),
          override.pvalues = list(coeftest(s2mm06_10,vcov.=vcovHC(s2mm06_10))[,4],
                                  coeftest(s2mm06_1A,vcov.=vcovHC(s2mm06_1A))[,4],
                                  coeftest(s2mm06_1B,vcov.=vcovHC(s2mm06_1B))[,4],
                                  coeftest(s2mm06_1C,vcov.=vcovHC(s2mm06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s2mm06_10,s2mm06_1A,s2mm06_1B,s2mm06_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s2mm06_10,vcov.=vcovHC(s2mm06_10))[,2],
                          coeftest(s2mm06_1A,vcov.=vcovHC(s2mm06_1A))[,2],
                          coeftest(s2mm06_1B,vcov.=vcovHC(s2mm06_1B))[,2],
                          coeftest(s2mm06_1C,vcov.=vcovHC(s2mm06_1C))[,2]),
       override.pvalues = list(coeftest(s2mm06_10,vcov.=vcovHC(s2mm06_10))[,4],
                               coeftest(s2mm06_1A,vcov.=vcovHC(s2mm06_1A))[,4],
                               coeftest(s2mm06_1B,vcov.=vcovHC(s2mm06_1B))[,4],
                               coeftest(s2mm06_1C,vcov.=vcovHC(s2mm06_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2mm06_1_tabular.tex"))


#' 
#' ## Income
#'

s2mm07_10 <- lm(update(income ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm07_1A <- lm(update(income ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm07_1B <- lm(update(income ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s2mm07_1C <- lm(update(income ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s2mm07_10,s2mm07_1A,s2mm07_1B,s2mm07_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s2mm07_10,vcov.=vcovHC(s2mm07_10))[,2],
                             coeftest(s2mm07_1A,vcov.=vcovHC(s2mm07_1A))[,2],
                             coeftest(s2mm07_1B,vcov.=vcovHC(s2mm07_1B))[,2],
                             coeftest(s2mm07_1C,vcov.=vcovHC(s2mm07_1C))[,2]),
          override.pvalues = list(coeftest(s2mm07_10,vcov.=vcovHC(s2mm07_10))[,4],
                                  coeftest(s2mm07_1A,vcov.=vcovHC(s2mm07_1A))[,4],
                                  coeftest(s2mm07_1B,vcov.=vcovHC(s2mm07_1B))[,4],
                                  coeftest(s2mm07_1C,vcov.=vcovHC(s2mm07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s2mm07_10,s2mm07_1A,s2mm07_1B,s2mm07_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s2mm07_10,vcov.=vcovHC(s2mm07_10))[,2],
                          coeftest(s2mm07_1A,vcov.=vcovHC(s2mm07_1A))[,2],
                          coeftest(s2mm07_1B,vcov.=vcovHC(s2mm07_1B))[,2],
                          coeftest(s2mm07_1C,vcov.=vcovHC(s2mm07_1C))[,2]),
       override.pvalues = list(coeftest(s2mm07_10,vcov.=vcovHC(s2mm07_10))[,4],
                               coeftest(s2mm07_1A,vcov.=vcovHC(s2mm07_1A))[,4],
                               coeftest(s2mm07_1B,vcov.=vcovHC(s2mm07_1B))[,4],
                               coeftest(s2mm07_1C,vcov.=vcovHC(s2mm07_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s2mm07_1_tabular.tex"))

#'
#' # With Matched Data (Lambda = 100km)
#'

sifcct <- readRDS(datadir3)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)

#'
#' ## Outcome Model 
#'

## Living in Local ZIP since at least age 15 ##

s3mo_10 <- lm(update(foreignsuff ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mo_1A <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mo_1B <- lm(update(foreignsuff ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mo_1C <- lm(update(foreignsuff ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s3mo_10,s3mo_1A,s3mo_1B,s3mo_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s3mo_10,vcov.=vcovHC(s3mo_10))[,2],
                             coeftest(s3mo_1A,vcov.=vcovHC(s3mo_1A))[,2],
                             coeftest(s3mo_1B,vcov.=vcovHC(s3mo_1B))[,2],
                             coeftest(s3mo_1C,vcov.=vcovHC(s3mo_1C))[,2]),
          override.pvalues = list(coeftest(s3mo_10,vcov.=vcovHC(s3mo_10))[,4],
                                  coeftest(s3mo_1A,vcov.=vcovHC(s3mo_1A))[,4],
                                  coeftest(s3mo_1B,vcov.=vcovHC(s3mo_1B))[,4],
                                  coeftest(s3mo_1C,vcov.=vcovHC(s3mo_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s3mo_10,s3mo_1A,s3mo_1B,s3mo_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s3mo_10,vcov.=vcovHC(s3mo_10))[,2],
                          coeftest(s3mo_1A,vcov.=vcovHC(s3mo_1A))[,2],
                          coeftest(s3mo_1B,vcov.=vcovHC(s3mo_1B))[,2],
                          coeftest(s3mo_1C,vcov.=vcovHC(s3mo_1C))[,2]),
       override.pvalues = list(coeftest(s3mo_10,vcov.=vcovHC(s3mo_10))[,4],
                               coeftest(s3mo_1A,vcov.=vcovHC(s3mo_1A))[,4],
                               coeftest(s3mo_1B,vcov.=vcovHC(s3mo_1B))[,4],
                               coeftest(s3mo_1C,vcov.=vcovHC(s3mo_1C))[,4]),
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s3mo_1_tabular.tex"))

#+
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

outdt3 <- rbind(extout("Female",25),
                extout("Female",35),
                extout("Female",45),
                extout("Female",55),
                extout("Female",65),
                extout("Male",25),
                extout("Male",35),
                extout("Male",45),
                extout("Male",55),
                extout("Male",65))
outdt3 <- as.data.frame(outdt3)
for(i in 2:9) outdt3[,i] <- as.numeric(outdt3[,i])
outdt3$gender <- factor(outdt3$gender, levels=unique(outdt3$gender))
summary(outdt3)

#'
#' ## Outcome Model 2
#'

## Living in Local ZIP since at least age 15 ##

# require(nnet)
# s3mo2_10 <- multinom(update(foreignsuff3x ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s3mo2_1A <- multinom(update(foreignsuff3x ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s3mo2_1B <- multinom(update(foreignsuff3x ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s3mo2_1C <- multinom(update(foreignsuff3x ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])

sifcct.mlogit <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen<=15),],
                       shape = "wide", choice = "foreignsuff3x")
# # levels(sifcct.mlogit$idx$id2) <- c("Disagree","Neither","Agree")
s3mo2_10 <- mlogit(outmod0.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s3mo2_1A <- mlogit(outmodA.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s3mo2_1B <- mlogit(outmodB.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s3mo2_1C <- mlogit(outmodC.mlogit, data=sifcct.mlogit, reflevel="Disagree")

screenreg(list(s3mo2_10,s3mo2_1A), digits = 4, #single.row = T,
          override.se = list(coeftest(s3mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_10))),2],
                             coeftest(s3mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_10))),2],
                             coeftest(s3mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1A))),2],
                             coeftest(s3mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1A))),2]),
          override.pvalues = list(coeftest(s3mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_10))),4],
                                  coeftest(s3mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_10))),4],
                                  coeftest(s3mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1A))),4],
                                  coeftest(s3mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+", 
          custom.model.names = c("Base: Agree","Base: Neither",
                                 "ZIP: Agree","ZIP: Neither"),
          custom.coef.map = vnmap)
screenreg(list(s3mo2_1B,s3mo2_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s3mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1B))),2],
                             coeftest(s3mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1B))),2],
                             coeftest(s3mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1C))),2],
                             coeftest(s3mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1C))),2]),
          override.pvalues = list(coeftest(s3mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1B))),4],
                                  coeftest(s3mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1B))),4],
                                  coeftest(s3mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1C))),4],
                                  coeftest(s3mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Agree","Mun.: Neither",
                                 "Full: Agree","Full: Neither"))

#+ include = FALSE
texreg(list(s3mo2_10,s3mo2_1A), digits = 4, #single.row = T,
       override.se = list(coeftest(s3mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_10))),2],
                          coeftest(s3mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_10))),2],
                          coeftest(s3mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1A))),2],
                          coeftest(s3mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1A))),2]),
       override.pvalues = list(coeftest(s3mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_10))),4],
                               coeftest(s3mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_10))),4],
                               coeftest(s3mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1A))),4],
                               coeftest(s3mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1A))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger", 
       custom.model.names = c("Base: Agree","Base: Neither",
                              "ZIP: Agree","ZIP: Neither"),
       custom.coef.map = vnmap,
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s3mo2_1_1_tabular.tex"))
texreg(list(s3mo2_1B,s3mo2_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s3mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1B))),2],
                          coeftest(s3mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1B))),2],
                          coeftest(s3mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1C))),2],
                          coeftest(s3mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1C))),2]),
       override.pvalues = list(coeftest(s3mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1B))),4],
                               coeftest(s3mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1B))),4],
                               coeftest(s3mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s3mo2_1C))),4],
                               coeftest(s3mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s3mo2_1C))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap,
       custom.model.names = c("Mun.: Agree","Mun.: Neither",
                              "Full: Agree","Full: Neither"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s3mo2_1_2_tabular.tex"))

#+
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
                       as.factor(wave), data=sifcct.mlogit.tmp, reflevel = "Disagree")
    subname = "Stayed"
  } else {
    # modset <- multinom(foreignsuff3x ~ edu2 * gender * ageset + lvpr + as.factor(wave), 
    #                    data=sifcct[which(sifcct$age - sifcct$lvlen>=23),],
    #                    Hess = TRUE)
    sifcct.mlogit.tmp <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen>=23),],
                               shape = "wide", choice = "foreignsuff3x")
    # levels(sifcct.mlogit.tmp$idx$id2) <- c("Disagree","Neither","Agree")
    modset <- mlogit(foreignsuff3x ~ 0 | edu2 * gender * ageset + lvpr + as.factor(wave), 
                     data=sifcct.mlogit.tmp, reflevel = "Disagree")
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

outdt3m <- rbind(extout("Female",25,1),
                 extout("Female",35,1),
                 extout("Female",45,1),
                 extout("Female",55,1),
                 extout("Female",65,1),
                 extout("Male",25,1),
                 extout("Male",35,1),
                 extout("Male",45,1),
                 extout("Male",55,1),
                 extout("Male",65,1))
outdt3m <- as.data.frame(outdt3m)
for(i in 2:9) outdt3m[,i] <- as.numeric(outdt3m[,i])
outdt3m$gender <- factor(outdt3m$gender, levels=unique(outdt3m$gender))
summary(outdt3m)

#'
#' ## Mediator Models
#' 
#' ## Knowledge
#'

s3mm01_10 <- lm(update(knowledge ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm01_1A <- lm(update(knowledge ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm01_1B <- lm(update(knowledge ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm01_1C <- lm(update(knowledge ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s3mm01_10,s3mm01_1A,s3mm01_1B,s3mm01_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s3mm01_10,vcov.=vcovHC(s3mm01_10))[,2],
                             coeftest(s3mm01_1A,vcov.=vcovHC(s3mm01_1A))[,2],
                             coeftest(s3mm01_1B,vcov.=vcovHC(s3mm01_1B))[,2],
                             coeftest(s3mm01_1C,vcov.=vcovHC(s3mm01_1C))[,2]),
          override.pvalues = list(coeftest(s3mm01_10,vcov.=vcovHC(s3mm01_10))[,4],
                                  coeftest(s3mm01_1A,vcov.=vcovHC(s3mm01_1A))[,4],
                                  coeftest(s3mm01_1B,vcov.=vcovHC(s3mm01_1B))[,4],
                                  coeftest(s3mm01_1C,vcov.=vcovHC(s3mm01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s3mm01_10,s3mm01_1A,s3mm01_1B,s3mm01_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s3mm01_10,vcov.=vcovHC(s3mm01_10))[,2],
                          coeftest(s3mm01_1A,vcov.=vcovHC(s3mm01_1A))[,2],
                          coeftest(s3mm01_1B,vcov.=vcovHC(s3mm01_1B))[,2],
                          coeftest(s3mm01_1C,vcov.=vcovHC(s3mm01_1C))[,2]),
       override.pvalues = list(coeftest(s3mm01_10,vcov.=vcovHC(s3mm01_10))[,4],
                               coeftest(s3mm01_1A,vcov.=vcovHC(s3mm01_1A))[,4],
                               coeftest(s3mm01_1B,vcov.=vcovHC(s3mm01_1B))[,4],
                               coeftest(s3mm01_1C,vcov.=vcovHC(s3mm01_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s3mm01_1_tabular.tex"))



#'
#' ## Ideology
#'

s3mm02_10 <- lm(update(ideology ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm02_1A <- lm(update(ideology ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm02_1B <- lm(update(ideology ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm02_1C <- lm(update(ideology ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s3mm02_10,s3mm02_1A,s3mm02_1B,s3mm02_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s3mm02_10,vcov.=vcovHC(s3mm02_10))[,2],
                             coeftest(s3mm02_1A,vcov.=vcovHC(s3mm02_1A))[,2],
                             coeftest(s3mm02_1B,vcov.=vcovHC(s3mm02_1B))[,2],
                             coeftest(s3mm02_1C,vcov.=vcovHC(s3mm02_1C))[,2]),
          override.pvalues = list(coeftest(s3mm02_10,vcov.=vcovHC(s3mm02_10))[,4],
                                  coeftest(s3mm02_1A,vcov.=vcovHC(s3mm02_1A))[,4],
                                  coeftest(s3mm02_1B,vcov.=vcovHC(s3mm02_1B))[,4],
                                  coeftest(s3mm02_1C,vcov.=vcovHC(s3mm02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s3mm02_10,s3mm02_1A,s3mm02_1B,s3mm02_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s3mm02_10,vcov.=vcovHC(s3mm02_10))[,2],
                          coeftest(s3mm02_1A,vcov.=vcovHC(s3mm02_1A))[,2],
                          coeftest(s3mm02_1B,vcov.=vcovHC(s3mm02_1B))[,2],
                          coeftest(s3mm02_1C,vcov.=vcovHC(s3mm02_1C))[,2]),
       override.pvalues = list(coeftest(s3mm02_10,vcov.=vcovHC(s3mm02_10))[,4],
                               coeftest(s3mm02_1A,vcov.=vcovHC(s3mm02_1A))[,4],
                               coeftest(s3mm02_1B,vcov.=vcovHC(s3mm02_1B))[,4],
                               coeftest(s3mm02_1C,vcov.=vcovHC(s3mm02_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s3mm02_1_tabular.tex"))

#'
#' ## LDP - DPJ FT
#'

s3mm03_10 <- lm(update(ldpdpjft ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm03_1A <- lm(update(ldpdpjft ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm03_1B <- lm(update(ldpdpjft ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm03_1C <- lm(update(ldpdpjft ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s3mm03_10,s3mm03_1A,s3mm03_1B,s3mm03_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s3mm03_10,vcov.=vcovHC(s3mm03_10))[,2],
                             coeftest(s3mm03_1A,vcov.=vcovHC(s3mm03_1A))[,2],
                             coeftest(s3mm03_1B,vcov.=vcovHC(s3mm03_1B))[,2],
                             coeftest(s3mm03_1C,vcov.=vcovHC(s3mm03_1C))[,2]),
          override.pvalues = list(coeftest(s3mm03_10,vcov.=vcovHC(s3mm03_10))[,4],
                                  coeftest(s3mm03_1A,vcov.=vcovHC(s3mm03_1A))[,4],
                                  coeftest(s3mm03_1B,vcov.=vcovHC(s3mm03_1B))[,4],
                                  coeftest(s3mm03_1C,vcov.=vcovHC(s3mm03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s3mm03_10,s3mm03_1A,s3mm03_1B,s3mm03_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s3mm03_10,vcov.=vcovHC(s3mm03_10))[,2],
                          coeftest(s3mm03_1A,vcov.=vcovHC(s3mm03_1A))[,2],
                          coeftest(s3mm03_1B,vcov.=vcovHC(s3mm03_1B))[,2],
                          coeftest(s3mm03_1C,vcov.=vcovHC(s3mm03_1C))[,2]),
       override.pvalues = list(coeftest(s3mm03_10,vcov.=vcovHC(s3mm03_10))[,4],
                               coeftest(s3mm03_1A,vcov.=vcovHC(s3mm03_1A))[,4],
                               coeftest(s3mm03_1B,vcov.=vcovHC(s3mm03_1B))[,4],
                               coeftest(s3mm03_1C,vcov.=vcovHC(s3mm03_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s3mm03_1_tabular.tex"))


#'
#' ## Favorability of South Korea
#'

s3mm04_10 <- lm(update(familiarityFT_KOR ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm04_1A <- lm(update(familiarityFT_KOR ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm04_1B <- lm(update(familiarityFT_KOR ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm04_1C <- lm(update(familiarityFT_KOR ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s3mm04_10,s3mm04_1A,s3mm04_1B,s3mm04_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s3mm04_10,vcov.=vcovHC(s3mm04_10))[,2],
                             coeftest(s3mm04_1A,vcov.=vcovHC(s3mm04_1A))[,2],
                             coeftest(s3mm04_1B,vcov.=vcovHC(s3mm04_1B))[,2],
                             coeftest(s3mm04_1C,vcov.=vcovHC(s3mm04_1C))[,2]),
          override.pvalues = list(coeftest(s3mm04_10,vcov.=vcovHC(s3mm04_10))[,4],
                                  coeftest(s3mm04_1A,vcov.=vcovHC(s3mm04_1A))[,4],
                                  coeftest(s3mm04_1B,vcov.=vcovHC(s3mm04_1B))[,4],
                                  coeftest(s3mm04_1C,vcov.=vcovHC(s3mm04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s3mm04_10,s3mm04_1A,s3mm04_1B,s3mm04_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s3mm04_10,vcov.=vcovHC(s3mm04_10))[,2],
                          coeftest(s3mm04_1A,vcov.=vcovHC(s3mm04_1A))[,2],
                          coeftest(s3mm04_1B,vcov.=vcovHC(s3mm04_1B))[,2],
                          coeftest(s3mm04_1C,vcov.=vcovHC(s3mm04_1C))[,2]),
       override.pvalues = list(coeftest(s3mm04_10,vcov.=vcovHC(s3mm04_10))[,4],
                               coeftest(s3mm04_1A,vcov.=vcovHC(s3mm04_1A))[,4],
                               coeftest(s3mm04_1B,vcov.=vcovHC(s3mm04_1B))[,4],
                               coeftest(s3mm04_1C,vcov.=vcovHC(s3mm04_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s3mm04_1_tabular.tex"))

#'
#' ## Favorability of China
#'

s3mm05_10 <- lm(update(familiarityFT_CHN ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm05_1A <- lm(update(familiarityFT_CHN ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm05_1B <- lm(update(familiarityFT_CHN ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm05_1C <- lm(update(familiarityFT_CHN ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s3mm05_10,s3mm05_1A,s3mm05_1B,s3mm05_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s3mm05_10,vcov.=vcovHC(s3mm05_10))[,2],
                             coeftest(s3mm05_1A,vcov.=vcovHC(s3mm05_1A))[,2],
                             coeftest(s3mm05_1B,vcov.=vcovHC(s3mm05_1B))[,2],
                             coeftest(s3mm05_1C,vcov.=vcovHC(s3mm05_1C))[,2]),
          override.pvalues = list(coeftest(s3mm05_10,vcov.=vcovHC(s3mm05_10))[,4],
                                  coeftest(s3mm05_1A,vcov.=vcovHC(s3mm05_1A))[,4],
                                  coeftest(s3mm05_1B,vcov.=vcovHC(s3mm05_1B))[,4],
                                  coeftest(s3mm05_1C,vcov.=vcovHC(s3mm05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s3mm05_10,s3mm05_1A,s3mm05_1B,s3mm05_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s3mm05_10,vcov.=vcovHC(s3mm05_10))[,2],
                          coeftest(s3mm05_1A,vcov.=vcovHC(s3mm05_1A))[,2],
                          coeftest(s3mm05_1B,vcov.=vcovHC(s3mm05_1B))[,2],
                          coeftest(s3mm05_1C,vcov.=vcovHC(s3mm05_1C))[,2]),
       override.pvalues = list(coeftest(s3mm05_10,vcov.=vcovHC(s3mm05_10))[,4],
                               coeftest(s3mm05_1A,vcov.=vcovHC(s3mm05_1A))[,4],
                               coeftest(s3mm05_1B,vcov.=vcovHC(s3mm05_1B))[,4],
                               coeftest(s3mm05_1C,vcov.=vcovHC(s3mm05_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s3mm05_1_tabular.tex"))

#'
#' ## Favorability of USA
#'

s3mm06_10 <- lm(update(familiarityFT_USA ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm06_1A <- lm(update(familiarityFT_USA ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm06_1B <- lm(update(familiarityFT_USA ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm06_1C <- lm(update(familiarityFT_USA ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s3mm06_10,s3mm06_1A,s3mm06_1B,s3mm06_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s3mm06_10,vcov.=vcovHC(s3mm06_10))[,2],
                             coeftest(s3mm06_1A,vcov.=vcovHC(s3mm06_1A))[,2],
                             coeftest(s3mm06_1B,vcov.=vcovHC(s3mm06_1B))[,2],
                             coeftest(s3mm06_1C,vcov.=vcovHC(s3mm06_1C))[,2]),
          override.pvalues = list(coeftest(s3mm06_10,vcov.=vcovHC(s3mm06_10))[,4],
                                  coeftest(s3mm06_1A,vcov.=vcovHC(s3mm06_1A))[,4],
                                  coeftest(s3mm06_1B,vcov.=vcovHC(s3mm06_1B))[,4],
                                  coeftest(s3mm06_1C,vcov.=vcovHC(s3mm06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s3mm06_10,s3mm06_1A,s3mm06_1B,s3mm06_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s3mm06_10,vcov.=vcovHC(s3mm06_10))[,2],
                          coeftest(s3mm06_1A,vcov.=vcovHC(s3mm06_1A))[,2],
                          coeftest(s3mm06_1B,vcov.=vcovHC(s3mm06_1B))[,2],
                          coeftest(s3mm06_1C,vcov.=vcovHC(s3mm06_1C))[,2]),
       override.pvalues = list(coeftest(s3mm06_10,vcov.=vcovHC(s3mm06_10))[,4],
                               coeftest(s3mm06_1A,vcov.=vcovHC(s3mm06_1A))[,4],
                               coeftest(s3mm06_1B,vcov.=vcovHC(s3mm06_1B))[,4],
                               coeftest(s3mm06_1C,vcov.=vcovHC(s3mm06_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s3mm06_1_tabular.tex"))


#' 
#' ## Income
#'

s3mm07_10 <- lm(update(income ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm07_1A <- lm(update(income ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm07_1B <- lm(update(income ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s3mm07_1C <- lm(update(income ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s3mm07_10,s3mm07_1A,s3mm07_1B,s3mm07_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s3mm07_10,vcov.=vcovHC(s3mm07_10))[,2],
                             coeftest(s3mm07_1A,vcov.=vcovHC(s3mm07_1A))[,2],
                             coeftest(s3mm07_1B,vcov.=vcovHC(s3mm07_1B))[,2],
                             coeftest(s3mm07_1C,vcov.=vcovHC(s3mm07_1C))[,2]),
          override.pvalues = list(coeftest(s3mm07_10,vcov.=vcovHC(s3mm07_10))[,4],
                                  coeftest(s3mm07_1A,vcov.=vcovHC(s3mm07_1A))[,4],
                                  coeftest(s3mm07_1B,vcov.=vcovHC(s3mm07_1B))[,4],
                                  coeftest(s3mm07_1C,vcov.=vcovHC(s3mm07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s3mm07_10,s3mm07_1A,s3mm07_1B,s3mm07_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s3mm07_10,vcov.=vcovHC(s3mm07_10))[,2],
                          coeftest(s3mm07_1A,vcov.=vcovHC(s3mm07_1A))[,2],
                          coeftest(s3mm07_1B,vcov.=vcovHC(s3mm07_1B))[,2],
                          coeftest(s3mm07_1C,vcov.=vcovHC(s3mm07_1C))[,2]),
       override.pvalues = list(coeftest(s3mm07_10,vcov.=vcovHC(s3mm07_10))[,4],
                               coeftest(s3mm07_1A,vcov.=vcovHC(s3mm07_1A))[,4],
                               coeftest(s3mm07_1B,vcov.=vcovHC(s3mm07_1B))[,4],
                               coeftest(s3mm07_1C,vcov.=vcovHC(s3mm07_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s3mm07_1_tabular.tex"))

#'
#' # With Matched Data (Lambda = 200km)
#'

sifcct <- readRDS(datadir4)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)

#'
#' ## Outcome Model 
#'

## Living in Local ZIP since at least age 15 ##

s4mo_10 <- lm(update(foreignsuff ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mo_1A <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mo_1B <- lm(update(foreignsuff ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mo_1C <- lm(update(foreignsuff ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s4mo_10,s4mo_1A,s4mo_1B,s4mo_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s4mo_10,vcov.=vcovHC(s4mo_10))[,2],
                             coeftest(s4mo_1A,vcov.=vcovHC(s4mo_1A))[,2],
                             coeftest(s4mo_1B,vcov.=vcovHC(s4mo_1B))[,2],
                             coeftest(s4mo_1C,vcov.=vcovHC(s4mo_1C))[,2]),
          override.pvalues = list(coeftest(s4mo_10,vcov.=vcovHC(s4mo_10))[,4],
                                  coeftest(s4mo_1A,vcov.=vcovHC(s4mo_1A))[,4],
                                  coeftest(s4mo_1B,vcov.=vcovHC(s4mo_1B))[,4],
                                  coeftest(s4mo_1C,vcov.=vcovHC(s4mo_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s4mo_10,s4mo_1A,s4mo_1B,s4mo_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s4mo_10,vcov.=vcovHC(s4mo_10))[,2],
                          coeftest(s4mo_1A,vcov.=vcovHC(s4mo_1A))[,2],
                          coeftest(s4mo_1B,vcov.=vcovHC(s4mo_1B))[,2],
                          coeftest(s4mo_1C,vcov.=vcovHC(s4mo_1C))[,2]),
       override.pvalues = list(coeftest(s4mo_10,vcov.=vcovHC(s4mo_10))[,4],
                               coeftest(s4mo_1A,vcov.=vcovHC(s4mo_1A))[,4],
                               coeftest(s4mo_1B,vcov.=vcovHC(s4mo_1B))[,4],
                               coeftest(s4mo_1C,vcov.=vcovHC(s4mo_1C))[,4]),
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s4mo_1_tabular.tex"))

#+
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

outdt4 <- rbind(extout("Female",25),
                extout("Female",35),
                extout("Female",45),
                extout("Female",55),
                extout("Female",65),
                extout("Male",25),
                extout("Male",35),
                extout("Male",45),
                extout("Male",55),
                extout("Male",65))
outdt4 <- as.data.frame(outdt4)
for(i in 2:9) outdt4[,i] <- as.numeric(outdt4[,i])
outdt4$gender <- factor(outdt4$gender, levels=unique(outdt4$gender))
summary(outdt4)

#'
#' ## Outcome Model 2
#'

## Living in Local ZIP since at least age 15 ##

# require(nnet)
# s4mo2_10 <- multinom(update(foreignsuff3x ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s4mo2_1A <- multinom(update(foreignsuff3x ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s4mo2_1B <- multinom(update(foreignsuff3x ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s4mo2_1C <- multinom(update(foreignsuff3x ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])

sifcct.mlogit <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen<=15),],
                       shape = "wide", choice = "foreignsuff3x")
# # levels(sifcct.mlogit$idx$id2) <- c("Disagree","Neither","Agree")
s4mo2_10 <- mlogit(outmod0.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s4mo2_1A <- mlogit(outmodA.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s4mo2_1B <- mlogit(outmodB.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s4mo2_1C <- mlogit(outmodC.mlogit, data=sifcct.mlogit, reflevel="Disagree")

screenreg(list(s4mo2_10,s4mo2_1A), digits = 4, #single.row = T,
          override.se = list(coeftest(s4mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_10))),2],
                             coeftest(s4mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_10))),2],
                             coeftest(s4mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1A))),2],
                             coeftest(s4mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1A))),2]),
          override.pvalues = list(coeftest(s4mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_10))),4],
                                  coeftest(s4mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_10))),4],
                                  coeftest(s4mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1A))),4],
                                  coeftest(s4mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+", 
          custom.model.names = c("Base: Agree","Base: Neither",
                                 "ZIP: Agree","ZIP: Neither"),
          custom.coef.map = vnmap)
screenreg(list(s4mo2_1B,s4mo2_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s4mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1B))),2],
                             coeftest(s4mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1B))),2],
                             coeftest(s4mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1C))),2],
                             coeftest(s4mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1C))),2]),
          override.pvalues = list(coeftest(s4mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1B))),4],
                                  coeftest(s4mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1B))),4],
                                  coeftest(s4mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1C))),4],
                                  coeftest(s4mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Agree","Mun.: Neither",
                                 "Full: Agree","Full: Neither"))

#+ include = FALSE
texreg(list(s4mo2_10,s4mo2_1A), digits = 4, #single.row = T,
       override.se = list(coeftest(s4mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_10))),2],
                          coeftest(s4mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_10))),2],
                          coeftest(s4mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1A))),2],
                          coeftest(s4mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1A))),2]),
       override.pvalues = list(coeftest(s4mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_10))),4],
                               coeftest(s4mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_10))),4],
                               coeftest(s4mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1A))),4],
                               coeftest(s4mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1A))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger", 
       custom.model.names = c("Base: Agree","Base: Neither",
                              "ZIP: Agree","ZIP: Neither"),
       custom.coef.map = vnmap,
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s4mo2_1_1_tabular.tex"))
texreg(list(s4mo2_1B,s4mo2_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s4mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1B))),2],
                          coeftest(s4mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1B))),2],
                          coeftest(s4mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1C))),2],
                          coeftest(s4mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1C))),2]),
       override.pvalues = list(coeftest(s4mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1B))),4],
                               coeftest(s4mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1B))),4],
                               coeftest(s4mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s4mo2_1C))),4],
                               coeftest(s4mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s4mo2_1C))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap,
       custom.model.names = c("Mun.: Agree","Mun.: Neither",
                              "Full: Agree","Full: Neither"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s4mo2_1_2_tabular.tex"))

#+
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
                       as.factor(wave), data=sifcct.mlogit.tmp, reflevel = "Disagree")
    subname = "Stayed"
  } else {
    # modset <- multinom(foreignsuff3x ~ edu2 * gender * ageset + lvpr + as.factor(wave), 
    #                    data=sifcct[which(sifcct$age - sifcct$lvlen>=23),],
    #                    Hess = TRUE)
    sifcct.mlogit.tmp <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen>=23),],
                               shape = "wide", choice = "foreignsuff3x")
    # levels(sifcct.mlogit.tmp$idx$id2) <- c("Disagree","Neither","Agree")
    modset <- mlogit(foreignsuff3x ~ 0 | edu2 * gender * ageset + lvpr + as.factor(wave), 
                     data=sifcct.mlogit.tmp, reflevel = "Disagree")
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

outdt4m <- rbind(extout("Female",25,1),
                 extout("Female",35,1),
                 extout("Female",45,1),
                 extout("Female",55,1),
                 extout("Female",65,1),
                 extout("Male",25,1),
                 extout("Male",35,1),
                 extout("Male",45,1),
                 extout("Male",55,1),
                 extout("Male",65,1))
outdt4m <- as.data.frame(outdt4m)
for(i in 2:9) outdt4m[,i] <- as.numeric(outdt4m[,i])
outdt4m$gender <- factor(outdt4m$gender, levels=unique(outdt4m$gender))
summary(outdt4m)

#'
#' ## Mediator Models
#' 
#' ## Knowledge
#'

s4mm01_10 <- lm(update(knowledge ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm01_1A <- lm(update(knowledge ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm01_1B <- lm(update(knowledge ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm01_1C <- lm(update(knowledge ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s4mm01_10,s4mm01_1A,s4mm01_1B,s4mm01_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s4mm01_10,vcov.=vcovHC(s4mm01_10))[,2],
                             coeftest(s4mm01_1A,vcov.=vcovHC(s4mm01_1A))[,2],
                             coeftest(s4mm01_1B,vcov.=vcovHC(s4mm01_1B))[,2],
                             coeftest(s4mm01_1C,vcov.=vcovHC(s4mm01_1C))[,2]),
          override.pvalues = list(coeftest(s4mm01_10,vcov.=vcovHC(s4mm01_10))[,4],
                                  coeftest(s4mm01_1A,vcov.=vcovHC(s4mm01_1A))[,4],
                                  coeftest(s4mm01_1B,vcov.=vcovHC(s4mm01_1B))[,4],
                                  coeftest(s4mm01_1C,vcov.=vcovHC(s4mm01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s4mm01_10,s4mm01_1A,s4mm01_1B,s4mm01_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s4mm01_10,vcov.=vcovHC(s4mm01_10))[,2],
                          coeftest(s4mm01_1A,vcov.=vcovHC(s4mm01_1A))[,2],
                          coeftest(s4mm01_1B,vcov.=vcovHC(s4mm01_1B))[,2],
                          coeftest(s4mm01_1C,vcov.=vcovHC(s4mm01_1C))[,2]),
       override.pvalues = list(coeftest(s4mm01_10,vcov.=vcovHC(s4mm01_10))[,4],
                               coeftest(s4mm01_1A,vcov.=vcovHC(s4mm01_1A))[,4],
                               coeftest(s4mm01_1B,vcov.=vcovHC(s4mm01_1B))[,4],
                               coeftest(s4mm01_1C,vcov.=vcovHC(s4mm01_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s4mm01_1_tabular.tex"))



#'
#' ## Ideology
#'

s4mm02_10 <- lm(update(ideology ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm02_1A <- lm(update(ideology ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm02_1B <- lm(update(ideology ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm02_1C <- lm(update(ideology ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s4mm02_10,s4mm02_1A,s4mm02_1B,s4mm02_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s4mm02_10,vcov.=vcovHC(s4mm02_10))[,2],
                             coeftest(s4mm02_1A,vcov.=vcovHC(s4mm02_1A))[,2],
                             coeftest(s4mm02_1B,vcov.=vcovHC(s4mm02_1B))[,2],
                             coeftest(s4mm02_1C,vcov.=vcovHC(s4mm02_1C))[,2]),
          override.pvalues = list(coeftest(s4mm02_10,vcov.=vcovHC(s4mm02_10))[,4],
                                  coeftest(s4mm02_1A,vcov.=vcovHC(s4mm02_1A))[,4],
                                  coeftest(s4mm02_1B,vcov.=vcovHC(s4mm02_1B))[,4],
                                  coeftest(s4mm02_1C,vcov.=vcovHC(s4mm02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s4mm02_10,s4mm02_1A,s4mm02_1B,s4mm02_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s4mm02_10,vcov.=vcovHC(s4mm02_10))[,2],
                          coeftest(s4mm02_1A,vcov.=vcovHC(s4mm02_1A))[,2],
                          coeftest(s4mm02_1B,vcov.=vcovHC(s4mm02_1B))[,2],
                          coeftest(s4mm02_1C,vcov.=vcovHC(s4mm02_1C))[,2]),
       override.pvalues = list(coeftest(s4mm02_10,vcov.=vcovHC(s4mm02_10))[,4],
                               coeftest(s4mm02_1A,vcov.=vcovHC(s4mm02_1A))[,4],
                               coeftest(s4mm02_1B,vcov.=vcovHC(s4mm02_1B))[,4],
                               coeftest(s4mm02_1C,vcov.=vcovHC(s4mm02_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s4mm02_1_tabular.tex"))

#'
#' ## LDP - DPJ FT
#'

s4mm03_10 <- lm(update(ldpdpjft ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm03_1A <- lm(update(ldpdpjft ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm03_1B <- lm(update(ldpdpjft ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm03_1C <- lm(update(ldpdpjft ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s4mm03_10,s4mm03_1A,s4mm03_1B,s4mm03_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s4mm03_10,vcov.=vcovHC(s4mm03_10))[,2],
                             coeftest(s4mm03_1A,vcov.=vcovHC(s4mm03_1A))[,2],
                             coeftest(s4mm03_1B,vcov.=vcovHC(s4mm03_1B))[,2],
                             coeftest(s4mm03_1C,vcov.=vcovHC(s4mm03_1C))[,2]),
          override.pvalues = list(coeftest(s4mm03_10,vcov.=vcovHC(s4mm03_10))[,4],
                                  coeftest(s4mm03_1A,vcov.=vcovHC(s4mm03_1A))[,4],
                                  coeftest(s4mm03_1B,vcov.=vcovHC(s4mm03_1B))[,4],
                                  coeftest(s4mm03_1C,vcov.=vcovHC(s4mm03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s4mm03_10,s4mm03_1A,s4mm03_1B,s4mm03_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s4mm03_10,vcov.=vcovHC(s4mm03_10))[,2],
                          coeftest(s4mm03_1A,vcov.=vcovHC(s4mm03_1A))[,2],
                          coeftest(s4mm03_1B,vcov.=vcovHC(s4mm03_1B))[,2],
                          coeftest(s4mm03_1C,vcov.=vcovHC(s4mm03_1C))[,2]),
       override.pvalues = list(coeftest(s4mm03_10,vcov.=vcovHC(s4mm03_10))[,4],
                               coeftest(s4mm03_1A,vcov.=vcovHC(s4mm03_1A))[,4],
                               coeftest(s4mm03_1B,vcov.=vcovHC(s4mm03_1B))[,4],
                               coeftest(s4mm03_1C,vcov.=vcovHC(s4mm03_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s4mm03_1_tabular.tex"))


#'
#' ## Favorability of South Korea
#'

s4mm04_10 <- lm(update(familiarityFT_KOR ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm04_1A <- lm(update(familiarityFT_KOR ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm04_1B <- lm(update(familiarityFT_KOR ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm04_1C <- lm(update(familiarityFT_KOR ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s4mm04_10,s4mm04_1A,s4mm04_1B,s4mm04_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s4mm04_10,vcov.=vcovHC(s4mm04_10))[,2],
                             coeftest(s4mm04_1A,vcov.=vcovHC(s4mm04_1A))[,2],
                             coeftest(s4mm04_1B,vcov.=vcovHC(s4mm04_1B))[,2],
                             coeftest(s4mm04_1C,vcov.=vcovHC(s4mm04_1C))[,2]),
          override.pvalues = list(coeftest(s4mm04_10,vcov.=vcovHC(s4mm04_10))[,4],
                                  coeftest(s4mm04_1A,vcov.=vcovHC(s4mm04_1A))[,4],
                                  coeftest(s4mm04_1B,vcov.=vcovHC(s4mm04_1B))[,4],
                                  coeftest(s4mm04_1C,vcov.=vcovHC(s4mm04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s4mm04_10,s4mm04_1A,s4mm04_1B,s4mm04_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s4mm04_10,vcov.=vcovHC(s4mm04_10))[,2],
                          coeftest(s4mm04_1A,vcov.=vcovHC(s4mm04_1A))[,2],
                          coeftest(s4mm04_1B,vcov.=vcovHC(s4mm04_1B))[,2],
                          coeftest(s4mm04_1C,vcov.=vcovHC(s4mm04_1C))[,2]),
       override.pvalues = list(coeftest(s4mm04_10,vcov.=vcovHC(s4mm04_10))[,4],
                               coeftest(s4mm04_1A,vcov.=vcovHC(s4mm04_1A))[,4],
                               coeftest(s4mm04_1B,vcov.=vcovHC(s4mm04_1B))[,4],
                               coeftest(s4mm04_1C,vcov.=vcovHC(s4mm04_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s4mm04_1_tabular.tex"))

#'
#' ## Favorability of China
#'

s4mm05_10 <- lm(update(familiarityFT_CHN ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm05_1A <- lm(update(familiarityFT_CHN ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm05_1B <- lm(update(familiarityFT_CHN ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm05_1C <- lm(update(familiarityFT_CHN ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s4mm05_10,s4mm05_1A,s4mm05_1B,s4mm05_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s4mm05_10,vcov.=vcovHC(s4mm05_10))[,2],
                             coeftest(s4mm05_1A,vcov.=vcovHC(s4mm05_1A))[,2],
                             coeftest(s4mm05_1B,vcov.=vcovHC(s4mm05_1B))[,2],
                             coeftest(s4mm05_1C,vcov.=vcovHC(s4mm05_1C))[,2]),
          override.pvalues = list(coeftest(s4mm05_10,vcov.=vcovHC(s4mm05_10))[,4],
                                  coeftest(s4mm05_1A,vcov.=vcovHC(s4mm05_1A))[,4],
                                  coeftest(s4mm05_1B,vcov.=vcovHC(s4mm05_1B))[,4],
                                  coeftest(s4mm05_1C,vcov.=vcovHC(s4mm05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s4mm05_10,s4mm05_1A,s4mm05_1B,s4mm05_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s4mm05_10,vcov.=vcovHC(s4mm05_10))[,2],
                          coeftest(s4mm05_1A,vcov.=vcovHC(s4mm05_1A))[,2],
                          coeftest(s4mm05_1B,vcov.=vcovHC(s4mm05_1B))[,2],
                          coeftest(s4mm05_1C,vcov.=vcovHC(s4mm05_1C))[,2]),
       override.pvalues = list(coeftest(s4mm05_10,vcov.=vcovHC(s4mm05_10))[,4],
                               coeftest(s4mm05_1A,vcov.=vcovHC(s4mm05_1A))[,4],
                               coeftest(s4mm05_1B,vcov.=vcovHC(s4mm05_1B))[,4],
                               coeftest(s4mm05_1C,vcov.=vcovHC(s4mm05_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s4mm05_1_tabular.tex"))

#'
#' ## Favorability of USA
#'

s4mm06_10 <- lm(update(familiarityFT_USA ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm06_1A <- lm(update(familiarityFT_USA ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm06_1B <- lm(update(familiarityFT_USA ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm06_1C <- lm(update(familiarityFT_USA ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s4mm06_10,s4mm06_1A,s4mm06_1B,s4mm06_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s4mm06_10,vcov.=vcovHC(s4mm06_10))[,2],
                             coeftest(s4mm06_1A,vcov.=vcovHC(s4mm06_1A))[,2],
                             coeftest(s4mm06_1B,vcov.=vcovHC(s4mm06_1B))[,2],
                             coeftest(s4mm06_1C,vcov.=vcovHC(s4mm06_1C))[,2]),
          override.pvalues = list(coeftest(s4mm06_10,vcov.=vcovHC(s4mm06_10))[,4],
                                  coeftest(s4mm06_1A,vcov.=vcovHC(s4mm06_1A))[,4],
                                  coeftest(s4mm06_1B,vcov.=vcovHC(s4mm06_1B))[,4],
                                  coeftest(s4mm06_1C,vcov.=vcovHC(s4mm06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s4mm06_10,s4mm06_1A,s4mm06_1B,s4mm06_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s4mm06_10,vcov.=vcovHC(s4mm06_10))[,2],
                          coeftest(s4mm06_1A,vcov.=vcovHC(s4mm06_1A))[,2],
                          coeftest(s4mm06_1B,vcov.=vcovHC(s4mm06_1B))[,2],
                          coeftest(s4mm06_1C,vcov.=vcovHC(s4mm06_1C))[,2]),
       override.pvalues = list(coeftest(s4mm06_10,vcov.=vcovHC(s4mm06_10))[,4],
                               coeftest(s4mm06_1A,vcov.=vcovHC(s4mm06_1A))[,4],
                               coeftest(s4mm06_1B,vcov.=vcovHC(s4mm06_1B))[,4],
                               coeftest(s4mm06_1C,vcov.=vcovHC(s4mm06_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s4mm06_1_tabular.tex"))


#' 
#' ## Income
#'

s4mm07_10 <- lm(update(income ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm07_1A <- lm(update(income ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm07_1B <- lm(update(income ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s4mm07_1C <- lm(update(income ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s4mm07_10,s4mm07_1A,s4mm07_1B,s4mm07_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s4mm07_10,vcov.=vcovHC(s4mm07_10))[,2],
                             coeftest(s4mm07_1A,vcov.=vcovHC(s4mm07_1A))[,2],
                             coeftest(s4mm07_1B,vcov.=vcovHC(s4mm07_1B))[,2],
                             coeftest(s4mm07_1C,vcov.=vcovHC(s4mm07_1C))[,2]),
          override.pvalues = list(coeftest(s4mm07_10,vcov.=vcovHC(s4mm07_10))[,4],
                                  coeftest(s4mm07_1A,vcov.=vcovHC(s4mm07_1A))[,4],
                                  coeftest(s4mm07_1B,vcov.=vcovHC(s4mm07_1B))[,4],
                                  coeftest(s4mm07_1C,vcov.=vcovHC(s4mm07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s4mm07_10,s4mm07_1A,s4mm07_1B,s4mm07_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s4mm07_10,vcov.=vcovHC(s4mm07_10))[,2],
                          coeftest(s4mm07_1A,vcov.=vcovHC(s4mm07_1A))[,2],
                          coeftest(s4mm07_1B,vcov.=vcovHC(s4mm07_1B))[,2],
                          coeftest(s4mm07_1C,vcov.=vcovHC(s4mm07_1C))[,2]),
       override.pvalues = list(coeftest(s4mm07_10,vcov.=vcovHC(s4mm07_10))[,4],
                               coeftest(s4mm07_1A,vcov.=vcovHC(s4mm07_1A))[,4],
                               coeftest(s4mm07_1B,vcov.=vcovHC(s4mm07_1B))[,4],
                               coeftest(s4mm07_1C,vcov.=vcovHC(s4mm07_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s4mm07_1_tabular.tex"))

#'
#' # With Matched Data (Lambda = 350km)
#'

sifcct <- readRDS(datadir5)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)

#'
#' ## Outcome Model 
#'

## Living in Local ZIP since at least age 15 ##

s5mo_10 <- lm(update(foreignsuff ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mo_1A <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mo_1B <- lm(update(foreignsuff ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mo_1C <- lm(update(foreignsuff ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s5mo_10,s5mo_1A,s5mo_1B,s5mo_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s5mo_10,vcov.=vcovHC(s5mo_10))[,2],
                             coeftest(s5mo_1A,vcov.=vcovHC(s5mo_1A))[,2],
                             coeftest(s5mo_1B,vcov.=vcovHC(s5mo_1B))[,2],
                             coeftest(s5mo_1C,vcov.=vcovHC(s5mo_1C))[,2]),
          override.pvalues = list(coeftest(s5mo_10,vcov.=vcovHC(s5mo_10))[,4],
                                  coeftest(s5mo_1A,vcov.=vcovHC(s5mo_1A))[,4],
                                  coeftest(s5mo_1B,vcov.=vcovHC(s5mo_1B))[,4],
                                  coeftest(s5mo_1C,vcov.=vcovHC(s5mo_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s5mo_10,s5mo_1A,s5mo_1B,s5mo_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s5mo_10,vcov.=vcovHC(s5mo_10))[,2],
                          coeftest(s5mo_1A,vcov.=vcovHC(s5mo_1A))[,2],
                          coeftest(s5mo_1B,vcov.=vcovHC(s5mo_1B))[,2],
                          coeftest(s5mo_1C,vcov.=vcovHC(s5mo_1C))[,2]),
       override.pvalues = list(coeftest(s5mo_10,vcov.=vcovHC(s5mo_10))[,4],
                               coeftest(s5mo_1A,vcov.=vcovHC(s5mo_1A))[,4],
                               coeftest(s5mo_1B,vcov.=vcovHC(s5mo_1B))[,4],
                               coeftest(s5mo_1C,vcov.=vcovHC(s5mo_1C))[,4]),
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s5mo_1_tabular.tex"))

#+
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

outdt5 <- rbind(extout("Female",25),
                extout("Female",35),
                extout("Female",45),
                extout("Female",55),
                extout("Female",65),
                extout("Male",25),
                extout("Male",35),
                extout("Male",45),
                extout("Male",55),
                extout("Male",65))
outdt5 <- as.data.frame(outdt5)
for(i in 2:9) outdt5[,i] <- as.numeric(outdt5[,i])
outdt5$gender <- factor(outdt5$gender, levels=unique(outdt5$gender))
summary(outdt5)

#'
#' ## Outcome Model 2
#'

## Living in Local ZIP since at least age 15 ##

# require(nnet)
# s5mo2_10 <- multinom(update(foreignsuff3x ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s5mo2_1A <- multinom(update(foreignsuff3x ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s5mo2_1B <- multinom(update(foreignsuff3x ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
# s5mo2_1C <- multinom(update(foreignsuff3x ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])

sifcct.mlogit <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen<=15),],
                       shape = "wide", choice = "foreignsuff3x")
# # levels(sifcct.mlogit$idx$id2) <- c("Disagree","Neither","Agree")
s5mo2_10 <- mlogit(outmod0.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s5mo2_1A <- mlogit(outmodA.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s5mo2_1B <- mlogit(outmodB.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s5mo2_1C <- mlogit(outmodC.mlogit, data=sifcct.mlogit, reflevel="Disagree")

screenreg(list(s5mo2_10,s5mo2_1A), digits = 4, #single.row = T,
          override.se = list(coeftest(s5mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_10))),2],
                             coeftest(s5mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_10))),2],
                             coeftest(s5mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1A))),2],
                             coeftest(s5mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1A))),2]),
          override.pvalues = list(coeftest(s5mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_10))),4],
                                  coeftest(s5mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_10))),4],
                                  coeftest(s5mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1A))),4],
                                  coeftest(s5mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+", 
          custom.model.names = c("Base: Agree","Base: Neither",
                                 "ZIP: Agree","ZIP: Neither"),
          custom.coef.map = vnmap)
screenreg(list(s5mo2_1B,s5mo2_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s5mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1B))),2],
                             coeftest(s5mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1B))),2],
                             coeftest(s5mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1C))),2],
                             coeftest(s5mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1C))),2]),
          override.pvalues = list(coeftest(s5mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1B))),4],
                                  coeftest(s5mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1B))),4],
                                  coeftest(s5mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1C))),4],
                                  coeftest(s5mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Agree","Mun.: Neither",
                                 "Full: Agree","Full: Neither"))

#+ include = FALSE
texreg(list(s5mo2_10,s5mo2_1A), digits = 4, #single.row = T,
       override.se = list(coeftest(s5mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_10))),2],
                          coeftest(s5mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_10))),2],
                          coeftest(s5mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1A))),2],
                          coeftest(s5mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1A))),2]),
       override.pvalues = list(coeftest(s5mo2_10,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_10))),4],
                               coeftest(s5mo2_10,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_10))),4],
                               coeftest(s5mo2_1A,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1A))),4],
                               coeftest(s5mo2_1A,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1A))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger", 
       custom.model.names = c("Base: Agree","Base: Neither",
                              "ZIP: Agree","ZIP: Neither"),
       custom.coef.map = vnmap,
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s5mo2_1_1_tabular.tex"))
texreg(list(s5mo2_1B,s5mo2_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s5mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1B))),2],
                          coeftest(s5mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1B))),2],
                          coeftest(s5mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1C))),2],
                          coeftest(s5mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1C))),2]),
       override.pvalues = list(coeftest(s5mo2_1B,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1B))),4],
                               coeftest(s5mo2_1B,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1B))),4],
                               coeftest(s5mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s5mo2_1C))),4],
                               coeftest(s5mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s5mo2_1C))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap,
       custom.model.names = c("Mun.: Agree","Mun.: Neither",
                              "Full: Agree","Full: Neither"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s5mo2_1_2_tabular.tex"))

#+
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
                       as.factor(wave), data=sifcct.mlogit.tmp, reflevel = "Disagree")
    subname = "Stayed"
  } else {
    # modset <- multinom(foreignsuff3x ~ edu2 * gender * ageset + lvpr + as.factor(wave), 
    #                    data=sifcct[which(sifcct$age - sifcct$lvlen>=23),],
    #                    Hess = TRUE)
    sifcct.mlogit.tmp <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen>=23),],
                               shape = "wide", choice = "foreignsuff3x")
    # levels(sifcct.mlogit.tmp$idx$id2) <- c("Disagree","Neither","Agree")
    modset <- mlogit(foreignsuff3x ~ 0 | edu2 * gender * ageset + lvpr + as.factor(wave), 
                     data=sifcct.mlogit.tmp, reflevel = "Disagree")
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

outdt5m <- rbind(extout("Female",25,1),
                 extout("Female",35,1),
                 extout("Female",45,1),
                 extout("Female",55,1),
                 extout("Female",65,1),
                 extout("Male",25,1),
                 extout("Male",35,1),
                 extout("Male",45,1),
                 extout("Male",55,1),
                 extout("Male",65,1))
outdt5m <- as.data.frame(outdt5m)
for(i in 2:9) outdt5m[,i] <- as.numeric(outdt5m[,i])
outdt5m$gender <- factor(outdt5m$gender, levels=unique(outdt5m$gender))
summary(outdt5m)

#'
#' ## Mediator Models
#' 
#' ## Knowledge
#'

s5mm01_10 <- lm(update(knowledge ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm01_1A <- lm(update(knowledge ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm01_1B <- lm(update(knowledge ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm01_1C <- lm(update(knowledge ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s5mm01_10,s5mm01_1A,s5mm01_1B,s5mm01_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s5mm01_10,vcov.=vcovHC(s5mm01_10))[,2],
                             coeftest(s5mm01_1A,vcov.=vcovHC(s5mm01_1A))[,2],
                             coeftest(s5mm01_1B,vcov.=vcovHC(s5mm01_1B))[,2],
                             coeftest(s5mm01_1C,vcov.=vcovHC(s5mm01_1C))[,2]),
          override.pvalues = list(coeftest(s5mm01_10,vcov.=vcovHC(s5mm01_10))[,4],
                                  coeftest(s5mm01_1A,vcov.=vcovHC(s5mm01_1A))[,4],
                                  coeftest(s5mm01_1B,vcov.=vcovHC(s5mm01_1B))[,4],
                                  coeftest(s5mm01_1C,vcov.=vcovHC(s5mm01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s5mm01_10,s5mm01_1A,s5mm01_1B,s5mm01_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s5mm01_10,vcov.=vcovHC(s5mm01_10))[,2],
                          coeftest(s5mm01_1A,vcov.=vcovHC(s5mm01_1A))[,2],
                          coeftest(s5mm01_1B,vcov.=vcovHC(s5mm01_1B))[,2],
                          coeftest(s5mm01_1C,vcov.=vcovHC(s5mm01_1C))[,2]),
       override.pvalues = list(coeftest(s5mm01_10,vcov.=vcovHC(s5mm01_10))[,4],
                               coeftest(s5mm01_1A,vcov.=vcovHC(s5mm01_1A))[,4],
                               coeftest(s5mm01_1B,vcov.=vcovHC(s5mm01_1B))[,4],
                               coeftest(s5mm01_1C,vcov.=vcovHC(s5mm01_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s5mm01_1_tabular.tex"))



#'
#' ## Ideology
#'

s5mm02_10 <- lm(update(ideology ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm02_1A <- lm(update(ideology ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm02_1B <- lm(update(ideology ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm02_1C <- lm(update(ideology ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s5mm02_10,s5mm02_1A,s5mm02_1B,s5mm02_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s5mm02_10,vcov.=vcovHC(s5mm02_10))[,2],
                             coeftest(s5mm02_1A,vcov.=vcovHC(s5mm02_1A))[,2],
                             coeftest(s5mm02_1B,vcov.=vcovHC(s5mm02_1B))[,2],
                             coeftest(s5mm02_1C,vcov.=vcovHC(s5mm02_1C))[,2]),
          override.pvalues = list(coeftest(s5mm02_10,vcov.=vcovHC(s5mm02_10))[,4],
                                  coeftest(s5mm02_1A,vcov.=vcovHC(s5mm02_1A))[,4],
                                  coeftest(s5mm02_1B,vcov.=vcovHC(s5mm02_1B))[,4],
                                  coeftest(s5mm02_1C,vcov.=vcovHC(s5mm02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s5mm02_10,s5mm02_1A,s5mm02_1B,s5mm02_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s5mm02_10,vcov.=vcovHC(s5mm02_10))[,2],
                          coeftest(s5mm02_1A,vcov.=vcovHC(s5mm02_1A))[,2],
                          coeftest(s5mm02_1B,vcov.=vcovHC(s5mm02_1B))[,2],
                          coeftest(s5mm02_1C,vcov.=vcovHC(s5mm02_1C))[,2]),
       override.pvalues = list(coeftest(s5mm02_10,vcov.=vcovHC(s5mm02_10))[,4],
                               coeftest(s5mm02_1A,vcov.=vcovHC(s5mm02_1A))[,4],
                               coeftest(s5mm02_1B,vcov.=vcovHC(s5mm02_1B))[,4],
                               coeftest(s5mm02_1C,vcov.=vcovHC(s5mm02_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s5mm02_1_tabular.tex"))

#'
#' ## LDP - DPJ FT
#'

s5mm03_10 <- lm(update(ldpdpjft ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm03_1A <- lm(update(ldpdpjft ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm03_1B <- lm(update(ldpdpjft ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm03_1C <- lm(update(ldpdpjft ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s5mm03_10,s5mm03_1A,s5mm03_1B,s5mm03_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s5mm03_10,vcov.=vcovHC(s5mm03_10))[,2],
                             coeftest(s5mm03_1A,vcov.=vcovHC(s5mm03_1A))[,2],
                             coeftest(s5mm03_1B,vcov.=vcovHC(s5mm03_1B))[,2],
                             coeftest(s5mm03_1C,vcov.=vcovHC(s5mm03_1C))[,2]),
          override.pvalues = list(coeftest(s5mm03_10,vcov.=vcovHC(s5mm03_10))[,4],
                                  coeftest(s5mm03_1A,vcov.=vcovHC(s5mm03_1A))[,4],
                                  coeftest(s5mm03_1B,vcov.=vcovHC(s5mm03_1B))[,4],
                                  coeftest(s5mm03_1C,vcov.=vcovHC(s5mm03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s5mm03_10,s5mm03_1A,s5mm03_1B,s5mm03_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s5mm03_10,vcov.=vcovHC(s5mm03_10))[,2],
                          coeftest(s5mm03_1A,vcov.=vcovHC(s5mm03_1A))[,2],
                          coeftest(s5mm03_1B,vcov.=vcovHC(s5mm03_1B))[,2],
                          coeftest(s5mm03_1C,vcov.=vcovHC(s5mm03_1C))[,2]),
       override.pvalues = list(coeftest(s5mm03_10,vcov.=vcovHC(s5mm03_10))[,4],
                               coeftest(s5mm03_1A,vcov.=vcovHC(s5mm03_1A))[,4],
                               coeftest(s5mm03_1B,vcov.=vcovHC(s5mm03_1B))[,4],
                               coeftest(s5mm03_1C,vcov.=vcovHC(s5mm03_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s5mm03_1_tabular.tex"))


#'
#' ## Favorability of South Korea
#'

s5mm04_10 <- lm(update(familiarityFT_KOR ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm04_1A <- lm(update(familiarityFT_KOR ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm04_1B <- lm(update(familiarityFT_KOR ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm04_1C <- lm(update(familiarityFT_KOR ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s5mm04_10,s5mm04_1A,s5mm04_1B,s5mm04_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s5mm04_10,vcov.=vcovHC(s5mm04_10))[,2],
                             coeftest(s5mm04_1A,vcov.=vcovHC(s5mm04_1A))[,2],
                             coeftest(s5mm04_1B,vcov.=vcovHC(s5mm04_1B))[,2],
                             coeftest(s5mm04_1C,vcov.=vcovHC(s5mm04_1C))[,2]),
          override.pvalues = list(coeftest(s5mm04_10,vcov.=vcovHC(s5mm04_10))[,4],
                                  coeftest(s5mm04_1A,vcov.=vcovHC(s5mm04_1A))[,4],
                                  coeftest(s5mm04_1B,vcov.=vcovHC(s5mm04_1B))[,4],
                                  coeftest(s5mm04_1C,vcov.=vcovHC(s5mm04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s5mm04_10,s5mm04_1A,s5mm04_1B,s5mm04_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s5mm04_10,vcov.=vcovHC(s5mm04_10))[,2],
                          coeftest(s5mm04_1A,vcov.=vcovHC(s5mm04_1A))[,2],
                          coeftest(s5mm04_1B,vcov.=vcovHC(s5mm04_1B))[,2],
                          coeftest(s5mm04_1C,vcov.=vcovHC(s5mm04_1C))[,2]),
       override.pvalues = list(coeftest(s5mm04_10,vcov.=vcovHC(s5mm04_10))[,4],
                               coeftest(s5mm04_1A,vcov.=vcovHC(s5mm04_1A))[,4],
                               coeftest(s5mm04_1B,vcov.=vcovHC(s5mm04_1B))[,4],
                               coeftest(s5mm04_1C,vcov.=vcovHC(s5mm04_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s5mm04_1_tabular.tex"))

#'
#' ## Favorability of China
#'

s5mm05_10 <- lm(update(familiarityFT_CHN ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm05_1A <- lm(update(familiarityFT_CHN ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm05_1B <- lm(update(familiarityFT_CHN ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm05_1C <- lm(update(familiarityFT_CHN ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s5mm05_10,s5mm05_1A,s5mm05_1B,s5mm05_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s5mm05_10,vcov.=vcovHC(s5mm05_10))[,2],
                             coeftest(s5mm05_1A,vcov.=vcovHC(s5mm05_1A))[,2],
                             coeftest(s5mm05_1B,vcov.=vcovHC(s5mm05_1B))[,2],
                             coeftest(s5mm05_1C,vcov.=vcovHC(s5mm05_1C))[,2]),
          override.pvalues = list(coeftest(s5mm05_10,vcov.=vcovHC(s5mm05_10))[,4],
                                  coeftest(s5mm05_1A,vcov.=vcovHC(s5mm05_1A))[,4],
                                  coeftest(s5mm05_1B,vcov.=vcovHC(s5mm05_1B))[,4],
                                  coeftest(s5mm05_1C,vcov.=vcovHC(s5mm05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s5mm05_10,s5mm05_1A,s5mm05_1B,s5mm05_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s5mm05_10,vcov.=vcovHC(s5mm05_10))[,2],
                          coeftest(s5mm05_1A,vcov.=vcovHC(s5mm05_1A))[,2],
                          coeftest(s5mm05_1B,vcov.=vcovHC(s5mm05_1B))[,2],
                          coeftest(s5mm05_1C,vcov.=vcovHC(s5mm05_1C))[,2]),
       override.pvalues = list(coeftest(s5mm05_10,vcov.=vcovHC(s5mm05_10))[,4],
                               coeftest(s5mm05_1A,vcov.=vcovHC(s5mm05_1A))[,4],
                               coeftest(s5mm05_1B,vcov.=vcovHC(s5mm05_1B))[,4],
                               coeftest(s5mm05_1C,vcov.=vcovHC(s5mm05_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s5mm05_1_tabular.tex"))

#'
#' ## Favorability of USA
#'

s5mm06_10 <- lm(update(familiarityFT_USA ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm06_1A <- lm(update(familiarityFT_USA ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm06_1B <- lm(update(familiarityFT_USA ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm06_1C <- lm(update(familiarityFT_USA ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s5mm06_10,s5mm06_1A,s5mm06_1B,s5mm06_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s5mm06_10,vcov.=vcovHC(s5mm06_10))[,2],
                             coeftest(s5mm06_1A,vcov.=vcovHC(s5mm06_1A))[,2],
                             coeftest(s5mm06_1B,vcov.=vcovHC(s5mm06_1B))[,2],
                             coeftest(s5mm06_1C,vcov.=vcovHC(s5mm06_1C))[,2]),
          override.pvalues = list(coeftest(s5mm06_10,vcov.=vcovHC(s5mm06_10))[,4],
                                  coeftest(s5mm06_1A,vcov.=vcovHC(s5mm06_1A))[,4],
                                  coeftest(s5mm06_1B,vcov.=vcovHC(s5mm06_1B))[,4],
                                  coeftest(s5mm06_1C,vcov.=vcovHC(s5mm06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s5mm06_10,s5mm06_1A,s5mm06_1B,s5mm06_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s5mm06_10,vcov.=vcovHC(s5mm06_10))[,2],
                          coeftest(s5mm06_1A,vcov.=vcovHC(s5mm06_1A))[,2],
                          coeftest(s5mm06_1B,vcov.=vcovHC(s5mm06_1B))[,2],
                          coeftest(s5mm06_1C,vcov.=vcovHC(s5mm06_1C))[,2]),
       override.pvalues = list(coeftest(s5mm06_10,vcov.=vcovHC(s5mm06_10))[,4],
                               coeftest(s5mm06_1A,vcov.=vcovHC(s5mm06_1A))[,4],
                               coeftest(s5mm06_1B,vcov.=vcovHC(s5mm06_1B))[,4],
                               coeftest(s5mm06_1C,vcov.=vcovHC(s5mm06_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s5mm06_1_tabular.tex"))


#' 
#' ## Income
#'

s5mm07_10 <- lm(update(income ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm07_1A <- lm(update(income ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm07_1B <- lm(update(income ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
s5mm07_1C <- lm(update(income ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen<=15),])
screenreg(list(s5mm07_10,s5mm07_1A,s5mm07_1B,s5mm07_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s5mm07_10,vcov.=vcovHC(s5mm07_10))[,2],
                             coeftest(s5mm07_1A,vcov.=vcovHC(s5mm07_1A))[,2],
                             coeftest(s5mm07_1B,vcov.=vcovHC(s5mm07_1B))[,2],
                             coeftest(s5mm07_1C,vcov.=vcovHC(s5mm07_1C))[,2]),
          override.pvalues = list(coeftest(s5mm07_10,vcov.=vcovHC(s5mm07_10))[,4],
                                  coeftest(s5mm07_1A,vcov.=vcovHC(s5mm07_1A))[,4],
                                  coeftest(s5mm07_1B,vcov.=vcovHC(s5mm07_1B))[,4],
                                  coeftest(s5mm07_1C,vcov.=vcovHC(s5mm07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s5mm07_10,s5mm07_1A,s5mm07_1B,s5mm07_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s5mm07_10,vcov.=vcovHC(s5mm07_10))[,2],
                          coeftest(s5mm07_1A,vcov.=vcovHC(s5mm07_1A))[,2],
                          coeftest(s5mm07_1B,vcov.=vcovHC(s5mm07_1B))[,2],
                          coeftest(s5mm07_1C,vcov.=vcovHC(s5mm07_1C))[,2]),
       override.pvalues = list(coeftest(s5mm07_10,vcov.=vcovHC(s5mm07_10))[,4],
                               coeftest(s5mm07_1A,vcov.=vcovHC(s5mm07_1A))[,4],
                               coeftest(s5mm07_1B,vcov.=vcovHC(s5mm07_1B))[,4],
                               coeftest(s5mm07_1C,vcov.=vcovHC(s5mm07_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s5mm07_1_tabular.tex"))

#'
#' # Organizing Outcomes
#' 
#' ## OLS
#'

outdt0$data <- "Unmatched"
outdt1$data <- "Matched without \nDistance Adj."
outdt2$data <- "Matched with \nLambda = 50km"
outdt3$data <- "Matched with \nLambda = 100km"
outdt4$data <- "Matched with \nLambda = 200km"
outdt5$data <- "Matched with \nLambda = 350km"

visdt <- rbind(outdt0,outdt1,outdt2,outdt3,outdt4,outdt5)

visdt$data <- factor(visdt$data, levels = c("Unmatched",
                                            "Matched without \nDistance Adj.",
                                            "Matched with \nLambda = 350km",
                                            "Matched with \nLambda = 200km",
                                            "Matched with \nLambda = 100km",
                                            "Matched with \nLambda = 50km"))
visdt$pstar <- factor(ifelse(visdt$p>=.1,"n.s.",ifelse(visdt$p>=.05,"p<.1","p<.05")),
                      levels = c("p<.05","p<.1","n.s."))

#+ eval=FALSE
saveRDS(visdt, paste0(projdir, "/out/visdt.rds"))

#'
#' ## Multinomial Logit
#'

outdt0m$data <- "Unmatched"
outdt1m$data <- "Matched without \nDistance Adj."
outdt2m$data <- "Matched with \nLambda = 50km"
outdt3m$data <- "Matched with \nLambda = 100km"
outdt4m$data <- "Matched with \nLambda = 200km"
outdt5m$data <- "Matched with \nLambda = 350km"

visdtm <- rbind(outdt0m,outdt1m,outdt2m,outdt3m,outdt4m,outdt5m)

visdtm$data <- factor(visdtm$data, levels = c("Unmatched",
                                              "Matched without \nDistance Adj.",
                                              "Matched with \nLambda = 350km",
                                              "Matched with \nLambda = 200km",
                                              "Matched with \nLambda = 100km",
                                              "Matched with \nLambda = 50km"))
visdtm$pstar <- factor(ifelse(visdtm$p>=.1,"n.s.",ifelse(visdtm$p>=.05,"p<.1","p<.05")),
                       levels = c("p<.05","p<.1","n.s."))

#+ eval=FALSE
saveRDS(visdtm, paste0(projdir, "/out/visdtm.rds"))

#'
#' ## Combining OLS and Multinomial Logit
#'

visdt$method = "OLS"
visdtm$method = "Multinomial Logit\nAgree vs. Disagree"
visdtall <- rbind(visdt,visdtm)
visdtall$method <- factor(visdtall$method, levels = unique(visdtall$method))
colnames(visdtall)

#'
#' ## Including Mail
#'

visdt_mail_ols <- readRDS(paste0(projdir, "/out/visdt_mail_ols.rds"))
visdt_mail_ols$method <- "OLS"
visdt_mail_multinom <- readRDS(paste0(projdir, "/out/visdt_mail_multinom.rds"))
visdt_mail_multinom$method <- "Multinomial Logit\nAgree vs. Disagree"
visdt_mail <- rbind(visdt_mail_ols,visdt_mail_multinom)
visdt_mail$lci95 <- NA
visdt_mail$uci95 <- NA
visdt_mail$lci90 <- NA
visdt_mail$uci90 <- NA
colnames(visdt_mail)
visdtall <- rbind(visdtall,visdt_mail)
visdtall$data <- factor(visdtall$data, levels = unique(visdtall$data))
table(visdtall$data)

#+ eval=FALSE
saveRDS(visdtall, paste0(projdir, "/out/visdtall.rds"))

#'
#' # Save Image
#'

#+ eval=FALSE
save.image(file=paste0(projdir,"/out/heavy/analysis_2_matched_v5.RData"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/analysis_2_matched_v5.R', rmarkdown::pdf_document(latex_engine="xelatex", extra_dependencies = list(bookmark=NULL, xltxtra=NULL, zxjatype=NULL, zxjafont=c("ipa"))), encoding = 'UTF-8')
# rmarkdown::render('./src/analysis_2_matched_v5.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
