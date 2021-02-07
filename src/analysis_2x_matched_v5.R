#' ---
#' title: "Analysis 2x: Main Analysis with Matched Data (Movers)"
#' author: "Fan Lu & Gento Kato"
#' date: "January 8, 2020"
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
#' If age - years of local ZIP residence is 23 or larger. 
#' 23 is the age of graduating university (the youngest possible) in Japan. 
#' Assuming that an individual is living in the local ZIP continuously, 
#' this condition implies that one moved to the ZIP of current residence 
#' (likely) after graduating the university. This incorporates the possibility 
#' that education changes attitudes through the movement in residence.
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
datadir0x <- paste0(projdir, "/data/sifcct_moved_unmatched_v5.rds")
datadir1x <- paste0(projdir,"/data/sifcct_moved_matched_1_all_v5.rds")

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

sifcct <- readRDS(datadir0x)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)

#'
#' ## Outcome Model 
#'

## Living in Local ZIP since at least age 15 ##

s0mox_10 <- lm(update(foreignsuff ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mox_1A <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mox_1B <- lm(update(foreignsuff ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mox_1C <- lm(update(foreignsuff ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s0mox_10,s0mox_1A,s0mox_1B,s0mox_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mox_10,vcov.=vcovHC(s0mox_10))[,2],
                             coeftest(s0mox_1A,vcov.=vcovHC(s0mox_1A))[,2],
                             coeftest(s0mox_1B,vcov.=vcovHC(s0mox_1B))[,2],
                             coeftest(s0mox_1C,vcov.=vcovHC(s0mox_1C))[,2]),
          override.pvalues = list(coeftest(s0mox_10,vcov.=vcovHC(s0mox_10))[,4],
                                  coeftest(s0mox_1A,vcov.=vcovHC(s0mox_1A))[,4],
                                  coeftest(s0mox_1B,vcov.=vcovHC(s0mox_1B))[,4],
                                  coeftest(s0mox_1C,vcov.=vcovHC(s0mox_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mox_10,s0mox_1A,s0mox_1B,s0mox_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mox_10,vcov.=vcovHC(s0mox_10))[,2],
                          coeftest(s0mox_1A,vcov.=vcovHC(s0mox_1A))[,2],
                          coeftest(s0mox_1B,vcov.=vcovHC(s0mox_1B))[,2],
                          coeftest(s0mox_1C,vcov.=vcovHC(s0mox_1C))[,2]),
       override.pvalues = list(coeftest(s0mox_10,vcov.=vcovHC(s0mox_10))[,4],
                               coeftest(s0mox_1A,vcov.=vcovHC(s0mox_1A))[,4],
                               coeftest(s0mox_1B,vcov.=vcovHC(s0mox_1B))[,4],
                               coeftest(s0mox_1C,vcov.=vcovHC(s0mox_1C))[,4]),
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mox_1_tabular.tex"))

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

outdt0x <- rbind(extout("Female",25,2),
                extout("Female",35,2),
                extout("Female",45,2),
                extout("Female",55,2),
                extout("Female",65,2),
                extout("Male",25,2),
                extout("Male",35,2),
                extout("Male",45,2),
                extout("Male",55,2),
                extout("Male",65,2))
outdt0x <- as.data.frame(outdt0x)
for(i in 2:9) outdt0x[,i] <- as.numeric(outdt0x[,i])
outdt0x$gender <- factor(outdt0x$gender, levels=unique(outdt0x$gender))
summary(outdt0x)

#'
#' ## Outcome Model 2
#'

## Living in Local ZIP since at least age 15 ##

# require(nnet)
# s0mox2_10 <- multinom(update(foreignsuff3x ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
# s0mox2_1A <- multinom(update(foreignsuff3x ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
# s0mox2_1B <- multinom(update(foreignsuff3x ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
# s0mox2_1C <- multinom(update(foreignsuff3x ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])

sifcct.mlogit <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen>=23),],
                       shape = "wide", choice = "foreignsuff3x")
# # levels(sifcct.mlogit$idx$id2) <- c("Disagree","Neither","Agree")
s0mox2_10 <- mlogit(outmod0.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s0mox2_1A <- mlogit(outmodA.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s0mox2_1B <- mlogit(outmodB.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s0mox2_1C <- mlogit(outmodC.mlogit, data=sifcct.mlogit, reflevel="Disagree")

screenreg(list(s0mox2_10,s0mox2_1A), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mox2_10,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_10))),2],
                             coeftest(s0mox2_10,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_10))),2],
                             coeftest(s0mox2_1A,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1A))),2],
                             coeftest(s0mox2_1A,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1A))),2]),
          override.pvalues = list(coeftest(s0mox2_10,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_10))),4],
                                  coeftest(s0mox2_10,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_10))),4],
                                  coeftest(s0mox2_1A,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1A))),4],
                                  coeftest(s0mox2_1A,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+", 
          custom.model.names = c("Base: Agree","Base: Neither",
                                 "ZIP: Agree","ZIP: Neither"),
          custom.coef.map = vnmap)
screenreg(list(s0mox2_1B,s0mox2_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mox2_1B,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1B))),2],
                             coeftest(s0mox2_1B,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1B))),2],
                             coeftest(s0mox2_1C,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1C))),2],
                             coeftest(s0mox2_1C,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1C))),2]),
          override.pvalues = list(coeftest(s0mox2_1B,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1B))),4],
                                  coeftest(s0mox2_1B,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1B))),4],
                                  coeftest(s0mox2_1C,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1C))),4],
                                  coeftest(s0mox2_1C,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Agree","Mun.: Neither",
                                 "Full: Agree","Full: Neither"))

#+ include = FALSE
texreg(list(s0mox2_10,s0mox2_1A), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mox2_10,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_10))),2],
                          coeftest(s0mox2_10,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_10))),2],
                          coeftest(s0mox2_1A,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1A))),2],
                          coeftest(s0mox2_1A,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1A))),2]),
       override.pvalues = list(coeftest(s0mox2_10,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_10))),4],
                               coeftest(s0mox2_10,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_10))),4],
                               coeftest(s0mox2_1A,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1A))),4],
                               coeftest(s0mox2_1A,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1A))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger", 
       custom.model.names = c("Base: Agree","Base: Neither",
                              "ZIP: Agree","ZIP: Neither"),
       custom.coef.map = vnmap,
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mox2_1_1_tabular.tex"))
texreg(list(s0mox2_1B,s0mox2_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mox2_1B,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1B))),2],
                          coeftest(s0mox2_1B,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1B))),2],
                          coeftest(s0mox2_1C,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1C))),2],
                          coeftest(s0mox2_1C,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1C))),2]),
       override.pvalues = list(coeftest(s0mox2_1B,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1B))),4],
                               coeftest(s0mox2_1B,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1B))),4],
                               coeftest(s0mox2_1C,vcov=sandwich)[grep(":Neither",names(coef(s0mox2_1C))),4],
                               coeftest(s0mox2_1C,vcov=sandwich)[grep(":Agree",names(coef(s0mox2_1C))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap,
       custom.model.names = c("Mun.: Agree","Mun.: Neither",
                              "Full: Agree","Full: Neither"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mox2_1_2_tabular.tex"))

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

outdt0xm <- rbind(extout("Female",25,2),
                 extout("Female",35,2),
                 extout("Female",45,2),
                 extout("Female",55,2),
                 extout("Female",65,2),
                 extout("Male",25,2),
                 extout("Male",35,2),
                 extout("Male",45,2),
                 extout("Male",55,2),
                 extout("Male",65,2))
outdt0xm <- as.data.frame(outdt0xm)
for(i in 2:9) outdt0xm[,i] <- as.numeric(outdt0xm[,i])
outdt0xm$gender <- factor(outdt0xm$gender, levels=unique(outdt0xm$gender))
summary(outdt0xm)

#'
#' ## Mediator Models
#' 
#' ## Knowledge
#'

s0mmx01_10 <- lm(update(knowledge ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx01_1A <- lm(update(knowledge ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx01_1B <- lm(update(knowledge ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx01_1C <- lm(update(knowledge ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s0mmx01_10,s0mmx01_1A,s0mmx01_1B,s0mmx01_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mmx01_10,vcov.=vcovHC(s0mmx01_10))[,2],
                             coeftest(s0mmx01_1A,vcov.=vcovHC(s0mmx01_1A))[,2],
                             coeftest(s0mmx01_1B,vcov.=vcovHC(s0mmx01_1B))[,2],
                             coeftest(s0mmx01_1C,vcov.=vcovHC(s0mmx01_1C))[,2]),
          override.pvalues = list(coeftest(s0mmx01_10,vcov.=vcovHC(s0mmx01_10))[,4],
                                  coeftest(s0mmx01_1A,vcov.=vcovHC(s0mmx01_1A))[,4],
                                  coeftest(s0mmx01_1B,vcov.=vcovHC(s0mmx01_1B))[,4],
                                  coeftest(s0mmx01_1C,vcov.=vcovHC(s0mmx01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mmx01_10,s0mmx01_1A,s0mmx01_1B,s0mmx01_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mmx01_10,vcov.=vcovHC(s0mmx01_10))[,2],
                          coeftest(s0mmx01_1A,vcov.=vcovHC(s0mmx01_1A))[,2],
                          coeftest(s0mmx01_1B,vcov.=vcovHC(s0mmx01_1B))[,2],
                          coeftest(s0mmx01_1C,vcov.=vcovHC(s0mmx01_1C))[,2]),
       override.pvalues = list(coeftest(s0mmx01_10,vcov.=vcovHC(s0mmx01_10))[,4],
                               coeftest(s0mmx01_1A,vcov.=vcovHC(s0mmx01_1A))[,4],
                               coeftest(s0mmx01_1B,vcov.=vcovHC(s0mmx01_1B))[,4],
                               coeftest(s0mmx01_1C,vcov.=vcovHC(s0mmx01_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mmx01_1_tabular.tex"))



#'
#' ## Ideology
#'

s0mmx02_10 <- lm(update(ideology ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx02_1A <- lm(update(ideology ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx02_1B <- lm(update(ideology ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx02_1C <- lm(update(ideology ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s0mmx02_10,s0mmx02_1A,s0mmx02_1B,s0mmx02_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mmx02_10,vcov.=vcovHC(s0mmx02_10))[,2],
                             coeftest(s0mmx02_1A,vcov.=vcovHC(s0mmx02_1A))[,2],
                             coeftest(s0mmx02_1B,vcov.=vcovHC(s0mmx02_1B))[,2],
                             coeftest(s0mmx02_1C,vcov.=vcovHC(s0mmx02_1C))[,2]),
          override.pvalues = list(coeftest(s0mmx02_10,vcov.=vcovHC(s0mmx02_10))[,4],
                                  coeftest(s0mmx02_1A,vcov.=vcovHC(s0mmx02_1A))[,4],
                                  coeftest(s0mmx02_1B,vcov.=vcovHC(s0mmx02_1B))[,4],
                                  coeftest(s0mmx02_1C,vcov.=vcovHC(s0mmx02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mmx02_10,s0mmx02_1A,s0mmx02_1B,s0mmx02_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mmx02_10,vcov.=vcovHC(s0mmx02_10))[,2],
                          coeftest(s0mmx02_1A,vcov.=vcovHC(s0mmx02_1A))[,2],
                          coeftest(s0mmx02_1B,vcov.=vcovHC(s0mmx02_1B))[,2],
                          coeftest(s0mmx02_1C,vcov.=vcovHC(s0mmx02_1C))[,2]),
       override.pvalues = list(coeftest(s0mmx02_10,vcov.=vcovHC(s0mmx02_10))[,4],
                               coeftest(s0mmx02_1A,vcov.=vcovHC(s0mmx02_1A))[,4],
                               coeftest(s0mmx02_1B,vcov.=vcovHC(s0mmx02_1B))[,4],
                               coeftest(s0mmx02_1C,vcov.=vcovHC(s0mmx02_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mmx02_1_tabular.tex"))

#'
#' ## LDP - DPJ FT
#'

s0mmx03_10 <- lm(update(ldpdpjft ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx03_1A <- lm(update(ldpdpjft ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx03_1B <- lm(update(ldpdpjft ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx03_1C <- lm(update(ldpdpjft ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s0mmx03_10,s0mmx03_1A,s0mmx03_1B,s0mmx03_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mmx03_10,vcov.=vcovHC(s0mmx03_10))[,2],
                             coeftest(s0mmx03_1A,vcov.=vcovHC(s0mmx03_1A))[,2],
                             coeftest(s0mmx03_1B,vcov.=vcovHC(s0mmx03_1B))[,2],
                             coeftest(s0mmx03_1C,vcov.=vcovHC(s0mmx03_1C))[,2]),
          override.pvalues = list(coeftest(s0mmx03_10,vcov.=vcovHC(s0mmx03_10))[,4],
                                  coeftest(s0mmx03_1A,vcov.=vcovHC(s0mmx03_1A))[,4],
                                  coeftest(s0mmx03_1B,vcov.=vcovHC(s0mmx03_1B))[,4],
                                  coeftest(s0mmx03_1C,vcov.=vcovHC(s0mmx03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mmx03_10,s0mmx03_1A,s0mmx03_1B,s0mmx03_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mmx03_10,vcov.=vcovHC(s0mmx03_10))[,2],
                          coeftest(s0mmx03_1A,vcov.=vcovHC(s0mmx03_1A))[,2],
                          coeftest(s0mmx03_1B,vcov.=vcovHC(s0mmx03_1B))[,2],
                          coeftest(s0mmx03_1C,vcov.=vcovHC(s0mmx03_1C))[,2]),
       override.pvalues = list(coeftest(s0mmx03_10,vcov.=vcovHC(s0mmx03_10))[,4],
                               coeftest(s0mmx03_1A,vcov.=vcovHC(s0mmx03_1A))[,4],
                               coeftest(s0mmx03_1B,vcov.=vcovHC(s0mmx03_1B))[,4],
                               coeftest(s0mmx03_1C,vcov.=vcovHC(s0mmx03_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mmx03_1_tabular.tex"))


#'
#' ## Favorability of South Korea
#'

s0mmx04_10 <- lm(update(familiarityFT_KOR ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx04_1A <- lm(update(familiarityFT_KOR ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx04_1B <- lm(update(familiarityFT_KOR ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx04_1C <- lm(update(familiarityFT_KOR ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s0mmx04_10,s0mmx04_1A,s0mmx04_1B,s0mmx04_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mmx04_10,vcov.=vcovHC(s0mmx04_10))[,2],
                             coeftest(s0mmx04_1A,vcov.=vcovHC(s0mmx04_1A))[,2],
                             coeftest(s0mmx04_1B,vcov.=vcovHC(s0mmx04_1B))[,2],
                             coeftest(s0mmx04_1C,vcov.=vcovHC(s0mmx04_1C))[,2]),
          override.pvalues = list(coeftest(s0mmx04_10,vcov.=vcovHC(s0mmx04_10))[,4],
                                  coeftest(s0mmx04_1A,vcov.=vcovHC(s0mmx04_1A))[,4],
                                  coeftest(s0mmx04_1B,vcov.=vcovHC(s0mmx04_1B))[,4],
                                  coeftest(s0mmx04_1C,vcov.=vcovHC(s0mmx04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mmx04_10,s0mmx04_1A,s0mmx04_1B,s0mmx04_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mmx04_10,vcov.=vcovHC(s0mmx04_10))[,2],
                          coeftest(s0mmx04_1A,vcov.=vcovHC(s0mmx04_1A))[,2],
                          coeftest(s0mmx04_1B,vcov.=vcovHC(s0mmx04_1B))[,2],
                          coeftest(s0mmx04_1C,vcov.=vcovHC(s0mmx04_1C))[,2]),
       override.pvalues = list(coeftest(s0mmx04_10,vcov.=vcovHC(s0mmx04_10))[,4],
                               coeftest(s0mmx04_1A,vcov.=vcovHC(s0mmx04_1A))[,4],
                               coeftest(s0mmx04_1B,vcov.=vcovHC(s0mmx04_1B))[,4],
                               coeftest(s0mmx04_1C,vcov.=vcovHC(s0mmx04_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mmx04_1_tabular.tex"))

#'
#' ## Favorability of China
#'

s0mmx05_10 <- lm(update(familiarityFT_CHN ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx05_1A <- lm(update(familiarityFT_CHN ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx05_1B <- lm(update(familiarityFT_CHN ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx05_1C <- lm(update(familiarityFT_CHN ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s0mmx05_10,s0mmx05_1A,s0mmx05_1B,s0mmx05_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mmx05_10,vcov.=vcovHC(s0mmx05_10))[,2],
                             coeftest(s0mmx05_1A,vcov.=vcovHC(s0mmx05_1A))[,2],
                             coeftest(s0mmx05_1B,vcov.=vcovHC(s0mmx05_1B))[,2],
                             coeftest(s0mmx05_1C,vcov.=vcovHC(s0mmx05_1C))[,2]),
          override.pvalues = list(coeftest(s0mmx05_10,vcov.=vcovHC(s0mmx05_10))[,4],
                                  coeftest(s0mmx05_1A,vcov.=vcovHC(s0mmx05_1A))[,4],
                                  coeftest(s0mmx05_1B,vcov.=vcovHC(s0mmx05_1B))[,4],
                                  coeftest(s0mmx05_1C,vcov.=vcovHC(s0mmx05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mmx05_10,s0mmx05_1A,s0mmx05_1B,s0mmx05_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mmx05_10,vcov.=vcovHC(s0mmx05_10))[,2],
                          coeftest(s0mmx05_1A,vcov.=vcovHC(s0mmx05_1A))[,2],
                          coeftest(s0mmx05_1B,vcov.=vcovHC(s0mmx05_1B))[,2],
                          coeftest(s0mmx05_1C,vcov.=vcovHC(s0mmx05_1C))[,2]),
       override.pvalues = list(coeftest(s0mmx05_10,vcov.=vcovHC(s0mmx05_10))[,4],
                               coeftest(s0mmx05_1A,vcov.=vcovHC(s0mmx05_1A))[,4],
                               coeftest(s0mmx05_1B,vcov.=vcovHC(s0mmx05_1B))[,4],
                               coeftest(s0mmx05_1C,vcov.=vcovHC(s0mmx05_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mmx05_1_tabular.tex"))

#'
#' ## Favorability of USA
#'

s0mmx06_10 <- lm(update(familiarityFT_USA ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx06_1A <- lm(update(familiarityFT_USA ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx06_1B <- lm(update(familiarityFT_USA ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx06_1C <- lm(update(familiarityFT_USA ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s0mmx06_10,s0mmx06_1A,s0mmx06_1B,s0mmx06_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mmx06_10,vcov.=vcovHC(s0mmx06_10))[,2],
                             coeftest(s0mmx06_1A,vcov.=vcovHC(s0mmx06_1A))[,2],
                             coeftest(s0mmx06_1B,vcov.=vcovHC(s0mmx06_1B))[,2],
                             coeftest(s0mmx06_1C,vcov.=vcovHC(s0mmx06_1C))[,2]),
          override.pvalues = list(coeftest(s0mmx06_10,vcov.=vcovHC(s0mmx06_10))[,4],
                                  coeftest(s0mmx06_1A,vcov.=vcovHC(s0mmx06_1A))[,4],
                                  coeftest(s0mmx06_1B,vcov.=vcovHC(s0mmx06_1B))[,4],
                                  coeftest(s0mmx06_1C,vcov.=vcovHC(s0mmx06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mmx06_10,s0mmx06_1A,s0mmx06_1B,s0mmx06_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mmx06_10,vcov.=vcovHC(s0mmx06_10))[,2],
                          coeftest(s0mmx06_1A,vcov.=vcovHC(s0mmx06_1A))[,2],
                          coeftest(s0mmx06_1B,vcov.=vcovHC(s0mmx06_1B))[,2],
                          coeftest(s0mmx06_1C,vcov.=vcovHC(s0mmx06_1C))[,2]),
       override.pvalues = list(coeftest(s0mmx06_10,vcov.=vcovHC(s0mmx06_10))[,4],
                               coeftest(s0mmx06_1A,vcov.=vcovHC(s0mmx06_1A))[,4],
                               coeftest(s0mmx06_1B,vcov.=vcovHC(s0mmx06_1B))[,4],
                               coeftest(s0mmx06_1C,vcov.=vcovHC(s0mmx06_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mmx06_1_tabular.tex"))


#' 
#' ## Income
#'

s0mmx07_10 <- lm(update(income ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx07_1A <- lm(update(income ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx07_1B <- lm(update(income ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s0mmx07_1C <- lm(update(income ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s0mmx07_10,s0mmx07_1A,s0mmx07_1B,s0mmx07_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s0mmx07_10,vcov.=vcovHC(s0mmx07_10))[,2],
                             coeftest(s0mmx07_1A,vcov.=vcovHC(s0mmx07_1A))[,2],
                             coeftest(s0mmx07_1B,vcov.=vcovHC(s0mmx07_1B))[,2],
                             coeftest(s0mmx07_1C,vcov.=vcovHC(s0mmx07_1C))[,2]),
          override.pvalues = list(coeftest(s0mmx07_10,vcov.=vcovHC(s0mmx07_10))[,4],
                                  coeftest(s0mmx07_1A,vcov.=vcovHC(s0mmx07_1A))[,4],
                                  coeftest(s0mmx07_1B,vcov.=vcovHC(s0mmx07_1B))[,4],
                                  coeftest(s0mmx07_1C,vcov.=vcovHC(s0mmx07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s0mmx07_10,s0mmx07_1A,s0mmx07_1B,s0mmx07_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s0mmx07_10,vcov.=vcovHC(s0mmx07_10))[,2],
                          coeftest(s0mmx07_1A,vcov.=vcovHC(s0mmx07_1A))[,2],
                          coeftest(s0mmx07_1B,vcov.=vcovHC(s0mmx07_1B))[,2],
                          coeftest(s0mmx07_1C,vcov.=vcovHC(s0mmx07_1C))[,2]),
       override.pvalues = list(coeftest(s0mmx07_10,vcov.=vcovHC(s0mmx07_10))[,4],
                               coeftest(s0mmx07_1A,vcov.=vcovHC(s0mmx07_1A))[,4],
                               coeftest(s0mmx07_1B,vcov.=vcovHC(s0mmx07_1B))[,4],
                               coeftest(s0mmx07_1C,vcov.=vcovHC(s0mmx07_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s0mmx07_1_tabular.tex"))

#'
#' # With Matched Data (Without Distance Adjustment)
#'

sifcct <- readRDS(datadir1x)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)

#'
#' ## Outcome Model 
#'

## Living in Local ZIP since at least age 15 ##

s1mox_10 <- lm(update(foreignsuff ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mox_1A <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mox_1B <- lm(update(foreignsuff ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mox_1C <- lm(update(foreignsuff ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s1mox_10,s1mox_1A,s1mox_1B,s1mox_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mox_10,vcov.=vcovHC(s1mox_10))[,2],
                             coeftest(s1mox_1A,vcov.=vcovHC(s1mox_1A))[,2],
                             coeftest(s1mox_1B,vcov.=vcovHC(s1mox_1B))[,2],
                             coeftest(s1mox_1C,vcov.=vcovHC(s1mox_1C))[,2]),
          override.pvalues = list(coeftest(s1mox_10,vcov.=vcovHC(s1mox_10))[,4],
                                  coeftest(s1mox_1A,vcov.=vcovHC(s1mox_1A))[,4],
                                  coeftest(s1mox_1B,vcov.=vcovHC(s1mox_1B))[,4],
                                  coeftest(s1mox_1C,vcov.=vcovHC(s1mox_1C))[,4]),
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mox_10,s1mox_1A,s1mox_1B,s1mox_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mox_10,vcov.=vcovHC(s1mox_10))[,2],
                          coeftest(s1mox_1A,vcov.=vcovHC(s1mox_1A))[,2],
                          coeftest(s1mox_1B,vcov.=vcovHC(s1mox_1B))[,2],
                          coeftest(s1mox_1C,vcov.=vcovHC(s1mox_1C))[,2]),
       override.pvalues = list(coeftest(s1mox_10,vcov.=vcovHC(s1mox_10))[,4],
                               coeftest(s1mox_1A,vcov.=vcovHC(s1mox_1A))[,4],
                               coeftest(s1mox_1B,vcov.=vcovHC(s1mox_1B))[,4],
                               coeftest(s1mox_1C,vcov.=vcovHC(s1mox_1C))[,4]),
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mox_1_tabular.tex"))

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

outdt1x <- rbind(extout("Female",25,2),
                extout("Female",35,2),
                extout("Female",45,2),
                extout("Female",55,2),
                extout("Female",65,2),
                extout("Male",25,2),
                extout("Male",35,2),
                extout("Male",45,2),
                extout("Male",55,2),
                extout("Male",65,2))
outdt1x <- as.data.frame(outdt1x)
for(i in 2:9) outdt1x[,i] <- as.numeric(outdt1x[,i])
outdt1x$gender <- factor(outdt1x$gender, levels=unique(outdt1x$gender))
summary(outdt1x)

#'
#' ## Outcome Model 2
#'

## Living in Local ZIP since at least age 15 ##

# require(nnet)
# s1mox2_10 <- multinom(update(foreignsuff3x ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
# s1mox2_1A <- multinom(update(foreignsuff3x ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
# s1mox2_1B <- multinom(update(foreignsuff3x ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
# s1mox2_1C <- multinom(update(foreignsuff3x ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])

sifcct.mlogit <- dfidx(sifcct[which(sifcct$age - sifcct$lvlen>=23),],
                       shape = "wide", choice = "foreignsuff3x")
# # levels(sifcct.mlogit$idx$id2) <- c("Disagree","Neither","Agree")
s1mox2_10 <- mlogit(outmod0.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s1mox2_1A <- mlogit(outmodA.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s1mox2_1B <- mlogit(outmodB.mlogit, data=sifcct.mlogit, reflevel="Disagree")
s1mox2_1C <- mlogit(outmodC.mlogit, data=sifcct.mlogit, reflevel="Disagree")

screenreg(list(s1mox2_10,s1mox2_1A), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mox2_10,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_10))),2],
                             coeftest(s1mox2_10,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_10))),2],
                             coeftest(s1mox2_1A,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1A))),2],
                             coeftest(s1mox2_1A,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1A))),2]),
          override.pvalues = list(coeftest(s1mox2_10,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_10))),4],
                                  coeftest(s1mox2_10,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_10))),4],
                                  coeftest(s1mox2_1A,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1A))),4],
                                  coeftest(s1mox2_1A,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1A))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+", 
          custom.model.names = c("Base: Agree","Base: Neither",
                                 "ZIP: Agree","ZIP: Neither"),
          custom.coef.map = vnmap)
screenreg(list(s1mox2_1B,s1mox2_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mox2_1B,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1B))),2],
                             coeftest(s1mox2_1B,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1B))),2],
                             coeftest(s1mox2_1C,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1C))),2],
                             coeftest(s1mox2_1C,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1C))),2]),
          override.pvalues = list(coeftest(s1mox2_1B,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1B))),4],
                                  coeftest(s1mox2_1B,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1B))),4],
                                  coeftest(s1mox2_1C,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1C))),4],
                                  coeftest(s1mox2_1C,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1C))),4]),
          beside = T,
          omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap,
          custom.model.names = c("Mun.: Agree","Mun.: Neither",
                                 "Full: Agree","Full: Neither"))

#+ include = FALSE
texreg(list(s1mox2_10,s1mox2_1A), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mox2_10,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_10))),2],
                          coeftest(s1mox2_10,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_10))),2],
                          coeftest(s1mox2_1A,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1A))),2],
                          coeftest(s1mox2_1A,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1A))),2]),
       override.pvalues = list(coeftest(s1mox2_10,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_10))),4],
                               coeftest(s1mox2_10,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_10))),4],
                               coeftest(s1mox2_1A,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1A))),4],
                               coeftest(s1mox2_1A,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1A))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger", 
       custom.model.names = c("Base: Agree","Base: Neither",
                              "ZIP: Agree","ZIP: Neither"),
       custom.coef.map = vnmap,
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mox2_1_1_tabular.tex"))
texreg(list(s1mox2_1B,s1mox2_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mox2_1B,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1B))),2],
                          coeftest(s1mox2_1B,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1B))),2],
                          coeftest(s1mox2_1C,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1C))),2],
                          coeftest(s1mox2_1C,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1C))),2]),
       override.pvalues = list(coeftest(s1mox2_1B,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1B))),4],
                               coeftest(s1mox2_1B,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1B))),4],
                               coeftest(s1mox2_1C,vcov=sandwich)[grep(":Neither",names(coef(s1mox2_1C))),4],
                               coeftest(s1mox2_1C,vcov=sandwich)[grep(":Agree",names(coef(s1mox2_1C))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap,
       custom.model.names = c("Mun.: Agree","Mun.: Neither",
                              "Full: Agree","Full: Neither"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mox2_1_2_tabular.tex"))

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

outdt1xm <- rbind(extout("Female",25,2),
                 extout("Female",35,2),
                 extout("Female",45,2),
                 extout("Female",55,2),
                 extout("Female",65,2),
                 extout("Male",25,2),
                 extout("Male",35,2),
                 extout("Male",45,2),
                 extout("Male",55,2),
                 extout("Male",65,2))
outdt1xm <- as.data.frame(outdt1xm)
for(i in 2:9) outdt1xm[,i] <- as.numeric(outdt1xm[,i])
outdt1xm$gender <- factor(outdt1xm$gender, levels=unique(outdt1xm$gender))
summary(outdt1xm)

#'
#' ## Mediator Models
#' 
#' ## Knowledge
#'

s1mmx01_10 <- lm(update(knowledge ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx01_1A <- lm(update(knowledge ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx01_1B <- lm(update(knowledge ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx01_1C <- lm(update(knowledge ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s1mmx01_10,s1mmx01_1A,s1mmx01_1B,s1mmx01_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mmx01_10,vcov.=vcovHC(s1mmx01_10))[,2],
                             coeftest(s1mmx01_1A,vcov.=vcovHC(s1mmx01_1A))[,2],
                             coeftest(s1mmx01_1B,vcov.=vcovHC(s1mmx01_1B))[,2],
                             coeftest(s1mmx01_1C,vcov.=vcovHC(s1mmx01_1C))[,2]),
          override.pvalues = list(coeftest(s1mmx01_10,vcov.=vcovHC(s1mmx01_10))[,4],
                                  coeftest(s1mmx01_1A,vcov.=vcovHC(s1mmx01_1A))[,4],
                                  coeftest(s1mmx01_1B,vcov.=vcovHC(s1mmx01_1B))[,4],
                                  coeftest(s1mmx01_1C,vcov.=vcovHC(s1mmx01_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mmx01_10,s1mmx01_1A,s1mmx01_1B,s1mmx01_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mmx01_10,vcov.=vcovHC(s1mmx01_10))[,2],
                          coeftest(s1mmx01_1A,vcov.=vcovHC(s1mmx01_1A))[,2],
                          coeftest(s1mmx01_1B,vcov.=vcovHC(s1mmx01_1B))[,2],
                          coeftest(s1mmx01_1C,vcov.=vcovHC(s1mmx01_1C))[,2]),
       override.pvalues = list(coeftest(s1mmx01_10,vcov.=vcovHC(s1mmx01_10))[,4],
                               coeftest(s1mmx01_1A,vcov.=vcovHC(s1mmx01_1A))[,4],
                               coeftest(s1mmx01_1B,vcov.=vcovHC(s1mmx01_1B))[,4],
                               coeftest(s1mmx01_1C,vcov.=vcovHC(s1mmx01_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mmx01_1_tabular.tex"))



#'
#' ## Ideology
#'

s1mmx02_10 <- lm(update(ideology ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx02_1A <- lm(update(ideology ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx02_1B <- lm(update(ideology ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx02_1C <- lm(update(ideology ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s1mmx02_10,s1mmx02_1A,s1mmx02_1B,s1mmx02_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mmx02_10,vcov.=vcovHC(s1mmx02_10))[,2],
                             coeftest(s1mmx02_1A,vcov.=vcovHC(s1mmx02_1A))[,2],
                             coeftest(s1mmx02_1B,vcov.=vcovHC(s1mmx02_1B))[,2],
                             coeftest(s1mmx02_1C,vcov.=vcovHC(s1mmx02_1C))[,2]),
          override.pvalues = list(coeftest(s1mmx02_10,vcov.=vcovHC(s1mmx02_10))[,4],
                                  coeftest(s1mmx02_1A,vcov.=vcovHC(s1mmx02_1A))[,4],
                                  coeftest(s1mmx02_1B,vcov.=vcovHC(s1mmx02_1B))[,4],
                                  coeftest(s1mmx02_1C,vcov.=vcovHC(s1mmx02_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mmx02_10,s1mmx02_1A,s1mmx02_1B,s1mmx02_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mmx02_10,vcov.=vcovHC(s1mmx02_10))[,2],
                          coeftest(s1mmx02_1A,vcov.=vcovHC(s1mmx02_1A))[,2],
                          coeftest(s1mmx02_1B,vcov.=vcovHC(s1mmx02_1B))[,2],
                          coeftest(s1mmx02_1C,vcov.=vcovHC(s1mmx02_1C))[,2]),
       override.pvalues = list(coeftest(s1mmx02_10,vcov.=vcovHC(s1mmx02_10))[,4],
                               coeftest(s1mmx02_1A,vcov.=vcovHC(s1mmx02_1A))[,4],
                               coeftest(s1mmx02_1B,vcov.=vcovHC(s1mmx02_1B))[,4],
                               coeftest(s1mmx02_1C,vcov.=vcovHC(s1mmx02_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mmx02_1_tabular.tex"))

#'
#' ## LDP - DPJ FT
#'

s1mmx03_10 <- lm(update(ldpdpjft ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx03_1A <- lm(update(ldpdpjft ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx03_1B <- lm(update(ldpdpjft ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx03_1C <- lm(update(ldpdpjft ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s1mmx03_10,s1mmx03_1A,s1mmx03_1B,s1mmx03_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mmx03_10,vcov.=vcovHC(s1mmx03_10))[,2],
                             coeftest(s1mmx03_1A,vcov.=vcovHC(s1mmx03_1A))[,2],
                             coeftest(s1mmx03_1B,vcov.=vcovHC(s1mmx03_1B))[,2],
                             coeftest(s1mmx03_1C,vcov.=vcovHC(s1mmx03_1C))[,2]),
          override.pvalues = list(coeftest(s1mmx03_10,vcov.=vcovHC(s1mmx03_10))[,4],
                                  coeftest(s1mmx03_1A,vcov.=vcovHC(s1mmx03_1A))[,4],
                                  coeftest(s1mmx03_1B,vcov.=vcovHC(s1mmx03_1B))[,4],
                                  coeftest(s1mmx03_1C,vcov.=vcovHC(s1mmx03_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mmx03_10,s1mmx03_1A,s1mmx03_1B,s1mmx03_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mmx03_10,vcov.=vcovHC(s1mmx03_10))[,2],
                          coeftest(s1mmx03_1A,vcov.=vcovHC(s1mmx03_1A))[,2],
                          coeftest(s1mmx03_1B,vcov.=vcovHC(s1mmx03_1B))[,2],
                          coeftest(s1mmx03_1C,vcov.=vcovHC(s1mmx03_1C))[,2]),
       override.pvalues = list(coeftest(s1mmx03_10,vcov.=vcovHC(s1mmx03_10))[,4],
                               coeftest(s1mmx03_1A,vcov.=vcovHC(s1mmx03_1A))[,4],
                               coeftest(s1mmx03_1B,vcov.=vcovHC(s1mmx03_1B))[,4],
                               coeftest(s1mmx03_1C,vcov.=vcovHC(s1mmx03_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mmx03_1_tabular.tex"))


#'
#' ## Favorability of South Korea
#'

s1mmx04_10 <- lm(update(familiarityFT_KOR ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx04_1A <- lm(update(familiarityFT_KOR ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx04_1B <- lm(update(familiarityFT_KOR ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx04_1C <- lm(update(familiarityFT_KOR ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s1mmx04_10,s1mmx04_1A,s1mmx04_1B,s1mmx04_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mmx04_10,vcov.=vcovHC(s1mmx04_10))[,2],
                             coeftest(s1mmx04_1A,vcov.=vcovHC(s1mmx04_1A))[,2],
                             coeftest(s1mmx04_1B,vcov.=vcovHC(s1mmx04_1B))[,2],
                             coeftest(s1mmx04_1C,vcov.=vcovHC(s1mmx04_1C))[,2]),
          override.pvalues = list(coeftest(s1mmx04_10,vcov.=vcovHC(s1mmx04_10))[,4],
                                  coeftest(s1mmx04_1A,vcov.=vcovHC(s1mmx04_1A))[,4],
                                  coeftest(s1mmx04_1B,vcov.=vcovHC(s1mmx04_1B))[,4],
                                  coeftest(s1mmx04_1C,vcov.=vcovHC(s1mmx04_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mmx04_10,s1mmx04_1A,s1mmx04_1B,s1mmx04_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mmx04_10,vcov.=vcovHC(s1mmx04_10))[,2],
                          coeftest(s1mmx04_1A,vcov.=vcovHC(s1mmx04_1A))[,2],
                          coeftest(s1mmx04_1B,vcov.=vcovHC(s1mmx04_1B))[,2],
                          coeftest(s1mmx04_1C,vcov.=vcovHC(s1mmx04_1C))[,2]),
       override.pvalues = list(coeftest(s1mmx04_10,vcov.=vcovHC(s1mmx04_10))[,4],
                               coeftest(s1mmx04_1A,vcov.=vcovHC(s1mmx04_1A))[,4],
                               coeftest(s1mmx04_1B,vcov.=vcovHC(s1mmx04_1B))[,4],
                               coeftest(s1mmx04_1C,vcov.=vcovHC(s1mmx04_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mmx04_1_tabular.tex"))

#'
#' ## Favorability of China
#'

s1mmx05_10 <- lm(update(familiarityFT_CHN ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx05_1A <- lm(update(familiarityFT_CHN ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx05_1B <- lm(update(familiarityFT_CHN ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx05_1C <- lm(update(familiarityFT_CHN ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s1mmx05_10,s1mmx05_1A,s1mmx05_1B,s1mmx05_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mmx05_10,vcov.=vcovHC(s1mmx05_10))[,2],
                             coeftest(s1mmx05_1A,vcov.=vcovHC(s1mmx05_1A))[,2],
                             coeftest(s1mmx05_1B,vcov.=vcovHC(s1mmx05_1B))[,2],
                             coeftest(s1mmx05_1C,vcov.=vcovHC(s1mmx05_1C))[,2]),
          override.pvalues = list(coeftest(s1mmx05_10,vcov.=vcovHC(s1mmx05_10))[,4],
                                  coeftest(s1mmx05_1A,vcov.=vcovHC(s1mmx05_1A))[,4],
                                  coeftest(s1mmx05_1B,vcov.=vcovHC(s1mmx05_1B))[,4],
                                  coeftest(s1mmx05_1C,vcov.=vcovHC(s1mmx05_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mmx05_10,s1mmx05_1A,s1mmx05_1B,s1mmx05_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mmx05_10,vcov.=vcovHC(s1mmx05_10))[,2],
                          coeftest(s1mmx05_1A,vcov.=vcovHC(s1mmx05_1A))[,2],
                          coeftest(s1mmx05_1B,vcov.=vcovHC(s1mmx05_1B))[,2],
                          coeftest(s1mmx05_1C,vcov.=vcovHC(s1mmx05_1C))[,2]),
       override.pvalues = list(coeftest(s1mmx05_10,vcov.=vcovHC(s1mmx05_10))[,4],
                               coeftest(s1mmx05_1A,vcov.=vcovHC(s1mmx05_1A))[,4],
                               coeftest(s1mmx05_1B,vcov.=vcovHC(s1mmx05_1B))[,4],
                               coeftest(s1mmx05_1C,vcov.=vcovHC(s1mmx05_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mmx05_1_tabular.tex"))

#'
#' ## Favorability of USA
#'

s1mmx06_10 <- lm(update(familiarityFT_USA ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx06_1A <- lm(update(familiarityFT_USA ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx06_1B <- lm(update(familiarityFT_USA ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx06_1C <- lm(update(familiarityFT_USA ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s1mmx06_10,s1mmx06_1A,s1mmx06_1B,s1mmx06_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mmx06_10,vcov.=vcovHC(s1mmx06_10))[,2],
                             coeftest(s1mmx06_1A,vcov.=vcovHC(s1mmx06_1A))[,2],
                             coeftest(s1mmx06_1B,vcov.=vcovHC(s1mmx06_1B))[,2],
                             coeftest(s1mmx06_1C,vcov.=vcovHC(s1mmx06_1C))[,2]),
          override.pvalues = list(coeftest(s1mmx06_10,vcov.=vcovHC(s1mmx06_10))[,4],
                                  coeftest(s1mmx06_1A,vcov.=vcovHC(s1mmx06_1A))[,4],
                                  coeftest(s1mmx06_1B,vcov.=vcovHC(s1mmx06_1B))[,4],
                                  coeftest(s1mmx06_1C,vcov.=vcovHC(s1mmx06_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mmx06_10,s1mmx06_1A,s1mmx06_1B,s1mmx06_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mmx06_10,vcov.=vcovHC(s1mmx06_10))[,2],
                          coeftest(s1mmx06_1A,vcov.=vcovHC(s1mmx06_1A))[,2],
                          coeftest(s1mmx06_1B,vcov.=vcovHC(s1mmx06_1B))[,2],
                          coeftest(s1mmx06_1C,vcov.=vcovHC(s1mmx06_1C))[,2]),
       override.pvalues = list(coeftest(s1mmx06_10,vcov.=vcovHC(s1mmx06_10))[,4],
                               coeftest(s1mmx06_1A,vcov.=vcovHC(s1mmx06_1A))[,4],
                               coeftest(s1mmx06_1B,vcov.=vcovHC(s1mmx06_1B))[,4],
                               coeftest(s1mmx06_1C,vcov.=vcovHC(s1mmx06_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mmx06_1_tabular.tex"))


#' 
#' ## Income
#'

s1mmx07_10 <- lm(update(income ~ ., basemod0), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx07_1A <- lm(update(income ~ ., basemodA), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx07_1B <- lm(update(income ~ ., basemodB), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
s1mmx07_1C <- lm(update(income ~ ., basemodC), data=sifcct[which(sifcct$age - sifcct$lvlen>=23),])
screenreg(list(s1mmx07_10,s1mmx07_1A,s1mmx07_1B,s1mmx07_1C), digits = 4, #single.row = T,
          override.se = list(coeftest(s1mmx07_10,vcov.=vcovHC(s1mmx07_10))[,2],
                             coeftest(s1mmx07_1A,vcov.=vcovHC(s1mmx07_1A))[,2],
                             coeftest(s1mmx07_1B,vcov.=vcovHC(s1mmx07_1B))[,2],
                             coeftest(s1mmx07_1C,vcov.=vcovHC(s1mmx07_1C))[,2]),
          override.pvalues = list(coeftest(s1mmx07_10,vcov.=vcovHC(s1mmx07_10))[,4],
                                  coeftest(s1mmx07_1A,vcov.=vcovHC(s1mmx07_1A))[,4],
                                  coeftest(s1mmx07_1B,vcov.=vcovHC(s1mmx07_1B))[,4],
                                  coeftest(s1mmx07_1C,vcov.=vcovHC(s1mmx07_1C))[,4]),
          omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "+",
          custom.coef.map = vnmap, 
          custom.model.names = c("Base","ZIP","Municipality","Full"))

#+ include = FALSE
texreg(list(s1mmx07_10,s1mmx07_1A,s1mmx07_1B,s1mmx07_1C), digits = 4, #single.row = T,
       override.se = list(coeftest(s1mmx07_10,vcov.=vcovHC(s1mmx07_10))[,2],
                          coeftest(s1mmx07_1A,vcov.=vcovHC(s1mmx07_1A))[,2],
                          coeftest(s1mmx07_1B,vcov.=vcovHC(s1mmx07_1B))[,2],
                          coeftest(s1mmx07_1C,vcov.=vcovHC(s1mmx07_1C))[,2]),
       override.pvalues = list(coeftest(s1mmx07_10,vcov.=vcovHC(s1mmx07_10))[,4],
                               coeftest(s1mmx07_1A,vcov.=vcovHC(s1mmx07_1A))[,4],
                               coeftest(s1mmx07_1B,vcov.=vcovHC(s1mmx07_1B))[,4],
                               coeftest(s1mmx07_1C,vcov.=vcovHC(s1mmx07_1C))[,4]),
       omit.coef = "(wave)", stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap, 
       custom.model.names = c("Base","ZIP","Municipality","Full"),
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, table = FALSE, fontsize = "scriptsize",
       file = paste0(projdir,"/out/s1mmx07_1_tabular.tex"))

#'
#' # Organizing Outcomes
#' 
#' ## OLS
#'

outdt0x$data <- "Unmatched"
outdt1x$data <- "Matched without \nDistance Adj."

visdtx <- rbind(outdt0x,outdt1x)

visdtx$data <- factor(visdtx$data, levels = c("Unmatched",
                                            "Matched without \nDistance Adj."))
visdtx$pstar <- factor(ifelse(visdtx$p>=.1,"n.s.",ifelse(visdtx$p>=.05,"p<.1","p<.05")),
                      levels = c("p<.05","p<.1","n.s."))

#+ eval=FALSE
saveRDS(visdtx, paste0(projdir, "/out/visdtx.rds"))

#'
#' ## Multinomial Logit
#'

outdt0xm$data <- "Unmatched"
outdt1xm$data <- "Matched without \nDistance Adj."

visdtxm <- rbind(outdt0xm,outdt1xm)

visdtxm$data <- factor(visdtxm$data, levels = c("Unmatched",
                                              "Matched without \nDistance Adj."))
visdtxm$pstar <- factor(ifelse(visdtxm$p>=.1,"n.s.",ifelse(visdtxm$p>=.05,"p<.1","p<.05")),
                       levels = c("p<.05","p<.1","n.s."))

#+ eval=FALSE
saveRDS(visdtxm, paste0(projdir, "/out/visdtxm.rds"))

#'
#' ## Combining OLS and Multinomial Logit
#'

visdtx$method = "OLS"
visdtxm$method = "Multinomial Logit\nAgree vs. Disagree"
visdtxall <- rbind(visdtx,visdtxm)
visdtxall$method <- factor(visdtxall$method, levels = unique(visdtxall$method))
colnames(visdtxall)

#'
#' ## Including Mail
#'

visdtx_mail_ols <- readRDS(paste0(projdir, "/out/visdtx_mail_ols.rds"))
visdtx_mail_ols$method <- "OLS"
visdtx_mail_multinom <- readRDS(paste0(projdir, "/out/visdtx_mail_multinom.rds"))
visdtx_mail_multinom$method <- "Multinomial Logit\nAgree vs. Disagree"
visdtx_mail <- rbind(visdtx_mail_ols,visdtx_mail_multinom)
visdtx_mail$lci95 <- NA
visdtx_mail$uci95 <- NA
visdtx_mail$lci90 <- NA
visdtx_mail$uci90 <- NA
colnames(visdtx_mail)
visdtxall <- rbind(visdtxall,visdtx_mail)
visdtxall$data <- factor(visdtxall$data, levels = unique(visdtxall$data))
table(visdtxall$data)

#+ eval=FALSE
saveRDS(visdtxall, paste0(projdir, "/out/visdtxall.rds"))

#'
#' # Save Image
#'

#+ eval=FALSE
save.image(file=paste0(projdir,"/out/heavy/analysis_2x_matched_v5.RData"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/analysis_2x_matched_v5.R', rmarkdown::pdf_document(latex_engine="xelatex", extra_dependencies = list(bookmark=NULL, xltxtra=NULL, zxjatype=NULL, zxjafont=c("ipa"))), encoding = 'UTF-8')
# rmarkdown::render('./src/analysis_2x_matched_v5.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
