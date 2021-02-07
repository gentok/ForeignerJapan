Analysis 2: Main Analysis with Unmatched and Matched Data
================
Fan Lu & Gento Kato
January 26, 2020

# Analytical Strategy

## Variables

  - Outcome: Foreigner Suffrage (min 0, max 1)

  - Mediator 1: (Objective) Political Knowledge (min = 0, max = 1)

  - Mediator 2: Ideology (min 0 = left/liberal, max 1 =
    right/conservative)

  - Mediator 3: LDP - DPJ FT (min 0 = favor DPJ, max 1 = favor LDP)

  - Mediator 4: Favorability of South Korea (min = 0, max = 1)  

  - Mediator 5: Favorability of China (min = 0, max = 1)  

  - Mediator 6: Favorability of USA (min = 0, max = 1)  

  - Mediator 7: Income (percentile, min = 0, max = 1)

  - Independent Variable: University Education (0 = Junior College or
    Less, 1 = University or More)

  - Moderator 1: Gender (0 = Female, 1 = Male), This means that all
    “base” coefficients are for female.

  - Moderator 2: Age (by 10 years, centered at 20). Reasoning: Two
    trends may influence the role of university education. (1) There is
    an evident increase in number of university graduates over the
    years, especially among women. This trend may impies that university
    experience may be more gendered in the past than today. (2) There is
    a trend of “internationalization” in university education in recent
    days. Therefore, the diversifying and liberalizing effect of
    education may be stronger for younger generation.

  - Control 1: Percent in life residing locally. More locally-identified
    individuals may dislike outsiders more.

  - Control 2: (ZIP level) Residing in densely inhabited district (DID)

  - Control 3: (ZIP level) Percent of foreigners in neighborhood
    (transformed by square root)  

  - Control 4: (ZIP level) Percent of university graduates in
    neighborhood (by 10 percent)

  - Control 5: (Municipality level) Percent of residents residing in DID

  - Control 6: (Municipality level) Percent of foreigners (transformed
    by square root)

  - Control 7: (Municipality level) Percent of university graduates (by
    10 percent)

## Subset Data

Analysis is conducted on the following subset.

If age - years of local ZIP residence is 15 or smaller. 15 is the age of
entering high school in Japan. Assuming that an individual is living in
the local ZIP continuously, this condition implies that one spend
significant time before college in the ZIP of current residence. This
filters out the possibility that education changes attitudes through the
movement in residence.

## Modeling Strategy

All models are estimated by OLS. For outcome model, alternative model is
estimated by the multinomial logit model, with 3 category DV (disagree,
neither, agree), with disagree as a reference category.

## Robustness Check (in this file)

SIFCCT has one special survey where they conducted a survey through
mail. Mail survey contains identical set of variables as online survey.
So I replicated the analysis with the mail survey.

# Preparation

``` r
## Clean Up Space
rm(list=ls())

## Set Working Directory (Automatically) ##
require(rstudioapi); require(rprojroot)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
```

    ## Working Directory Set to:
    ##  /home/gentok/GoogleDrive/Projects/Fan-Gento-Lab/ForeignerJapan

``` r
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
```

# With Unmatched Data

``` r
sifcct <- readRDS(datadir0)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.5000  0.5000  0.5676  0.6500  1.0000

``` r
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.04098 0.18484 0.40915 0.48958 0.78565 0.97505

## Outcome Model

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0345 *      -0.0331 *      -0.0325 *      -0.0327 *  
    ##                                      (0.0136)       (0.0137)       (0.0137)       (0.0137)   
    ## Gender (male)                        -0.1089 ***    -0.1094 ***    -0.1096 ***    -0.1097 ***
    ##                                      (0.0108)       (0.0108)       (0.0108)       (0.0108)   
    ## Age (by 10 years, centered at 45)     0.0013         0.0014         0.0014         0.0013    
    ##                                      (0.0057)       (0.0057)       (0.0057)       (0.0057)   
    ## University * Male                     0.0341 *       0.0340 *       0.0343 *       0.0343 *  
    ##                                      (0.0169)       (0.0170)       (0.0170)       (0.0170)   
    ## University * Age                     -0.0149        -0.0150        -0.0151        -0.0149    
    ##                                      (0.0092)       (0.0092)       (0.0092)       (0.0092)   
    ## University * Male * Age               0.0150         0.0151         0.0150         0.0151    
    ##                                      (0.0118)       (0.0118)       (0.0118)       (0.0118)   
    ## Male * Age                            0.0107         0.0106         0.0107         0.0106    
    ##                                      (0.0081)       (0.0081)       (0.0081)       (0.0081)   
    ## % of Life Residing Locally (zip)     -0.0356        -0.0359        -0.0358        -0.0358    
    ##                                      (0.0294)       (0.0295)       (0.0295)       (0.0296)   
    ## DID residence (zip)                                  0.0065                        0.0110    
    ##                                                     (0.0092)                      (0.0113)   
    ## Foreigner % sqrt. (zip)                             -0.0151 *                     -0.0129    
    ##                                                     (0.0066)                      (0.0089)   
    ## University % by 10% (zip)                           -0.0013                        0.0004    
    ##                                                     (0.0051)                      (0.0073)   
    ## DID proportion (mun.)                                              -0.0029        -0.0129    
    ##                                                                    (0.0162)       (0.0198)   
    ## Foreigner % sqrt. (mun.)                                           -0.0150        -0.0031    
    ##                                                                    (0.0093)       (0.0124)   
    ## University % by 10% (mun.)                                         -0.0012        -0.0012    
    ##                                                                    (0.0074)       (0.0103)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0281         0.0288         0.0285         0.0289    
    ## Adj. R^2                              0.0246         0.0249         0.0247         0.0246    
    ## Num. obs.                          7827           7827           7827           7827         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est                 lci95              uci95               lci90         
    ##  Female:5   Min.   :25   Min.   :-0.0625514   Min.   :-0.11568   Min.   :-0.009569   Min.   :-0.10714  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.0289301   1st Qu.:-0.05548   1st Qu.:-0.002376   1st Qu.:-0.05121  
    ##             Median :45   Median :-0.0007147   Median :-0.03598   Median : 0.024121   Median :-0.03027  
    ##             Mean   :45   Mean   :-0.0155266   Mean   :-0.04755   Mean   : 0.016493   Mean   :-0.04240  
    ##             3rd Qu.:55   3rd Qu.: 0.0015798   3rd Qu.:-0.02615   3rd Qu.: 0.031033   3rd Qu.:-0.02168  
    ##             Max.   :65   Max.   : 0.0018743   Max.   :-0.01882   Max.   : 0.037822   Max.   :-0.01553  
    ##      uci90                 se                p                lv           
    ##  Min.   :-0.017963   Min.   :0.01042   Min.   :0.01417   Length:10         
    ##  1st Qu.:-0.006646   1st Qu.:0.01297   1st Qu.:0.05919   Class :character  
    ##  Median : 0.020491   Median :0.01579   Median :0.87686   Mode  :character  
    ##  Mean   : 0.011344   Mean   :0.01633   Mean   :0.56376                     
    ##  3rd Qu.: 0.025778   3rd Qu.:0.01823   3rd Qu.:0.90307                     
    ##  Max.   : 0.032042   Max.   :0.02710   Max.   :0.93969

## Outcome Model 2

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base: Agree     Base: Neither   ZIP: Agree      ZIP: Neither  
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.2366 ***     -0.5074 *       -0.2280 ***     -0.4878 *  
    ##                                       (0.1019)        (0.1026)        (0.1029)        (0.1034)   
    ## Gender (male)                         -0.7822 ***     -0.7924 ***     -0.7867 ***     -0.8027 ***
    ##                                       (0.0815)        (0.0853)        (0.0817)        (0.0855)   
    ## Age (by 10 years, centered at 45)      0.0267 +       -0.0845          0.0274 +       -0.0818    
    ##                                       (0.0447)        (0.0464)        (0.0448)        (0.0464)   
    ## University * Male                      0.3166 *        0.3177 *        0.3170 *        0.3198 *  
    ##                                       (0.1256)        (0.1270)        (0.1258)        (0.1272)   
    ## University * Age                      -0.1114          0.0384         -0.1120          0.0358    
    ##                                       (0.0689)        (0.0701)        (0.0689)        (0.0701)   
    ## University * Male * Age                0.0813          0.0493          0.0821          0.0522    
    ##                                       (0.0877)        (0.0884)        (0.0877)        (0.0884)   
    ## Male * Age                             0.0955         -0.0154          0.0949         -0.0175    
    ##                                       (0.0620)        (0.0634)        (0.0620)        (0.0634)   
    ## % of Life Residing Locally (zip)      -0.1575          0.1758         -0.1588          0.1545    
    ##                                       (0.2161)        (0.2144)        (0.2174)        (0.2153)   
    ## DID residence (zip)                                                    0.0404          0.0117    
    ##                                                                       (0.0679)        (0.0677)   
    ## Foreigner % sqrt. (zip)                                               -0.1095 *       -0.1045 *  
    ##                                                                       (0.0477)        (0.0494)   
    ## University % by 10% (zip)                                             -0.0057         -0.0319    
    ##                                                                       (0.0373)        (0.0370)   
    ## -------------------------------------------------------------------------------------------------
    ## AIC                                16612.6702      16612.6702      16615.5868      16615.5868    
    ## Log Likelihood                     -8248.3351      -8248.3351      -8243.7934      -8243.7934    
    ## Num. obs.                           7827            7827            7827            7827         
    ## K                                      3               3               3               3         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Mun.: Agree     Mun.: Neither   Full: Agree     Full: Neither 
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.2225 ***     -0.4957 *       -0.2250 ***     -0.4884 *  
    ##                                       (0.1027)        (0.1033)        (0.1029)        (0.1036)   
    ## Gender (male)                         -0.7863 ***     -0.8100 ***     -0.7877 ***     -0.8149 ***
    ##                                       (0.0817)        (0.0857)        (0.0819)        (0.0857)   
    ## Age (by 10 years, centered at 45)      0.0273 +       -0.0823          0.0267 +       -0.0816    
    ##                                       (0.0448)        (0.0464)        (0.0448)        (0.0464)   
    ## University * Male                      0.3170 **       0.3288 *        0.3177 **       0.3265 *  
    ##                                       (0.1257)        (0.1272)        (0.1258)        (0.1273)   
    ## University * Age                      -0.1124          0.0360         -0.1117          0.0359    
    ##                                       (0.0689)        (0.0701)        (0.0689)        (0.0701)   
    ## University * Male * Age                0.0807          0.0515          0.0818          0.0541    
    ##                                       (0.0877)        (0.0884)        (0.0878)        (0.0884)   
    ## Male * Age                             0.0962         -0.0180          0.0953         -0.0205    
    ##                                       (0.0621)        (0.0634)        (0.0622)        (0.0634)   
    ## % of Life Residing Locally (zip)      -0.1593          0.1667         -0.1588          0.1554    
    ##                                       (0.2175)        (0.2150)        (0.2178)        (0.2153)   
    ## DID residence (zip)                                                    0.0576 +        0.1353    
    ##                                                                       (0.0821)        (0.0823)   
    ## Foreigner % sqrt. (zip)                                               -0.0909 *       -0.1365    
    ##                                                                       (0.0665)        (0.0678)   
    ## University % by 10% (zip)                                              0.0115         -0.0661    
    ##                                                                       (0.0530)        (0.0525)   
    ## DID proportion (mun.)                  0.0063 *       -0.2650         -0.0445 **      -0.3924    
    ##                                       (0.1195)        (0.1198)        (0.1434)        (0.1455)   
    ## Foreigner % sqrt. (mun.)              -0.1130         -0.0532 +       -0.0283          0.0716    
    ##                                       (0.0671)        (0.0677)        (0.0917)        (0.0929)   
    ## University % by 10% (mun.)            -0.0143          0.0418         -0.0233          0.1103    
    ##                                       (0.0554)        (0.0540)        (0.0759)        (0.0746)   
    ## -------------------------------------------------------------------------------------------------
    ## AIC                                16614.2088      16614.2088      16618.2864      16618.2864    
    ## Log Likelihood                     -8243.1044      -8243.1044      -8239.1432      -8239.1432    
    ## Num. obs.                           7827            7827            7827            7827         
    ## K                                      3               3               3               3         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est               lci95              uci95               lci90         
    ##  Female:5   Min.   :25   Min.   :-0.44832   Min.   :-0.84560   Min.   :-0.052650   Min.   :-0.78171  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.19705   1st Qu.:-0.39904   1st Qu.: 0.004953   1st Qu.:-0.36656  
    ##             Median :45   Median : 0.01565   Median :-0.24651   Median : 0.241227   Median :-0.20435  
    ##             Mean   :45   Mean   :-0.06611   Mean   :-0.30558   Mean   : 0.173355   Mean   :-0.26707  
    ##             3rd Qu.:55   3rd Qu.: 0.08526   3rd Qu.:-0.11455   3rd Qu.: 0.280028   3rd Qu.:-0.07653  
    ##             Max.   :65   Max.   : 0.15257   Max.   :-0.05757   Max.   : 0.422067   Max.   :-0.03340  
    ##      uci90                se                p                lv           
    ##  Min.   :-0.11494   Min.   :0.07668   Min.   :0.02017   Length:10         
    ##  1st Qu.:-0.02753   1st Qu.:0.09837   1st Qu.:0.07434   Class :character  
    ##  Median : 0.21494   Median :0.11539   Median :0.24534   Mode  :character  
    ##  Mean   : 0.13485   Mean   :0.12216   Mean   :0.33148                     
    ##  3rd Qu.: 0.23890   3rd Qu.:0.13957   3rd Qu.:0.43096                     
    ##  Max.   : 0.37873   Max.   :0.20266   Max.   :0.99087

## Mediator Models

## Knowledge

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1553 ***     0.1483 ***     0.1510 ***     0.1486 ***
    ##                                      (0.0125)       (0.0126)       (0.0126)       (0.0126)   
    ## Gender (male)                         0.1842 ***     0.1857 ***     0.1859 ***     0.1867 ***
    ##                                      (0.0100)       (0.0100)       (0.0100)       (0.0100)   
    ## Age (by 10 years, centered at 45)     0.0542 ***     0.0536 ***     0.0540 ***     0.0537 ***
    ##                                      (0.0053)       (0.0053)       (0.0053)       (0.0053)   
    ## University * Male                    -0.0287 +      -0.0278 +      -0.0293 +      -0.0285 +  
    ##                                      (0.0152)       (0.0152)       (0.0152)       (0.0152)   
    ## University * Age                     -0.0158 +      -0.0151 +      -0.0153 +      -0.0151 +  
    ##                                      (0.0083)       (0.0083)       (0.0083)       (0.0083)   
    ## University * Male * Age               0.0054         0.0048         0.0046         0.0044    
    ##                                      (0.0104)       (0.0104)       (0.0104)       (0.0104)   
    ## Male * Age                            0.0020         0.0025         0.0025         0.0028    
    ##                                      (0.0074)       (0.0074)       (0.0074)       (0.0074)   
    ## % of Life Residing Locally (zip)     -0.1088 ***    -0.0984 ***    -0.0987 ***    -0.0961 ***
    ##                                      (0.0257)       (0.0257)       (0.0257)       (0.0257)   
    ## DID residence (zip)                                 -0.0117                       -0.0206 *  
    ##                                                     (0.0079)                      (0.0096)   
    ## Foreigner % sqrt. (zip)                             -0.0016                        0.0083    
    ##                                                     (0.0057)                      (0.0077)   
    ## University % by 10% (zip)                            0.0205 ***                    0.0178 ** 
    ##                                                     (0.0043)                      (0.0061)   
    ## DID proportion (mun.)                                               0.0052         0.0256    
    ##                                                                    (0.0137)       (0.0167)   
    ## Foreigner % sqrt. (mun.)                                           -0.0157 +      -0.0228 *  
    ##                                                                    (0.0081)       (0.0107)   
    ## University % by 10% (mun.)                                          0.0209 ***     0.0032    
    ##                                                                    (0.0062)       (0.0084)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.1892         0.1916         0.1912         0.1924    
    ## Adj. R^2                              0.1863         0.1884         0.1880         0.1888    
    ## Num. obs.                          7827           7827           7827           7827         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Ideology

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0120        -0.0130        -0.0127        -0.0126    
    ##                                      (0.0083)       (0.0083)       (0.0083)       (0.0083)   
    ## Gender (male)                        -0.0254 ***    -0.0251 ***    -0.0262 ***    -0.0260 ***
    ##                                      (0.0070)       (0.0070)       (0.0070)       (0.0070)   
    ## Age (by 10 years, centered at 45)    -0.0052        -0.0053        -0.0051        -0.0053    
    ##                                      (0.0034)       (0.0034)       (0.0034)       (0.0034)   
    ## University * Male                     0.0147         0.0148         0.0154         0.0152    
    ##                                      (0.0107)       (0.0107)       (0.0107)       (0.0107)   
    ## University * Age                     -0.0046        -0.0044        -0.0046        -0.0044    
    ##                                      (0.0055)       (0.0055)       (0.0055)       (0.0055)   
    ## University * Male * Age               0.0104         0.0103         0.0104         0.0102    
    ##                                      (0.0074)       (0.0074)       (0.0074)       (0.0074)   
    ## Male * Age                           -0.0003        -0.0002        -0.0004        -0.0003    
    ##                                      (0.0051)       (0.0051)       (0.0051)       (0.0051)   
    ## % of Life Residing Locally (zip)      0.0190         0.0211         0.0215         0.0223    
    ##                                      (0.0183)       (0.0183)       (0.0183)       (0.0184)   
    ## DID residence (zip)                                  0.0014                        0.0112    
    ##                                                     (0.0060)                      (0.0070)   
    ## Foreigner % sqrt. (zip)                             -0.0040                       -0.0008    
    ##                                                     (0.0042)                      (0.0057)   
    ## University % by 10% (zip)                            0.0033                        0.0004    
    ##                                                     (0.0033)                      (0.0045)   
    ## DID proportion (mun.)                                              -0.0207 +      -0.0316 *  
    ##                                                                    (0.0107)       (0.0125)   
    ## Foreigner % sqrt. (mun.)                                           -0.0067        -0.0062    
    ##                                                                    (0.0060)       (0.0081)   
    ## University % by 10% (mun.)                                          0.0104 *       0.0100    
    ##                                                                    (0.0048)       (0.0064)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0054         0.0057         0.0063         0.0066    
    ## Adj. R^2                              0.0018         0.0017         0.0023         0.0023    
    ## Num. obs.                          7827           7827           7827           7827         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## LDP - DPJ FT

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0038        -0.0045        -0.0035        -0.0042    
    ##                                      (0.0068)       (0.0069)       (0.0069)       (0.0069)   
    ## Gender (male)                         0.0220 ***     0.0222 ***     0.0216 ***     0.0220 ***
    ##                                      (0.0053)       (0.0054)       (0.0054)       (0.0054)   
    ## Age (by 10 years, centered at 45)    -0.0020        -0.0020        -0.0019        -0.0022    
    ##                                      (0.0028)       (0.0028)       (0.0028)       (0.0028)   
    ## University * Male                     0.0038         0.0039         0.0041         0.0041    
    ##                                      (0.0085)       (0.0086)       (0.0086)       (0.0086)   
    ## University * Age                     -0.0057        -0.0057        -0.0058        -0.0057    
    ##                                      (0.0046)       (0.0045)       (0.0046)       (0.0046)   
    ## University * Male * Age               0.0062         0.0062         0.0064         0.0062    
    ##                                      (0.0059)       (0.0059)       (0.0059)       (0.0059)   
    ## Male * Age                           -0.0135 ***    -0.0135 ***    -0.0137 ***    -0.0134 ***
    ##                                      (0.0041)       (0.0041)       (0.0041)       (0.0041)   
    ## % of Life Residing Locally (zip)      0.0194         0.0199         0.0178         0.0192    
    ##                                      (0.0142)       (0.0142)       (0.0142)       (0.0142)   
    ## DID residence (zip)                                 -0.0024                        0.0002    
    ##                                                     (0.0046)                      (0.0056)   
    ## Foreigner % sqrt. (zip)                              0.0042                        0.0043    
    ##                                                     (0.0033)                      (0.0044)   
    ## University % by 10% (zip)                            0.0012                        0.0058 +  
    ##                                                     (0.0025)                      (0.0035)   
    ## DID proportion (mun.)                                              -0.0055        -0.0057    
    ##                                                                    (0.0081)       (0.0098)   
    ## Foreigner % sqrt. (mun.)                                            0.0059         0.0020    
    ##                                                                    (0.0047)       (0.0062)   
    ## University % by 10% (mun.)                                         -0.0019        -0.0078    
    ##                                                                    (0.0038)       (0.0051)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0989         0.0991         0.0992         0.0996    
    ## Adj. R^2                              0.0956         0.0955         0.0956         0.0957    
    ## Num. obs.                          7827           7827           7827           7827         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of South Korea

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0088        -0.0106        -0.0107        -0.0113    
    ##                                      (0.0104)       (0.0105)       (0.0105)       (0.0105)   
    ## Gender (male)                        -0.0580 ***    -0.0581 ***    -0.0578 ***    -0.0580 ***
    ##                                      (0.0083)       (0.0083)       (0.0083)       (0.0084)   
    ## Age (by 10 years, centered at 45)     0.0094 *       0.0094 *       0.0093 *       0.0096 *  
    ##                                      (0.0046)       (0.0046)       (0.0046)       (0.0046)   
    ## University * Male                     0.0153         0.0159         0.0155         0.0159    
    ##                                      (0.0127)       (0.0127)       (0.0127)       (0.0127)   
    ## University * Age                     -0.0141 +      -0.0141 *      -0.0140 +      -0.0142 *  
    ##                                      (0.0072)       (0.0072)       (0.0072)       (0.0072)   
    ## University * Male * Age               0.0073         0.0073         0.0073         0.0075    
    ##                                      (0.0090)       (0.0090)       (0.0090)       (0.0090)   
    ## Male * Age                            0.0190 **      0.0190 **      0.0189 **      0.0188 ** 
    ##                                      (0.0063)       (0.0063)       (0.0063)       (0.0063)   
    ## % of Life Residing Locally (zip)     -0.0103        -0.0087        -0.0088        -0.0096    
    ##                                      (0.0227)       (0.0227)       (0.0227)       (0.0227)   
    ## DID residence (zip)                                 -0.0109                       -0.0127    
    ##                                                     (0.0068)                      (0.0082)   
    ## Foreigner % sqrt. (zip)                              0.0030                       -0.0037    
    ##                                                     (0.0047)                      (0.0065)   
    ## University % by 10% (zip)                            0.0055                        0.0023    
    ##                                                     (0.0038)                      (0.0053)   
    ## DID proportion (mun.)                                              -0.0084         0.0044    
    ##                                                                    (0.0118)       (0.0143)   
    ## Foreigner % sqrt. (mun.)                                            0.0086         0.0124    
    ##                                                                    (0.0068)       (0.0092)   
    ## University % by 10% (mun.)                                          0.0063         0.0042    
    ##                                                                    (0.0056)       (0.0076)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0740         0.0744         0.0744         0.0747    
    ## Adj. R^2                              0.0706         0.0707         0.0707         0.0707    
    ## Num. obs.                          7827           7827           7827           7827         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of China

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0055       -0.0052       -0.0053       -0.0053   
    ##                                      (0.0088)      (0.0089)      (0.0089)      (0.0089)  
    ## Gender (male)                        -0.0185 **    -0.0192 **    -0.0195 **    -0.0197 **
    ##                                      (0.0072)      (0.0072)      (0.0072)      (0.0072)  
    ## Age (by 10 years, centered at 45)    -0.0051       -0.0049       -0.0050       -0.0049   
    ##                                      (0.0041)      (0.0041)      (0.0041)      (0.0041)  
    ## University * Male                     0.0131        0.0136        0.0138        0.0139   
    ##                                      (0.0108)      (0.0108)      (0.0108)      (0.0108)  
    ## University * Age                     -0.0122 *     -0.0124 *     -0.0124 *     -0.0124 * 
    ##                                      (0.0062)      (0.0062)      (0.0062)      (0.0062)  
    ## University * Male * Age               0.0043        0.0045        0.0045        0.0046   
    ##                                      (0.0078)      (0.0078)      (0.0078)      (0.0078)  
    ## Male * Age                            0.0071        0.0070        0.0069        0.0068   
    ##                                      (0.0056)      (0.0056)      (0.0056)      (0.0056)  
    ## % of Life Residing Locally (zip)     -0.0446 *     -0.0454 *     -0.0449 *     -0.0456 * 
    ##                                      (0.0195)      (0.0195)      (0.0195)      (0.0195)  
    ## DID residence (zip)                                -0.0060                     -0.0006   
    ##                                                    (0.0058)                    (0.0069)  
    ## Foreigner % sqrt. (zip)                            -0.0044                     -0.0072   
    ##                                                    (0.0041)                    (0.0056)  
    ## University % by 10% (zip)                           0.0008                     -0.0009   
    ##                                                    (0.0032)                    (0.0045)  
    ## DID proportion (mun.)                                            -0.0182 +     -0.0173   
    ##                                                                  (0.0102)      (0.0122)  
    ## Foreigner % sqrt. (mun.)                                         -0.0008        0.0060   
    ##                                                                  (0.0057)      (0.0077)  
    ## University % by 10% (mun.)                                        0.0039        0.0050   
    ##                                                                  (0.0047)      (0.0065)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0332        0.0336        0.0337        0.0339   
    ## Adj. R^2                              0.0298        0.0297        0.0298        0.0297   
    ## Num. obs.                          7827          7827          7827          7827        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of USA

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0085        -0.0102        -0.0108        -0.0111    
    ##                                      (0.0090)       (0.0091)       (0.0091)       (0.0091)   
    ## Gender (male)                         0.0263 ***     0.0268 ***     0.0274 ***     0.0271 ***
    ##                                      (0.0073)       (0.0073)       (0.0073)       (0.0073)   
    ## Age (by 10 years, centered at 45)     0.0066 +       0.0064         0.0065 +       0.0067 +  
    ##                                      (0.0039)       (0.0039)       (0.0039)       (0.0039)   
    ## University * Male                     0.0211 +       0.0212 +       0.0207 +       0.0210 +  
    ##                                      (0.0112)       (0.0112)       (0.0112)       (0.0112)   
    ## University * Age                     -0.0136 *      -0.0133 *      -0.0133 *      -0.0134 *  
    ##                                      (0.0061)       (0.0061)       (0.0061)       (0.0061)   
    ## University * Male * Age               0.0134 +       0.0133 +       0.0132 +       0.0135 +  
    ##                                      (0.0078)       (0.0078)       (0.0078)       (0.0078)   
    ## Male * Age                            0.0043         0.0044         0.0044         0.0042    
    ##                                      (0.0055)       (0.0055)       (0.0055)       (0.0055)   
    ## % of Life Residing Locally (zip)     -0.0302        -0.0271        -0.0268        -0.0277    
    ##                                      (0.0192)       (0.0193)       (0.0193)       (0.0194)   
    ## DID residence (zip)                                 -0.0002                       -0.0048    
    ##                                                     (0.0060)                      (0.0071)   
    ## Foreigner % sqrt. (zip)                             -0.0032                       -0.0100 +  
    ##                                                     (0.0042)                      (0.0058)   
    ## University % by 10% (zip)                            0.0054                        0.0005    
    ##                                                     (0.0034)                      (0.0048)   
    ## DID proportion (mun.)                                               0.0068         0.0121    
    ##                                                                    (0.0107)       (0.0127)   
    ## Foreigner % sqrt. (mun.)                                            0.0018         0.0113    
    ##                                                                    (0.0060)       (0.0080)   
    ## University % by 10% (mun.)                                          0.0065         0.0063    
    ##                                                                    (0.0050)       (0.0068)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0230         0.0235         0.0238         0.0243    
    ## Adj. R^2                              0.0195         0.0196         0.0199         0.0200    
    ## Num. obs.                          7827           7827           7827           7827         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Income

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1449 ***     0.1313 ***     0.1328 ***     0.1294 ***
    ##                                      (0.0116)       (0.0117)       (0.0116)       (0.0117)   
    ## Gender (male)                         0.0169 +       0.0211 *       0.0197 *       0.0206 *  
    ##                                      (0.0090)       (0.0090)       (0.0090)       (0.0090)   
    ## Age (by 10 years, centered at 45)     0.0092 +       0.0077         0.0088 +       0.0082    
    ##                                      (0.0050)       (0.0050)       (0.0050)       (0.0050)   
    ## University * Male                    -0.0295 *      -0.0289 *      -0.0296 *      -0.0287 *  
    ##                                      (0.0143)       (0.0142)       (0.0142)       (0.0142)   
    ## University * Age                     -0.0014         0.0002        -0.0004         0.0000    
    ##                                      (0.0081)       (0.0081)       (0.0081)       (0.0081)   
    ## University * Male * Age               0.0153         0.0139         0.0147         0.0145    
    ##                                      (0.0102)       (0.0102)       (0.0101)       (0.0102)   
    ## Male * Age                           -0.0088        -0.0077        -0.0087        -0.0084    
    ##                                      (0.0069)       (0.0069)       (0.0069)       (0.0069)   
    ## % of Life Residing Locally (zip)     -0.0650 **     -0.0450 +      -0.0508 *      -0.0470 +  
    ##                                      (0.0250)       (0.0249)       (0.0250)       (0.0250)   
    ## DID residence (zip)                                 -0.0102                       -0.0087    
    ##                                                     (0.0075)                      (0.0091)   
    ## Foreigner % sqrt. (zip)                              0.0107 *                     -0.0076    
    ##                                                     (0.0054)                      (0.0070)   
    ## University % by 10% (zip)                            0.0348 ***                    0.0248 ***
    ##                                                     (0.0042)                      (0.0061)   
    ## DID proportion (mun.)                                              -0.0187        -0.0088    
    ##                                                                    (0.0133)       (0.0159)   
    ## Foreigner % sqrt. (mun.)                                            0.0265 ***     0.0343 ***
    ##                                                                    (0.0075)       (0.0098)   
    ## University % by 10% (mun.)                                          0.0407 ***     0.0166 +  
    ##                                                                    (0.0062)       (0.0087)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0562         0.0664         0.0662         0.0685    
    ## Adj. R^2                              0.0528         0.0627         0.0625         0.0644    
    ## Num. obs.                          7827           7827           7827           7827         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

# With Matched Data (Without Distance Adjustment)

``` r
sifcct <- readRDS(datadir1)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.5000  0.5000  0.5689  0.6500  1.0000

``` r
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.04098 0.18484 0.40915 0.48308 0.78565 0.97505

## Outcome Model

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0244        -0.0244        -0.0244        -0.0246    
    ##                                      (0.0173)       (0.0173)       (0.0173)       (0.0174)   
    ## Gender (male)                        -0.1009 ***    -0.1011 ***    -0.1014 ***    -0.1014 ***
    ##                                      (0.0146)       (0.0148)       (0.0148)       (0.0148)   
    ## Age (by 10 years, centered at 45)     0.0011         0.0010         0.0012         0.0011    
    ##                                      (0.0086)       (0.0086)       (0.0086)       (0.0086)   
    ## University * Male                     0.0255         0.0256         0.0256         0.0257    
    ##                                      (0.0215)       (0.0215)       (0.0215)       (0.0215)   
    ## University * Age                     -0.0079        -0.0080        -0.0080        -0.0081    
    ##                                      (0.0123)       (0.0123)       (0.0123)       (0.0123)   
    ## University * Male * Age               0.0071         0.0071         0.0072         0.0073    
    ##                                      (0.0155)       (0.0155)       (0.0155)       (0.0155)   
    ## Male * Age                            0.0104         0.0105         0.0102         0.0103    
    ##                                      (0.0107)       (0.0107)       (0.0107)       (0.0107)   
    ## % of Life Residing Locally (zip)      0.0388         0.0399         0.0376         0.0373    
    ##                                      (0.0399)       (0.0400)       (0.0401)       (0.0401)   
    ## DID residence (zip)                                 -0.0018                        0.0037    
    ##                                                     (0.0121)                      (0.0153)   
    ## Foreigner % sqrt. (zip)                             -0.0076                       -0.0176    
    ##                                                     (0.0097)                      (0.0139)   
    ## University % by 10% (zip)                            0.0031                        0.0054    
    ##                                                     (0.0076)                      (0.0108)   
    ## DID proportion (mun.)                                              -0.0112        -0.0139    
    ##                                                                    (0.0213)       (0.0269)   
    ## Foreigner % sqrt. (mun.)                                            0.0060         0.0223    
    ##                                                                    (0.0133)       (0.0183)   
    ## University % by 10% (mun.)                                          0.0003        -0.0040    
    ##                                                                    (0.0106)       (0.0147)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0233         0.0234         0.0234         0.0239    
    ## Adj. R^2                              0.0173         0.0168         0.0168         0.0166    
    ## Num. obs.                          4614           4614           4614           4614         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est                 lci95              uci95              lci90         
    ##  Female:5   Min.   :25   Min.   :-0.0408169   Min.   :-0.11169   Min.   :0.009489   Min.   :-0.10029  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.0225509   1st Qu.:-0.05718   1st Qu.:0.019541   1st Qu.:-0.05129  
    ##             Median :45   Median :-0.0043609   Median :-0.04639   Median :0.030728   Median :-0.04028  
    ##             Mean   :45   Mean   :-0.0117065   Mean   :-0.05239   Mean   :0.028973   Mean   :-0.04584  
    ##             3rd Qu.:55   3rd Qu.: 0.0009743   3rd Qu.:-0.03342   3rd Qu.:0.035295   3rd Qu.:-0.02789  
    ##             Max.   :65   Max.   : 0.0027126   Max.   :-0.02371   Max.   :0.047264   Max.   :-0.01971  
    ##      uci90               se                p               lv           
    ##  Min.   :0.00401   Min.   :0.01269   Min.   :0.1573   Length:10         
    ##  1st Qu.:0.01205   1st Qu.:0.01586   1st Qu.:0.2700   Class :character  
    ##  Median :0.02423   Median :0.02002   Median :0.8075   Mode  :character  
    ##  Mean   :0.02243   Mean   :0.02075   Mean   :0.6334                     
    ##  3rd Qu.:0.02869   3rd Qu.:0.02274   3rd Qu.:0.9213                     
    ##  Max.   :0.04010   Max.   :0.03615   Max.   :0.9868

## Outcome Model 2

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base: Agree     Base: Neither   ZIP: Agree      ZIP: Neither  
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.1724 ***     -0.5601         -0.1723 ***     -0.5631    
    ##                                       (0.1325)        (0.1357)        (0.1325)        (0.1358)   
    ## Gender (male)                         -0.7240 ***     -0.8545 ***     -0.7233 ***     -0.8804 ***
    ##                                       (0.1133)        (0.1196)        (0.1145)        (0.1207)   
    ## Age (by 10 years, centered at 45)      0.0481         -0.0174          0.0472         -0.0094    
    ##                                       (0.0664)        (0.0712)        (0.0668)        (0.0714)   
    ## University * Male                      0.2811 *        0.3444 +        0.2809 *        0.3476 +  
    ##                                       (0.1612)        (0.1637)        (0.1613)        (0.1638)   
    ## University * Age                      -0.0851          0.0408         -0.0850          0.0378    
    ##                                       (0.0922)        (0.0959)        (0.0922)        (0.0960)   
    ## University * Male * Age                0.0497          0.0905          0.0495          0.0955    
    ##                                       (0.1151)        (0.1174)        (0.1151)        (0.1175)   
    ## Male * Age                             0.0647         -0.0785          0.0654         -0.0853    
    ##                                       (0.0817)        (0.0855)        (0.0820)        (0.0858)   
    ## % of Life Residing Locally (zip)       0.3168 *        0.7419          0.3238 *        0.7359    
    ##                                       (0.2992)        (0.2962)        (0.3000)        (0.2965)   
    ## DID residence (zip)                                                    0.0224         -0.0388    
    ##                                                                       (0.0887)        (0.0886)   
    ## Foreigner % sqrt. (zip)                                               -0.0319 +       -0.1246    
    ##                                                                       (0.0689)        (0.0697)   
    ## University % by 10% (zip)                                              0.0086         -0.0194    
    ##                                                                       (0.0564)        (0.0555)   
    ## -------------------------------------------------------------------------------------------------
    ## AIC                                 9829.3582       9829.3582       9835.4466       9835.4466    
    ## Log Likelihood                     -4856.6791      -4856.6791      -4853.7233      -4853.7233    
    ## Num. obs.                           4614            4614            4614            4614         
    ## K                                      3               3               3               3         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Mun.: Agree     Mun.: Neither   Full: Agree     Full: Neither 
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.1726 ***     -0.5611         -0.1731 ***     -0.5640    
    ##                                       (0.1324)        (0.1357)        (0.1327)        (0.1359)   
    ## Gender (male)                         -0.7258 ***     -0.8742 ***     -0.7250 ***     -0.8874 ***
    ##                                       (0.1140)        (0.1207)        (0.1147)        (0.1210)   
    ## Age (by 10 years, centered at 45)      0.0489         -0.0130          0.0477         -0.0085    
    ##                                       (0.0666)        (0.0714)        (0.0669)        (0.0714)   
    ## University * Male                      0.2813 *        0.3462 +        0.2816 *        0.3487 +  
    ##                                       (0.1611)        (0.1637)        (0.1614)        (0.1639)   
    ## University * Age                      -0.0853          0.0391         -0.0859          0.0364    
    ##                                       (0.0921)        (0.0960)        (0.0922)        (0.0960)   
    ## University * Male * Age                0.0503          0.0929          0.0513          0.0975    
    ##                                       (0.1151)        (0.1175)        (0.1152)        (0.1175)   
    ## Male * Age                             0.0636         -0.0830          0.0635         -0.0877    
    ##                                       (0.0819)        (0.0858)        (0.0822)        (0.0858)   
    ## % of Life Residing Locally (zip)       0.3062 *        0.7404          0.3071 *        0.7308    
    ##                                       (0.3006)        (0.2970)        (0.3006)        (0.2970)   
    ## DID residence (zip)                                                    0.0424          0.0972    
    ##                                                                       (0.1095)        (0.1101)   
    ## Foreigner % sqrt. (zip)                                               -0.0746 *       -0.2302    
    ##                                                                       (0.0987)        (0.0995)   
    ## University % by 10% (zip)                                              0.0352         -0.0685    
    ##                                                                       (0.0787)        (0.0786)   
    ## DID proportion (mun.)                 -0.0010 *       -0.3243         -0.0408 *       -0.4089    
    ##                                       (0.1578)        (0.1581)        (0.1931)        (0.1962)   
    ## Foreigner % sqrt. (mun.)               0.0326          0.0049          0.1024          0.2154    
    ##                                       (0.0967)        (0.0971)        (0.1335)        (0.1347)   
    ## University % by 10% (mun.)            -0.0199          0.0559         -0.0491          0.1286    
    ##                                       (0.0799)        (0.0783)        (0.1070)        (0.1078)   
    ## -------------------------------------------------------------------------------------------------
    ## AIC                                 9835.3615       9835.3615       9839.8440       9839.8440    
    ## Log Likelihood                     -4853.6807      -4853.6807      -4849.9220      -4849.9220    
    ## Num. obs.                           4614            4614            4614            4614         
    ## K                                      3               3               3               3         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est               lci95             uci95             lci90              uci90        
    ##  Female:5   Min.   :25   Min.   :-0.34500   Min.   :-0.8983   Min.   :0.09325   Min.   :-0.80930   Min.   :0.05041  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.15165   1st Qu.:-0.4163   1st Qu.:0.16988   1st Qu.:-0.37055   1st Qu.:0.11817  
    ##             Median :45   Median : 0.01901   Median :-0.3005   Median :0.28825   Median :-0.25544   Median :0.25656  
    ##             Mean   :45   Mean   :-0.03232   Mean   :-0.3389   Mean   :0.27422   Mean   :-0.28956   Mean   :0.22492  
    ##             3rd Qu.:55   3rd Qu.: 0.09984   3rd Qu.:-0.1441   3rd Qu.:0.34705   3rd Qu.:-0.10170   3rd Qu.:0.29592  
    ##             Max.   :65   Max.   : 0.17770   Max.   :-0.0708   Max.   :0.50878   Max.   :-0.04197   Max.   :0.45553  
    ##        se                p               lv           
    ##  Min.   :0.09145   Min.   :0.1944   Length:10         
    ##  1st Qu.:0.11947   1st Qu.:0.2223   Class :character  
    ##  Median :0.14669   Median :0.2641   Mode  :character  
    ##  Mean   :0.15636   Mean   :0.4153                     
    ##  3rd Qu.:0.17432   3rd Qu.:0.4962                     
    ##  Max.   :0.28222   Max.   :0.9943

## Mediator Models

## Knowledge

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1490 ***     0.1492 ***     0.1494 ***     0.1494 ***
    ##                                      (0.0164)       (0.0164)       (0.0164)       (0.0164)   
    ## Gender (male)                         0.1902 ***     0.1952 ***     0.1947 ***     0.1960 ***
    ##                                      (0.0140)       (0.0141)       (0.0141)       (0.0141)   
    ## Age (by 10 years, centered at 45)     0.0498 ***     0.0477 ***     0.0482 ***     0.0476 ***
    ##                                      (0.0080)       (0.0081)       (0.0081)       (0.0081)   
    ## University * Male                    -0.0377 +      -0.0380 +      -0.0383 *      -0.0383 +  
    ##                                      (0.0195)       (0.0195)       (0.0195)       (0.0195)   
    ## University * Age                     -0.0069        -0.0064        -0.0062        -0.0062    
    ##                                      (0.0112)       (0.0112)       (0.0113)       (0.0113)   
    ## University * Male * Age              -0.0071        -0.0080        -0.0083        -0.0084    
    ##                                      (0.0137)       (0.0137)       (0.0137)       (0.0137)   
    ## Male * Age                            0.0104         0.0123         0.0122         0.0127    
    ##                                      (0.0099)       (0.0099)       (0.0099)       (0.0099)   
    ## % of Life Residing Locally (zip)     -0.0913 *      -0.0852 *      -0.0842 *      -0.0833 *  
    ##                                      (0.0355)       (0.0355)       (0.0355)       (0.0356)   
    ## DID residence (zip)                                  0.0068                       -0.0024    
    ##                                                     (0.0104)                      (0.0129)   
    ## Foreigner % sqrt. (zip)                             -0.0036                        0.0043    
    ##                                                     (0.0085)                      (0.0117)   
    ## University % by 10% (zip)                            0.0155 *                      0.0125    
    ##                                                     (0.0066)                      (0.0090)   
    ## DID proportion (mun.)                                               0.0227         0.0248    
    ##                                                                    (0.0183)       (0.0226)   
    ## Foreigner % sqrt. (mun.)                                           -0.0149        -0.0187    
    ##                                                                    (0.0116)       (0.0154)   
    ## University % by 10% (mun.)                                          0.0147         0.0031    
    ##                                                                    (0.0091)       (0.0120)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.1853         0.1869         0.1871         0.1875    
    ## Adj. R^2                              0.1803         0.1814         0.1816         0.1814    
    ## Num. obs.                          4614           4614           4614           4614         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Ideology

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0228 *     -0.0228 *     -0.0227 *     -0.0226 * 
    ##                                      (0.0103)      (0.0104)      (0.0103)      (0.0104)  
    ## Gender (male)                        -0.0282 **    -0.0273 **    -0.0277 **    -0.0281 **
    ##                                      (0.0091)      (0.0093)      (0.0092)      (0.0093)  
    ## Age (by 10 years, centered at 45)    -0.0039       -0.0042       -0.0043       -0.0041   
    ##                                      (0.0049)      (0.0050)      (0.0050)      (0.0050)  
    ## University * Male                     0.0315 *      0.0314 *      0.0313 *      0.0313 * 
    ##                                      (0.0134)      (0.0134)      (0.0134)      (0.0134)  
    ## University * Age                     -0.0005       -0.0004       -0.0004       -0.0005   
    ##                                      (0.0073)      (0.0073)      (0.0073)      (0.0073)  
    ## University * Male * Age               0.0069        0.0068        0.0065        0.0067   
    ##                                      (0.0097)      (0.0097)      (0.0097)      (0.0097)  
    ## Male * Age                           -0.0039       -0.0036       -0.0033       -0.0037   
    ##                                      (0.0065)      (0.0066)      (0.0066)      (0.0066)  
    ## % of Life Residing Locally (zip)      0.0110        0.0118        0.0148        0.0161   
    ##                                      (0.0252)      (0.0252)      (0.0252)      (0.0252)  
    ## DID residence (zip)                                 0.0046                      0.0148   
    ##                                                    (0.0079)                    (0.0093)  
    ## Foreigner % sqrt. (zip)                             0.0014                      0.0098   
    ##                                                    (0.0058)                    (0.0080)  
    ## University % by 10% (zip)                           0.0010                     -0.0052   
    ##                                                    (0.0048)                    (0.0067)  
    ## DID proportion (mun.)                                            -0.0184       -0.0343 * 
    ##                                                                  (0.0143)      (0.0168)  
    ## Foreigner % sqrt. (mun.)                                         -0.0088       -0.0180   
    ##                                                                  (0.0085)      (0.0113)  
    ## University % by 10% (mun.)                                        0.0131 +      0.0176 + 
    ##                                                                  (0.0068)      (0.0091)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0069        0.0071        0.0078        0.0089   
    ## Adj. R^2                              0.0008        0.0003        0.0011        0.0015   
    ## Num. obs.                          4614          4614          4614          4614        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## LDP - DPJ FT

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0111       -0.0111       -0.0112       -0.0111   
    ##                                      (0.0086)      (0.0087)      (0.0087)      (0.0087)  
    ## Gender (male)                         0.0150 *      0.0152 *      0.0146 *      0.0148 * 
    ##                                      (0.0072)      (0.0073)      (0.0073)      (0.0073)  
    ## Age (by 10 years, centered at 45)    -0.0003       -0.0004       -0.0002       -0.0003   
    ##                                      (0.0040)      (0.0041)      (0.0041)      (0.0041)  
    ## University * Male                     0.0129        0.0129        0.0129        0.0129   
    ##                                      (0.0108)      (0.0108)      (0.0108)      (0.0108)  
    ## University * Age                     -0.0009       -0.0009       -0.0009       -0.0009   
    ##                                      (0.0060)      (0.0060)      (0.0060)      (0.0060)  
    ## University * Male * Age              -0.0006       -0.0005       -0.0005       -0.0005   
    ##                                      (0.0077)      (0.0077)      (0.0077)      (0.0077)  
    ## Male * Age                           -0.0138 **    -0.0139 **    -0.0140 **    -0.0140 **
    ##                                      (0.0053)      (0.0053)      (0.0053)      (0.0053)  
    ## % of Life Residing Locally (zip)      0.0046        0.0045        0.0052        0.0063   
    ##                                      (0.0196)      (0.0196)      (0.0197)      (0.0196)  
    ## DID residence (zip)                                 0.0067                      0.0064   
    ##                                                    (0.0060)                    (0.0074)  
    ## Foreigner % sqrt. (zip)                             0.0033                      0.0123 + 
    ##                                                    (0.0045)                    (0.0067)  
    ## University % by 10% (zip)                          -0.0024                     -0.0000   
    ##                                                    (0.0037)                    (0.0051)  
    ## DID proportion (mun.)                                             0.0080        0.0005   
    ##                                                                  (0.0108)      (0.0130)  
    ## Foreigner % sqrt. (mun.)                                         -0.0061       -0.0174 + 
    ##                                                                  (0.0066)      (0.0092)  
    ## University % by 10% (mun.)                                       -0.0022       -0.0026   
    ##                                                                  (0.0055)      (0.0073)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0958        0.0962        0.0960        0.0971   
    ## Adj. R^2                              0.0902        0.0901        0.0899        0.0904   
    ## Num. obs.                          4614          4614          4614          4614        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of South Korea

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.0001         0.0003         0.0004         0.0003    
    ##                                      (0.0137)       (0.0137)       (0.0137)       (0.0137)   
    ## Gender (male)                        -0.0485 ***    -0.0455 ***    -0.0457 ***    -0.0453 ***
    ##                                      (0.0117)       (0.0118)       (0.0118)       (0.0118)   
    ## Age (by 10 years, centered at 45)     0.0089         0.0076         0.0078         0.0075    
    ##                                      (0.0071)       (0.0071)       (0.0071)       (0.0071)   
    ## University * Male                     0.0050         0.0047         0.0046         0.0046    
    ##                                      (0.0164)       (0.0164)       (0.0164)       (0.0164)   
    ## University * Age                     -0.0124        -0.0119        -0.0119        -0.0119    
    ##                                      (0.0099)       (0.0099)       (0.0099)       (0.0099)   
    ## University * Male * Age               0.0072         0.0064         0.0063         0.0063    
    ##                                      (0.0120)       (0.0120)       (0.0120)       (0.0120)   
    ## Male * Age                            0.0201 *       0.0216 *       0.0214 *       0.0217 *  
    ##                                      (0.0086)       (0.0086)       (0.0086)       (0.0086)   
    ## % of Life Residing Locally (zip)      0.0159         0.0184         0.0177         0.0170    
    ##                                      (0.0311)       (0.0310)       (0.0311)       (0.0311)   
    ## DID residence (zip)                                 -0.0134                       -0.0068    
    ##                                                     (0.0089)                      (0.0110)   
    ## Foreigner % sqrt. (zip)                              0.0014                       -0.0114    
    ##                                                     (0.0068)                      (0.0092)   
    ## University % by 10% (zip)                            0.0129 *                      0.0062    
    ##                                                     (0.0058)                      (0.0080)   
    ## DID proportion (mun.)                                              -0.0285 +      -0.0207    
    ##                                                                    (0.0158)       (0.0194)   
    ## Foreigner % sqrt. (mun.)                                            0.0138         0.0244 +  
    ##                                                                    (0.0097)       (0.0130)   
    ## University % by 10% (mun.)                                          0.0180 *       0.0127    
    ##                                                                    (0.0081)       (0.0109)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0724         0.0736         0.0742         0.0747    
    ## Adj. R^2                              0.0668         0.0673         0.0679         0.0678    
    ## Num. obs.                          4614           4614           4614           4614         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of China

``` r
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
```

    ## 
    ## ======================================================================================
    ##                                    Base         ZIP          Municipality  Full       
    ## --------------------------------------------------------------------------------------
    ## University education                  0.0006       0.0006       0.0006        0.0006  
    ##                                      (0.0114)     (0.0114)     (0.0114)      (0.0114) 
    ## Gender (male)                        -0.0076      -0.0081      -0.0086       -0.0088  
    ##                                      (0.0099)     (0.0100)     (0.0100)      (0.0100) 
    ## Age (by 10 years, centered at 45)    -0.0141 *    -0.0140 *    -0.0139 *     -0.0139 *
    ##                                      (0.0062)     (0.0062)     (0.0062)      (0.0062) 
    ## University * Male                     0.0028       0.0028       0.0028        0.0028  
    ##                                      (0.0138)     (0.0138)     (0.0138)      (0.0138) 
    ## University * Age                      0.0057       0.0058       0.0057        0.0056  
    ##                                      (0.0084)     (0.0084)     (0.0084)      (0.0084) 
    ## University * Male * Age              -0.0147      -0.0148      -0.0147       -0.0146  
    ##                                      (0.0103)     (0.0103)     (0.0103)      (0.0103) 
    ## Male * Age                            0.0170 *     0.0171 *     0.0169 *      0.0169 *
    ##                                      (0.0074)     (0.0075)     (0.0075)      (0.0075) 
    ## % of Life Residing Locally (zip)     -0.0016      -0.0020      -0.0010       -0.0013  
    ##                                      (0.0259)     (0.0260)     (0.0260)      (0.0260) 
    ## DID residence (zip)                               -0.0102                     0.0009  
    ##                                                   (0.0076)                   (0.0091) 
    ## Foreigner % sqrt. (zip)                           -0.0023                    -0.0055  
    ##                                                   (0.0061)                   (0.0084) 
    ## University % by 10% (zip)                          0.0023                    -0.0007  
    ##                                                   (0.0049)                   (0.0067) 
    ## DID proportion (mun.)                                          -0.0334 *     -0.0340 *
    ##                                                                (0.0135)      (0.0162) 
    ## Foreigner % sqrt. (mun.)                                        0.0021        0.0071  
    ##                                                                (0.0082)      (0.0111) 
    ## University % by 10% (mun.)                                      0.0089        0.0098  
    ##                                                                (0.0069)      (0.0091) 
    ## --------------------------------------------------------------------------------------
    ## R^2                                   0.0326       0.0330       0.0339        0.0340  
    ## Adj. R^2                              0.0266       0.0265       0.0273        0.0268  
    ## Num. obs.                          4614         4614         4614          4614       
    ## ======================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of USA

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0133       -0.0132       -0.0129       -0.0129   
    ##                                      (0.0116)      (0.0116)      (0.0116)      (0.0116)  
    ## Gender (male)                         0.0261 **     0.0295 **     0.0309 **     0.0307 **
    ##                                      (0.0100)      (0.0101)      (0.0101)      (0.0101)  
    ## Age (by 10 years, centered at 45)     0.0051        0.0038        0.0034        0.0035   
    ##                                      (0.0060)      (0.0061)      (0.0061)      (0.0061)  
    ## University * Male                     0.0227        0.0225        0.0221        0.0222   
    ##                                      (0.0142)      (0.0142)      (0.0142)      (0.0142)  
    ## University * Age                     -0.0121       -0.0118       -0.0114       -0.0114   
    ##                                      (0.0084)      (0.0084)      (0.0084)      (0.0084)  
    ## University * Male * Age               0.0136        0.0130        0.0124        0.0124   
    ##                                      (0.0104)      (0.0104)      (0.0104)      (0.0104)  
    ## Male * Age                            0.0053        0.0066        0.0073        0.0072   
    ##                                      (0.0074)      (0.0074)      (0.0074)      (0.0074)  
    ## % of Life Residing Locally (zip)     -0.0114       -0.0076       -0.0064       -0.0069   
    ##                                      (0.0267)      (0.0268)      (0.0269)      (0.0269)  
    ## DID residence (zip)                                 0.0040                     -0.0003   
    ##                                                    (0.0080)                    (0.0097)  
    ## Foreigner % sqrt. (zip)                            -0.0016                     -0.0070   
    ##                                                    (0.0064)                    (0.0088)  
    ## University % by 10% (zip)                           0.0100 +                   -0.0013   
    ##                                                    (0.0052)                    (0.0070)  
    ## DID proportion (mun.)                                             0.0079        0.0088   
    ##                                                                  (0.0146)      (0.0175)  
    ## Foreigner % sqrt. (mun.)                                         -0.0007        0.0057   
    ##                                                                  (0.0088)      (0.0117)  
    ## University % by 10% (mun.)                                        0.0165 *      0.0180 + 
    ##                                                                  (0.0072)      (0.0094)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0228        0.0242        0.0255        0.0257   
    ## Adj. R^2                              0.0168        0.0176        0.0189        0.0185   
    ## Num. obs.                          4614          4614          4614          4614        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Income

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1168 ***     0.1175 ***     0.1178 ***     0.1176 ***
    ##                                      (0.0152)       (0.0150)       (0.0151)       (0.0150)   
    ## Gender (male)                        -0.0063         0.0051         0.0040         0.0060    
    ##                                      (0.0127)       (0.0127)       (0.0128)       (0.0128)   
    ## Age (by 10 years, centered at 45)     0.0267 ***     0.0222 **      0.0231 **      0.0221 ** 
    ##                                      (0.0077)       (0.0077)       (0.0077)       (0.0078)   
    ## University * Male                    -0.0205        -0.0214        -0.0216        -0.0215    
    ##                                      (0.0184)       (0.0182)       (0.0183)       (0.0182)   
    ## University * Age                     -0.0169        -0.0154        -0.0153        -0.0152    
    ##                                      (0.0111)       (0.0110)       (0.0110)       (0.0110)   
    ## University * Male * Age               0.0252 +       0.0227 +       0.0226 +       0.0225 +  
    ##                                      (0.0136)       (0.0135)       (0.0135)       (0.0135)   
    ## Male * Age                           -0.0285 **     -0.0241 *      -0.0245 **     -0.0238 *  
    ##                                      (0.0094)       (0.0094)       (0.0094)       (0.0094)   
    ## % of Life Residing Locally (zip)     -0.0533        -0.0444        -0.0485        -0.0476    
    ##                                      (0.0339)       (0.0337)       (0.0339)       (0.0338)   
    ## DID residence (zip)                                 -0.0079                       -0.0041    
    ##                                                     (0.0099)                      (0.0121)   
    ## Foreigner % sqrt. (zip)                              0.0150 +                     -0.0063    
    ##                                                     (0.0079)                      (0.0102)   
    ## University % by 10% (zip)                            0.0324 ***                    0.0227 *  
    ##                                                     (0.0063)                      (0.0090)   
    ## DID proportion (mun.)                                              -0.0168        -0.0122    
    ##                                                                    (0.0175)       (0.0214)   
    ## Foreigner % sqrt. (mun.)                                            0.0341 **      0.0402 ** 
    ##                                                                    (0.0109)       (0.0141)   
    ## University % by 10% (mun.)                                          0.0357 ***     0.0151    
    ##                                                                    (0.0088)       (0.0122)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0467         0.0546         0.0552         0.0567    
    ## Adj. R^2                              0.0409         0.0482         0.0488         0.0497    
    ## Num. obs.                          4614           4614           4614           4614         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

# With Matched Data (Lambda = 50km)

``` r
sifcct <- readRDS(datadir2)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.5000  0.5000  0.5727  0.7000  1.0000

``` r
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.04098 0.18484 0.40915 0.49906 0.78565 0.97505

## Outcome Model

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.0001         0.0002         0.0002         0.0003    
    ##                                      (0.0262)       (0.0262)       (0.0262)       (0.0263)   
    ## Gender (male)                        -0.0791 ***    -0.0798 ***    -0.0786 ***    -0.0794 ***
    ##                                      (0.0223)       (0.0224)       (0.0224)       (0.0225)   
    ## Age (by 10 years, centered at 45)    -0.0182        -0.0182        -0.0183        -0.0182    
    ##                                      (0.0133)       (0.0133)       (0.0133)       (0.0133)   
    ## University * Male                    -0.0003        -0.0004        -0.0004        -0.0006    
    ##                                      (0.0325)       (0.0325)       (0.0325)       (0.0326)   
    ## University * Age                      0.0020         0.0021         0.0020         0.0022    
    ##                                      (0.0189)       (0.0190)       (0.0190)       (0.0190)   
    ## University * Male * Age              -0.0063        -0.0063        -0.0064        -0.0063    
    ##                                      (0.0237)       (0.0237)       (0.0237)       (0.0238)   
    ## Male * Age                            0.0276 +       0.0275 +       0.0278 +       0.0275 +  
    ##                                      (0.0165)       (0.0165)       (0.0165)       (0.0165)   
    ## % of Life Residing Locally (zip)      0.1409 *       0.1409 *       0.1422 *       0.1402 *  
    ##                                      (0.0565)       (0.0569)       (0.0569)       (0.0571)   
    ## DID residence (zip)                                 -0.0043                       -0.0063    
    ##                                                     (0.0220)                      (0.0290)   
    ## Foreigner % sqrt. (zip)                             -0.0075                       -0.0156    
    ##                                                     (0.0121)                      (0.0176)   
    ## University % by 10% (zip)                            0.0003                       -0.0065    
    ##                                                     (0.0096)                      (0.0142)   
    ## DID proportion (mun.)                                              -0.0046         0.0020    
    ##                                                                    (0.0349)       (0.0459)   
    ## Foreigner % sqrt. (mun.)                                            0.0007         0.0149    
    ##                                                                    (0.0165)       (0.0230)   
    ## University % by 10% (mun.)                                          0.0049         0.0115    
    ##                                                                    (0.0130)       (0.0190)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0270         0.0273         0.0271         0.0277    
    ## Adj. R^2                              0.0140         0.0129         0.0127         0.0119    
    ## Num. obs.                          2122           2122           2122           2122         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est                 lci95              uci95             lci90         
    ##  Female:5   Min.   :25   Min.   :-8.498e-03   Min.   :-0.10424   Min.   :0.03723   Min.   :-0.08671  
    ##  Male  :5   1st Qu.:35   1st Qu.:-3.556e-03   1st Qu.:-0.07350   1st Qu.:0.04632   1st Qu.:-0.06154  
    ##             Median :45   Median :-1.075e-06   Median :-0.05546   Median :0.05624   Median :-0.04625  
    ##             Mean   :45   Mean   :-1.075e-06   Mean   :-0.06187   Mean   :0.06187   Mean   :-0.05192  
    ##             3rd Qu.:55   3rd Qu.: 3.476e-03   3rd Qu.:-0.04954   3rd Qu.:0.07164   3rd Qu.:-0.04179  
    ##             Max.   :65   Max.   : 7.903e-03   Max.   :-0.03782   Max.   :0.11363   Max.   :-0.03178  
    ##      uci90               se                p               lv           
    ##  Min.   :0.03119   Min.   :0.01913   Min.   :0.8096   Length:10         
    ##  1st Qu.:0.03879   1st Qu.:0.02411   1st Qu.:0.8607   Class :character  
    ##  Median :0.04653   Median :0.03010   Median :0.9188   Mode  :character  
    ##  Mean   :0.05192   Mean   :0.03155   Mean   :0.9056                     
    ##  3rd Qu.:0.06091   3rd Qu.:0.03505   3rd Qu.:0.9460                     
    ##  Max.   :0.09611   Max.   :0.05555   Max.   :0.9910

## Outcome Model 2

``` r
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
```

    ## 
    ## ===============================================================================================
    ##                                    Base: Agree     Base: Neither  ZIP: Agree      ZIP: Neither 
    ## -----------------------------------------------------------------------------------------------
    ## University education                   0.0989         -0.1999         0.0993         -0.1995   
    ##                                       (0.1916)        (0.1946)       (0.1917)        (0.1948)  
    ## Gender (male)                         -0.4693 ***     -0.6083 **     -0.4778 ***     -0.6287 **
    ##                                       (0.1653)        (0.1712)       (0.1663)        (0.1718)  
    ## Age (by 10 years, centered at 45)     -0.0959 *       -0.1872        -0.0942 +       -0.1827   
    ##                                       (0.0946)        (0.1021)       (0.0949)        (0.1021)  
    ## University * Male                     -0.0509          0.2147        -0.0511          0.2155   
    ##                                       (0.2352)        (0.2389)       (0.2353)        (0.2391)  
    ## University * Age                       0.0046          0.1977         0.0047          0.1970   
    ##                                       (0.1350)        (0.1383)       (0.1352)        (0.1384)  
    ## University * Male * Age               -0.0861         -0.0618        -0.0852         -0.0591   
    ##                                       (0.1713)        (0.1716)       (0.1715)        (0.1717)  
    ## Male * Age                             0.2291          0.0407 +       0.2270          0.0350 + 
    ##                                       (0.1202)        (0.1243)       (0.1206)        (0.1242)  
    ## % of Life Residing Locally (zip)       1.0474          0.5444 *       1.0344          0.5287 * 
    ##                                       (0.4092)        (0.4271)       (0.4117)        (0.4291)  
    ## DID residence (zip)                                                  -0.0402          0.0510   
    ##                                                                      (0.1581)        (0.1557)  
    ## Foreigner % sqrt. (zip)                                              -0.0439         -0.1049   
    ##                                                                      (0.0842)        (0.0884)  
    ## University % by 10% (zip)                                            -0.0137         -0.0595   
    ##                                                                      (0.0688)        (0.0691)  
    ## -----------------------------------------------------------------------------------------------
    ## AIC                                 4568.6683       4568.6683      4578.3252       4578.3252   
    ## Log Likelihood                     -2226.3341      -2226.3341     -2225.1626      -2225.1626   
    ## Num. obs.                           2122            2122           2122            2122        
    ## K                                      3               3              3               3        
    ## ===============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ## 
    ## ===============================================================================================
    ##                                    Mun.: Agree     Mun.: Neither  Full: Agree     Full: Neither
    ## -----------------------------------------------------------------------------------------------
    ## University education                   0.0989         -0.2012         0.1005         -0.2006   
    ##                                       (0.1912)        (0.1946)       (0.1914)        (0.1948)  
    ## Gender (male)                         -0.4690 ***     -0.6181 **     -0.4764 ***     -0.6312 **
    ##                                       (0.1659)        (0.1715)       (0.1665)        (0.1722)  
    ## Age (by 10 years, centered at 45)     -0.0964 +       -0.1854        -0.0939 +       -0.1823   
    ##                                       (0.0946)        (0.1020)       (0.0950)        (0.1019)  
    ## University * Male                     -0.0511          0.2174        -0.0535          0.2164   
    ##                                       (0.2350)        (0.2389)       (0.2352)        (0.2392)  
    ## University * Age                       0.0044          0.1946         0.0049          0.1930   
    ##                                       (0.1347)        (0.1384)       (0.1348)        (0.1384)  
    ## University * Male * Age               -0.0855         -0.0612        -0.0839         -0.0574   
    ##                                       (0.1711)        (0.1718)       (0.1713)        (0.1719)  
    ## Male * Age                             0.2299          0.0377 +       0.2276          0.0288 + 
    ##                                       (0.1203)        (0.1242)       (0.1209)        (0.1240)  
    ## % of Life Residing Locally (zip)       1.0590          0.5055 *       1.0391          0.4956 * 
    ##                                       (0.4108)        (0.4294)       (0.4122)        (0.4299)  
    ## DID residence (zip)                                                  -0.0876          0.2819   
    ##                                                                      (0.2066)        (0.2009)  
    ## Foreigner % sqrt. (zip)                                              -0.0696 +       -0.2240   
    ##                                                                      (0.1177)        (0.1230)  
    ## University % by 10% (zip)                                            -0.0585         -0.1120   
    ##                                                                      (0.1000)        (0.1006)  
    ## DID proportion (mun.)                  0.0156         -0.3117         0.1036 +       -0.5992   
    ##                                       (0.2568)        (0.2602)       (0.3337)        (0.3368)  
    ## Foreigner % sqrt. (mun.)              -0.0246          0.0533         0.0383          0.2607   
    ##                                       (0.1207)        (0.1208)       (0.1636)        (0.1673)  
    ## University % by 10% (mun.)             0.0147          0.0054         0.0717          0.1201   
    ##                                       (0.0947)        (0.0922)       (0.1363)        (0.1331)  
    ## -----------------------------------------------------------------------------------------------
    ## AIC                                 4578.2752       4578.2752      4583.8452       4583.8452   
    ## Log Likelihood                     -2225.1376      -2225.1376     -2221.9226      -2221.9226   
    ## Num. obs.                           2122            2122           2122            2122        
    ## K                                      3               3              3               3        
    ## ===============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est               lci95             uci95            lci90             uci90       
    ##  Female:5   Min.   :25   Min.   :-0.11109   Min.   :-0.6837   Min.   :0.2952   Min.   :-0.5560   Min.   :0.2426  
    ##  Male  :5   1st Qu.:35   1st Qu.: 0.05790   1st Qu.:-0.4441   1st Qu.:0.3826   1st Qu.:-0.3563   1st Qu.:0.3115  
    ##             Median :45   Median : 0.09806   Median :-0.3251   Median :0.4767   Median :-0.2634   Median :0.4182  
    ##             Mean   :45   Mean   : 0.07372   Mean   :-0.3763   Mean   :0.5237   Mean   :-0.3039   Mean   :0.4513  
    ##             3rd Qu.:55   3rd Qu.: 0.10899   3rd Qu.:-0.2636   3rd Qu.:0.6475   3rd Qu.:-0.2034   3rd Qu.:0.5597  
    ##             Max.   :65   Max.   : 0.20497   Max.   :-0.2189   Max.   :0.9041   Max.   :-0.1634   Max.   :0.7764  
    ##        se               p               lv           
    ##  Min.   :0.1374   Min.   :0.4177   Length:10         
    ##  1st Qu.:0.1769   1st Qu.:0.5982   Class :character  
    ##  Median :0.2176   Median :0.6784   Mode  :character  
    ##  Mean   :0.2295   Mean   :0.6538                     
    ##  3rd Qu.:0.2548   3rd Qu.:0.7301                     
    ##  Max.   :0.4048   Max.   :0.8476

## Mediator Models

## Knowledge

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1730 ***     0.1735 ***     0.1733 ***     0.1736 ***
    ##                                      (0.0241)       (0.0241)       (0.0241)       (0.0241)   
    ## Gender (male)                         0.2073 ***     0.2138 ***     0.2104 ***     0.2143 ***
    ##                                      (0.0209)       (0.0210)       (0.0210)       (0.0211)   
    ## Age (by 10 years, centered at 45)     0.0504 ***     0.0478 ***     0.0494 ***     0.0477 ***
    ##                                      (0.0121)       (0.0121)       (0.0121)       (0.0121)   
    ## University * Male                    -0.0619 *      -0.0628 *      -0.0627 *      -0.0630 *  
    ##                                      (0.0288)       (0.0288)       (0.0288)       (0.0288)   
    ## University * Age                     -0.0185        -0.0173        -0.0179        -0.0170    
    ##                                      (0.0168)       (0.0168)       (0.0168)       (0.0168)   
    ## University * Male * Age              -0.0188        -0.0196        -0.0194        -0.0196    
    ##                                      (0.0207)       (0.0206)       (0.0207)       (0.0207)   
    ## Male * Age                            0.0220         0.0243         0.0232         0.0247 +  
    ##                                      (0.0150)       (0.0150)       (0.0150)       (0.0150)   
    ## % of Life Residing Locally (zip)     -0.1456 **     -0.1290 *      -0.1322 **     -0.1277 *  
    ##                                      (0.0508)       (0.0504)       (0.0505)       (0.0503)   
    ## DID residence (zip)                                 -0.0104                       -0.0265    
    ##                                                     (0.0177)                      (0.0233)   
    ## Foreigner % sqrt. (zip)                             -0.0127                       -0.0121    
    ##                                                     (0.0101)                      (0.0133)   
    ## University % by 10% (zip)                            0.0320 ***                    0.0319 ** 
    ##                                                     (0.0083)                      (0.0120)   
    ## DID proportion (mun.)                                               0.0096         0.0412    
    ##                                                                    (0.0291)       (0.0381)   
    ## Foreigner % sqrt. (mun.)                                           -0.0144        -0.0036    
    ##                                                                    (0.0142)       (0.0186)   
    ## University % by 10% (mun.)                                          0.0282 **     -0.0022    
    ##                                                                    (0.0105)       (0.0150)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.1963         0.2033         0.2002         0.2038    
    ## Adj. R^2                              0.1856         0.1915         0.1884         0.1908    
    ## Num. obs.                          2122           2122           2122           2122         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Ideology

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0237       -0.0237       -0.0237       -0.0238   
    ##                                      (0.0157)      (0.0157)      (0.0156)      (0.0156)  
    ## Gender (male)                        -0.0396 **    -0.0396 **    -0.0415 **    -0.0420 **
    ##                                      (0.0135)      (0.0136)      (0.0135)      (0.0136)  
    ## Age (by 10 years, centered at 45)    -0.0030       -0.0032       -0.0029       -0.0025   
    ##                                      (0.0073)      (0.0073)      (0.0072)      (0.0072)  
    ## University * Male                     0.0271        0.0270        0.0271        0.0272   
    ##                                      (0.0202)      (0.0202)      (0.0201)      (0.0201)  
    ## University * Age                     -0.0101       -0.0099       -0.0102       -0.0105   
    ##                                      (0.0111)      (0.0111)      (0.0110)      (0.0111)  
    ## University * Male * Age               0.0186        0.0184        0.0182        0.0181   
    ##                                      (0.0146)      (0.0146)      (0.0145)      (0.0145)  
    ## Male * Age                           -0.0082       -0.0078       -0.0083       -0.0082   
    ##                                      (0.0098)      (0.0097)      (0.0097)      (0.0097)  
    ## % of Life Residing Locally (zip)      0.0402        0.0376        0.0419        0.0410   
    ##                                      (0.0359)      (0.0361)      (0.0359)      (0.0357)  
    ## DID residence (zip)                                -0.0290 *                   -0.0084   
    ##                                                    (0.0140)                    (0.0174)  
    ## Foreigner % sqrt. (zip)                             0.0021                      0.0222 * 
    ##                                                    (0.0076)                    (0.0110)  
    ## University % by 10% (zip)                           0.0050                     -0.0040   
    ##                                                    (0.0062)                    (0.0085)  
    ## DID proportion (mun.)                                            -0.0618 **    -0.0550 + 
    ##                                                                  (0.0238)      (0.0298)  
    ## Foreigner % sqrt. (mun.)                                         -0.0197 +     -0.0402 **
    ##                                                                  (0.0106)      (0.0144)  
    ## University % by 10% (mun.)                                        0.0170 *      0.0202 + 
    ##                                                                  (0.0084)      (0.0115)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0161        0.0182        0.0230        0.0254   
    ## Adj. R^2                              0.0030        0.0036        0.0085        0.0096   
    ## Num. obs.                          2122          2122          2122          2122        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## LDP - DPJ FT

``` r
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
```

    ## 
    ## ======================================================================================
    ##                                    Base         ZIP          Municipality  Full       
    ## --------------------------------------------------------------------------------------
    ## University education                  0.0010       0.0010       0.0010        0.0010  
    ##                                      (0.0135)     (0.0135)     (0.0135)      (0.0135) 
    ## Gender (male)                         0.0117       0.0112       0.0110        0.0109  
    ##                                      (0.0104)     (0.0104)     (0.0104)      (0.0105) 
    ## Age (by 10 years, centered at 45)     0.0026       0.0027       0.0027        0.0028  
    ##                                      (0.0058)     (0.0058)     (0.0058)      (0.0058) 
    ## University * Male                     0.0137       0.0137       0.0138        0.0139  
    ##                                      (0.0166)     (0.0166)     (0.0166)      (0.0167) 
    ## University * Age                      0.0010       0.0009       0.0009        0.0008  
    ##                                      (0.0094)     (0.0094)     (0.0094)      (0.0095) 
    ## University * Male * Age              -0.0006      -0.0006      -0.0006       -0.0006  
    ##                                      (0.0120)     (0.0120)     (0.0120)      (0.0120) 
    ## Male * Age                           -0.0133 +    -0.0135 +    -0.0135 +     -0.0136 +
    ##                                      (0.0076)     (0.0076)     (0.0076)      (0.0077) 
    ## % of Life Residing Locally (zip)      0.0124       0.0116       0.0106        0.0110  
    ##                                      (0.0270)     (0.0272)     (0.0272)      (0.0273) 
    ## DID residence (zip)                                0.0011                     0.0086  
    ##                                                   (0.0112)                   (0.0146) 
    ## Foreigner % sqrt. (zip)                           -0.0010                    -0.0003  
    ##                                                   (0.0057)                   (0.0078) 
    ## University % by 10% (zip)                         -0.0021                    -0.0006  
    ##                                                   (0.0046)                   (0.0066) 
    ## DID proportion (mun.)                                          -0.0098       -0.0186  
    ##                                                                (0.0180)      (0.0235) 
    ## Foreigner % sqrt. (mun.)                                       -0.0004       -0.0000  
    ##                                                                (0.0085)      (0.0114) 
    ## University % by 10% (mun.)                                     -0.0021       -0.0015  
    ##                                                                (0.0064)      (0.0091) 
    ## --------------------------------------------------------------------------------------
    ## R^2                                   0.0983       0.0984       0.0986        0.0988  
    ## Adj. R^2                              0.0862       0.0850       0.0852        0.0841  
    ## Num. obs.                          2122         2122         2122          2122       
    ## ======================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of South Korea

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0056        -0.0055        -0.0057        -0.0055    
    ##                                      (0.0197)       (0.0197)       (0.0197)       (0.0197)   
    ## Gender (male)                        -0.0602 ***    -0.0604 ***    -0.0608 ***    -0.0601 ***
    ##                                      (0.0169)       (0.0170)       (0.0169)       (0.0170)   
    ## Age (by 10 years, centered at 45)    -0.0046        -0.0048        -0.0045        -0.0049    
    ##                                      (0.0104)       (0.0105)       (0.0105)       (0.0105)   
    ## University * Male                     0.0097         0.0095         0.0098         0.0095    
    ##                                      (0.0240)       (0.0240)       (0.0240)       (0.0240)   
    ## University * Age                     -0.0025        -0.0022        -0.0027        -0.0022    
    ##                                      (0.0145)       (0.0145)       (0.0145)       (0.0145)   
    ## University * Male * Age               0.0036         0.0034         0.0035         0.0035    
    ##                                      (0.0180)       (0.0180)       (0.0180)       (0.0180)   
    ## Male * Age                            0.0301 *       0.0304 *       0.0300 *       0.0304 *  
    ##                                      (0.0128)       (0.0128)       (0.0129)       (0.0129)   
    ## % of Life Residing Locally (zip)      0.0434         0.0426         0.0421         0.0417    
    ##                                      (0.0438)       (0.0439)       (0.0439)       (0.0440)   
    ## DID residence (zip)                                 -0.0215                       -0.0218    
    ##                                                     (0.0159)                      (0.0204)   
    ## Foreigner % sqrt. (zip)                             -0.0058                       -0.0097    
    ##                                                     (0.0086)                      (0.0109)   
    ## University % by 10% (zip)                            0.0048                        0.0060    
    ##                                                     (0.0071)                      (0.0106)   
    ## DID proportion (mun.)                                              -0.0225         0.0012    
    ##                                                                    (0.0264)       (0.0339)   
    ## Foreigner % sqrt. (mun.)                                           -0.0003         0.0083    
    ##                                                                    (0.0130)       (0.0166)   
    ## University % by 10% (mun.)                                          0.0031        -0.0025    
    ##                                                                    (0.0097)       (0.0143)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0796         0.0808         0.0800         0.0810    
    ## Adj. R^2                              0.0673         0.0672         0.0663         0.0660    
    ## Num. obs.                          2122           2122           2122           2122         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of China

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                  0.0143        0.0144        0.0143        0.0144   
    ##                                      (0.0161)      (0.0161)      (0.0161)      (0.0161)  
    ## Gender (male)                        -0.0049       -0.0065       -0.0067       -0.0070   
    ##                                      (0.0139)      (0.0141)      (0.0140)      (0.0141)  
    ## Age (by 10 years, centered at 45)    -0.0219 **    -0.0217 **    -0.0216 **    -0.0215 **
    ##                                      (0.0083)      (0.0083)      (0.0082)      (0.0083)  
    ## University * Male                    -0.0143       -0.0143       -0.0142       -0.0142   
    ##                                      (0.0199)      (0.0199)      (0.0199)      (0.0199)  
    ## University * Age                      0.0042        0.0044        0.0042        0.0043   
    ##                                      (0.0119)      (0.0119)      (0.0119)      (0.0119)  
    ## University * Male * Age              -0.0143       -0.0141       -0.0143       -0.0142   
    ##                                      (0.0147)      (0.0148)      (0.0147)      (0.0148)  
    ## Male * Age                            0.0209 *      0.0205 *      0.0207 *      0.0204 * 
    ##                                      (0.0103)      (0.0104)      (0.0103)      (0.0104)  
    ## % of Life Residing Locally (zip)     -0.0042       -0.0042       -0.0031       -0.0034   
    ##                                      (0.0365)      (0.0367)      (0.0367)      (0.0367)  
    ## DID residence (zip)                                 0.0026                      0.0066   
    ##                                                    (0.0129)                    (0.0163)  
    ## Foreigner % sqrt. (zip)                            -0.0141 *                   -0.0084   
    ##                                                    (0.0071)                    (0.0095)  
    ## University % by 10% (zip)                          -0.0028                     -0.0028   
    ##                                                    (0.0061)                    (0.0085)  
    ## DID proportion (mun.)                                            -0.0033       -0.0098   
    ##                                                                  (0.0221)      (0.0281)  
    ## Foreigner % sqrt. (mun.)                                         -0.0190 +     -0.0112   
    ##                                                                  (0.0099)      (0.0127)  
    ## University % by 10% (mun.)                                       -0.0020        0.0010   
    ##                                                                  (0.0082)      (0.0114)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0406        0.0424        0.0425        0.0428   
    ## Adj. R^2                              0.0278        0.0282        0.0283        0.0272   
    ## Num. obs.                          2122          2122          2122          2122        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of USA

``` r
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
```

    ## 
    ## ======================================================================================
    ##                                    Base         ZIP          Municipality  Full       
    ## --------------------------------------------------------------------------------------
    ## University education                 -0.0218      -0.0217      -0.0217       -0.0216  
    ##                                      (0.0175)     (0.0175)     (0.0175)      (0.0175) 
    ## Gender (male)                         0.0195       0.0214       0.0219        0.0216  
    ##                                      (0.0147)     (0.0148)     (0.0147)      (0.0148) 
    ## Age (by 10 years, centered at 45)     0.0008       0.0000       0.0001        0.0001  
    ##                                      (0.0086)     (0.0086)     (0.0086)      (0.0086) 
    ## University * Male                     0.0447 *     0.0445 *     0.0442 *      0.0441 *
    ##                                      (0.0215)     (0.0215)     (0.0215)      (0.0215) 
    ## University * Age                     -0.0093      -0.0089      -0.0090       -0.0088  
    ##                                      (0.0124)     (0.0124)     (0.0124)      (0.0124) 
    ## University * Male * Age               0.0176       0.0174       0.0173        0.0174  
    ##                                      (0.0155)     (0.0155)     (0.0155)      (0.0155) 
    ## Male * Age                            0.0121       0.0128       0.0129        0.0128  
    ##                                      (0.0108)     (0.0108)     (0.0108)      (0.0108) 
    ## % of Life Residing Locally (zip)      0.0016       0.0065       0.0080        0.0069  
    ##                                      (0.0368)     (0.0370)     (0.0371)      (0.0372) 
    ## DID residence (zip)                               -0.0026                    -0.0085  
    ##                                                   (0.0141)                   (0.0175) 
    ## Foreigner % sqrt. (zip)                           -0.0037                    -0.0104  
    ##                                                   (0.0086)                   (0.0123) 
    ## University % by 10% (zip)                          0.0093                    -0.0021  
    ##                                                   (0.0063)                   (0.0091) 
    ## DID proportion (mun.)                                           0.0013        0.0105  
    ##                                                                (0.0242)      (0.0300) 
    ## Foreigner % sqrt. (mun.)                                        0.0011        0.0104  
    ##                                                                (0.0114)      (0.0155) 
    ## University % by 10% (mun.)                                      0.0169 +      0.0191  
    ##                                                                (0.0088)      (0.0124) 
    ## --------------------------------------------------------------------------------------
    ## R^2                                   0.0355       0.0367       0.0380        0.0387  
    ## Adj. R^2                              0.0226       0.0224       0.0238        0.0231  
    ## Num. obs.                          2122         2122         2122          2122       
    ## ======================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Income

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1108 ***     0.1112 ***     0.1111 ***     0.1113 ***
    ##                                      (0.0213)       (0.0211)       (0.0211)       (0.0211)   
    ## Gender (male)                         0.0083         0.0174         0.0153         0.0190    
    ##                                      (0.0181)       (0.0181)       (0.0180)       (0.0181)   
    ## Age (by 10 years, centered at 45)     0.0311 **      0.0281 *       0.0294 **      0.0278 *  
    ##                                      (0.0111)       (0.0109)       (0.0110)       (0.0109)   
    ## University * Male                    -0.0188        -0.0197        -0.0198        -0.0201    
    ##                                      (0.0265)       (0.0264)       (0.0263)       (0.0263)   
    ## University * Age                     -0.0316 *      -0.0306 +      -0.0312 *      -0.0303 +  
    ##                                      (0.0158)       (0.0156)       (0.0156)       (0.0156)   
    ## University * Male * Age               0.0347 +       0.0339 +       0.0342 +       0.0341 +  
    ##                                      (0.0201)       (0.0199)       (0.0199)       (0.0199)   
    ## Male * Age                           -0.0401 **     -0.0373 **     -0.0385 **     -0.0374 ** 
    ##                                      (0.0140)       (0.0138)       (0.0138)       (0.0138)   
    ## % of Life Residing Locally (zip)     -0.0132         0.0053        -0.0034         0.0017    
    ##                                      (0.0469)       (0.0466)       (0.0470)       (0.0468)   
    ## DID residence (zip)                                 -0.0035                       -0.0081    
    ##                                                     (0.0176)                      (0.0219)   
    ## Foreigner % sqrt. (zip)                              0.0053                       -0.0184    
    ##                                                     (0.0097)                      (0.0118)   
    ## University % by 10% (zip)                            0.0356 ***                    0.0294 *  
    ##                                                     (0.0078)                      (0.0116)   
    ## DID proportion (mun.)                                              -0.0047         0.0083    
    ##                                                                    (0.0298)       (0.0374)   
    ## Foreigner % sqrt. (mun.)                                            0.0300 *       0.0468 ** 
    ##                                                                    (0.0137)       (0.0171)   
    ## University % by 10% (mun.)                                          0.0367 ***     0.0090    
    ##                                                                    (0.0106)       (0.0156)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0499         0.0602         0.0593         0.0636    
    ## Adj. R^2                              0.0372         0.0462         0.0454         0.0483    
    ## Num. obs.                          2122           2122           2122           2122         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

# With Matched Data (Lambda = 100km)

``` r
sifcct <- readRDS(datadir3)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.5000  0.5000  0.5720  0.6963  1.0000

``` r
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.04098 0.18484 0.40915 0.49094 0.78565 0.97505

## Outcome Model

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0036        -0.0035        -0.0036        -0.0033    
    ##                                      (0.0228)       (0.0228)       (0.0228)       (0.0228)   
    ## Gender (male)                        -0.0904 ***    -0.0921 ***    -0.0896 ***    -0.0911 ***
    ##                                      (0.0191)       (0.0193)       (0.0193)       (0.0193)   
    ## Age (by 10 years, centered at 45)    -0.0035        -0.0032        -0.0036        -0.0035    
    ##                                      (0.0117)       (0.0118)       (0.0117)       (0.0117)   
    ## University * Male                     0.0053         0.0053         0.0053         0.0049    
    ##                                      (0.0279)       (0.0279)       (0.0279)       (0.0279)   
    ## University * Age                     -0.0116        -0.0115        -0.0117        -0.0113    
    ##                                      (0.0166)       (0.0166)       (0.0166)       (0.0166)   
    ## University * Male * Age               0.0069         0.0070         0.0070         0.0071    
    ##                                      (0.0206)       (0.0206)       (0.0206)       (0.0206)   
    ## Male * Age                            0.0150         0.0147         0.0151         0.0148    
    ##                                      (0.0143)       (0.0143)       (0.0143)       (0.0143)   
    ## % of Life Residing Locally (zip)      0.0113         0.0107         0.0098         0.0089    
    ##                                      (0.0502)       (0.0503)       (0.0503)       (0.0504)   
    ## DID residence (zip)                                 -0.0062                       -0.0063    
    ##                                                     (0.0163)                      (0.0215)   
    ## Foreigner % sqrt. (zip)                             -0.0063                       -0.0220    
    ##                                                     (0.0105)                      (0.0151)   
    ## University % by 10% (zip)                           -0.0018                       -0.0080    
    ##                                                     (0.0085)                      (0.0119)   
    ## DID proportion (mun.)                                              -0.0090        -0.0008    
    ##                                                                    (0.0277)       (0.0364)   
    ## Foreigner % sqrt. (mun.)                                            0.0115         0.0309    
    ##                                                                    (0.0144)       (0.0199)   
    ## University % by 10% (mun.)                                          0.0019         0.0099    
    ##                                                                    (0.0120)       (0.0167)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0236         0.0239         0.0238         0.0250    
    ## Adj. R^2                              0.0142         0.0134         0.0134         0.0135    
    ## Num. obs.                          2928           2928           2928           2928         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est                 lci95              uci95             lci90         
    ##  Female:5   Min.   :25   Min.   :-0.0258961   Min.   :-0.12109   Min.   :0.03299   Min.   :-0.10578  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.0059760   1st Qu.:-0.05973   1st Qu.:0.04259   1st Qu.:-0.05108  
    ##             Median :45   Median :-0.0005160   Median :-0.04479   Median :0.04934   Median :-0.03698  
    ##             Mean   :45   Mean   :-0.0008406   Mean   :-0.05398   Mean   :0.05230   Mean   :-0.04544  
    ##             3rd Qu.:55   3rd Qu.: 0.0074766   3rd Qu.:-0.03542   3rd Qu.:0.06410   3rd Qu.:-0.02824  
    ##             Max.   :65   Max.   : 0.0193276   Max.   :-0.02978   Max.   :0.07801   Max.   :-0.02473  
    ##      uci90               se                p               lv           
    ##  Min.   :0.02794   Min.   :0.01601   Min.   :0.5184   Length:10         
    ##  1st Qu.:0.03557   1st Qu.:0.02048   1st Qu.:0.6766   Class :character  
    ##  Median :0.04113   Median :0.02589   Median :0.7537   Mode  :character  
    ##  Mean   :0.04375   Mean   :0.02710   Mean   :0.7501                     
    ##  3rd Qu.:0.05105   3rd Qu.:0.02983   3rd Qu.:0.8674                     
    ##  Max.   :0.06857   Max.   :0.04855   Max.   :0.9202

## Outcome Model 2

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base: Agree     Base: Neither   ZIP: Agree      ZIP: Neither  
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.0261         -0.1785          0.0267         -0.1773    
    ##                                       (0.1681)        (0.1710)        (0.1681)        (0.1711)   
    ## Gender (male)                         -0.5556 ***     -0.6127 ***     -0.5734 ***     -0.6377 ***
    ##                                       (0.1425)        (0.1472)        (0.1438)        (0.1487)   
    ## Age (by 10 years, centered at 45)      0.0054 +       -0.1605          0.0093 +       -0.1564    
    ##                                       (0.0845)        (0.0901)        (0.0848)        (0.0902)   
    ## University * Male                      0.0471          0.0755          0.0469          0.0749    
    ##                                       (0.2035)        (0.2060)        (0.2035)        (0.2062)   
    ## University * Age                      -0.1011          0.1683         -0.1009          0.1692    
    ##                                       (0.1193)        (0.1229)        (0.1194)        (0.1229)   
    ## University * Male * Age                0.0565         -0.0801          0.0576         -0.0777    
    ##                                       (0.1482)        (0.1507)        (0.1483)        (0.1507)   
    ## Male * Age                             0.1096          0.0636          0.1051          0.0573    
    ##                                       (0.1037)        (0.1088)        (0.1039)        (0.1089)   
    ## % of Life Residing Locally (zip)       0.1253          0.3394          0.1109          0.3317    
    ##                                       (0.3529)        (0.3611)        (0.3548)        (0.3621)   
    ## DID residence (zip)                                                   -0.0143          0.0147    
    ##                                                                       (0.1187)        (0.1168)   
    ## Foreigner % sqrt. (zip)                                               -0.0456         -0.1101    
    ##                                                                       (0.0745)        (0.0764)   
    ## University % by 10% (zip)                                             -0.0364         -0.0488    
    ##                                                                       (0.0611)        (0.0611)   
    ## -------------------------------------------------------------------------------------------------
    ## AIC                                 6261.3473       6261.3473       6270.1191       6270.1191    
    ## Log Likelihood                     -3072.6736      -3072.6736      -3071.0596      -3071.0596    
    ## Num. obs.                           2928            2928            2928            2928         
    ## K                                      3               3               3               3         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Mun.: Agree     Mun.: Neither   Full: Agree     Full: Neither 
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.0265         -0.1783          0.0289         -0.1762    
    ##                                       (0.1679)        (0.1709)        (0.1680)        (0.1711)   
    ## Gender (male)                         -0.5535 ***     -0.6298 ***     -0.5665 ***     -0.6381 ***
    ##                                       (0.1436)        (0.1481)        (0.1440)        (0.1488)   
    ## Age (by 10 years, centered at 45)      0.0053 +       -0.1572          0.0072 +       -0.1576    
    ##                                       (0.0848)        (0.0900)        (0.0850)        (0.0899)   
    ## University * Male                      0.0464          0.0771          0.0435          0.0744    
    ##                                       (0.2034)        (0.2061)        (0.2036)        (0.2062)   
    ## University * Age                      -0.1011          0.1661         -0.0987          0.1675    
    ##                                       (0.1192)        (0.1227)        (0.1193)        (0.1227)   
    ## University * Male * Age                0.0569         -0.0765          0.0569         -0.0726    
    ##                                       (0.1483)        (0.1505)        (0.1484)        (0.1506)   
    ## Male * Age                             0.1095          0.0569          0.1071          0.0509    
    ##                                       (0.1041)        (0.1088)        (0.1043)        (0.1087)   
    ## % of Life Residing Locally (zip)       0.1140          0.3153          0.1038          0.3104    
    ##                                       (0.3550)        (0.3616)        (0.3554)        (0.3624)   
    ## DID residence (zip)                                                   -0.0401          0.2221    
    ##                                                                       (0.1520)        (0.1501)   
    ## Foreigner % sqrt. (zip)                                               -0.1295 *       -0.2178    
    ##                                                                       (0.1026)        (0.1127)   
    ## University % by 10% (zip)                                             -0.0722         -0.0786    
    ##                                                                       (0.0857)        (0.0846)   
    ## DID proportion (mun.)                  0.0145 +       -0.3372          0.0639 *       -0.5536    
    ##                                       (0.2033)        (0.2049)        (0.2587)        (0.2630)   
    ## Foreigner % sqrt. (mun.)               0.0450          0.0562          0.1593 +        0.2559    
    ##                                       (0.1056)        (0.1044)        (0.1417)        (0.1464)   
    ## University % by 10% (mun.)            -0.0174          0.0100          0.0531          0.0929    
    ##                                       (0.0883)        (0.0860)        (0.1217)        (0.1176)   
    ## -------------------------------------------------------------------------------------------------
    ## AIC                                 6268.8151       6268.8151       6272.9549       6272.9549    
    ## Log Likelihood                     -3070.4075      -3070.4075      -3066.4775      -3066.4775    
    ## Num. obs.                           2928            2928            2928            2928         
    ## K                                      3               3               3               3         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est                lci95             uci95            lci90             uci90       
    ##  Female:5   Min.   :25   Min.   :-0.168548   Min.   :-0.8702   Min.   :0.2972   Min.   :-0.7574   Min.   :0.2569  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.001217   1st Qu.:-0.3790   1st Qu.:0.3685   1st Qu.:-0.3182   1st Qu.:0.3122  
    ##             Median :45   Median : 0.051454   Median :-0.2548   Median :0.4169   Median :-0.1988   Median :0.3534  
    ##             Mean   :45   Mean   : 0.050626   Mean   :-0.3386   Mean   :0.4399   Mean   :-0.2760   Mean   :0.3773  
    ##             3rd Qu.:55   3rd Qu.: 0.124254   3rd Qu.:-0.1912   3rd Qu.:0.5094   3rd Qu.:-0.1362   3rd Qu.:0.4122  
    ##             Max.   :65   Max.   : 0.226340   Max.   :-0.1525   Max.   :0.6690   Max.   :-0.1163   Max.   :0.5978  
    ##        se               p               lv           
    ##  Min.   :0.1147   Min.   :0.3161   Length:10         
    ##  1st Qu.:0.1524   1st Qu.:0.4542   Class :character  
    ##  Median :0.1855   Median :0.5829   Mode  :character  
    ##  Mean   :0.1985   Mean   :0.6250                     
    ##  3rd Qu.:0.2237   3rd Qu.:0.8137                     
    ##  Max.   :0.3579   Max.   :0.9551

## Mediator Models

## Knowledge

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1695 ***     0.1697 ***     0.1698 ***     0.1697 ***
    ##                                      (0.0211)       (0.0211)       (0.0211)       (0.0211)   
    ## Gender (male)                         0.2011 ***     0.2078 ***     0.2052 ***     0.2081 ***
    ##                                      (0.0180)       (0.0182)       (0.0181)       (0.0182)   
    ## Age (by 10 years, centered at 45)     0.0562 ***     0.0536 ***     0.0549 ***     0.0536 ***
    ##                                      (0.0107)       (0.0107)       (0.0107)       (0.0108)   
    ## University * Male                    -0.0448 +      -0.0451 +      -0.0453 +      -0.0453 +  
    ##                                      (0.0248)       (0.0248)       (0.0248)       (0.0248)   
    ## University * Age                     -0.0197        -0.0190        -0.0192        -0.0188    
    ##                                      (0.0147)       (0.0147)       (0.0147)       (0.0147)   
    ## University * Male * Age              -0.0039        -0.0048        -0.0048        -0.0052    
    ##                                      (0.0178)       (0.0178)       (0.0178)       (0.0178)   
    ## Male * Age                            0.0105         0.0129         0.0123         0.0135    
    ##                                      (0.0129)       (0.0130)       (0.0130)       (0.0130)   
    ## % of Life Residing Locally (zip)     -0.1433 ***    -0.1301 **     -0.1323 **     -0.1287 ** 
    ##                                      (0.0429)       (0.0427)       (0.0428)       (0.0427)   
    ## DID residence (zip)                                 -0.0112                       -0.0261    
    ##                                                     (0.0135)                      (0.0176)   
    ## Foreigner % sqrt. (zip)                             -0.0056                       -0.0023    
    ##                                                     (0.0092)                      (0.0120)   
    ## University % by 10% (zip)                            0.0268 ***                    0.0256 ** 
    ##                                                     (0.0072)                      (0.0098)   
    ## DID proportion (mun.)                                               0.0091         0.0394    
    ##                                                                    (0.0228)       (0.0296)   
    ## Foreigner % sqrt. (mun.)                                           -0.0122        -0.0102    
    ##                                                                    (0.0128)       (0.0163)   
    ## University % by 10% (mun.)                                          0.0236 *      -0.0009    
    ##                                                                    (0.0097)       (0.0130)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.2047         0.2089         0.2074         0.2096    
    ## Adj. R^2                              0.1971         0.2005         0.1989         0.2003    
    ## Num. obs.                          2928           2928           2928           2928         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Ideology

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0132       -0.0131       -0.0130       -0.0132   
    ##                                      (0.0136)      (0.0136)      (0.0135)      (0.0136)  
    ## Gender (male)                        -0.0313 **    -0.0311 **    -0.0325 **    -0.0331 **
    ##                                      (0.0117)      (0.0118)      (0.0117)      (0.0118)  
    ## Age (by 10 years, centered at 45)    -0.0084       -0.0087       -0.0086       -0.0079   
    ##                                      (0.0066)      (0.0066)      (0.0066)      (0.0066)  
    ## University * Male                     0.0213        0.0212        0.0212        0.0215   
    ##                                      (0.0173)      (0.0173)      (0.0172)      (0.0172)  
    ## University * Age                     -0.0010       -0.0009       -0.0009       -0.0013   
    ##                                      (0.0098)      (0.0098)      (0.0098)      (0.0098)  
    ## University * Male * Age               0.0145        0.0145        0.0143        0.0144   
    ##                                      (0.0127)      (0.0127)      (0.0126)      (0.0126)  
    ## Male * Age                           -0.0029       -0.0027       -0.0028       -0.0031   
    ##                                      (0.0086)      (0.0086)      (0.0086)      (0.0086)  
    ## % of Life Residing Locally (zip)      0.0295        0.0311        0.0377        0.0365   
    ##                                      (0.0312)      (0.0313)      (0.0312)      (0.0311)  
    ## DID residence (zip)                                -0.0063                      0.0110   
    ##                                                    (0.0105)                    (0.0127)  
    ## Foreigner % sqrt. (zip)                            -0.0035                      0.0159 + 
    ##                                                    (0.0066)                    (0.0092)  
    ## University % by 10% (zip)                           0.0034                     -0.0071   
    ##                                                    (0.0054)                    (0.0072)  
    ## DID proportion (mun.)                                            -0.0411 *     -0.0551 * 
    ##                                                                  (0.0188)      (0.0227)  
    ## Foreigner % sqrt. (mun.)                                         -0.0258 **    -0.0400 **
    ##                                                                  (0.0093)      (0.0124)  
    ## University % by 10% (mun.)                                        0.0199 **     0.0263 **
    ##                                                                  (0.0077)      (0.0101)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0137        0.0141        0.0200        0.0219   
    ## Adj. R^2                              0.0042        0.0035        0.0095        0.0104   
    ## Num. obs.                          2928          2928          2928          2928        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## LDP - DPJ FT

``` r
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
```

    ## 
    ## ======================================================================================
    ##                                    Base         ZIP          Municipality  Full       
    ## --------------------------------------------------------------------------------------
    ## University education                 -0.0064      -0.0065      -0.0065       -0.0066  
    ##                                      (0.0117)     (0.0117)     (0.0117)      (0.0117) 
    ## Gender (male)                         0.0169 +     0.0169 +     0.0158 +      0.0162 +
    ##                                      (0.0092)     (0.0093)     (0.0093)      (0.0093) 
    ## Age (by 10 years, centered at 45)     0.0021       0.0023       0.0023        0.0024  
    ##                                      (0.0055)     (0.0055)     (0.0055)      (0.0055) 
    ## University * Male                     0.0166       0.0167       0.0167        0.0169  
    ##                                      (0.0142)     (0.0142)     (0.0142)      (0.0142) 
    ## University * Age                     -0.0056      -0.0056      -0.0056       -0.0059  
    ##                                      (0.0084)     (0.0084)     (0.0084)      (0.0084) 
    ## University * Male * Age               0.0052       0.0053       0.0053        0.0054  
    ##                                      (0.0104)     (0.0104)     (0.0104)      (0.0104) 
    ## Male * Age                           -0.0140 *    -0.0141 *    -0.0143 *     -0.0144 *
    ##                                      (0.0068)     (0.0068)     (0.0068)      (0.0068) 
    ## % of Life Residing Locally (zip)      0.0200       0.0189       0.0194        0.0195  
    ##                                      (0.0243)     (0.0243)     (0.0244)      (0.0244) 
    ## DID residence (zip)                                0.0030                     0.0095  
    ##                                                   (0.0083)                   (0.0108) 
    ## Foreigner % sqrt. (zip)                            0.0028                     0.0096  
    ##                                                   (0.0053)                   (0.0075) 
    ## University % by 10% (zip)                         -0.0019                     0.0006  
    ##                                                   (0.0041)                   (0.0057) 
    ## DID proportion (mun.)                                          -0.0063       -0.0173  
    ##                                                                (0.0142)      (0.0184) 
    ## Foreigner % sqrt. (mun.)                                       -0.0037       -0.0121  
    ##                                                                (0.0074)      (0.0101) 
    ## University % by 10% (mun.)                                     -0.0018       -0.0025  
    ##                                                                (0.0060)      (0.0081) 
    ## --------------------------------------------------------------------------------------
    ## R^2                                   0.0962       0.0964       0.0966        0.0977  
    ## Adj. R^2                              0.0875       0.0868       0.0869        0.0871  
    ## Num. obs.                          2928         2928         2928          2928       
    ## ======================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of South Korea

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.0025         0.0026         0.0025         0.0027    
    ##                                      (0.0172)       (0.0172)       (0.0173)       (0.0172)   
    ## Gender (male)                        -0.0555 ***    -0.0560 ***    -0.0557 ***    -0.0559 ***
    ##                                      (0.0147)       (0.0148)       (0.0147)       (0.0148)   
    ## Age (by 10 years, centered at 45)     0.0037         0.0035         0.0037         0.0035    
    ##                                      (0.0093)       (0.0093)       (0.0093)       (0.0093)   
    ## University * Male                    -0.0074        -0.0075        -0.0074        -0.0076    
    ##                                      (0.0207)       (0.0207)       (0.0207)       (0.0207)   
    ## University * Age                     -0.0113        -0.0110        -0.0113        -0.0111    
    ##                                      (0.0128)       (0.0128)       (0.0128)       (0.0128)   
    ## University * Male * Age               0.0071         0.0070         0.0071         0.0073    
    ##                                      (0.0156)       (0.0156)       (0.0156)       (0.0156)   
    ## Male * Age                            0.0267 *       0.0267 *       0.0266 *       0.0264 *  
    ##                                      (0.0112)       (0.0112)       (0.0112)       (0.0113)   
    ## % of Life Residing Locally (zip)      0.0073         0.0095         0.0083         0.0085    
    ##                                      (0.0377)       (0.0378)       (0.0379)       (0.0379)   
    ## DID residence (zip)                                 -0.0017                        0.0079    
    ##                                                     (0.0120)                      (0.0154)   
    ## Foreigner % sqrt. (zip)                             -0.0100                       -0.0165 +  
    ##                                                     (0.0074)                      (0.0095)   
    ## University % by 10% (zip)                            0.0021                       -0.0013    
    ##                                                     (0.0064)                      (0.0090)   
    ## DID proportion (mun.)                                              -0.0204        -0.0268    
    ##                                                                    (0.0205)       (0.0262)   
    ## Foreigner % sqrt. (mun.)                                           -0.0005         0.0146    
    ##                                                                    (0.0109)       (0.0141)   
    ## University % by 10% (mun.)                                          0.0063         0.0080    
    ##                                                                    (0.0090)       (0.0125)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0772         0.0778         0.0775         0.0784    
    ## Adj. R^2                              0.0683         0.0679         0.0677         0.0676    
    ## Num. obs.                          2928           2928           2928           2928         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of China

``` r
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
```

    ## 
    ## =======================================================================================
    ##                                    Base         ZIP           Municipality  Full       
    ## ---------------------------------------------------------------------------------------
    ## University education                  0.0073       0.0075        0.0073        0.0075  
    ##                                      (0.0142)     (0.0142)      (0.0142)      (0.0142) 
    ## Gender (male)                        -0.0089      -0.0125       -0.0123       -0.0129  
    ##                                      (0.0123)     (0.0124)      (0.0124)      (0.0125) 
    ## Age (by 10 years, centered at 45)    -0.0196 *    -0.0191 *     -0.0189 *     -0.0190 *
    ##                                      (0.0076)     (0.0076)      (0.0076)      (0.0076) 
    ## University * Male                     0.0005       0.0004        0.0007        0.0005  
    ##                                      (0.0172)     (0.0172)      (0.0172)      (0.0172) 
    ## University * Age                      0.0001       0.0003       -0.0000        0.0001  
    ##                                      (0.0107)     (0.0107)      (0.0107)      (0.0107) 
    ## University * Male * Age              -0.0079      -0.0077       -0.0076       -0.0074  
    ##                                      (0.0130)     (0.0130)      (0.0130)      (0.0131) 
    ## Male * Age                            0.0212 *     0.0204 *      0.0203 *      0.0200 *
    ##                                      (0.0092)     (0.0092)      (0.0092)      (0.0092) 
    ## % of Life Residing Locally (zip)     -0.0195      -0.0203       -0.0202       -0.0205  
    ##                                      (0.0320)     (0.0321)      (0.0321)      (0.0321) 
    ## DID residence (zip)                               -0.0031                      0.0068  
    ##                                                   (0.0100)                    (0.0127) 
    ## Foreigner % sqrt. (zip)                           -0.0166 **                  -0.0148 +
    ##                                                   (0.0062)                    (0.0081) 
    ## University % by 10% (zip)                         -0.0052                     -0.0043  
    ##                                                   (0.0054)                    (0.0073) 
    ## DID proportion (mun.)                                           -0.0208       -0.0267  
    ##                                                                 (0.0174)      (0.0219) 
    ## Foreigner % sqrt. (mun.)                                        -0.0149 +     -0.0015  
    ##                                                                 (0.0085)      (0.0109) 
    ## University % by 10% (mun.)                                      -0.0038        0.0007  
    ##                                                                 (0.0076)      (0.0101) 
    ## ---------------------------------------------------------------------------------------
    ## R^2                                   0.0374       0.0405        0.0401        0.0411  
    ## Adj. R^2                              0.0281       0.0302        0.0298        0.0299  
    ## Num. obs.                          2928         2928          2928          2928       
    ## =======================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of USA

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0099       -0.0098       -0.0098       -0.0097   
    ##                                      (0.0147)      (0.0148)      (0.0147)      (0.0148)  
    ## Gender (male)                         0.0380 **     0.0382 **     0.0394 **     0.0382 **
    ##                                      (0.0125)      (0.0126)      (0.0125)      (0.0126)  
    ## Age (by 10 years, centered at 45)     0.0050        0.0047        0.0045        0.0048   
    ##                                      (0.0078)      (0.0077)      (0.0077)      (0.0077)  
    ## University * Male                     0.0151        0.0150        0.0149        0.0147   
    ##                                      (0.0181)      (0.0181)      (0.0181)      (0.0181)  
    ## University * Age                     -0.0104       -0.0102       -0.0103       -0.0103   
    ##                                      (0.0108)      (0.0108)      (0.0108)      (0.0108)  
    ## University * Male * Age               0.0189        0.0189        0.0187        0.0189   
    ##                                      (0.0134)      (0.0134)      (0.0133)      (0.0134)  
    ## Male * Age                            0.0038        0.0038        0.0043        0.0037   
    ##                                      (0.0095)      (0.0094)      (0.0094)      (0.0094)  
    ## % of Life Residing Locally (zip)     -0.0320       -0.0298       -0.0276       -0.0287   
    ##                                      (0.0320)      (0.0322)      (0.0323)      (0.0323)  
    ## DID residence (zip)                                 0.0080                      0.0158   
    ##                                                    (0.0108)                    (0.0136)  
    ## Foreigner % sqrt. (zip)                            -0.0085                     -0.0136   
    ##                                                    (0.0073)                    (0.0098)  
    ## University % by 10% (zip)                           0.0009                     -0.0105   
    ##                                                    (0.0057)                    (0.0078)  
    ## DID proportion (mun.)                                            -0.0104       -0.0266   
    ##                                                                  (0.0185)      (0.0231)  
    ## Foreigner % sqrt. (mun.)                                         -0.0036        0.0087   
    ##                                                                  (0.0098)      (0.0128)  
    ## University % by 10% (mun.)                                        0.0127        0.0231 * 
    ##                                                                  (0.0081)      (0.0109)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0285        0.0292        0.0295        0.0309   
    ## Adj. R^2                              0.0191        0.0188        0.0191        0.0195   
    ## Num. obs.                          2928          2928          2928          2928        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Income

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1057 ***     0.1056 ***     0.1060 ***     0.1059 ***
    ##                                      (0.0191)       (0.0189)       (0.0189)       (0.0189)   
    ## Gender (male)                        -0.0033         0.0082         0.0055         0.0092    
    ##                                      (0.0158)       (0.0157)       (0.0158)       (0.0158)   
    ## Age (by 10 years, centered at 45)     0.0247 *       0.0212 *       0.0226 *       0.0210 *  
    ##                                      (0.0099)       (0.0098)       (0.0098)       (0.0098)   
    ## University * Male                     0.0080         0.0078         0.0073         0.0074    
    ##                                      (0.0231)       (0.0229)       (0.0229)       (0.0229)   
    ## University * Age                     -0.0251 +      -0.0245 +      -0.0247 +      -0.0244 +  
    ##                                      (0.0144)       (0.0143)       (0.0143)       (0.0143)   
    ## University * Male * Age               0.0404 *       0.0393 *       0.0395 *       0.0395 *  
    ##                                      (0.0177)       (0.0175)       (0.0175)       (0.0175)   
    ## Male * Age                           -0.0340 **     -0.0306 *      -0.0314 **     -0.0306 *  
    ##                                      (0.0121)       (0.0120)       (0.0120)       (0.0120)   
    ## % of Life Residing Locally (zip)     -0.0618        -0.0455        -0.0516        -0.0472    
    ##                                      (0.0413)       (0.0411)       (0.0414)       (0.0413)   
    ## DID residence (zip)                                 -0.0009                        0.0030    
    ##                                                     (0.0132)                      (0.0162)   
    ## Foreigner % sqrt. (zip)                              0.0079                       -0.0084    
    ##                                                     (0.0085)                      (0.0105)   
    ## University % by 10% (zip)                            0.0349 ***                    0.0269 ** 
    ##                                                     (0.0068)                      (0.0098)   
    ## DID proportion (mun.)                                              -0.0135        -0.0127    
    ##                                                                    (0.0229)       (0.0284)   
    ## Foreigner % sqrt. (mun.)                                            0.0242 *       0.0325 *  
    ##                                                                    (0.0119)       (0.0149)   
    ## University % by 10% (mun.)                                          0.0396 ***     0.0144    
    ##                                                                    (0.0097)       (0.0137)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0543         0.0643         0.0630         0.0661    
    ## Adj. R^2                              0.0451         0.0543         0.0529         0.0551    
    ## Num. obs.                          2928           2928           2928           2928         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

# With Matched Data (Lambda = 200km)

``` r
sifcct <- readRDS(datadir4)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.5000  0.5000  0.5712  0.6937  1.0000

``` r
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.04098 0.18484 0.40915 0.48581 0.78565 0.97505

## Outcome Model

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0276        -0.0275        -0.0275        -0.0275    
    ##                                      (0.0194)       (0.0194)       (0.0194)       (0.0194)   
    ## Gender (male)                        -0.0976 ***    -0.0981 ***    -0.0967 ***    -0.0975 ***
    ##                                      (0.0165)       (0.0166)       (0.0166)       (0.0167)   
    ## Age (by 10 years, centered at 45)    -0.0054        -0.0052        -0.0056        -0.0052    
    ##                                      (0.0099)       (0.0099)       (0.0099)       (0.0099)   
    ## University * Male                     0.0171         0.0171         0.0170         0.0171    
    ##                                      (0.0239)       (0.0239)       (0.0239)       (0.0239)   
    ## University * Age                     -0.0121        -0.0121        -0.0120        -0.0122    
    ##                                      (0.0141)       (0.0141)       (0.0141)       (0.0141)   
    ## University * Male * Age               0.0107         0.0106         0.0106         0.0109    
    ##                                      (0.0175)       (0.0176)       (0.0175)       (0.0176)   
    ## Male * Age                            0.0173         0.0171         0.0174         0.0170    
    ##                                      (0.0122)       (0.0122)       (0.0122)       (0.0122)   
    ## % of Life Residing Locally (zip)      0.0397         0.0389         0.0376         0.0369    
    ##                                      (0.0438)       (0.0439)       (0.0439)       (0.0440)   
    ## DID residence (zip)                                 -0.0050                       -0.0047    
    ##                                                     (0.0135)                      (0.0173)   
    ## Foreigner % sqrt. (zip)                              0.0013                       -0.0092    
    ##                                                     (0.0092)                      (0.0126)   
    ## University % by 10% (zip)                           -0.0011                       -0.0049    
    ##                                                     (0.0077)                      (0.0107)   
    ## DID proportion (mun.)                                              -0.0074        -0.0025    
    ##                                                                    (0.0233)       (0.0298)   
    ## Foreigner % sqrt. (mun.)                                            0.0130         0.0214    
    ##                                                                    (0.0133)       (0.0176)   
    ## University % by 10% (mun.)                                          0.0014         0.0063    
    ##                                                                    (0.0110)       (0.0150)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0267         0.0267         0.0269         0.0272    
    ## Adj. R^2                              0.0194         0.0187         0.0189         0.0184    
    ## Num. obs.                          3786           3786           3786           3786         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est                lci95              uci95             lci90         
    ##  Female:5   Min.   :25   Min.   :-0.051832   Min.   :-0.13185   Min.   :0.01050   Min.   :-0.11898  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.024470   1st Qu.:-0.06481   1st Qu.:0.01777   1st Qu.:-0.05823  
    ##             Median :45   Median :-0.012352   Median :-0.05562   Median :0.02388   Median :-0.04756  
    ##             Mean   :45   Mean   :-0.018971   Mean   :-0.06453   Mean   :0.02659   Mean   :-0.05720  
    ##             3rd Qu.:55   3rd Qu.:-0.009471   3rd Qu.:-0.04730   3rd Qu.:0.03456   3rd Qu.:-0.04172  
    ##             Max.   :65   Max.   :-0.003189   Max.   :-0.03787   Max.   :0.04764   Max.   :-0.03346  
    ##      uci90               se                p               lv           
    ##  Min.   :0.00439   Min.   :0.01400   Min.   :0.1560   Length:10         
    ##  1st Qu.:0.01303   1st Qu.:0.01769   1st Qu.:0.2519   Class :character  
    ##  Median :0.01625   Median :0.02228   Median :0.4805   Mode  :character  
    ##  Mean   :0.01926   Mean   :0.02324   Mean   :0.4748                     
    ##  3rd Qu.:0.02641   3rd Qu.:0.02578   3rd Qu.:0.6061                     
    ##  Max.   :0.03947   Max.   :0.04081   Max.   :0.9021

## Outcome Model 2

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base: Agree     Base: Neither   ZIP: Agree      ZIP: Neither  
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.1888 **      -0.3972         -0.1890 **      -0.3981    
    ##                                       (0.1458)        (0.1477)        (0.1457)        (0.1476)   
    ## Gender (male)                         -0.6832 ***     -0.6968 ***     -0.6887 ***     -0.7305 ***
    ##                                       (0.1249)        (0.1282)        (0.1259)        (0.1293)   
    ## Age (by 10 years, centered at 45)     -0.0119 *       -0.1570         -0.0099 *       -0.1480    
    ##                                       (0.0746)        (0.0782)        (0.0747)        (0.0782)   
    ## University * Male                      0.2177          0.1554          0.2180          0.1572    
    ##                                       (0.1776)        (0.1790)        (0.1775)        (0.1790)   
    ## University * Age                      -0.1163          0.1244         -0.1166          0.1229    
    ##                                       (0.1046)        (0.1067)        (0.1045)        (0.1067)   
    ## University * Male * Age                0.0695          0.0507          0.0702          0.0551    
    ##                                       (0.1292)        (0.1308)        (0.1292)        (0.1308)   
    ## Male * Age                             0.1279          0.0490          0.1258          0.0395    
    ##                                       (0.0909)        (0.0947)        (0.0910)        (0.0947)   
    ## % of Life Residing Locally (zip)       0.4193          0.4131          0.4113          0.3916    
    ##                                       (0.3162)        (0.3264)        (0.3176)        (0.3268)   
    ## DID residence (zip)                                                    0.0127         -0.0254    
    ##                                                                       (0.0979)        (0.0986)   
    ## Foreigner % sqrt. (zip)                                                0.0037         -0.0648    
    ##                                                                       (0.0669)        (0.0664)   
    ## University % by 10% (zip)                                             -0.0225         -0.0807    
    ##                                                                       (0.0561)        (0.0559)   
    ## -------------------------------------------------------------------------------------------------
    ## AIC                                 8051.2164       8051.2164       8058.2648       8058.2648    
    ## Log Likelihood                     -3967.6082      -3967.6082      -3965.1324      -3965.1324    
    ## Num. obs.                           3786            3786            3786            3786         
    ## K                                      3               3               3               3         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Mun.: Agree     Mun.: Neither   Full: Agree     Full: Neither 
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.1879 **      -0.3970         -0.1884 **      -0.3978    
    ##                                       (0.1458)        (0.1477)        (0.1457)        (0.1478)   
    ## Gender (male)                         -0.6754 ***     -0.7213 ***     -0.6827 ***     -0.7377 ***
    ##                                       (0.1255)        (0.1292)        (0.1260)        (0.1297)   
    ## Age (by 10 years, centered at 45)     -0.0132 *       -0.1531         -0.0101 *       -0.1479    
    ##                                       (0.0747)        (0.0783)        (0.0748)        (0.0781)   
    ## University * Male                      0.2165          0.1561          0.2169          0.1575    
    ##                                       (0.1776)        (0.1790)        (0.1775)        (0.1791)   
    ## University * Age                      -0.1149          0.1235         -0.1161          0.1199    
    ##                                       (0.1045)        (0.1068)        (0.1045)        (0.1067)   
    ## University * Male * Age                0.0684          0.0527          0.0705          0.0592    
    ##                                       (0.1293)        (0.1308)        (0.1293)        (0.1308)   
    ## Male * Age                             0.1291          0.0433          0.1261          0.0365    
    ##                                       (0.0911)        (0.0948)        (0.0912)        (0.0946)   
    ## % of Life Residing Locally (zip)       0.4073          0.3925          0.4002          0.3790    
    ##                                       (0.3180)        (0.3265)        (0.3185)        (0.3269)   
    ## DID residence (zip)                                                   -0.0199          0.1299    
    ##                                                                       (0.1209)        (0.1247)   
    ## Foreigner % sqrt. (zip)                                               -0.0519         -0.1288    
    ##                                                                       (0.0893)        (0.0924)   
    ## University % by 10% (zip)                                             -0.0433         -0.1078    
    ##                                                                       (0.0765)        (0.0766)   
    ## DID proportion (mun.)                  0.0658 +       -0.3159          0.0865 *       -0.4486    
    ##                                       (0.1721)        (0.1729)        (0.2107)        (0.2186)   
    ## Foreigner % sqrt. (mun.)               0.0609          0.0407          0.1069          0.1577    
    ##                                       (0.0975)        (0.0964)        (0.1287)        (0.1299)   
    ## University % by 10% (mun.)            -0.0179         -0.0128          0.0241          0.0937    
    ##                                       (0.0827)        (0.0799)        (0.1100)        (0.1079)   
    ## -------------------------------------------------------------------------------------------------
    ## AIC                                 8054.9390       8054.9390       8062.5346       8062.5346    
    ## Log Likelihood                     -3963.4695      -3963.4695      -3961.2673      -3961.2673    
    ## Num. obs.                           3786            3786            3786            3786         
    ## K                                      3               3               3               3         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est               lci95             uci95            lci90             uci90        
    ##  Female:5   Min.   :25   Min.   :-0.42059   Min.   :-1.0233   Min.   :0.1014   Min.   :-0.9264   Min.   :0.05134  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.15936   1st Qu.:-0.4605   1st Qu.:0.1873   1st Qu.:-0.4116   1st Qu.:0.10354  
    ##             Median :45   Median :-0.03978   Median :-0.3487   Median :0.2242   Median :-0.2949   Median :0.18908  
    ##             Mean   :45   Mean   :-0.07992   Mean   :-0.4190   Mean   :0.2591   Mean   :-0.3644   Mean   :0.20461  
    ##             3rd Qu.:55   3rd Qu.: 0.04000   3rd Qu.:-0.2502   3rd Qu.:0.3178   3rd Qu.:-0.1962   3rd Qu.:0.27311  
    ##             Max.   :65   Max.   : 0.11967   Max.   :-0.1694   Max.   :0.4876   Max.   :-0.1376   Max.   :0.42842  
    ##        se               p               lv           
    ##  Min.   :0.1010   Min.   :0.1593   Length:10         
    ##  1st Qu.:0.1328   1st Qu.:0.2828   Class :character  
    ##  Median :0.1618   Median :0.5881   Mode  :character  
    ##  Mean   :0.1729   Mean   :0.5449                     
    ##  3rd Qu.:0.1976   3rd Qu.:0.7635                     
    ##  Max.   :0.3074   Max.   :0.8890

## Mediator Models

## Knowledge

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1616 ***     0.1615 ***     0.1613 ***     0.1614 ***
    ##                                      (0.0181)       (0.0180)       (0.0181)       (0.0181)   
    ## Gender (male)                         0.1914 ***     0.1972 ***     0.1953 ***     0.1975 ***
    ##                                      (0.0154)       (0.0156)       (0.0155)       (0.0156)   
    ## Age (by 10 years, centered at 45)     0.0567 ***     0.0547 ***     0.0558 ***     0.0550 ***
    ##                                      (0.0092)       (0.0093)       (0.0093)       (0.0093)   
    ## University * Male                    -0.0389 +      -0.0390 +      -0.0388 +      -0.0390 +  
    ##                                      (0.0214)       (0.0214)       (0.0214)       (0.0214)   
    ## University * Age                     -0.0134        -0.0132        -0.0137        -0.0132    
    ##                                      (0.0127)       (0.0126)       (0.0127)       (0.0127)   
    ## University * Male * Age              -0.0057        -0.0062        -0.0057        -0.0064    
    ##                                      (0.0154)       (0.0154)       (0.0154)       (0.0154)   
    ## Male * Age                            0.0047         0.0066         0.0059         0.0069    
    ##                                      (0.0112)       (0.0112)       (0.0112)       (0.0112)   
    ## % of Life Residing Locally (zip)     -0.1283 ***    -0.1210 **     -0.1194 **     -0.1175 ** 
    ##                                      (0.0388)       (0.0388)       (0.0388)       (0.0388)   
    ## DID residence (zip)                                 -0.0043                       -0.0177    
    ##                                                     (0.0113)                      (0.0143)   
    ## Foreigner % sqrt. (zip)                              0.0020                        0.0113    
    ##                                                     (0.0082)                      (0.0112)   
    ## University % by 10% (zip)                            0.0198 **                     0.0155 +  
    ##                                                     (0.0067)                      (0.0089)   
    ## DID proportion (mun.)                                               0.0169         0.0354    
    ##                                                                    (0.0195)       (0.0246)   
    ## Foreigner % sqrt. (mun.)                                           -0.0139        -0.0240    
    ##                                                                    (0.0114)       (0.0150)   
    ## University % by 10% (mun.)                                          0.0206 *       0.0056    
    ##                                                                    (0.0091)       (0.0119)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.1951         0.1974         0.1976         0.1985    
    ## Adj. R^2                              0.1891         0.1908         0.1910         0.1913    
    ## Num. obs.                          3786           3786           3786           3786         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Ideology

``` r
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
```

    ## 
    ## =======================================================================================
    ##                                    Base         ZIP          Municipality  Full        
    ## ---------------------------------------------------------------------------------------
    ## University education                 -0.0088      -0.0089      -0.0091       -0.0091   
    ##                                      (0.0114)     (0.0114)     (0.0114)      (0.0114)  
    ## Gender (male)                        -0.0206 *    -0.0202 *    -0.0214 *     -0.0219 * 
    ##                                      (0.0100)     (0.0102)     (0.0101)      (0.0102)  
    ## Age (by 10 years, centered at 45)    -0.0093      -0.0097 +    -0.0094       -0.0092   
    ##                                      (0.0057)     (0.0058)     (0.0057)      (0.0057)  
    ## University * Male                     0.0132       0.0134       0.0136        0.0136   
    ##                                      (0.0147)     (0.0147)     (0.0147)      (0.0147)  
    ## University * Age                      0.0031       0.0030       0.0024        0.0022   
    ##                                      (0.0083)     (0.0083)     (0.0083)      (0.0083)  
    ## University * Male * Age               0.0031       0.0032       0.0036        0.0037   
    ##                                      (0.0109)     (0.0109)     (0.0108)      (0.0108)  
    ## Male * Age                            0.0021       0.0023       0.0023        0.0020   
    ##                                      (0.0075)     (0.0075)     (0.0075)      (0.0075)  
    ## % of Life Residing Locally (zip)      0.0186       0.0217       0.0265        0.0260   
    ##                                      (0.0272)     (0.0272)     (0.0272)      (0.0272)  
    ## DID residence (zip)                               -0.0031                     0.0124   
    ##                                                   (0.0087)                   (0.0104)  
    ## Foreigner % sqrt. (zip)                           -0.0080                     0.0015   
    ##                                                   (0.0058)                   (0.0077)  
    ## University % by 10% (zip)                          0.0052                    -0.0044   
    ##                                                   (0.0049)                   (0.0065)  
    ## DID proportion (mun.)                                          -0.0372 *     -0.0502 **
    ##                                                                (0.0157)      (0.0187)  
    ## Foreigner % sqrt. (mun.)                                       -0.0193 *     -0.0207 + 
    ##                                                                (0.0084)      (0.0110)  
    ## University % by 10% (mun.)                                      0.0210 **     0.0252 **
    ##                                                                (0.0070)      (0.0092)  
    ## ---------------------------------------------------------------------------------------
    ## R^2                                   0.0082       0.0090       0.0125        0.0129   
    ## Adj. R^2                              0.0008       0.0008       0.0044        0.0040   
    ## Num. obs.                          3786         3786         3786          3786        
    ## =======================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## LDP - DPJ FT

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0031       -0.0031       -0.0031       -0.0031   
    ##                                      (0.0096)      (0.0096)      (0.0096)      (0.0096)  
    ## Gender (male)                         0.0149 +      0.0150 +      0.0143 +      0.0145 + 
    ##                                      (0.0079)      (0.0080)      (0.0079)      (0.0080)  
    ## Age (by 10 years, centered at 45)     0.0012        0.0012        0.0013        0.0013   
    ##                                      (0.0046)      (0.0046)      (0.0046)      (0.0046)  
    ## University * Male                     0.0126        0.0126        0.0126        0.0126   
    ##                                      (0.0120)      (0.0120)      (0.0120)      (0.0120)  
    ## University * Age                     -0.0038       -0.0038       -0.0039       -0.0039   
    ##                                      (0.0068)      (0.0068)      (0.0068)      (0.0068)  
    ## University * Male * Age               0.0047        0.0047        0.0048        0.0047   
    ##                                      (0.0087)      (0.0087)      (0.0087)      (0.0087)  
    ## Male * Age                           -0.0161 **    -0.0161 **    -0.0162 **    -0.0161 **
    ##                                      (0.0058)      (0.0058)      (0.0058)      (0.0058)  
    ## % of Life Residing Locally (zip)      0.0261        0.0261        0.0269        0.0271   
    ##                                      (0.0215)      (0.0215)      (0.0216)      (0.0216)  
    ## DID residence (zip)                                 0.0006                      0.0046   
    ##                                                    (0.0069)                    (0.0089)  
    ## Foreigner % sqrt. (zip)                             0.0006                      0.0046   
    ##                                                    (0.0046)                    (0.0064)  
    ## University % by 10% (zip)                           0.0001                      0.0005   
    ##                                                    (0.0038)                    (0.0051)  
    ## DID proportion (mun.)                                            -0.0065       -0.0114   
    ##                                                                  (0.0118)      (0.0152)  
    ## Foreigner % sqrt. (mun.)                                         -0.0035       -0.0078   
    ##                                                                  (0.0067)      (0.0090)  
    ## University % by 10% (mun.)                                        0.0016        0.0009   
    ##                                                                  (0.0056)      (0.0075)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0986        0.0986        0.0987        0.0990   
    ## Adj. R^2                              0.0918        0.0911        0.0913        0.0909   
    ## Num. obs.                          3786          3786          3786          3786        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of South Korea

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.0024         0.0024         0.0024         0.0023    
    ##                                      (0.0148)       (0.0148)       (0.0148)       (0.0148)   
    ## Gender (male)                        -0.0472 ***    -0.0465 ***    -0.0452 ***    -0.0461 ***
    ##                                      (0.0125)       (0.0126)       (0.0126)       (0.0127)   
    ## Age (by 10 years, centered at 45)     0.0001        -0.0001        -0.0003        -0.0000    
    ##                                      (0.0079)       (0.0079)       (0.0079)       (0.0079)   
    ## University * Male                    -0.0067        -0.0067        -0.0068        -0.0067    
    ##                                      (0.0179)       (0.0179)       (0.0179)       (0.0179)   
    ## University * Age                      0.0007         0.0008         0.0007         0.0005    
    ##                                      (0.0111)       (0.0111)       (0.0111)       (0.0111)   
    ## University * Male * Age              -0.0070        -0.0071        -0.0070        -0.0067    
    ##                                      (0.0134)       (0.0134)       (0.0134)       (0.0134)   
    ## Male * Age                            0.0309 **      0.0311 **      0.0313 **      0.0310 ** 
    ##                                      (0.0096)       (0.0096)       (0.0096)       (0.0096)   
    ## % of Life Residing Locally (zip)      0.0032         0.0037         0.0030         0.0023    
    ##                                      (0.0334)       (0.0334)       (0.0335)       (0.0335)   
    ## DID residence (zip)                                 -0.0010                        0.0014    
    ##                                                     (0.0100)                      (0.0125)   
    ## Foreigner % sqrt. (zip)                              0.0018                       -0.0085    
    ##                                                     (0.0067)                      (0.0088)   
    ## University % by 10% (zip)                            0.0019                       -0.0052    
    ##                                                     (0.0058)                      (0.0079)   
    ## DID proportion (mun.)                                              -0.0088        -0.0101    
    ##                                                                    (0.0173)       (0.0216)   
    ## Foreigner % sqrt. (mun.)                                            0.0126         0.0204    
    ##                                                                    (0.0100)       (0.0131)   
    ## University % by 10% (mun.)                                          0.0083         0.0134    
    ##                                                                    (0.0084)       (0.0112)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0753         0.0754         0.0761         0.0764    
    ## Adj. R^2                              0.0684         0.0677         0.0685         0.0680    
    ## Num. obs.                          3786           3786           3786           3786         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of China

``` r
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
```

    ## 
    ## ======================================================================================
    ##                                    Base         ZIP          Municipality  Full       
    ## --------------------------------------------------------------------------------------
    ## University education                 -0.0073      -0.0073      -0.0073       -0.0074  
    ##                                      (0.0123)     (0.0123)     (0.0123)      (0.0123) 
    ## Gender (male)                        -0.0140      -0.0161      -0.0158       -0.0167  
    ##                                      (0.0108)     (0.0110)     (0.0109)      (0.0110) 
    ## Age (by 10 years, centered at 45)    -0.0159 *    -0.0154 *    -0.0155 *     -0.0153 *
    ##                                      (0.0067)     (0.0067)     (0.0067)      (0.0067) 
    ## University * Male                     0.0125       0.0127       0.0126        0.0127  
    ##                                      (0.0150)     (0.0150)     (0.0150)      (0.0150) 
    ## University * Age                     -0.0011      -0.0013      -0.0012       -0.0015  
    ##                                      (0.0092)     (0.0092)     (0.0092)      (0.0092) 
    ## University * Male * Age              -0.0048      -0.0045      -0.0047       -0.0043  
    ##                                      (0.0112)     (0.0112)     (0.0112)      (0.0112) 
    ## Male * Age                            0.0188 *     0.0182 *     0.0184 *      0.0180 *
    ##                                      (0.0081)     (0.0081)     (0.0081)      (0.0081) 
    ## % of Life Residing Locally (zip)     -0.0056      -0.0061      -0.0053       -0.0060  
    ##                                      (0.0281)     (0.0282)     (0.0282)      (0.0282) 
    ## DID residence (zip)                                0.0033                     0.0108  
    ##                                                   (0.0084)                   (0.0103) 
    ## Foreigner % sqrt. (zip)                           -0.0083                    -0.0078  
    ##                                                   (0.0057)                   (0.0076) 
    ## University % by 10% (zip)                         -0.0048                    -0.0052  
    ##                                                   (0.0049)                   (0.0065) 
    ## DID proportion (mun.)                                          -0.0107       -0.0217  
    ##                                                                (0.0146)      (0.0180) 
    ## Foreigner % sqrt. (mun.)                                       -0.0071        0.0000  
    ##                                                                (0.0079)      (0.0104) 
    ## University % by 10% (mun.)                                     -0.0020        0.0032  
    ##                                                                (0.0070)      (0.0092) 
    ## --------------------------------------------------------------------------------------
    ## R^2                                   0.0345       0.0353       0.0352        0.0357  
    ## Adj. R^2                              0.0273       0.0273       0.0272        0.0270  
    ## Num. obs.                          3786         3786         3786          3786       
    ## ======================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of USA

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0085       -0.0085       -0.0085       -0.0086   
    ##                                      (0.0125)      (0.0125)      (0.0125)      (0.0126)  
    ## Gender (male)                         0.0340 **     0.0350 **     0.0362 **     0.0354 **
    ##                                      (0.0110)      (0.0111)      (0.0110)      (0.0111)  
    ## Age (by 10 years, centered at 45)     0.0044        0.0040        0.0039        0.0042   
    ##                                      (0.0067)      (0.0067)      (0.0067)      (0.0067)  
    ## University * Male                     0.0160        0.0160        0.0159        0.0160   
    ##                                      (0.0155)      (0.0155)      (0.0155)      (0.0155)  
    ## University * Age                     -0.0095       -0.0095       -0.0096       -0.0098   
    ##                                      (0.0093)      (0.0093)      (0.0093)      (0.0093)  
    ## University * Male * Age               0.0175        0.0175        0.0175        0.0178   
    ##                                      (0.0115)      (0.0115)      (0.0115)      (0.0115)  
    ## Male * Age                            0.0054        0.0057        0.0060        0.0056   
    ##                                      (0.0082)      (0.0082)      (0.0082)      (0.0082)  
    ## % of Life Residing Locally (zip)     -0.0464 +     -0.0445       -0.0437       -0.0444   
    ##                                      (0.0281)      (0.0281)      (0.0282)      (0.0282)  
    ## DID residence (zip)                                 0.0038                      0.0039   
    ##                                                    (0.0090)                    (0.0111)  
    ## Foreigner % sqrt. (zip)                            -0.0031                     -0.0095   
    ##                                                    (0.0063)                    (0.0084)  
    ## University % by 10% (zip)                           0.0034                     -0.0053   
    ##                                                    (0.0051)                    (0.0068)  
    ## DID proportion (mun.)                                            -0.0009       -0.0047   
    ##                                                                  (0.0160)      (0.0197)  
    ## Foreigner % sqrt. (mun.)                                          0.0022        0.0109   
    ##                                                                  (0.0088)      (0.0114)  
    ## University % by 10% (mun.)                                        0.0112        0.0165 + 
    ##                                                                  (0.0076)      (0.0100)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0282        0.0285        0.0292        0.0297   
    ## Adj. R^2                              0.0210        0.0205        0.0212        0.0209   
    ## Num. obs.                          3786          3786          3786          3786        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Income

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1049 ***     0.1049 ***     0.1048 ***     0.1048 ***
    ##                                      (0.0164)       (0.0162)       (0.0162)       (0.0162)   
    ## Gender (male)                        -0.0020         0.0073         0.0059         0.0081    
    ##                                      (0.0135)       (0.0135)       (0.0135)       (0.0135)   
    ## Age (by 10 years, centered at 45)     0.0235 **      0.0205 *       0.0219 **      0.0206 *  
    ##                                      (0.0084)       (0.0083)       (0.0083)       (0.0083)   
    ## University * Male                     0.0049         0.0046         0.0046         0.0046    
    ##                                      (0.0199)       (0.0198)       (0.0198)       (0.0198)   
    ## University * Age                     -0.0190        -0.0186        -0.0194        -0.0190    
    ##                                      (0.0123)       (0.0122)       (0.0122)       (0.0122)   
    ## University * Male * Age               0.0313 *       0.0305 *       0.0313 *       0.0311 *  
    ##                                      (0.0151)       (0.0150)       (0.0150)       (0.0150)   
    ## Male * Age                           -0.0273 **     -0.0243 *      -0.0253 *      -0.0245 *  
    ##                                      (0.0103)       (0.0102)       (0.0102)       (0.0102)   
    ## % of Life Residing Locally (zip)     -0.0897 *      -0.0788 *      -0.0838 *      -0.0814 *  
    ##                                      (0.0366)       (0.0366)       (0.0367)       (0.0367)   
    ## DID residence (zip)                                 -0.0066                       -0.0023    
    ##                                                     (0.0108)                      (0.0133)   
    ## Foreigner % sqrt. (zip)                              0.0063                       -0.0120    
    ##                                                     (0.0076)                      (0.0095)   
    ## University % by 10% (zip)                            0.0309 ***                    0.0195 *  
    ##                                                     (0.0063)                      (0.0088)   
    ## DID proportion (mun.)                                              -0.0221        -0.0175    
    ##                                                                    (0.0192)       (0.0235)   
    ## Foreigner % sqrt. (mun.)                                            0.0247 *       0.0365 ** 
    ##                                                                    (0.0109)       (0.0138)   
    ## University % by 10% (mun.)                                          0.0394 ***     0.0215 +  
    ##                                                                    (0.0090)       (0.0124)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0506         0.0576         0.0583         0.0600    
    ## Adj. R^2                              0.0435         0.0498         0.0505         0.0515    
    ## Num. obs.                          3786           3786           3786           3786         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

# With Matched Data (Lambda = 350km)

``` r
sifcct <- readRDS(datadir5)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.5000  0.5000  0.5701  0.6750  1.0000

``` r
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.04098 0.18484 0.40915 0.48501 0.78565 0.97505

## Outcome Model

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0237        -0.0237        -0.0237        -0.0237    
    ##                                      (0.0181)       (0.0181)       (0.0181)       (0.0181)   
    ## Gender (male)                        -0.0929 ***    -0.0949 ***    -0.0944 ***    -0.0947 ***
    ##                                      (0.0154)       (0.0155)       (0.0155)       (0.0156)   
    ## Age (by 10 years, centered at 45)    -0.0025        -0.0019        -0.0021        -0.0020    
    ##                                      (0.0092)       (0.0093)       (0.0093)       (0.0093)   
    ## University * Male                     0.0158         0.0158         0.0158         0.0158    
    ##                                      (0.0223)       (0.0223)       (0.0223)       (0.0224)   
    ## University * Age                     -0.0074        -0.0074        -0.0073        -0.0073    
    ##                                      (0.0131)       (0.0131)       (0.0131)       (0.0132)   
    ## University * Male * Age               0.0045         0.0045         0.0047         0.0047    
    ##                                      (0.0163)       (0.0163)       (0.0163)       (0.0164)   
    ## Male * Age                            0.0130         0.0125         0.0124         0.0123    
    ##                                      (0.0114)       (0.0114)       (0.0114)       (0.0114)   
    ## % of Life Residing Locally (zip)      0.0430         0.0409         0.0395         0.0393    
    ##                                      (0.0407)       (0.0408)       (0.0408)       (0.0408)   
    ## DID residence (zip)                                 -0.0083                       -0.0061    
    ##                                                     (0.0125)                      (0.0157)   
    ## Foreigner % sqrt. (zip)                              0.0013                       -0.0054    
    ##                                                     (0.0087)                      (0.0117)   
    ## University % by 10% (zip)                           -0.0046                       -0.0019    
    ##                                                     (0.0074)                      (0.0102)   
    ## DID proportion (mun.)                                              -0.0110        -0.0047    
    ##                                                                    (0.0218)       (0.0273)   
    ## Foreigner % sqrt. (mun.)                                            0.0107         0.0158    
    ##                                                                    (0.0127)       (0.0167)   
    ## University % by 10% (mun.)                                         -0.0073        -0.0054    
    ##                                                                    (0.0106)       (0.0143)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0218         0.0222         0.0223         0.0224    
    ## Adj. R^2                              0.0154         0.0150         0.0151         0.0146    
    ## Num. obs.                          4280           4280           4280           4280         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est                lci95              uci95             lci90         
    ##  Female:5   Min.   :25   Min.   :-0.038274   Min.   :-0.11295   Min.   :0.01172   Min.   :-0.10094  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.021901   1st Qu.:-0.05917   1st Qu.:0.01878   1st Qu.:-0.05303  
    ##             Median :45   Median :-0.011722   Median :-0.05309   Median :0.02420   Median :-0.04661  
    ##             Mean   :45   Mean   :-0.015805   Mean   :-0.05833   Mean   :0.02672   Mean   :-0.05149  
    ##             3rd Qu.:55   3rd Qu.:-0.008209   3rd Qu.:-0.04414   3rd Qu.:0.03559   3rd Qu.:-0.03841  
    ##             Max.   :65   Max.   :-0.002781   Max.   :-0.03367   Max.   :0.04323   Max.   :-0.02952  
    ##      uci90                se                p               lv           
    ##  Min.   :0.006024   Min.   :0.01315   Min.   :0.1896   Length:10         
    ##  1st Qu.:0.013199   1st Qu.:0.01651   1st Qu.:0.3186   Class :character  
    ##  Median :0.019037   Median :0.02077   Median :0.5364   Mode  :character  
    ##  Mean   :0.019884   Mean   :0.02169   Mean   :0.5089                     
    ##  3rd Qu.:0.025407   3rd Qu.:0.02407   3rd Qu.:0.6743                     
    ##  Max.   :0.035831   Max.   :0.03809   Max.   :0.9057

## Outcome Model 2

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base: Agree     Base: Neither   ZIP: Agree      ZIP: Neither  
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.1550 **      -0.4082         -0.1551 **      -0.4107    
    ##                                       (0.1366)        (0.1389)        (0.1366)        (0.1389)   
    ## Gender (male)                         -0.6468 ***     -0.7075 ***     -0.6606 ***     -0.7540 ***
    ##                                       (0.1168)        (0.1210)        (0.1181)        (0.1223)   
    ## Age (by 10 years, centered at 45)      0.0063         -0.1007          0.0110         -0.0893    
    ##                                       (0.0698)        (0.0734)        (0.0699)        (0.0733)   
    ## University * Male                      0.1901          0.2103          0.1904          0.2135    
    ##                                       (0.1661)        (0.1683)        (0.1661)        (0.1683)   
    ## University * Age                      -0.0821          0.0909         -0.0824          0.0888    
    ##                                       (0.0975)        (0.1003)        (0.0974)        (0.1003)   
    ## University * Male * Age                0.0318          0.0526          0.0323          0.0583    
    ##                                       (0.1205)        (0.1223)        (0.1205)        (0.1223)   
    ## Male * Age                             0.0976         -0.0059          0.0935         -0.0172    
    ##                                       (0.0851)        (0.0884)        (0.0852)        (0.0884)   
    ## % of Life Residing Locally (zip)       0.4482          0.2880          0.4317          0.2727    
    ##                                       (0.2980)        (0.3080)        (0.2990)        (0.3083)   
    ## DID residence (zip)                                                   -0.0162         -0.0708    
    ##                                                                       (0.0908)        (0.0914)   
    ## Foreigner % sqrt. (zip)                                                0.0115 +       -0.1067    
    ##                                                                       (0.0642)        (0.0613)   
    ## University % by 10% (zip)                                             -0.0406         -0.0815    
    ##                                                                       (0.0534)        (0.0540)   
    ## -------------------------------------------------------------------------------------------------
    ## AIC                                 9143.2694       9143.2694       9144.8164       9144.8164    
    ## Log Likelihood                     -4513.6347      -4513.6347      -4508.4082      -4508.4082    
    ## Num. obs.                           4280            4280            4280            4280         
    ## K                                      3               3               3               3         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Mun.: Agree     Mun.: Neither   Full: Agree     Full: Neither 
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.1548 **      -0.4108         -0.1550 **      -0.4126    
    ##                                       (0.1366)        (0.1389)        (0.1365)        (0.1390)   
    ## Gender (male)                         -0.6567 ***     -0.7549 ***     -0.6591 ***     -0.7656 ***
    ##                                       (0.1177)        (0.1222)        (0.1183)        (0.1228)   
    ## Age (by 10 years, centered at 45)      0.0088         -0.0905          0.0099         -0.0886    
    ##                                       (0.0699)        (0.0735)        (0.0700)        (0.0733)   
    ## University * Male                      0.1901          0.2139          0.1901          0.2157    
    ##                                       (0.1662)        (0.1683)        (0.1662)        (0.1683)   
    ## University * Age                      -0.0812          0.0869         -0.0813          0.0858    
    ##                                       (0.0975)        (0.1004)        (0.0974)        (0.1003)   
    ## University * Male * Age                0.0323          0.0586          0.0326          0.0640    
    ##                                       (0.1207)        (0.1223)        (0.1206)        (0.1223)   
    ## Male * Age                             0.0942         -0.0177          0.0934         -0.0218    
    ##                                       (0.0852)        (0.0885)        (0.0854)        (0.0885)   
    ## % of Life Residing Locally (zip)       0.4265          0.2673          0.4233          0.2641    
    ##                                       (0.2991)        (0.3078)        (0.2991)        (0.3080)   
    ## DID residence (zip)                                                   -0.0164          0.0508    
    ##                                                                       (0.1107)        (0.1138)   
    ## Foreigner % sqrt. (zip)                                               -0.0075 +       -0.1494    
    ##                                                                       (0.0862)        (0.0826)   
    ## University % by 10% (zip)                                             -0.0153         -0.0517    
    ##                                                                       (0.0724)        (0.0735)   
    ## DID proportion (mun.)                 -0.0013 +       -0.2928          0.0153 +       -0.3395    
    ##                                       (0.1615)        (0.1616)        (0.1959)        (0.2005)   
    ## Foreigner % sqrt. (mun.)               0.0453         -0.0150          0.0513          0.1230    
    ##                                       (0.0925)        (0.0916)        (0.1226)        (0.1215)   
    ## University % by 10% (mun.)            -0.0668         -0.0736         -0.0528         -0.0206    
    ##                                       (0.0785)        (0.0771)        (0.1036)        (0.1031)   
    ## -------------------------------------------------------------------------------------------------
    ## AIC                                 9143.3037       9143.3037       9151.4122       9151.4122    
    ## Log Likelihood                     -4507.6518      -4507.6518      -4505.7061      -4505.7061    
    ## Num. obs.                           4280            4280            4280            4280         
    ## K                                      3               3               3               3         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ##     gender       age          est               lci95             uci95            lci90             uci90        
    ##  Female:5   Min.   :25   Min.   :-0.31747   Min.   :-0.8854   Min.   :0.1175   Min.   :-0.7940   Min.   :0.07369  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.13464   1st Qu.:-0.4166   1st Qu.:0.1904   1st Qu.:-0.3708   1st Qu.:0.14646  
    ##             Median :45   Median :-0.03787   Median :-0.3459   Median :0.2358   Median :-0.2956   Median :0.18289  
    ##             Mean   :45   Mean   :-0.05990   Mean   :-0.3777   Mean   :0.2578   Mean   :-0.3265   Mean   :0.20674  
    ##             3rd Qu.:55   3rd Qu.: 0.02825   3rd Qu.:-0.2163   3rd Qu.:0.3065   3rd Qu.:-0.1660   3rd Qu.:0.26481  
    ##             Max.   :65   Max.   : 0.13251   Max.   :-0.1508   Max.   :0.4741   Max.   :-0.1161   Max.   :0.41920  
    ##        se                p               lv           
    ##  Min.   :0.09486   Min.   :0.2465   Length:10         
    ##  1st Qu.:0.12395   1st Qu.:0.3166   Class :character  
    ##  Median :0.15161   Median :0.5324   Mode  :character  
    ##  Mean   :0.16207   Mean   :0.5586                     
    ##  3rd Qu.:0.18444   3rd Qu.:0.7095                     
    ##  Max.   :0.28966   Max.   :0.9679

## Mediator Models

## Knowledge

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1548 ***     0.1548 ***     0.1547 ***     0.1549 ***
    ##                                      (0.0167)       (0.0167)       (0.0167)       (0.0167)   
    ## Gender (male)                         0.1915 ***     0.1979 ***     0.1958 ***     0.1985 ***
    ##                                      (0.0142)       (0.0143)       (0.0143)       (0.0143)   
    ## Age (by 10 years, centered at 45)     0.0573 ***     0.0553 ***     0.0562 ***     0.0554 ***
    ##                                      (0.0084)       (0.0084)       (0.0084)       (0.0084)   
    ## University * Male                    -0.0347 +      -0.0350 +      -0.0348 +      -0.0351 +  
    ##                                      (0.0199)       (0.0199)       (0.0199)       (0.0199)   
    ## University * Age                     -0.0190        -0.0188        -0.0192        -0.0188    
    ##                                      (0.0117)       (0.0117)       (0.0117)       (0.0117)   
    ## University * Male * Age              -0.0001        -0.0006        -0.0004        -0.0013    
    ##                                      (0.0143)       (0.0143)       (0.0142)       (0.0143)   
    ## Male * Age                            0.0045         0.0065         0.0059         0.0069    
    ##                                      (0.0103)       (0.0103)       (0.0103)       (0.0103)   
    ## % of Life Residing Locally (zip)     -0.1315 ***    -0.1257 ***    -0.1242 ***    -0.1231 ***
    ##                                      (0.0362)       (0.0362)       (0.0362)       (0.0362)   
    ## DID residence (zip)                                 -0.0138                       -0.0246 +  
    ##                                                     (0.0105)                      (0.0131)   
    ## Foreigner % sqrt. (zip)                              0.0064                        0.0167    
    ##                                                     (0.0077)                      (0.0107)   
    ## University % by 10% (zip)                            0.0212 ***                    0.0153 +  
    ##                                                     (0.0064)                      (0.0085)   
    ## DID proportion (mun.)                                               0.0036         0.0284    
    ##                                                                    (0.0183)       (0.0229)   
    ## Foreigner % sqrt. (mun.)                                           -0.0113        -0.0265 +  
    ##                                                                    (0.0108)       (0.0144)   
    ## University % by 10% (mun.)                                          0.0241 **      0.0092    
    ##                                                                    (0.0087)       (0.0113)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.1951         0.1975         0.1973         0.1987    
    ## Adj. R^2                              0.1898         0.1917         0.1915         0.1923    
    ## Num. obs.                          4280           4280           4280           4280         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Ideology

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base         ZIP          Municipality   Full         
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0043      -0.0045      -0.0046        -0.0048    
    ##                                      (0.0105)     (0.0105)     (0.0105)       (0.0105)   
    ## Gender (male)                        -0.0223 *    -0.0212 *    -0.0232 *      -0.0235 *  
    ##                                      (0.0093)     (0.0094)     (0.0093)       (0.0094)   
    ## Age (by 10 years, centered at 45)    -0.0095 +    -0.0101 +    -0.0094 +      -0.0095 +  
    ##                                      (0.0052)     (0.0052)     (0.0052)       (0.0052)   
    ## University * Male                     0.0162       0.0164       0.0166         0.0167    
    ##                                      (0.0136)     (0.0136)     (0.0136)       (0.0136)   
    ## University * Age                      0.0028       0.0027       0.0020         0.0017    
    ##                                      (0.0077)     (0.0077)     (0.0077)       (0.0077)   
    ## University * Male * Age               0.0033       0.0035       0.0038         0.0041    
    ##                                      (0.0101)     (0.0101)     (0.0100)       (0.0100)   
    ## Male * Age                            0.0017       0.0022       0.0018         0.0016    
    ##                                      (0.0068)     (0.0068)     (0.0068)       (0.0068)   
    ## % of Life Residing Locally (zip)      0.0191       0.0231       0.0257         0.0261    
    ##                                      (0.0253)     (0.0253)     (0.0253)       (0.0253)   
    ## DID residence (zip)                               -0.0013                      0.0166 +  
    ##                                                   (0.0081)                    (0.0096)   
    ## Foreigner % sqrt. (zip)                           -0.0101 +                   -0.0047    
    ##                                                   (0.0055)                    (0.0074)   
    ## University % by 10% (zip)                          0.0077 +                   -0.0007    
    ##                                                   (0.0046)                    (0.0061)   
    ## DID proportion (mun.)                                          -0.0414 **     -0.0580 ***
    ##                                                                (0.0145)       (0.0172)   
    ## Foreigner % sqrt. (mun.)                                       -0.0160 *      -0.0117    
    ##                                                                (0.0080)       (0.0105)   
    ## University % by 10% (mun.)                                      0.0230 ***     0.0240 ** 
    ##                                                                (0.0068)       (0.0088)   
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0087       0.0101       0.0129         0.0137    
    ## Adj. R^2                              0.0022       0.0028       0.0057         0.0058    
    ## Num. obs.                          4280         4280         4280           4280         
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## LDP - DPJ FT

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                 -0.0097       -0.0097       -0.0098       -0.0098   
    ##                                      (0.0091)      (0.0091)      (0.0091)      (0.0091)  
    ## Gender (male)                         0.0106        0.0112        0.0105        0.0107   
    ##                                      (0.0075)      (0.0076)      (0.0076)      (0.0076)  
    ## Age (by 10 years, centered at 45)    -0.0009       -0.0011       -0.0009       -0.0009   
    ##                                      (0.0044)      (0.0044)      (0.0044)      (0.0044)  
    ## University * Male                     0.0194 +      0.0194 +      0.0195 +      0.0195 + 
    ##                                      (0.0113)      (0.0113)      (0.0113)      (0.0113)  
    ## University * Age                     -0.0053       -0.0053       -0.0054       -0.0055   
    ##                                      (0.0065)      (0.0065)      (0.0066)      (0.0066)  
    ## University * Male * Age               0.0080        0.0079        0.0080        0.0080   
    ##                                      (0.0082)      (0.0082)      (0.0083)      (0.0083)  
    ## Male * Age                           -0.0150 **    -0.0148 **    -0.0150 **    -0.0149 **
    ##                                      (0.0056)      (0.0056)      (0.0056)      (0.0056)  
    ## % of Life Residing Locally (zip)      0.0133        0.0137        0.0141        0.0143   
    ##                                      (0.0201)      (0.0202)      (0.0202)      (0.0202)  
    ## DID residence (zip)                                 0.0021                      0.0055   
    ##                                                    (0.0064)                    (0.0081)  
    ## Foreigner % sqrt. (zip)                             0.0007                      0.0032   
    ##                                                    (0.0044)                    (0.0059)  
    ## University % by 10% (zip)                           0.0010                      0.0011   
    ##                                                    (0.0037)                    (0.0050)  
    ## DID proportion (mun.)                                            -0.0045       -0.0102   
    ##                                                                  (0.0111)      (0.0140)  
    ## Foreigner % sqrt. (mun.)                                         -0.0017       -0.0047   
    ##                                                                  (0.0064)      (0.0085)  
    ## University % by 10% (mun.)                                        0.0025        0.0015   
    ##                                                                  (0.0054)      (0.0072)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0981        0.0982        0.0982        0.0984   
    ## Adj. R^2                              0.0921        0.0916        0.0916        0.0912   
    ## Num. obs.                          4280          4280          4280          4280        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of South Korea

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                 -0.0007        -0.0007        -0.0007        -0.0007    
    ##                                      (0.0141)       (0.0141)       (0.0141)       (0.0141)   
    ## Gender (male)                        -0.0460 ***    -0.0454 ***    -0.0439 ***    -0.0448 ***
    ##                                      (0.0118)       (0.0119)       (0.0119)       (0.0120)   
    ## Age (by 10 years, centered at 45)    -0.0017        -0.0018        -0.0021        -0.0018    
    ##                                      (0.0074)       (0.0075)       (0.0075)       (0.0075)   
    ## University * Male                    -0.0047        -0.0048        -0.0048        -0.0048    
    ##                                      (0.0170)       (0.0170)       (0.0170)       (0.0170)   
    ## University * Age                     -0.0005        -0.0005        -0.0005        -0.0005    
    ##                                      (0.0105)       (0.0105)       (0.0105)       (0.0105)   
    ## University * Male * Age              -0.0053        -0.0054        -0.0054        -0.0052    
    ##                                      (0.0127)       (0.0127)       (0.0127)       (0.0127)   
    ## Male * Age                            0.0294 **      0.0295 **      0.0298 ***     0.0294 ** 
    ##                                      (0.0090)       (0.0090)       (0.0090)       (0.0090)   
    ## % of Life Residing Locally (zip)      0.0130         0.0131         0.0131         0.0125    
    ##                                      (0.0317)       (0.0317)       (0.0318)       (0.0318)   
    ## DID residence (zip)                                 -0.0019                       -0.0022    
    ##                                                     (0.0092)                      (0.0115)   
    ## Foreigner % sqrt. (zip)                              0.0022                       -0.0063    
    ##                                                     (0.0064)                      (0.0085)   
    ## University % by 10% (zip)                            0.0014                       -0.0057    
    ##                                                     (0.0056)                      (0.0076)   
    ## DID proportion (mun.)                                              -0.0046        -0.0024    
    ##                                                                    (0.0162)       (0.0200)   
    ## Foreigner % sqrt. (mun.)                                            0.0103         0.0161    
    ##                                                                    (0.0095)       (0.0126)   
    ## University % by 10% (mun.)                                          0.0072         0.0127    
    ##                                                                    (0.0081)       (0.0107)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0753         0.0753         0.0759         0.0762    
    ## Adj. R^2                              0.0692         0.0686         0.0691         0.0688    
    ## Num. obs.                          4280           4280           4280           4280         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of China

``` r
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
```

    ## 
    ## =========================================================================================
    ##                                    Base          ZIP           Municipality  Full        
    ## -----------------------------------------------------------------------------------------
    ## University education                  0.0020        0.0020        0.0019        0.0018   
    ##                                      (0.0116)      (0.0116)      (0.0116)      (0.0116)  
    ## Gender (male)                        -0.0062       -0.0089       -0.0086       -0.0098   
    ##                                      (0.0101)      (0.0102)      (0.0102)      (0.0102)  
    ## Age (by 10 years, centered at 45)    -0.0184 **    -0.0177 **    -0.0179 **    -0.0175 **
    ##                                      (0.0062)      (0.0062)      (0.0062)      (0.0062)  
    ## University * Male                     0.0009        0.0010        0.0011        0.0012   
    ##                                      (0.0141)      (0.0142)      (0.0142)      (0.0141)  
    ## University * Age                      0.0050        0.0049        0.0047        0.0045   
    ##                                      (0.0086)      (0.0086)      (0.0086)      (0.0086)  
    ## University * Male * Age              -0.0093       -0.0090       -0.0090       -0.0086   
    ##                                      (0.0106)      (0.0106)      (0.0106)      (0.0106)  
    ## Male * Age                            0.0190 *      0.0183 *      0.0185 *      0.0180 * 
    ##                                      (0.0075)      (0.0075)      (0.0075)      (0.0076)  
    ## % of Life Residing Locally (zip)     -0.0047       -0.0059       -0.0049       -0.0055   
    ##                                      (0.0265)      (0.0265)      (0.0265)      (0.0265)  
    ## DID residence (zip)                                -0.0002                      0.0086   
    ##                                                    (0.0078)                    (0.0094)  
    ## Foreigner % sqrt. (zip)                            -0.0063                     -0.0071   
    ##                                                    (0.0055)                    (0.0074)  
    ## University % by 10% (zip)                          -0.0059                     -0.0072   
    ##                                                    (0.0047)                    (0.0063)  
    ## DID proportion (mun.)                                            -0.0180       -0.0267   
    ##                                                                  (0.0138)      (0.0166)  
    ## Foreigner % sqrt. (mun.)                                         -0.0036        0.0028   
    ##                                                                  (0.0076)      (0.0101)  
    ## University % by 10% (mun.)                                       -0.0012        0.0057   
    ##                                                                  (0.0068)      (0.0089)  
    ## -----------------------------------------------------------------------------------------
    ## R^2                                   0.0349        0.0357        0.0358        0.0363   
    ## Adj. R^2                              0.0285        0.0287        0.0288        0.0286   
    ## Num. obs.                          4280          4280          4280          4280        
    ## =========================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of USA

``` r
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
```

    ## 
    ## ============================================================================================
    ##                                    Base          ZIP            Municipality   Full         
    ## --------------------------------------------------------------------------------------------
    ## University education                 -0.0081       -0.0082        -0.0081        -0.0082    
    ##                                      (0.0117)      (0.0117)       (0.0117)       (0.0117)   
    ## Gender (male)                         0.0332 **     0.0344 ***     0.0357 ***     0.0349 ***
    ##                                      (0.0101)      (0.0102)       (0.0102)       (0.0102)   
    ## Age (by 10 years, centered at 45)     0.0024        0.0019         0.0018         0.0020    
    ##                                      (0.0060)      (0.0060)       (0.0060)       (0.0061)   
    ## University * Male                     0.0135        0.0135         0.0134         0.0135    
    ##                                      (0.0144)      (0.0144)       (0.0144)       (0.0145)   
    ## University * Age                     -0.0078       -0.0078        -0.0078        -0.0078    
    ##                                      (0.0086)      (0.0086)       (0.0086)       (0.0086)   
    ## University * Male * Age               0.0130        0.0130         0.0128         0.0131    
    ##                                      (0.0106)      (0.0106)       (0.0106)       (0.0106)   
    ## Male * Age                            0.0056        0.0060         0.0063         0.0060    
    ##                                      (0.0075)      (0.0075)       (0.0075)       (0.0075)   
    ## % of Life Residing Locally (zip)     -0.0282       -0.0261        -0.0256        -0.0260    
    ##                                      (0.0266)      (0.0267)       (0.0267)       (0.0268)   
    ## DID residence (zip)                                 0.0028                        0.0008    
    ##                                                    (0.0082)                      (0.0100)   
    ## Foreigner % sqrt. (zip)                            -0.0031                       -0.0080    
    ##                                                    (0.0058)                      (0.0077)   
    ## University % by 10% (zip)                           0.0042                       -0.0040    
    ##                                                    (0.0049)                      (0.0065)   
    ## DID proportion (mun.)                                              0.0026         0.0019    
    ##                                                                   (0.0149)       (0.0181)   
    ## Foreigner % sqrt. (mun.)                                           0.0001         0.0075    
    ##                                                                   (0.0083)       (0.0107)   
    ## University % by 10% (mun.)                                         0.0108         0.0147    
    ##                                                                   (0.0073)       (0.0094)   
    ## --------------------------------------------------------------------------------------------
    ## R^2                                   0.0255        0.0258         0.0265         0.0269    
    ## Adj. R^2                              0.0191        0.0187         0.0194         0.0191    
    ## Num. obs.                          4280          4280           4280           4280         
    ## ============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Income

``` r
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
```

    ## 
    ## =============================================================================================
    ##                                    Base           ZIP            Municipality   Full         
    ## ---------------------------------------------------------------------------------------------
    ## University education                  0.1107 ***     0.1107 ***     0.1108 ***     0.1107 ***
    ##                                      (0.0154)       (0.0153)       (0.0153)       (0.0153)   
    ## Gender (male)                        -0.0049         0.0054         0.0043         0.0068    
    ##                                      (0.0126)       (0.0127)       (0.0127)       (0.0127)   
    ## Age (by 10 years, centered at 45)     0.0237 **      0.0205 **      0.0217 **      0.0204 ** 
    ##                                      (0.0079)       (0.0078)       (0.0078)       (0.0078)   
    ## University * Male                    -0.0060        -0.0063        -0.0065        -0.0064    
    ##                                      (0.0187)       (0.0186)       (0.0186)       (0.0186)   
    ## University * Age                     -0.0170        -0.0167        -0.0168        -0.0166    
    ##                                      (0.0115)       (0.0114)       (0.0114)       (0.0114)   
    ## University * Male * Age               0.0271 +       0.0264 +       0.0267 +       0.0269 +  
    ##                                      (0.0141)       (0.0140)       (0.0139)       (0.0140)   
    ## Male * Age                           -0.0263 **     -0.0233 *      -0.0242 *      -0.0235 *  
    ##                                      (0.0096)       (0.0095)       (0.0095)       (0.0095)   
    ## % of Life Residing Locally (zip)     -0.0872 *      -0.0775 *      -0.0827 *      -0.0800 *  
    ##                                      (0.0343)       (0.0343)       (0.0344)       (0.0344)   
    ## DID residence (zip)                                 -0.0061                       -0.0067    
    ##                                                     (0.0101)                      (0.0124)   
    ## Foreigner % sqrt. (zip)                              0.0061                       -0.0148 +  
    ##                                                     (0.0071)                      (0.0090)   
    ## University % by 10% (zip)                            0.0312 ***                    0.0198 *  
    ##                                                     (0.0060)                      (0.0084)   
    ## DID proportion (mun.)                                              -0.0118        -0.0032    
    ##                                                                    (0.0180)       (0.0221)   
    ## Foreigner % sqrt. (mun.)                                            0.0269 **      0.0414 ** 
    ##                                                                    (0.0103)       (0.0131)   
    ## University % by 10% (mun.)                                          0.0369 ***     0.0190    
    ##                                                                    (0.0087)       (0.0119)   
    ## ---------------------------------------------------------------------------------------------
    ## R^2                                   0.0496         0.0565         0.0577         0.0596    
    ## Adj. R^2                              0.0434         0.0496         0.0508         0.0521    
    ## Num. obs.                          4280           4280           4280           4280         
    ## =============================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

# Organizing Outcomes

## OLS

``` r
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
```

``` r
saveRDS(visdt, paste0(projdir, "/out/visdt.rds"))
```

## Multinomial Logit

``` r
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
```

``` r
saveRDS(visdtm, paste0(projdir, "/out/visdtm.rds"))
```

## Combining OLS and Multinomial Logit

``` r
visdt$method = "OLS"
visdtm$method = "Multinomial Logit\nAgree vs. Disagree"
visdtall <- rbind(visdt,visdtm)
visdtall$method <- factor(visdtall$method, levels = unique(visdtall$method))
colnames(visdtall)
```

    ##  [1] "gender" "age"    "est"    "lci95"  "uci95"  "lci90"  "uci90"  "se"     "p"      "lv"     "data"   "pstar" 
    ## [13] "method"

## Including Mail

``` r
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
```

    ##  [1] "gender" "age"    "est"    "lci95"  "uci95"  "lci90"  "uci90"  "se"     "p"      "lv"     "data"   "pstar" 
    ## [13] "method"

``` r
visdtall <- rbind(visdtall,visdt_mail)
visdtall$data <- factor(visdtall$data, levels = unique(visdtall$data))
table(visdtall$data)
```

    ## 
    ##                       Unmatched Matched without \nDistance Adj.    Matched with \nLambda = 50km 
    ##                              20                              20                              20 
    ##   Matched with \nLambda = 100km   Matched with \nLambda = 200km   Matched with \nLambda = 350km 
    ##                              20                              20                              20 
    ##                         Mail-in 
    ##                              20

``` r
saveRDS(visdtall, paste0(projdir, "/out/visdtall.rds"))
```

# Save Image

``` r
save.image(file=paste0(projdir,"/out/heavy/analysis_2_matched_v5.RData"))
```
