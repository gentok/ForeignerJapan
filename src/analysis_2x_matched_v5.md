Analysis 2x: Main Analysis with Matched Data (Movers)
================
Fan Lu & Gento Kato
January 8, 2020

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

If age - years of local ZIP residence is 23 or larger. 23 is the age of
graduating university (the youngest possible) in Japan. Assuming that an
individual is living in the local ZIP continuously, this condition
implies that one moved to the ZIP of current residence (likely) after
graduating the university. This incorporates the possibility that
education changes attitudes through the movement in residence.

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
```

# With Unmatched Data

``` r
sifcct <- readRDS(datadir0x)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.5000  0.5000  0.5573  0.6500  1.0000

``` r
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.04098 0.18484 0.40915 0.51647 0.78565 0.97505

## Outcome Model

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.0019         -0.0002         -0.0012         -0.0002    
    ##                                       (0.0063)        (0.0064)        (0.0063)        (0.0064)   
    ## Gender (male)                         -0.0560 ***     -0.0566 ***     -0.0564 ***     -0.0566 ***
    ##                                       (0.0076)        (0.0076)        (0.0076)        (0.0076)   
    ## Age (by 10 years, centered at 45)     -0.0126 ***     -0.0122 ***     -0.0124 ***     -0.0123 ***
    ##                                       (0.0034)        (0.0034)        (0.0034)        (0.0034)   
    ## University * Male                     -0.0208 *       -0.0204 *       -0.0206 *       -0.0205 *  
    ##                                       (0.0098)        (0.0098)        (0.0098)        (0.0098)   
    ## University * Age                       0.0125 *        0.0122 *        0.0123 *        0.0122 *  
    ##                                       (0.0051)        (0.0051)        (0.0051)        (0.0051)   
    ## University * Male * Age               -0.0045         -0.0041         -0.0043         -0.0041    
    ##                                       (0.0076)        (0.0076)        (0.0076)        (0.0076)   
    ## Male * Age                             0.0170 **       0.0166 **       0.0167 **       0.0166 ** 
    ##                                       (0.0057)        (0.0057)        (0.0057)        (0.0057)   
    ## % of Life Residing Locally (zip)      -0.0276 +       -0.0307 *       -0.0290 +       -0.0305 *  
    ##                                       (0.0149)        (0.0149)        (0.0149)        (0.0150)   
    ## DID residence (zip)                                   -0.0162 **                      -0.0190 ** 
    ##                                                       (0.0056)                        (0.0066)   
    ## Foreigner % sqrt. (zip)                               -0.0037                         -0.0021    
    ##                                                       (0.0039)                        (0.0054)   
    ## University % by 10% (zip)                             -0.0001                         -0.0024    
    ##                                                       (0.0025)                        (0.0036)   
    ## DID proportion (mun.)                                                 -0.0103          0.0077    
    ##                                                                       (0.0101)        (0.0119)   
    ## Foreigner % sqrt. (mun.)                                              -0.0071         -0.0049    
    ##                                                                       (0.0053)        (0.0074)   
    ## University % by 10% (mun.)                                             0.0012          0.0036    
    ##                                                                       (0.0038)        (0.0052)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0122          0.0127          0.0124          0.0128    
    ## Adj. R^2                               0.0110          0.0115          0.0111          0.0114    
    ## Num. obs.                          24147           24147           24147           24147         
    ## =================================================================================================
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
```

    ##     gender       age          est                lci95               uci95                lci90          
    ##  Female:5   Min.   :25   Min.   :-0.038697   Min.   :-0.070512   Min.   :-0.0084444   Min.   :-0.065397  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.025859   1st Qu.:-0.048402   1st Qu.:-0.0056096   1st Qu.:-0.044777  
    ##             Median :45   Median :-0.014550   Median :-0.029892   Median : 0.0007912   Median :-0.027425  
    ##             Mean   :45   Mean   :-0.012305   Mean   :-0.031594   Mean   : 0.0069831   Mean   :-0.028493  
    ##             3rd Qu.:55   3rd Qu.:-0.003113   3rd Qu.:-0.017312   3rd Qu.: 0.0124302   3rd Qu.:-0.015029  
    ##             Max.   :65   Max.   : 0.023066   Max.   : 0.001465   Max.   : 0.0446669   Max.   : 0.004938  
    ##      uci90                 se                 p                 lv           
    ##  Min.   :-0.012021   Min.   :0.006296   Min.   :0.002659   Length:10         
    ##  1st Qu.:-0.009158   1st Qu.:0.007436   1st Qu.:0.021370   Class :character  
    ##  Median :-0.001675   Median :0.009408   Median :0.036055   Mode  :character  
    ##  Mean   : 0.003882   Mean   :0.009841   Mean   :0.165248                     
    ##  3rd Qu.: 0.009550   3rd Qu.:0.011268   3rd Qu.:0.139450                     
    ##  Max.   : 0.041194   Max.   :0.016232   Max.   :0.759719

## Outcome Model 2

``` r
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
```

    ## 
    ## =====================================================================================================
    ##                                    Base: Agree      Base: Neither    ZIP: Agree       ZIP: Neither   
    ## -----------------------------------------------------------------------------------------------------
    ## University education                    0.0063 ***      -0.3391           0.0199 ***      -0.3256    
    ##                                        (0.0498)         (0.0503)         (0.0503)         (0.0508)   
    ## Gender (male)                          -0.3609 ***      -0.5868 ***      -0.3662 ***      -0.5918 ***
    ##                                        (0.0563)         (0.0580)         (0.0563)         (0.0581)   
    ## Age (by 10 years, centered at 45)      -0.0835 ***      -0.1487 **       -0.0807 ***      -0.1470 ** 
    ##                                        (0.0271)         (0.0281)         (0.0272)         (0.0281)   
    ## University * Male                      -0.1262           0.0215 +        -0.1232           0.0239 +  
    ##                                        (0.0734)         (0.0738)         (0.0734)         (0.0738)   
    ## University * Age                        0.0947 *         0.1007 *         0.0927 *         0.0997 *  
    ##                                        (0.0398)         (0.0402)         (0.0398)         (0.0402)   
    ## University * Male * Age                -0.0331          -0.0377          -0.0301          -0.0370    
    ##                                        (0.0569)         (0.0563)         (0.0569)         (0.0564)   
    ## Male * Age                              0.1272           0.0550 **        0.1244           0.0535 ** 
    ##                                        (0.0428)         (0.0431)         (0.0428)         (0.0431)   
    ## % of Life Residing Locally (zip)       -0.2106          -0.0032 +        -0.2329          -0.0243 *  
    ##                                        (0.1123)         (0.1081)         (0.1128)         (0.1085)   
    ## DID residence (zip)                                                      -0.1274          -0.0373 ** 
    ##                                                                          (0.0418)         (0.0409)   
    ## Foreigner % sqrt. (zip)                                                  -0.0176          -0.0425    
    ##                                                                          (0.0290)         (0.0280)   
    ## University % by 10% (zip)                                                -0.0036          -0.0168    
    ##                                                                          (0.0185)         (0.0183)   
    ## -----------------------------------------------------------------------------------------------------
    ## AIC                                 51942.8378       51942.8378       51938.2602       51938.2602    
    ## Log Likelihood                     -25913.4189      -25913.4189      -25905.1301      -25905.1301    
    ## Num. obs.                           24147            24147            24147            24147         
    ## K                                       3                3                3                3         
    ## =====================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ## 
    ## =====================================================================================================
    ##                                    Mun.: Agree      Mun.: Neither    Full: Agree      Full: Neither  
    ## -----------------------------------------------------------------------------------------------------
    ## University education                    0.0126 ***      -0.3328           0.0197 ***      -0.3261    
    ##                                        (0.0502)         (0.0506)         (0.0503)         (0.0508)   
    ## Gender (male)                          -0.3639 ***      -0.5893 ***      -0.3657 ***      -0.5914 ***
    ##                                        (0.0563)         (0.0581)         (0.0563)         (0.0581)   
    ## Age (by 10 years, centered at 45)      -0.0823 ***      -0.1478 **       -0.0813 ***      -0.1472 ** 
    ##                                        (0.0272)         (0.0281)         (0.0272)         (0.0281)   
    ## University * Male                      -0.1245           0.0226 +        -0.1238           0.0237 +  
    ##                                        (0.0734)         (0.0738)         (0.0734)         (0.0738)   
    ## University * Age                        0.0935 *         0.1001 *         0.0932 *         0.1004 *  
    ##                                        (0.0398)         (0.0402)         (0.0398)         (0.0402)   
    ## University * Male * Age                -0.0317          -0.0369          -0.0306          -0.0381    
    ##                                        (0.0569)         (0.0563)         (0.0569)         (0.0563)   
    ## Male * Age                              0.1256           0.0543 **        0.1248           0.0541 ** 
    ##                                        (0.0428)         (0.0431)         (0.0428)         (0.0431)   
    ## % of Life Residing Locally (zip)       -0.2215          -0.0098 *        -0.2318          -0.0260 *  
    ##                                        (0.1126)         (0.1083)         (0.1130)         (0.1087)   
    ## DID residence (zip)                                                      -0.1480          -0.0490 ** 
    ##                                                                          (0.0494)         (0.0483)   
    ## Foreigner % sqrt. (zip)                                                  -0.0169 +        -0.0685    
    ##                                                                          (0.0403)         (0.0391)   
    ## University % by 10% (zip)                                                -0.0157          -0.0262    
    ##                                                                          (0.0263)         (0.0261)   
    ## DID proportion (mun.)                  -0.0794          -0.0162           0.0607           0.0327    
    ##                                        (0.0752)         (0.0739)         (0.0887)         (0.0871)   
    ## Foreigner % sqrt. (mun.)               -0.0296          -0.0201          -0.0114           0.0436    
    ##                                        (0.0394)         (0.0388)         (0.0539)         (0.0536)   
    ## University % by 10% (mun.)              0.0003          -0.0162           0.0163           0.0113    
    ##                                        (0.0280)         (0.0275)         (0.0384)         (0.0380)   
    ## -----------------------------------------------------------------------------------------------------
    ## AIC                                 51951.2486       51951.2486       51948.0233       51948.0233    
    ## Log Likelihood                     -25911.6243      -25911.6243      -25904.0117      -25904.0117    
    ## Num. obs.                           24147            24147            24147            24147         
    ## K                                       3                3                3                3         
    ## =====================================================================================================
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
```

    ##     gender       age          est                lci95              uci95               lci90         
    ##  Female:5   Min.   :25   Min.   :-0.243140   Min.   :-0.47155   Min.   :-0.021194   Min.   :-0.43483  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.166098   1st Qu.:-0.31315   1st Qu.:-0.005643   1st Qu.:-0.28920  
    ##             Median :45   Median :-0.073352   Median :-0.18847   Median : 0.042493   Median :-0.17057  
    ##             Mean   :45   Mean   :-0.056795   Mean   :-0.20097   Mean   : 0.087382   Mean   :-0.17779  
    ##             3rd Qu.:55   3rd Qu.: 0.005561   3rd Qu.:-0.10201   3rd Qu.: 0.129604   3rd Qu.:-0.08472  
    ##             Max.   :65   Max.   : 0.195778   Max.   : 0.02856   Max.   : 0.363000   Max.   : 0.05544  
    ##      uci90                se                p                lv           
    ##  Min.   :-0.05145   Min.   :0.04746   Min.   :0.02175   Length:10         
    ##  1st Qu.:-0.02656   1st Qu.:0.05482   1st Qu.:0.02910   Class :character  
    ##  Median : 0.02387   Median :0.06967   Median :0.07742   Mode  :character  
    ##  Mean   : 0.06420   Mean   :0.07356   Mean   :0.25585                     
    ##  3rd Qu.: 0.10943   3rd Qu.:0.08443   3rd Qu.:0.21760                     
    ##  Max.   : 0.33611   Max.   :0.11653   Max.   :0.96055

## Mediator Models

## Knowledge

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.1545 ***      0.1438 ***      0.1467 ***      0.1436 ***
    ##                                       (0.0058)        (0.0059)        (0.0059)        (0.0059)   
    ## Gender (male)                          0.1738 ***      0.1781 ***      0.1767 ***      0.1780 ***
    ##                                       (0.0069)        (0.0069)        (0.0069)        (0.0069)   
    ## Age (by 10 years, centered at 45)      0.0668 ***      0.0656 ***      0.0658 ***      0.0655 ***
    ##                                       (0.0032)        (0.0032)        (0.0032)        (0.0032)   
    ## University * Male                      0.0035          0.0017          0.0023          0.0018    
    ##                                       (0.0088)        (0.0087)        (0.0087)        (0.0087)   
    ## University * Age                      -0.0119 *       -0.0123 **      -0.0116 *       -0.0121 ** 
    ##                                       (0.0047)        (0.0047)        (0.0047)        (0.0047)   
    ## University * Male * Age               -0.0238 ***     -0.0238 ***     -0.0244 ***     -0.0240 ***
    ##                                       (0.0065)        (0.0065)        (0.0065)        (0.0065)   
    ## Male * Age                             0.0196 ***      0.0199 ***      0.0200 ***      0.0199 ***
    ##                                       (0.0050)        (0.0049)        (0.0049)        (0.0049)   
    ## % of Life Residing Locally (zip)      -0.0369 **      -0.0235 +       -0.0307 *       -0.0250 *  
    ##                                       (0.0127)        (0.0127)        (0.0127)        (0.0127)   
    ## DID residence (zip)                                    0.0079 +                        0.0108 +  
    ##                                                       (0.0047)                        (0.0056)   
    ## Foreigner % sqrt. (zip)                                0.0092 **                       0.0068    
    ##                                                       (0.0032)                        (0.0046)   
    ## University % by 10% (zip)                              0.0234 ***                      0.0179 ***
    ##                                                       (0.0021)                        (0.0030)   
    ## DID proportion (mun.)                                                 -0.0041         -0.0135    
    ##                                                                       (0.0086)        (0.0101)   
    ## Foreigner % sqrt. (mun.)                                               0.0086 +        0.0028    
    ##                                                                       (0.0044)        (0.0061)   
    ## University % by 10% (mun.)                                             0.0298 ***      0.0118 ** 
    ##                                                                       (0.0032)        (0.0044)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.2267          0.2325          0.2312          0.2328    
    ## Adj. R^2                               0.2258          0.2315          0.2302          0.2317    
    ## Num. obs.                          24147           24147           24147           24147         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Ideology

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.0132 ***     -0.0130 ***     -0.0125 **      -0.0129 ***
    ##                                       (0.0039)        (0.0039)        (0.0039)        (0.0039)   
    ## Gender (male)                         -0.0377 ***     -0.0378 ***     -0.0379 ***     -0.0379 ***
    ##                                       (0.0052)        (0.0052)        (0.0052)        (0.0052)   
    ## Age (by 10 years, centered at 45)     -0.0082 ***     -0.0083 ***     -0.0081 ***     -0.0082 ***
    ##                                       (0.0022)        (0.0022)        (0.0022)        (0.0022)   
    ## University * Male                      0.0229 ***      0.0229 ***      0.0230 ***      0.0230 ***
    ##                                       (0.0065)        (0.0065)        (0.0065)        (0.0065)   
    ## University * Age                      -0.0061 +       -0.0059 +       -0.0061 +       -0.0060 +  
    ##                                       (0.0032)        (0.0032)        (0.0032)        (0.0032)   
    ## University * Male * Age               -0.0025         -0.0028         -0.0024         -0.0026    
    ##                                       (0.0050)        (0.0050)        (0.0050)        (0.0050)   
    ## Male * Age                             0.0144 ***      0.0146 ***      0.0144 ***      0.0145 ***
    ##                                       (0.0039)        (0.0039)        (0.0039)        (0.0039)   
    ## % of Life Residing Locally (zip)       0.0184 +        0.0184 +        0.0178 +        0.0182 +  
    ##                                       (0.0098)        (0.0098)        (0.0098)        (0.0098)   
    ## DID residence (zip)                                    0.0096 **                       0.0144 ***
    ##                                                       (0.0036)                        (0.0043)   
    ## Foreigner % sqrt. (zip)                               -0.0017                          0.0001    
    ##                                                       (0.0025)                        (0.0035)   
    ## University % by 10% (zip)                             -0.0023                         -0.0003    
    ##                                                       (0.0016)                        (0.0023)   
    ## DID proportion (mun.)                                                 -0.0009         -0.0146 +  
    ##                                                                       (0.0065)        (0.0077)   
    ## Foreigner % sqrt. (mun.)                                              -0.0012         -0.0016    
    ##                                                                       (0.0035)        (0.0047)   
    ## University % by 10% (mun.)                                            -0.0023         -0.0020    
    ##                                                                       (0.0024)        (0.0033)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0069          0.0072          0.0069          0.0074    
    ## Adj. R^2                               0.0057          0.0059          0.0057          0.0060    
    ## Num. obs.                          24147           24147           24147           24147         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## LDP - DPJ FT

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.0122 ***     -0.0131 ***     -0.0125 ***     -0.0131 ***
    ##                                       (0.0030)        (0.0030)        (0.0030)        (0.0030)   
    ## Gender (male)                          0.0195 ***      0.0198 ***      0.0197 ***      0.0198 ***
    ##                                       (0.0037)        (0.0037)        (0.0037)        (0.0037)   
    ## Age (by 10 years, centered at 45)      0.0018          0.0016          0.0017          0.0016    
    ##                                       (0.0017)        (0.0017)        (0.0017)        (0.0017)   
    ## University * Male                      0.0093 +        0.0091 +        0.0092 +        0.0091 +  
    ##                                       (0.0047)        (0.0047)        (0.0047)        (0.0047)   
    ## University * Age                      -0.0097 ***     -0.0095 ***     -0.0096 ***     -0.0096 ***
    ##                                       (0.0025)        (0.0025)        (0.0025)        (0.0025)   
    ## University * Male * Age                0.0028          0.0027          0.0027          0.0027    
    ##                                       (0.0038)        (0.0038)        (0.0038)        (0.0038)   
    ## Male * Age                            -0.0072 *       -0.0070 *       -0.0070 *       -0.0070 *  
    ##                                       (0.0029)        (0.0029)        (0.0029)        (0.0029)   
    ## % of Life Residing Locally (zip)      -0.0062         -0.0046         -0.0056         -0.0043    
    ##                                       (0.0075)        (0.0075)        (0.0075)        (0.0075)   
    ## DID residence (zip)                                    0.0062 *                        0.0071 *  
    ##                                                       (0.0028)                        (0.0033)   
    ## Foreigner % sqrt. (zip)                                0.0038 *                        0.0049 +  
    ##                                                       (0.0019)                        (0.0027)   
    ## University % by 10% (zip)                              0.0001                          0.0018    
    ##                                                       (0.0012)                        (0.0018)   
    ## DID proportion (mun.)                                                  0.0050         -0.0019    
    ##                                                                       (0.0049)        (0.0058)   
    ## Foreigner % sqrt. (mun.)                                               0.0036         -0.0010    
    ##                                                                       (0.0026)        (0.0036)   
    ## University % by 10% (mun.)                                            -0.0011         -0.0031    
    ##                                                                       (0.0018)        (0.0025)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.1203          0.1207          0.1204          0.1208    
    ## Adj. R^2                               0.1192          0.1196          0.1193          0.1196    
    ## Num. obs.                          24147           24147           24147           24147         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of South Korea

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.0100 *        0.0104 *        0.0099 +        0.0103 *  
    ##                                       (0.0050)        (0.0051)        (0.0050)        (0.0051)   
    ## Gender (male)                         -0.0589 ***     -0.0591 ***     -0.0590 ***     -0.0591 ***
    ##                                       (0.0057)        (0.0057)        (0.0057)        (0.0058)   
    ## Age (by 10 years, centered at 45)     -0.0015         -0.0014         -0.0015         -0.0014    
    ##                                       (0.0027)        (0.0027)        (0.0027)        (0.0027)   
    ## University * Male                      0.0077          0.0078          0.0078          0.0078    
    ##                                       (0.0074)        (0.0074)        (0.0074)        (0.0074)   
    ## University * Age                      -0.0001         -0.0002         -0.0001         -0.0001    
    ##                                       (0.0040)        (0.0040)        (0.0040)        (0.0040)   
    ## University * Male * Age                0.0004          0.0006          0.0005          0.0005    
    ##                                       (0.0057)        (0.0057)        (0.0057)        (0.0057)   
    ## Male * Age                             0.0272 ***      0.0271 ***      0.0272 ***      0.0271 ***
    ##                                       (0.0043)        (0.0043)        (0.0043)        (0.0043)   
    ## % of Life Residing Locally (zip)      -0.0209 +       -0.0218 *       -0.0215 *       -0.0222 *  
    ##                                       (0.0108)        (0.0109)        (0.0108)        (0.0109)   
    ## DID residence (zip)                                   -0.0083 *                       -0.0082 +  
    ##                                                       (0.0041)                        (0.0049)   
    ## Foreigner % sqrt. (zip)                                0.0004                         -0.0023    
    ##                                                       (0.0028)                        (0.0039)   
    ## University % by 10% (zip)                              0.0005                         -0.0009    
    ##                                                       (0.0018)                        (0.0026)   
    ## DID proportion (mun.)                                                 -0.0092         -0.0013    
    ##                                                                       (0.0073)        (0.0087)   
    ## Foreigner % sqrt. (mun.)                                               0.0025          0.0048    
    ##                                                                       (0.0039)        (0.0052)   
    ## University % by 10% (mun.)                                             0.0016          0.0026    
    ##                                                                       (0.0027)        (0.0038)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0684          0.0686          0.0685          0.0686    
    ## Adj. R^2                               0.0673          0.0674          0.0673          0.0673    
    ## Num. obs.                          24147           24147           24147           24147         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of China

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.0201 ***      0.0196 ***      0.0190 ***      0.0194 ***
    ##                                       (0.0043)        (0.0043)        (0.0043)        (0.0043)   
    ## Gender (male)                         -0.0137 **      -0.0136 **      -0.0134 **      -0.0136 ** 
    ##                                       (0.0049)        (0.0050)        (0.0050)        (0.0050)   
    ## Age (by 10 years, centered at 45)     -0.0000         -0.0001         -0.0001         -0.0001    
    ##                                       (0.0024)        (0.0024)        (0.0024)        (0.0024)   
    ## University * Male                      0.0029          0.0029          0.0028          0.0029    
    ##                                       (0.0064)        (0.0064)        (0.0064)        (0.0064)   
    ## University * Age                      -0.0024         -0.0024         -0.0023         -0.0022    
    ##                                       (0.0034)        (0.0034)        (0.0034)        (0.0034)   
    ## University * Male * Age                0.0014          0.0015          0.0014          0.0013    
    ##                                       (0.0049)        (0.0049)        (0.0049)        (0.0049)   
    ## Male * Age                             0.0070 +        0.0070 +        0.0071 +        0.0071 +  
    ##                                       (0.0037)        (0.0037)        (0.0037)        (0.0037)   
    ## % of Life Residing Locally (zip)      -0.0257 **      -0.0250 **      -0.0252 **      -0.0259 ** 
    ##                                       (0.0094)        (0.0094)        (0.0094)        (0.0094)   
    ## DID residence (zip)                                   -0.0022                         -0.0014    
    ##                                                       (0.0035)                        (0.0041)   
    ## Foreigner % sqrt. (zip)                                0.0027                         -0.0008    
    ##                                                       (0.0024)                        (0.0033)   
    ## University % by 10% (zip)                              0.0009                         -0.0024    
    ##                                                       (0.0016)                        (0.0022)   
    ## DID proportion (mun.)                                                 -0.0064         -0.0052    
    ##                                                                       (0.0064)        (0.0075)   
    ## Foreigner % sqrt. (mun.)                                               0.0049          0.0055    
    ##                                                                       (0.0034)        (0.0046)   
    ## University % by 10% (mun.)                                             0.0042 +        0.0066 *  
    ##                                                                       (0.0024)        (0.0033)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0241          0.0242          0.0244          0.0244    
    ## Adj. R^2                               0.0230          0.0229          0.0231          0.0231    
    ## Num. obs.                          24147           24147           24147           24147         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of USA

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.0128 **       0.0090 *        0.0104 *        0.0091 *  
    ##                                       (0.0041)        (0.0041)        (0.0041)        (0.0041)   
    ## Gender (male)                          0.0061          0.0077          0.0070          0.0077    
    ##                                       (0.0052)        (0.0052)        (0.0052)        (0.0052)   
    ## Age (by 10 years, centered at 45)      0.0080 ***      0.0076 ***      0.0077 ***      0.0076 ***
    ##                                       (0.0023)        (0.0023)        (0.0023)        (0.0023)   
    ## University * Male                      0.0215 ***      0.0209 **       0.0211 **       0.0209 ** 
    ##                                       (0.0065)        (0.0065)        (0.0065)        (0.0065)   
    ## University * Age                      -0.0045         -0.0048         -0.0044         -0.0048    
    ##                                       (0.0032)        (0.0032)        (0.0032)        (0.0032)   
    ## University * Male * Age               -0.0034         -0.0032         -0.0036         -0.0033    
    ##                                       (0.0049)        (0.0049)        (0.0049)        (0.0049)   
    ## Male * Age                             0.0207 ***      0.0206 ***      0.0208 ***      0.0207 ***
    ##                                       (0.0038)        (0.0038)        (0.0038)        (0.0038)   
    ## % of Life Residing Locally (zip)      -0.0151         -0.0107         -0.0130         -0.0104    
    ##                                       (0.0095)        (0.0095)        (0.0095)        (0.0095)   
    ## DID residence (zip)                                   -0.0036                         -0.0058    
    ##                                                       (0.0036)                        (0.0043)   
    ## Foreigner % sqrt. (zip)                                0.0030                          0.0025    
    ##                                                       (0.0025)                        (0.0034)   
    ## University % by 10% (zip)                              0.0096 ***                      0.0098 ***
    ##                                                       (0.0016)                        (0.0023)   
    ## DID proportion (mun.)                                                  0.0014          0.0075    
    ##                                                                       (0.0065)        (0.0076)   
    ## Foreigner % sqrt. (mun.)                                               0.0024          0.0006    
    ##                                                                       (0.0035)        (0.0047)   
    ## University % by 10% (mun.)                                             0.0086 ***     -0.0012    
    ##                                                                       (0.0024)        (0.0033)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0324          0.0341          0.0334          0.0341    
    ## Adj. R^2                               0.0313          0.0328          0.0321          0.0328    
    ## Num. obs.                          24147           24147           24147           24147         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Income

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.0961 ***      0.0747 ***      0.0807 ***      0.0742 ***
    ##                                       (0.0054)        (0.0053)        (0.0053)        (0.0053)   
    ## Gender (male)                         -0.0430 ***     -0.0342 ***     -0.0374 ***     -0.0340 ***
    ##                                       (0.0059)        (0.0058)        (0.0058)        (0.0058)   
    ## Age (by 10 years, centered at 45)     -0.0049 +       -0.0072 *       -0.0069 *       -0.0072 *  
    ##                                       (0.0028)        (0.0028)        (0.0028)        (0.0028)   
    ## University * Male                      0.0385 ***      0.0350 ***      0.0362 ***      0.0351 ***
    ##                                       (0.0078)        (0.0076)        (0.0077)        (0.0076)   
    ## University * Age                       0.0332 ***      0.0320 ***      0.0342 ***      0.0325 ***
    ##                                       (0.0044)        (0.0043)        (0.0044)        (0.0043)   
    ## University * Male * Age               -0.0167 **      -0.0160 **      -0.0178 **      -0.0168 ** 
    ##                                       (0.0060)        (0.0060)        (0.0060)        (0.0060)   
    ## Male * Age                            -0.0056         -0.0055         -0.0045         -0.0051    
    ##                                       (0.0043)        (0.0043)        (0.0043)        (0.0043)   
    ## % of Life Residing Locally (zip)       0.1046 ***      0.1305 ***      0.1169 ***      0.1279 ***
    ##                                       (0.0127)        (0.0125)        (0.0126)        (0.0125)   
    ## DID residence (zip)                                   -0.0031                         -0.0028    
    ##                                                       (0.0044)                        (0.0052)   
    ## Foreigner % sqrt. (zip)                                0.0169 ***                     -0.0042    
    ##                                                       (0.0031)                        (0.0042)   
    ## University % by 10% (zip)                              0.0514 ***                      0.0455 ***
    ##                                                       (0.0020)                        (0.0029)   
    ## DID proportion (mun.)                                                 -0.0118         -0.0055    
    ##                                                                       (0.0080)        (0.0094)   
    ## Foreigner % sqrt. (mun.)                                               0.0326 ***      0.0386 ***
    ##                                                                       (0.0043)        (0.0059)   
    ## University % by 10% (mun.)                                             0.0549 ***      0.0095 *  
    ##                                                                       (0.0031)        (0.0042)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0537          0.0847          0.0765          0.0866    
    ## Adj. R^2                               0.0526          0.0835          0.0754          0.0853    
    ## Num. obs.                          24147           24147           24147           24147         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

# With Matched Data (Without Distance Adjustment)

``` r
sifcct <- readRDS(datadir1x)
sifcct$agex <- sifcct$age/10 - 4.5
sifcct$ldpdpjft <- original$ldpdpjft[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$ldpdpjft)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.5000  0.5000  0.5564  0.6500  1.0000

``` r
sifcct$income <- original$income[match(paste(sifcct$id,sifcct$wave),paste(original$id,original$wave))]
summary(sifcct$income)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.04098 0.18484 0.40915 0.50495 0.78565 0.97505

## Outcome Model

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.0063         -0.0048         -0.0057         -0.0048    
    ##                                       (0.0071)        (0.0072)        (0.0072)        (0.0072)   
    ## Gender (male)                         -0.0589 ***     -0.0592 ***     -0.0590 ***     -0.0591 ***
    ##                                       (0.0081)        (0.0081)        (0.0081)        (0.0081)   
    ## Age (by 10 years, centered at 45)     -0.0091 *       -0.0088 *       -0.0092 *       -0.0090 *  
    ##                                       (0.0044)        (0.0044)        (0.0045)        (0.0045)   
    ## University * Male                     -0.0238 *       -0.0234 *       -0.0238 *       -0.0236 *  
    ##                                       (0.0118)        (0.0118)        (0.0118)        (0.0118)   
    ## University * Age                       0.0075          0.0070          0.0072          0.0071    
    ##                                       (0.0060)        (0.0060)        (0.0060)        (0.0060)   
    ## University * Male * Age                0.0039          0.0042          0.0040          0.0041    
    ##                                       (0.0091)        (0.0091)        (0.0091)        (0.0091)   
    ## Male * Age                             0.0118 +        0.0113 +        0.0116 +        0.0114 +  
    ##                                       (0.0063)        (0.0063)        (0.0063)        (0.0063)   
    ## % of Life Residing Locally (zip)      -0.0067         -0.0103         -0.0075         -0.0096    
    ##                                       (0.0192)        (0.0192)        (0.0192)        (0.0193)   
    ## DID residence (zip)                                   -0.0139 *                       -0.0200 *  
    ##                                                       (0.0069)                        (0.0082)   
    ## Foreigner % sqrt. (zip)                               -0.0090 +                       -0.0070    
    ##                                                       (0.0049)                        (0.0068)   
    ## University % by 10% (zip)                              0.0016                         -0.0010    
    ##                                                       (0.0032)                        (0.0045)   
    ## DID proportion (mun.)                                                 -0.0005          0.0188    
    ##                                                                       (0.0126)        (0.0149)   
    ## Foreigner % sqrt. (mun.)                                              -0.0133 *       -0.0064    
    ##                                                                       (0.0067)        (0.0093)   
    ## University % by 10% (mun.)                                             0.0019          0.0031    
    ##                                                                       (0.0048)        (0.0066)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0133          0.0139          0.0136          0.0141    
    ## Adj. R^2                               0.0115          0.0119          0.0116          0.0119    
    ## Num. obs.                          15252           15252           15252           15252         
    ## =================================================================================================
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
```

    ##     gender       age          est                lci95              uci95               lci90         
    ##  Female:5   Min.   :25   Min.   :-0.052828   Min.   :-0.09244   Min.   :-0.013634   Min.   :-0.08607  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.027854   1st Qu.:-0.04975   1st Qu.:-0.009429   1st Qu.:-0.04553  
    ##             Median :45   Median :-0.016217   Median :-0.03395   Median : 0.006637   Median :-0.03110  
    ##             Mean   :45   Mean   :-0.018163   Mean   :-0.04119   Mean   : 0.004863   Mean   :-0.03749  
    ##             3rd Qu.:55   3rd Qu.:-0.006526   3rd Qu.:-0.02278   3rd Qu.: 0.013895   3rd Qu.:-0.02017  
    ##             Max.   :65   Max.   : 0.008715   Max.   :-0.01585   Max.   : 0.034568   Max.   :-0.01298  
    ##      uci90                 se                 p                 lv           
    ##  Min.   :-0.019585   Min.   :0.007107   Min.   :0.001321   Length:10         
    ##  1st Qu.:-0.012287   1st Qu.:0.008871   1st Qu.:0.012454   Class :character  
    ##  Median : 0.002760   Median :0.010875   Median :0.156826   Mode  :character  
    ##  Mean   : 0.001161   Mean   :0.011748   Mean   :0.266470                     
    ##  3rd Qu.: 0.010528   3rd Qu.:0.013936   3rd Qu.:0.475869                     
    ##  Max.   : 0.030411   Max.   :0.020209   Max.   :0.888705

## Outcome Model 2

``` r
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
```

    ## 
    ## =====================================================================================================
    ##                                    Base: Agree      Base: Neither    ZIP: Agree       ZIP: Neither   
    ## -----------------------------------------------------------------------------------------------------
    ## University education                   -0.0154 ***      -0.3494          -0.0065 ***      -0.3375    
    ##                                        (0.0563)         (0.0571)         (0.0570)         (0.0579)   
    ## Gender (male)                          -0.3766 ***      -0.5889 ***      -0.3785 ***      -0.5919 ***
    ##                                        (0.0607)         (0.0627)         (0.0607)         (0.0628)   
    ## Age (by 10 years, centered at 45)      -0.0666 ***      -0.1437 +        -0.0643 ***      -0.1432 +  
    ##                                        (0.0350)         (0.0366)         (0.0351)         (0.0367)   
    ## University * Male                      -0.1583           0.0201 +        -0.1560           0.0216 +  
    ##                                        (0.0870)         (0.0873)         (0.0870)         (0.0874)   
    ## University * Age                        0.0621 +         0.0857           0.0584 +         0.0844    
    ##                                        (0.0469)         (0.0480)         (0.0469)         (0.0480)   
    ## University * Male * Age                 0.0235          -0.0014           0.0262          -0.0024    
    ##                                        (0.0679)         (0.0676)         (0.0679)         (0.0676)   
    ## Male * Age                              0.0966           0.0351 *         0.0933           0.0343 +  
    ##                                        (0.0477)         (0.0487)         (0.0477)         (0.0487)   
    ## % of Life Residing Locally (zip)       -0.0466           0.1499          -0.0701           0.1302    
    ##                                        (0.1436)         (0.1383)         (0.1442)         (0.1389)   
    ## DID residence (zip)                                                      -0.1058           0.0173 *  
    ##                                                                          (0.0524)         (0.0511)   
    ## Foreigner % sqrt. (zip)                                                  -0.0553 *        -0.0738    
    ##                                                                          (0.0366)         (0.0355)   
    ## University % by 10% (zip)                                                 0.0151          -0.0143    
    ##                                                                          (0.0236)         (0.0234)   
    ## -----------------------------------------------------------------------------------------------------
    ## AIC                                 32956.9671       32956.9671       32956.4506       32956.4506    
    ## Log Likelihood                     -16420.4836      -16420.4836      -16414.2253      -16414.2253    
    ## Num. obs.                           15252            15252            15252            15252         
    ## K                                       3                3                3                3         
    ## =====================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

``` r
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
```

    ## 
    ## =====================================================================================================
    ##                                    Mun.: Agree      Mun.: Neither    Full: Agree      Full: Neither  
    ## -----------------------------------------------------------------------------------------------------
    ## University education                   -0.0118 ***      -0.3405          -0.0063 ***      -0.3367    
    ##                                        (0.0568)         (0.0577)         (0.0570)         (0.0579)   
    ## Gender (male)                          -0.3769 ***      -0.5903 ***      -0.3770 ***      -0.5905 ***
    ##                                        (0.0607)         (0.0628)         (0.0607)         (0.0629)   
    ## Age (by 10 years, centered at 45)      -0.0670 ***      -0.1445 +        -0.0655 ***      -0.1440 +  
    ##                                        (0.0351)         (0.0367)         (0.0351)         (0.0367)   
    ## University * Male                      -0.1587           0.0186 +        -0.1571           0.0201 +  
    ##                                        (0.0870)         (0.0874)         (0.0870)         (0.0874)   
    ## University * Age                        0.0607 +         0.0851           0.0595 +         0.0850    
    ##                                        (0.0469)         (0.0480)         (0.0469)         (0.0480)   
    ## University * Male * Age                 0.0240          -0.0016           0.0248          -0.0030    
    ##                                        (0.0679)         (0.0676)         (0.0680)         (0.0676)   
    ## Male * Age                              0.0958           0.0356 *         0.0946           0.0354 +  
    ##                                        (0.0477)         (0.0487)         (0.0477)         (0.0487)   
    ## % of Life Residing Locally (zip)       -0.0493           0.1521          -0.0637           0.1389    
    ##                                        (0.1440)         (0.1386)         (0.1444)         (0.1391)   
    ## DID residence (zip)                                                      -0.1606          -0.0330 ** 
    ##                                                                          (0.0618)         (0.0605)   
    ## Foreigner % sqrt. (zip)                                                  -0.0594          -0.0788    
    ##                                                                          (0.0519)         (0.0505)   
    ## University % by 10% (zip)                                                 0.0043          -0.0073    
    ##                                                                          (0.0336)         (0.0333)   
    ## DID proportion (mun.)                   0.0201           0.1358           0.1757           0.1708    
    ##                                        (0.0942)         (0.0925)         (0.1109)         (0.1092)   
    ## Foreigner % sqrt. (mun.)               -0.0690          -0.0727          -0.0092           0.0016    
    ##                                        (0.0496)         (0.0487)         (0.0692)         (0.0682)   
    ## University % by 10% (mun.)              0.0032          -0.0420          -0.0000          -0.0333    
    ##                                        (0.0357)         (0.0352)         (0.0493)         (0.0485)   
    ## -----------------------------------------------------------------------------------------------------
    ## AIC                                 32963.5875       32963.5875       32964.1731       32964.1731    
    ## Log Likelihood                     -16417.7937      -16417.7937      -16412.0865      -16412.0865    
    ## Num. obs.                           15252            15252            15252            15252         
    ## K                                       3                3                3                3         
    ## =====================================================================================================
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
```

    ##     gender       age          est                lci95              uci95              lci90         
    ##  Female:5   Min.   :25   Min.   :-0.344897   Min.   :-0.62524   Min.   :-0.06456   Min.   :-0.58016  
    ##  Male  :5   1st Qu.:35   1st Qu.:-0.165143   1st Qu.:-0.35545   1st Qu.:-0.02789   1st Qu.:-0.32211  
    ##             Median :45   Median :-0.082760   Median :-0.21581   Median : 0.08655   Median :-0.19441  
    ##             Mean   :45   Mean   :-0.094508   Mean   :-0.26585   Mean   : 0.07683   Mean   :-0.23830  
    ##             3rd Qu.:55   3rd Qu.:-0.005643   3rd Qu.:-0.13522   3rd Qu.: 0.13969   3rd Qu.:-0.11543  
    ##             Max.   :65   Max.   : 0.108890   Max.   :-0.08799   Max.   : 0.31257   Max.   :-0.06204  
    ##      uci90                se                p                 lv           
    ##  Min.   :-0.10963   Min.   :0.05569   Min.   :0.008546   Length:10         
    ##  1st Qu.:-0.04788   1st Qu.:0.06671   1st Qu.:0.040427   Class :character  
    ##  Median : 0.05518   Median :0.07994   Median :0.267730   Mode  :character  
    ##  Mean   : 0.04928   Mean   :0.08741   Mean   :0.327735                     
    ##  3rd Qu.: 0.11633   3rd Qu.:0.10311   3rd Qu.:0.455550                     
    ##  Max.   : 0.27982   Max.   :0.14302   Max.   :0.975988

## Mediator Models

## Knowledge

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.1555 ***      0.1449 ***      0.1476 ***      0.1447 ***
    ##                                       (0.0067)        (0.0067)        (0.0067)        (0.0067)   
    ## Gender (male)                          0.1737 ***      0.1776 ***      0.1764 ***      0.1775 ***
    ##                                       (0.0075)        (0.0074)        (0.0075)        (0.0074)   
    ## Age (by 10 years, centered at 45)      0.0725 ***      0.0713 ***      0.0716 ***      0.0713 ***
    ##                                       (0.0041)        (0.0041)        (0.0041)        (0.0041)   
    ## University * Male                      0.0017          0.0002          0.0013          0.0005    
    ##                                       (0.0103)        (0.0103)        (0.0103)        (0.0103)   
    ## University * Age                      -0.0180 **      -0.0182 **      -0.0176 **      -0.0181 ** 
    ##                                       (0.0056)        (0.0055)        (0.0056)        (0.0055)   
    ## University * Male * Age               -0.0184 *       -0.0182 *       -0.0191 *       -0.0184 *  
    ##                                       (0.0078)        (0.0077)        (0.0077)        (0.0077)   
    ## Male * Age                             0.0151 **       0.0153 **       0.0154 **       0.0153 ** 
    ##                                       (0.0056)        (0.0055)        (0.0056)        (0.0055)   
    ## % of Life Residing Locally (zip)      -0.0509 **      -0.0370 *       -0.0447 **      -0.0386 *  
    ##                                       (0.0161)        (0.0162)        (0.0161)        (0.0162)   
    ## DID residence (zip)                                    0.0032                          0.0069    
    ##                                                       (0.0060)                        (0.0070)   
    ## Foreigner % sqrt. (zip)                                0.0120 **                       0.0121 *  
    ##                                                       (0.0042)                        (0.0059)   
    ## University % by 10% (zip)                              0.0226 ***                      0.0161 ***
    ##                                                       (0.0027)                        (0.0039)   
    ## DID proportion (mun.)                                                 -0.0106         -0.0166    
    ##                                                                       (0.0109)        (0.0127)   
    ## Foreigner % sqrt. (mun.)                                               0.0082         -0.0025    
    ##                                                                       (0.0056)        (0.0079)   
    ## University % by 10% (mun.)                                             0.0307 ***      0.0143 *  
    ##                                                                       (0.0041)        (0.0057)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.2213          0.2266          0.2256          0.2269    
    ## Adj. R^2                               0.2199          0.2250          0.2240          0.2252    
    ## Num. obs.                          15252           15252           15252           15252         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Ideology

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.0142 **      -0.0135 **      -0.0139 **      -0.0137 ** 
    ##                                       (0.0044)        (0.0044)        (0.0044)        (0.0044)   
    ## Gender (male)                         -0.0384 ***     -0.0385 ***     -0.0386 ***     -0.0388 ***
    ##                                       (0.0055)        (0.0055)        (0.0055)        (0.0055)   
    ## Age (by 10 years, centered at 45)     -0.0103 ***     -0.0104 ***     -0.0102 ***     -0.0103 ***
    ##                                       (0.0029)        (0.0029)        (0.0029)        (0.0029)   
    ## University * Male                      0.0258 ***      0.0258 ***      0.0259 ***      0.0260 ***
    ##                                       (0.0077)        (0.0077)        (0.0077)        (0.0077)   
    ## University * Age                      -0.0037         -0.0035         -0.0038         -0.0036    
    ##                                       (0.0038)        (0.0038)        (0.0038)        (0.0038)   
    ## University * Male * Age               -0.0068         -0.0071         -0.0066         -0.0071    
    ##                                       (0.0060)        (0.0060)        (0.0060)        (0.0060)   
    ## Male * Age                             0.0159 ***      0.0160 ***      0.0157 ***      0.0159 ***
    ##                                       (0.0042)        (0.0042)        (0.0042)        (0.0042)   
    ## % of Life Residing Locally (zip)       0.0259 *        0.0254 *        0.0248 *        0.0241 +  
    ##                                       (0.0125)        (0.0126)        (0.0125)        (0.0126)   
    ## DID residence (zip)                                    0.0103 *                        0.0176 ** 
    ##                                                       (0.0045)                        (0.0054)   
    ## Foreigner % sqrt. (zip)                               -0.0046                         -0.0042    
    ##                                                       (0.0032)                        (0.0044)   
    ## University % by 10% (zip)                             -0.0027                         -0.0041    
    ##                                                       (0.0020)                        (0.0029)   
    ## DID proportion (mun.)                                                 -0.0085         -0.0252 ** 
    ##                                                                       (0.0081)        (0.0096)   
    ## Foreigner % sqrt. (mun.)                                              -0.0031          0.0003    
    ##                                                                       (0.0043)        (0.0060)   
    ## University % by 10% (mun.)                                             0.0014          0.0056    
    ##                                                                       (0.0031)        (0.0043)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0083          0.0087          0.0084          0.0092    
    ## Adj. R^2                               0.0065          0.0067          0.0064          0.0070    
    ## Num. obs.                          15252           15252           15252           15252         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## LDP - DPJ FT

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                  -0.0108 **      -0.0116 ***     -0.0114 ***     -0.0117 ***
    ##                                       (0.0034)        (0.0035)        (0.0035)        (0.0035)   
    ## Gender (male)                          0.0187 ***      0.0189 ***      0.0189 ***      0.0189 ***
    ##                                       (0.0039)        (0.0040)        (0.0040)        (0.0040)   
    ## Age (by 10 years, centered at 45)      0.0047 *        0.0046 *        0.0047 *        0.0046 *  
    ##                                       (0.0022)        (0.0022)        (0.0022)        (0.0022)   
    ## University * Male                      0.0096 +        0.0095 +        0.0096 +        0.0096 +  
    ##                                       (0.0056)        (0.0056)        (0.0056)        (0.0056)   
    ## University * Age                      -0.0142 ***     -0.0140 ***     -0.0141 ***     -0.0140 ***
    ##                                       (0.0030)        (0.0030)        (0.0030)        (0.0030)   
    ## University * Male * Age                0.0053          0.0053          0.0053          0.0053    
    ##                                       (0.0045)        (0.0045)        (0.0045)        (0.0045)   
    ## Male * Age                            -0.0103 **      -0.0102 **      -0.0103 **      -0.0102 ** 
    ##                                       (0.0032)        (0.0032)        (0.0032)        (0.0032)   
    ## % of Life Residing Locally (zip)      -0.0055         -0.0039         -0.0051         -0.0043    
    ##                                       (0.0095)        (0.0096)        (0.0095)        (0.0096)   
    ## DID residence (zip)                                    0.0033                          0.0054    
    ##                                                       (0.0035)                        (0.0041)   
    ## Foreigner % sqrt. (zip)                                0.0040 +                        0.0036    
    ##                                                       (0.0024)                        (0.0034)   
    ## University % by 10% (zip)                              0.0002                          0.0002    
    ##                                                       (0.0016)                        (0.0022)   
    ## DID proportion (mun.)                                                 -0.0017         -0.0070    
    ##                                                                       (0.0062)        (0.0073)   
    ## Foreigner % sqrt. (mun.)                                               0.0047          0.0013    
    ##                                                                       (0.0032)        (0.0045)   
    ## University % by 10% (mun.)                                             0.0011          0.0009    
    ##                                                                       (0.0023)        (0.0032)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.1193          0.1197          0.1195          0.1197    
    ## Adj. R^2                               0.1177          0.1179          0.1177          0.1177    
    ## Num. obs.                          15252           15252           15252           15252         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of South Korea

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.0096 +        0.0103 +        0.0099 +        0.0103 +  
    ##                                       (0.0057)        (0.0057)        (0.0057)        (0.0057)   
    ## Gender (male)                         -0.0589 ***     -0.0591 ***     -0.0591 ***     -0.0591 ***
    ##                                       (0.0062)        (0.0062)        (0.0062)        (0.0062)   
    ## Age (by 10 years, centered at 45)     -0.0003         -0.0001         -0.0002         -0.0001    
    ##                                       (0.0035)        (0.0035)        (0.0035)        (0.0035)   
    ## University * Male                     -0.0011         -0.0009         -0.0010         -0.0008    
    ##                                       (0.0087)        (0.0087)        (0.0087)        (0.0087)   
    ## University * Age                      -0.0005         -0.0007         -0.0006         -0.0006    
    ##                                       (0.0047)        (0.0047)        (0.0047)        (0.0047)   
    ## University * Male * Age                0.0061          0.0062          0.0062          0.0062    
    ##                                       (0.0067)        (0.0067)        (0.0067)        (0.0067)   
    ## Male * Age                             0.0266 ***      0.0264 ***      0.0265 ***      0.0264 ***
    ##                                       (0.0047)        (0.0047)        (0.0047)        (0.0047)   
    ## % of Life Residing Locally (zip)      -0.0230 +       -0.0244 +       -0.0238 +       -0.0247 +  
    ##                                       (0.0137)        (0.0137)        (0.0137)        (0.0137)   
    ## DID residence (zip)                                   -0.0069                         -0.0070    
    ##                                                       (0.0051)                        (0.0060)   
    ## Foreigner % sqrt. (zip)                               -0.0010                         -0.0022    
    ##                                                       (0.0035)                        (0.0049)   
    ## University % by 10% (zip)                              0.0000                         -0.0010    
    ##                                                       (0.0023)                        (0.0033)   
    ## DID proportion (mun.)                                                 -0.0070         -0.0004    
    ##                                                                       (0.0092)        (0.0109)   
    ## Foreigner % sqrt. (mun.)                                              -0.0003          0.0018    
    ##                                                                       (0.0048)        (0.0066)   
    ## University % by 10% (mun.)                                             0.0008          0.0018    
    ##                                                                       (0.0035)        (0.0049)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0698          0.0699          0.0698          0.0699    
    ## Adj. R^2                               0.0681          0.0680          0.0679          0.0679    
    ## Num. obs.                          15252           15252           15252           15252         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of China

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.0165 ***      0.0163 ***      0.0159 **       0.0162 ***
    ##                                       (0.0048)        (0.0049)        (0.0049)        (0.0049)   
    ## Gender (male)                         -0.0181 ***     -0.0180 ***     -0.0179 ***     -0.0181 ***
    ##                                       (0.0053)        (0.0053)        (0.0053)        (0.0053)   
    ## Age (by 10 years, centered at 45)     -0.0002         -0.0003         -0.0003         -0.0003    
    ##                                       (0.0030)        (0.0030)        (0.0030)        (0.0030)   
    ## University * Male                      0.0007          0.0006          0.0007          0.0008    
    ##                                       (0.0075)        (0.0075)        (0.0075)        (0.0075)   
    ## University * Age                      -0.0025         -0.0025         -0.0025         -0.0024    
    ##                                       (0.0041)        (0.0041)        (0.0041)        (0.0041)   
    ## University * Male * Age                0.0035          0.0035          0.0035          0.0034    
    ##                                       (0.0058)        (0.0058)        (0.0058)        (0.0058)   
    ## Male * Age                             0.0073 +        0.0074 +        0.0073 +        0.0074 +  
    ##                                       (0.0042)        (0.0042)        (0.0042)        (0.0042)   
    ## % of Life Residing Locally (zip)      -0.0238 *       -0.0234 *       -0.0236 *       -0.0242 *  
    ##                                       (0.0118)        (0.0119)        (0.0119)        (0.0119)   
    ## DID residence (zip)                                    0.0024                          0.0046    
    ##                                                       (0.0044)                        (0.0052)   
    ## Foreigner % sqrt. (zip)                                0.0009                          0.0005    
    ##                                                       (0.0031)                        (0.0043)   
    ## University % by 10% (zip)                             -0.0003                         -0.0032    
    ##                                                       (0.0020)                        (0.0028)   
    ## DID proportion (mun.)                                                 -0.0046         -0.0092    
    ##                                                                       (0.0079)        (0.0093)   
    ## Foreigner % sqrt. (mun.)                                               0.0008          0.0000    
    ##                                                                       (0.0042)        (0.0057)   
    ## University % by 10% (mun.)                                             0.0033          0.0065    
    ##                                                                       (0.0030)        (0.0041)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0259          0.0259          0.0260          0.0261    
    ## Adj. R^2                               0.0241          0.0240          0.0240          0.0239    
    ## Num. obs.                          15252           15252           15252           15252         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Favorability of USA

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.0108 *        0.0068          0.0083 +        0.0069    
    ##                                       (0.0046)        (0.0047)        (0.0047)        (0.0047)   
    ## Gender (male)                          0.0040          0.0057          0.0050          0.0057    
    ##                                       (0.0056)        (0.0056)        (0.0056)        (0.0056)   
    ## Age (by 10 years, centered at 45)      0.0104 ***      0.0100 ***      0.0100 ***      0.0100 ***
    ##                                       (0.0030)        (0.0030)        (0.0030)        (0.0030)   
    ## University * Male                      0.0186 *        0.0181 *        0.0184 *        0.0180 *  
    ##                                       (0.0077)        (0.0077)        (0.0077)        (0.0077)   
    ## University * Age                      -0.0063         -0.0066 +       -0.0062         -0.0066 +  
    ##                                       (0.0039)        (0.0039)        (0.0039)        (0.0039)   
    ## University * Male * Age                0.0036          0.0038          0.0034          0.0038    
    ##                                       (0.0058)        (0.0058)        (0.0058)        (0.0058)   
    ## Male * Age                             0.0189 ***      0.0188 ***      0.0189 ***      0.0188 ***
    ##                                       (0.0042)        (0.0042)        (0.0042)        (0.0042)   
    ## % of Life Residing Locally (zip)      -0.0254 *       -0.0210 +       -0.0234 +       -0.0207 +  
    ##                                       (0.0122)        (0.0122)        (0.0122)        (0.0122)   
    ## DID residence (zip)                                   -0.0032                         -0.0044    
    ##                                                       (0.0046)                        (0.0053)   
    ## Foreigner % sqrt. (zip)                                0.0004                          0.0019    
    ##                                                       (0.0031)                        (0.0044)   
    ## University % by 10% (zip)                              0.0104 ***                      0.0101 ***
    ##                                                       (0.0020)                        (0.0029)   
    ## DID proportion (mun.)                                                 -0.0008          0.0040    
    ##                                                                       (0.0082)        (0.0095)   
    ## Foreigner % sqrt. (mun.)                                              -0.0021         -0.0032    
    ##                                                                       (0.0043)        (0.0060)   
    ## University % by 10% (mun.)                                             0.0105 ***      0.0003    
    ##                                                                       (0.0031)        (0.0043)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0323          0.0341          0.0333          0.0341    
    ## Adj. R^2                               0.0305          0.0321          0.0313          0.0319    
    ## Num. obs.                          15252           15252           15252           15252         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

## Income

``` r
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
```

    ## 
    ## =================================================================================================
    ##                                    Base            ZIP             Municipality    Full          
    ## -------------------------------------------------------------------------------------------------
    ## University education                   0.1103 ***      0.0886 ***      0.0946 ***      0.0881 ***
    ##                                       (0.0060)        (0.0060)        (0.0060)        (0.0060)   
    ## Gender (male)                         -0.0342 ***     -0.0258 ***     -0.0289 ***     -0.0257 ***
    ##                                       (0.0063)        (0.0063)        (0.0063)        (0.0063)   
    ## Age (by 10 years, centered at 45)      0.0145 ***      0.0123 ***      0.0128 ***      0.0124 ***
    ##                                       (0.0037)        (0.0037)        (0.0037)        (0.0037)   
    ## University * Male                      0.0259 **       0.0229 *        0.0251 **       0.0234 ** 
    ##                                       (0.0091)        (0.0090)        (0.0091)        (0.0090)   
    ## University * Age                       0.0081          0.0071          0.0092 +        0.0076    
    ##                                       (0.0051)        (0.0051)        (0.0051)        (0.0051)   
    ## University * Male * Age               -0.0120 +       -0.0111         -0.0135 +       -0.0119 +  
    ##                                       (0.0072)        (0.0072)        (0.0072)        (0.0072)   
    ## Male * Age                            -0.0251 ***     -0.0250 ***     -0.0243 ***     -0.0248 ***
    ##                                       (0.0048)        (0.0048)        (0.0048)        (0.0048)   
    ## % of Life Residing Locally (zip)       0.1072 ***      0.1340 ***      0.1199 ***      0.1312 ***
    ##                                       (0.0162)        (0.0159)        (0.0160)        (0.0159)   
    ## DID residence (zip)                                   -0.0062                         -0.0039    
    ##                                                       (0.0054)                        (0.0064)   
    ## Foreigner % sqrt. (zip)                                0.0187 ***                     -0.0009    
    ##                                                       (0.0038)                        (0.0054)   
    ## University % by 10% (zip)                              0.0506 ***                      0.0446 ***
    ##                                                       (0.0025)                        (0.0036)   
    ## DID proportion (mun.)                                                 -0.0187 +       -0.0114    
    ##                                                                       (0.0099)        (0.0116)   
    ## Foreigner % sqrt. (mun.)                                               0.0316 ***      0.0351 ***
    ##                                                                       (0.0054)        (0.0074)   
    ## University % by 10% (mun.)                                             0.0552 ***      0.0105 +  
    ##                                                                       (0.0039)        (0.0053)   
    ## -------------------------------------------------------------------------------------------------
    ## R^2                                    0.0638          0.0933          0.0855          0.0950    
    ## Adj. R^2                               0.0621          0.0915          0.0836          0.0929    
    ## Num. obs.                          15252           15252           15252           15252         
    ## =================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.1

# Organizing Outcomes

## OLS

``` r
outdt0x$data <- "Unmatched"
outdt1x$data <- "Matched without \nDistance Adj."

visdtx <- rbind(outdt0x,outdt1x)

visdtx$data <- factor(visdtx$data, levels = c("Unmatched",
                                            "Matched without \nDistance Adj."))
visdtx$pstar <- factor(ifelse(visdtx$p>=.1,"n.s.",ifelse(visdtx$p>=.05,"p<.1","p<.05")),
                      levels = c("p<.05","p<.1","n.s."))
```

``` r
saveRDS(visdtx, paste0(projdir, "/out/visdtx.rds"))
```

## Multinomial Logit

``` r
outdt0xm$data <- "Unmatched"
outdt1xm$data <- "Matched without \nDistance Adj."

visdtxm <- rbind(outdt0xm,outdt1xm)

visdtxm$data <- factor(visdtxm$data, levels = c("Unmatched",
                                              "Matched without \nDistance Adj."))
visdtxm$pstar <- factor(ifelse(visdtxm$p>=.1,"n.s.",ifelse(visdtxm$p>=.05,"p<.1","p<.05")),
                       levels = c("p<.05","p<.1","n.s."))
```

``` r
saveRDS(visdtxm, paste0(projdir, "/out/visdtxm.rds"))
```

## Combining OLS and Multinomial Logit

``` r
visdtx$method = "OLS"
visdtxm$method = "Multinomial Logit\nAgree vs. Disagree"
visdtxall <- rbind(visdtx,visdtxm)
visdtxall$method <- factor(visdtxall$method, levels = unique(visdtxall$method))
colnames(visdtxall)
```

    ##  [1] "gender" "age"    "est"    "lci95"  "uci95"  "lci90"  "uci90"  "se"     "p"      "lv"     "data"   "pstar" 
    ## [13] "method"

## Including Mail

``` r
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
```

    ##  [1] "gender" "age"    "est"    "lci95"  "uci95"  "lci90"  "uci90"  "se"     "p"      "lv"     "data"   "pstar" 
    ## [13] "method"

``` r
visdtxall <- rbind(visdtxall,visdtx_mail)
visdtxall$data <- factor(visdtxall$data, levels = unique(visdtxall$data))
table(visdtxall$data)
```

    ## 
    ##                       Unmatched Matched without \nDistance Adj.                         Mail-in 
    ##                              20                              20                              20

``` r
saveRDS(visdtxall, paste0(projdir, "/out/visdtxall.rds"))
```

# Save Image

``` r
save.image(file=paste0(projdir,"/out/heavy/analysis_2x_matched_v5.RData"))
```
