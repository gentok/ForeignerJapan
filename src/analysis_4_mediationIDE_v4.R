#' ---
#' title: "Analysis 4: Mediation Analysis with Ideology"
#' author: "Fan Lu & Gento Kato"
#' date: "Dec 31, 2019"
#' ---
#' 
#' # Preparation 

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

## Load Image of Main Analysis
load(paste0(projdir,"/out/analysis_main_v4.RData"))

# Import Matched Data
sifcct_m1 <- readRDS("./data/sifcct_young_matched_1.rds")
sifcct_m2 <- readRDS("./data/sifcct_young_matched_2.rds")
sifcct_m3 <- readRDS("./data/sifcct_young_matched_3.rds")
sifcct_m4 <- readRDS("./data/sifcct_young_matched_4.rds")
sifcct_m5 <- readRDS("./data/sifcct_young_matched_5.rds")
head(sifcct$zip_pref)

# Replace zip_pref variable
sifcct_m1$zip_pref <- sifcct$zip_pref[match(sifcct_m1$zip,sifcct$zip)]
sifcct_m2$zip_pref <- sifcct$zip_pref[match(sifcct_m2$zip,sifcct$zip)]
sifcct_m3$zip_pref <- sifcct$zip_pref[match(sifcct_m3$zip,sifcct$zip)]
sifcct_m4$zip_pref <- sifcct$zip_pref[match(sifcct_m4$zip,sifcct$zip)]
sifcct_m5$zip_pref <- sifcct$zip_pref[match(sifcct_m5$zip,sifcct$zip)]

## packages
# devtools::install_github("gentok/estvis")
require(estvis)
require(multiwayvcov)
require(sandwich)
require(lmtest)
require(MASS)
require(ggplot2)
require(texreg)
require(mediation)

#'
#' # Limit Data to Young People
#'

table(sifcct$agecat)
sifcct <- sifcct[which(sifcct$agecat=="Young (<=30s)"),]

sifcct$wave <- as.factor(sifcct$wave)
sifcct_m1$wave <- as.factor(sifcct_m1$wave)
sifcct_m2$wave <- as.factor(sifcct_m2$wave)
sifcct_m3$wave <- as.factor(sifcct_m3$wave)
sifcct_m4$wave <- as.factor(sifcct_m4$wave)
sifcct_m5$wave <- as.factor(sifcct_m5$wave)

sifcct$foreignsuff_agree <- ifelse(sifcct$foreignsuff>=0.75,1,0)
sifcct_m1$foreignsuff_agree <- ifelse(sifcct_m1$foreignsuff>=0.75,1,0)
sifcct_m2$foreignsuff_agree <- ifelse(sifcct_m2$foreignsuff>=0.75,1,0)
sifcct_m3$foreignsuff_agree <- ifelse(sifcct_m3$foreignsuff>=0.75,1,0)
sifcct_m4$foreignsuff_agree <- ifelse(sifcct_m4$foreignsuff>=0.75,1,0)
sifcct_m5$foreignsuff_agree <- ifelse(sifcct_m5$foreignsuff>=0.75,1,0)

# Reverse Education Variable
sifcct$edu <- 1 - sifcct$edu
sifcct_m1$edu <- ifelse(sifcct_m1$edu=="<=SHS",1,0)
sifcct_m2$edu <- ifelse(sifcct_m2$edu=="<=SHS",1,0)
sifcct_m3$edu <- ifelse(sifcct_m3$edu=="<=SHS",1,0)
sifcct_m4$edu <- ifelse(sifcct_m4$edu=="<=SHS",1,0)
sifcct_m5$edu <- ifelse(sifcct_m5$edu=="<=SHS",1,0)

#'
#' # Models
#' 
#' ## SIFCCT (Original)
#'

fdt <- sifcct[which(sifcct$female==1),]
mdt <- sifcct[which(sifcct$female==0),]

## Mediator Models
medf_IDE <- lm(ideology  ~ edu + knowledge + polint + employed + evecon + income + lvpr + wave, 
               data=fdt)
coeftest(medf_IDE, vcov.=vcovCL(medf_IDE,factor(fdt$zip_pref)))
medm_IDE <- lm(ideology  ~ edu + knowledge + polint + employed + evecon + income + lvpr + wave, 
               data=mdt)
coeftest(medm_IDE, vcov.=vcovCL(medm_IDE,factor(mdt$zip_pref)))

## Outcome Models 
outf_IDE <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + 
                  knowledge + polint + employed + evecon + income + lvpr + wave, data=fdt, family=binomial("logit"))
coeftest(outf_IDE, vcov.=vcovCL(outf_IDE,factor(fdt$zip_pref)))
outm_IDE <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + 
                  knowledge + polint + employed + evecon + income + lvpr + wave, data=mdt, family=binomial("logit"))
coeftest(outm_IDE, vcov.=vcovCL(outm_IDE,factor(mdt$zip_pref)))

## Causal Mediation Analysis
set.seed(2345)
medoutf_IDE <- mediate(medf_IDE, outf_IDE, treat = "edu", 
                       mediator = "ideology", 
                       cluster=factor(fdt$zip_pref))
summary(medoutf_IDE)
set.seed(2345)
medoutm_IDE <- mediate(medm_IDE, outm_IDE, treat = "edu", 
                       mediator = "ideology", 
                       cluster=factor(mdt$zip_pref))
summary(medoutm_IDE)

#'
#' ## Matched without Distance Adjustment
#'

fdt_m1 <- sifcct_m1[which(sifcct_m1$female==1),]
mdt_m1 <- sifcct_m1[which(sifcct_m1$female==0),]

## Mediator Models
medf_IDE_m1 <- lm(ideology  ~ edu + #,
                    knowledge + polint + employed + evecon + income + lvpr + wave, 
                  data=fdt_m1)
coeftest(medf_IDE_m1, vcov.=vcovCL(medf_IDE_m1, factor(fdt_m1$zip_pref)))
medm_IDE_m1 <- lm(ideology  ~ edu + #,
                    knowledge + polint + employed + evecon + income + lvpr + wave, 
                  data=mdt_m1)
coeftest(medm_IDE_m1, vcov.=vcovCL(medm_IDE_m1,factor(mdt_m1$zip_pref)))

## Outcome Models
outf_IDE_m1 <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + #,
                     knowledge + polint + employed + evecon + income + lvpr + wave, 
                   data=fdt_m1, family=binomial("logit"))
coeftest(outf_IDE_m1, vcov.=vcovCL(outf_IDE_m1,factor(fdt_m1$zip_pref)))
outm_IDE_m1 <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + #,
                     knowledge + polint + employed + evecon + income + lvpr + wave, 
                   data=mdt_m1, family=binomial("logit"))
coeftest(outm_IDE_m1, vcov.=vcovCL(outm_IDE_m1,factor(mdt_m1$zip_pref)))

## Causal Mediation Analysis
set.seed(2345)
medoutf_IDE_m1 <- mediate(medf_IDE_m1, outf_IDE_m1, treat = "edu", 
                          mediator = "ideology", 
                          cluster=factor(fdt_m1$zip_pref))
summary(medoutf_IDE_m1)
set.seed(2345)
medoutm_IDE_m1 <- mediate(medm_IDE_m1, outm_IDE_m1, treat = "edu", 
                          mediator = "ideology", 
                          cluster=factor(mdt_m1$zip_pref))
summary(medoutm_IDE_m1)

#'
#' ## Matched with Distance Adjustment (Lambda=50km)
#'

fdt_m2 <- sifcct_m2[which(sifcct_m2$female==1),]
mdt_m2 <- sifcct_m2[which(sifcct_m2$female==0),]

## Mediator Models
medf_IDE_m2 <- lm(ideology  ~ edu + #,
                    knowledge + polint + employed + evecon + income + lvpr + wave, 
                  data=fdt_m2)
coeftest(medf_IDE_m2, vcov.=vcovCL(medf_IDE_m2, factor(fdt_m2$zip_pref)))
medm_IDE_m2 <- lm(ideology  ~ edu + #,
                    knowledge + polint + employed + evecon + income + lvpr + wave, 
                  data=mdt_m2)
coeftest(medm_IDE_m2, vcov.=vcovCL(medm_IDE_m2,factor(mdt_m2$zip_pref)))

## Outcome Models
outf_IDE_m2 <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + #,
                     knowledge + polint + employed + evecon + income + lvpr + wave, 
                   data=fdt_m2, family=binomial("logit"))
coeftest(outf_IDE_m2, vcov.=vcovCL(outf_IDE_m2,factor(fdt_m2$zip_pref)))
outm_IDE_m2 <- glm(foreignsuff_agree  ~ edu+ ideology + edu:ideology + #,
                     knowledge + polint + employed + evecon + income + lvpr + wave, 
                   data=mdt_m2, family=binomial("logit"))
coeftest(outm_IDE_m2, vcov.=vcovCL(outm_IDE_m2,factor(mdt_m2$zip_pref)))

## Causal Mediation Analysis
set.seed(2345)
medoutf_IDE_m2 <- mediate(medf_IDE_m2, outf_IDE_m2, treat = "edu", 
                          mediator = "ideology", 
                          cluster=factor(fdt_m2$zip_pref))
summary(medoutf_IDE_m2)
set.seed(2345)
medoutm_IDE_m2 <- mediate(medm_IDE_m2, outm_IDE_m2, treat = "edu", 
                          mediator = "ideology", 
                          cluster=factor(mdt_m2$zip_pref))
summary(medoutm_IDE_m2)

#'
#' ## Matched with Distance Adjustment (Lambda=100km)
#'

fdt_m3 <- sifcct_m3[which(sifcct_m3$female==1),]
mdt_m3 <- sifcct_m3[which(sifcct_m3$female==0),]

## Mediator Models
medf_IDE_m3 <- lm(ideology  ~ edu + #,
                    knowledge + polint + employed + evecon + income + lvpr + wave, 
                  data=fdt_m3)
coeftest(medf_IDE_m3, vcov.=vcovCL(medf_IDE_m3, factor(fdt_m3$zip_pref)))
medm_IDE_m3 <- lm(ideology  ~ edu + #,
                    knowledge + polint + employed + evecon + income + lvpr + wave, 
                  data=mdt_m3)
coeftest(medm_IDE_m3, vcov.=vcovCL(medm_IDE_m3,factor(mdt_m3$zip_pref)))

## Outcome Models
outf_IDE_m3 <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + #,
                     knowledge + polint + employed + evecon + income + lvpr + wave, 
                   data=fdt_m3, family=binomial("logit"))
coeftest(outf_IDE_m3, vcov.=vcovCL(outf_IDE_m3,factor(fdt_m3$zip_pref)))
outm_IDE_m3 <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + #,
                     knowledge + polint + employed + evecon + income + lvpr + wave, 
                   data=mdt_m3, family=binomial("logit"))
coeftest(outm_IDE_m3, vcov.=vcovCL(outm_IDE_m3,factor(mdt_m3$zip_pref)))

## Causal Mediation Analysis
set.seed(2345)
medoutf_IDE_m3 <- mediate(medf_IDE_m3, outf_IDE_m3, treat = "edu", 
                          mediator = "ideology", 
                          cluster=factor(fdt_m3$zip_pref))
summary(medoutf_IDE_m3)
set.seed(2345)
medoutm_IDE_m3 <- mediate(medm_IDE_m3, outm_IDE_m3, treat = "edu", 
                          mediator = "ideology", 
                          cluster=factor(mdt_m3$zip_pref))
summary(medoutm_IDE_m3)

#'
#' ## Matched with Distance Adjustment (Lambda=200km)
#'

fdt_m4 <- sifcct_m4[which(sifcct_m4$female==1),]
mdt_m4 <- sifcct_m4[which(sifcct_m4$female==0),]

## Mediator Models
medf_IDE_m4 <- lm(ideology  ~ edu + #,
                    knowledge + polint + employed + evecon + income + lvpr + wave, 
                  data=fdt_m4)
coeftest(medf_IDE_m4, vcov.=vcovCL(medf_IDE_m4, factor(fdt_m4$zip_pref)))
medm_IDE_m4 <- lm(ideology  ~ edu + #,
                    knowledge + polint + employed + evecon + income + lvpr + wave, 
                  data=mdt_m4)
coeftest(medm_IDE_m4, vcov.=vcovCL(medm_IDE_m4,factor(mdt_m4$zip_pref)))

## Outcome Models
outf_IDE_m4 <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + #,
                     knowledge + polint + employed + evecon + income + lvpr + wave, 
                   data=fdt_m4, family=binomial("logit"))
coeftest(outf_IDE_m4, vcov.=vcovCL(outf_IDE_m4,factor(fdt_m4$zip_pref)))
outm_IDE_m4 <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + #,
                     knowledge + polint + employed + evecon + income + lvpr + wave, 
                   data=mdt_m4, family=binomial("logit"))
coeftest(outm_IDE_m4, vcov.=vcovCL(outm_IDE_m4,factor(mdt_m4$zip_pref)))

## Causal Mediation Analysis
set.seed(2345)
medoutf_IDE_m4 <- mediate(medf_IDE_m4, outf_IDE_m4, treat = "edu", 
                          mediator = "ideology", 
                          cluster=factor(fdt_m4$zip_pref))
summary(medoutf_IDE_m4)
set.seed(2345)
medoutm_IDE_m4 <- mediate(medm_IDE_m4, outm_IDE_m4, treat = "edu", 
                          mediator = "ideology", 
                          cluster=factor(mdt_m4$zip_pref))
summary(medoutm_IDE_m4)

#'
#' ## Matched with Distance Adjustment (Lambda=200km)
#'

fdt_m5 <- sifcct_m5[which(sifcct_m5$female==1),]
mdt_m5 <- sifcct_m5[which(sifcct_m5$female==0),]

## Mediator Models
medf_IDE_m5 <- lm(ideology  ~ edu + #,
                    knowledge + polint + employed + evecon + income + lvpr + wave,
                  data=fdt_m5)
coeftest(medf_IDE_m5, vcov.=vcovCL(medf_IDE_m5, factor(fdt_m5$zip_pref)))
medm_IDE_m5 <- lm(ideology  ~ edu + #, 
                    knowledge + polint + employed + evecon + income + lvpr + wave,
                  data=mdt_m5)
coeftest(medm_IDE_m5, vcov.=vcovCL(medm_IDE_m5,factor(mdt_m5$zip_pref)))

## Outcome Models
outf_IDE_m5 <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + #,
                     knowledge + polint + employed + evecon + income + lvpr + wave, 
                   data=fdt_m5, family=binomial("logit"))
coeftest(outf_IDE_m5, vcov.=vcovCL(outf_IDE_m5,factor(fdt_m5$zip_pref)))
outm_IDE_m5 <- glm(foreignsuff_agree  ~ edu + ideology + edu:ideology + #,
                     knowledge + polint + employed + evecon + income + lvpr + wave, 
                   data=mdt_m5, family=binomial("logit"))
coeftest(outm_IDE_m5, vcov.=vcovCL(outm_IDE_m5,factor(mdt_m5$zip_pref)))

## Causal Mediation Analysis
set.seed(2345)
medoutf_IDE_m5 <- mediate(medf_IDE_m5, outf_IDE_m5, treat = "edu", 
                          mediator = "ideology", 
                          cluster=factor(fdt_m5$zip_pref))
summary(medoutf_IDE_m5)
set.seed(2345)
medoutm_IDE_m5 <- mediate(medm_IDE_m5, outm_IDE_m5, treat = "edu", 
                          mediator = "ideology", 
                          cluster=factor(mdt_m5$zip_pref))
summary(medoutm_IDE_m5)

#'
#' # Coefficient Plot
#'

coefdt <- as.data.frame(rbind(
  c(-coef(medf_IDE)[2],
    -rev(coefci(medf_IDE, vcov.=vcovCL(medf_IDE,factor(fdt$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medf_IDE, vcov.=vcovCL(medf_IDE,factor(fdt$zip_pref)), level=0.95)[2,]),
    coeftest(medf_IDE, vcov.=vcovCL(medf_IDE,factor(fdt$zip_pref)), level=0.95)[2,4]),
  c(coef(outf_IDE)[3],
    coefci(outf_IDE, vcov.=vcovCL(outf_IDE,factor(fdt$zip_pref)), level=0.90)[3,],
    coefci(outf_IDE, vcov.=vcovCL(outf_IDE,factor(fdt$zip_pref)), level=0.95)[3,],
    coeftest(outf_IDE, vcov.=vcovCL(outf_IDE,factor(fdt$zip_pref)), level=0.95)[3,4]),
  c(-medoutf_IDE$d0,-quantile(medoutf_IDE$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE$d0.p),
  c(-medoutf_IDE$z0,-quantile(medoutf_IDE$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE$z0.p),
  c(-coef(medm_IDE)[2],
    -rev(coefci(medm_IDE, vcov.=vcovCL(medm_IDE,factor(mdt$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medm_IDE, vcov.=vcovCL(medm_IDE,factor(mdt$zip_pref)), level=0.95)[2,]),
    coeftest(medm_IDE, vcov.=vcovCL(medm_IDE,factor(mdt$zip_pref)), level=0.95)[2,4]),
  c(coef(outm_IDE)[3],
    coefci(outm_IDE, vcov.=vcovCL(outm_IDE,factor(mdt$zip_pref)), level=0.90)[3,],
    coefci(outm_IDE, vcov.=vcovCL(outm_IDE,factor(mdt$zip_pref)), level=0.95)[3,],
    coeftest(outm_IDE, vcov.=vcovCL(outm_IDE,factor(mdt$zip_pref)), level=0.95)[3,4]),
  c(-medoutm_IDE$d0,-quantile(medoutm_IDE$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE$d0.p),
  c(-medoutm_IDE$z0,-quantile(medoutm_IDE$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE$z0.p),
  c(-coef(medf_IDE_m1)[2],
    -rev(coefci(medf_IDE_m1, vcov.=vcovCL(medf_IDE_m1,factor(fdt_m1$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medf_IDE_m1, vcov.=vcovCL(medf_IDE_m1,factor(fdt_m1$zip_pref)), level=0.95)[2,]),
    coeftest(medf_IDE_m1, vcov.=vcovCL(medf_IDE_m1,factor(fdt_m1$zip_pref)), level=0.95)[2,4]),
  c(coef(outf_IDE_m1)[3],
    coefci(outf_IDE_m1, vcov.=vcovCL(outf_IDE_m1,factor(fdt_m1$zip_pref)), level=0.90)[3,],
    coefci(outf_IDE_m1, vcov.=vcovCL(outf_IDE_m1,factor(fdt_m1$zip_pref)), level=0.95)[3,],
    coeftest(outf_IDE_m1, vcov.=vcovCL(outf_IDE_m1,factor(fdt_m1$zip_pref)), level=0.95)[3,4]),
  c(-medoutf_IDE_m1$d0,-quantile(medoutf_IDE_m1$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE_m1$d0.p),
  c(-medoutf_IDE_m1$z0,-quantile(medoutf_IDE_m1$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE_m1$z0.p),
  c(-coef(medm_IDE_m1)[2],
    -rev(coefci(medm_IDE_m1, vcov.=vcovCL(medm_IDE_m1,factor(mdt_m1$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medm_IDE_m1, vcov.=vcovCL(medm_IDE_m1,factor(mdt_m1$zip_pref)), level=0.95)[2,]),
    coeftest(medm_IDE_m1, vcov.=vcovCL(medm_IDE_m1,factor(mdt_m1$zip_pref)), level=0.95)[2,4]),
  c(coef(outm_IDE_m1)[3],
    coefci(outm_IDE_m1, vcov.=vcovCL(outm_IDE_m1,factor(mdt_m1$zip_pref)), level=0.90)[3,],
    coefci(outm_IDE_m1, vcov.=vcovCL(outm_IDE_m1,factor(mdt_m1$zip_pref)), level=0.95)[3,],
    coeftest(outm_IDE_m1, vcov.=vcovCL(outm_IDE_m1,factor(mdt_m1$zip_pref)), level=0.95)[3,4]),
  c(-medoutm_IDE_m1$d0,-quantile(medoutm_IDE_m1$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE_m1$d0.p),
  c(-medoutm_IDE_m1$z0,-quantile(medoutm_IDE_m1$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE_m1$z0.p),
  c(-coef(medf_IDE_m5)[2],
    -rev(coefci(medf_IDE_m5, vcov.=vcovCL(medf_IDE_m5,factor(fdt_m5$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medf_IDE_m5, vcov.=vcovCL(medf_IDE_m5,factor(fdt_m5$zip_pref)), level=0.95)[2,]),
    coeftest(medf_IDE_m5, vcov.=vcovCL(medf_IDE_m5,factor(fdt_m5$zip_pref)), level=0.95)[2,4]),
  c(coef(outf_IDE_m5)[3],
    coefci(outf_IDE_m5, vcov.=vcovCL(outf_IDE_m5,factor(fdt_m5$zip_pref)), level=0.90)[3,],
    coefci(outf_IDE_m5, vcov.=vcovCL(outf_IDE_m5,factor(fdt_m5$zip_pref)), level=0.95)[3,],
    coeftest(outf_IDE_m5, vcov.=vcovCL(outf_IDE_m5,factor(fdt_m5$zip_pref)), level=0.95)[3,4]),
  c(-medoutf_IDE_m5$d0,-quantile(medoutf_IDE_m5$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE_m5$d0.p),
  c(-medoutf_IDE_m5$z0,-quantile(medoutf_IDE_m5$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE_m5$z0.p),
  c(-coef(medm_IDE_m5)[2],
    -rev(coefci(medm_IDE_m5, vcov.=vcovCL(medm_IDE_m5,factor(mdt_m5$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medm_IDE_m5, vcov.=vcovCL(medm_IDE_m5,factor(mdt_m5$zip_pref)), level=0.95)[2,]),
    coeftest(medm_IDE_m5, vcov.=vcovCL(medm_IDE_m5,factor(mdt_m5$zip_pref)), level=0.95)[2,4]),
  c(coef(outm_IDE_m5)[3],
    coefci(outm_IDE_m5, vcov.=vcovCL(outm_IDE_m5,factor(mdt_m5$zip_pref)), level=0.90)[3,],
    coefci(outm_IDE_m5, vcov.=vcovCL(outm_IDE_m5,factor(mdt_m5$zip_pref)), level=0.95)[3,],
    coeftest(outm_IDE_m5, vcov.=vcovCL(outm_IDE_m5,factor(mdt_m5$zip_pref)), level=0.95)[3,4]),
  c(-medoutm_IDE_m5$d0,-quantile(medoutm_IDE_m5$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE_m5$d0.p),
  c(-medoutm_IDE_m5$z0,-quantile(medoutm_IDE_m5$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE_m5$z0.p),
  c(-coef(medf_IDE_m4)[2],
    -rev(coefci(medf_IDE_m4, vcov.=vcovCL(medf_IDE_m4,factor(fdt_m4$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medf_IDE_m4, vcov.=vcovCL(medf_IDE_m4,factor(fdt_m4$zip_pref)), level=0.95)[2,]),
    coeftest(medf_IDE_m4, vcov.=vcovCL(medf_IDE_m4,factor(fdt_m4$zip_pref)), level=0.95)[2,4]),
  c(coef(outf_IDE_m4)[3],
    coefci(outf_IDE_m4, vcov.=vcovCL(outf_IDE_m4,factor(fdt_m4$zip_pref)), level=0.90)[3,],
    coefci(outf_IDE_m4, vcov.=vcovCL(outf_IDE_m4,factor(fdt_m4$zip_pref)), level=0.95)[3,],
    coeftest(outf_IDE_m4, vcov.=vcovCL(outf_IDE_m4,factor(fdt_m4$zip_pref)), level=0.95)[3,4]),
  c(-medoutf_IDE_m4$d0,-quantile(medoutf_IDE_m4$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE_m4$d0.p),
  c(-medoutf_IDE_m4$z0,-quantile(medoutf_IDE_m4$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE_m4$z0.p),
  c(-coef(medm_IDE_m4)[2],
    -rev(coefci(medm_IDE_m4, vcov.=vcovCL(medm_IDE_m4,factor(mdt_m4$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medm_IDE_m4, vcov.=vcovCL(medm_IDE_m4,factor(mdt_m4$zip_pref)), level=0.95)[2,]),
    coeftest(medm_IDE_m4, vcov.=vcovCL(medm_IDE_m4,factor(mdt_m4$zip_pref)), level=0.95)[2,4]),
  c(coef(outm_IDE_m4)[3],
    coefci(outm_IDE_m4, vcov.=vcovCL(outm_IDE_m4,factor(mdt_m4$zip_pref)), level=0.90)[3,],
    coefci(outm_IDE_m4, vcov.=vcovCL(outm_IDE_m4,factor(mdt_m4$zip_pref)), level=0.95)[3,],
    coeftest(outm_IDE_m4, vcov.=vcovCL(outm_IDE_m4,factor(mdt_m4$zip_pref)), level=0.95)[3,4]),
  c(-medoutm_IDE_m4$d0,-quantile(medoutm_IDE_m4$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE_m4$d0.p),
  c(-medoutm_IDE_m4$z0,-quantile(medoutm_IDE_m4$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE_m4$z0.p),
  c(-coef(medf_IDE_m3)[2],
    -rev(coefci(medf_IDE_m3, vcov.=vcovCL(medf_IDE_m3,factor(fdt_m3$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medf_IDE_m3, vcov.=vcovCL(medf_IDE_m3,factor(fdt_m3$zip_pref)), level=0.95)[2,]),
    coeftest(medf_IDE_m3, vcov.=vcovCL(medf_IDE_m3,factor(fdt_m3$zip_pref)), level=0.95)[2,4]),
  c(coef(outf_IDE_m3)[3],
    coefci(outf_IDE_m3, vcov.=vcovCL(outf_IDE_m3,factor(fdt_m3$zip_pref)), level=0.90)[3,],
    coefci(outf_IDE_m3, vcov.=vcovCL(outf_IDE_m3,factor(fdt_m3$zip_pref)), level=0.95)[3,],
    coeftest(outf_IDE_m3, vcov.=vcovCL(outf_IDE_m3,factor(fdt_m3$zip_pref)), level=0.95)[3,4]),
  c(-medoutf_IDE_m3$d0,-quantile(medoutf_IDE_m3$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE_m3$d0.p),
  c(-medoutf_IDE_m3$z0,-quantile(medoutf_IDE_m3$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE_m3$z0.p),
  c(-coef(medm_IDE_m3)[2],
    -rev(coefci(medm_IDE_m3, vcov.=vcovCL(medm_IDE_m3,factor(mdt_m3$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medm_IDE_m3, vcov.=vcovCL(medm_IDE_m3,factor(mdt_m3$zip_pref)), level=0.95)[2,]),
    coeftest(medm_IDE_m3, vcov.=vcovCL(medm_IDE_m3,factor(mdt_m3$zip_pref)), level=0.95)[2,4]),
  c(coef(outm_IDE_m3)[3],
    coefci(outm_IDE_m3, vcov.=vcovCL(outm_IDE_m3,factor(mdt_m3$zip_pref)), level=0.90)[3,],
    coefci(outm_IDE_m3, vcov.=vcovCL(outm_IDE_m3,factor(mdt_m3$zip_pref)), level=0.95)[3,],
    coeftest(outm_IDE_m3, vcov.=vcovCL(outm_IDE_m3,factor(mdt_m3$zip_pref)), level=0.95)[3,4]),
  c(-medoutm_IDE_m3$d0,-quantile(medoutm_IDE_m3$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE_m3$d0.p),
  c(-medoutm_IDE_m3$z0,-quantile(medoutm_IDE_m3$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE_m3$z0.p),
  c(-coef(medf_IDE_m2)[2],
    -rev(coefci(medf_IDE_m2, vcov.=vcovCL(medf_IDE_m2,factor(fdt_m2$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medf_IDE_m2, vcov.=vcovCL(medf_IDE_m2,factor(fdt_m2$zip_pref)), level=0.95)[2,]),
    coeftest(medf_IDE_m2, vcov.=vcovCL(medf_IDE_m2,factor(fdt_m2$zip_pref)), level=0.95)[2,4]),
  c(coef(outf_IDE_m2)[3],
    coefci(outf_IDE_m2, vcov.=vcovCL(outf_IDE_m2,factor(fdt_m2$zip_pref)), level=0.90)[3,],
    coefci(outf_IDE_m2, vcov.=vcovCL(outf_IDE_m2,factor(fdt_m2$zip_pref)), level=0.95)[3,],
    coeftest(outf_IDE_m2, vcov.=vcovCL(outf_IDE_m2,factor(fdt_m2$zip_pref)), level=0.95)[3,4]),
  c(-medoutf_IDE_m2$d0,-quantile(medoutf_IDE_m2$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE_m2$d0.p),
  c(-medoutf_IDE_m2$z0,-quantile(medoutf_IDE_m2$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutf_IDE_m2$z0.p),
  c(-coef(medm_IDE_m2)[2],
    -rev(coefci(medm_IDE_m2, vcov.=vcovCL(medm_IDE_m2,factor(mdt_m2$zip_pref)), level=0.90)[2,]),
    -rev(coefci(medm_IDE_m2, vcov.=vcovCL(medm_IDE_m2,factor(mdt_m2$zip_pref)), level=0.95)[2,]),
    coeftest(medm_IDE_m2, vcov.=vcovCL(medm_IDE_m2,factor(mdt_m2$zip_pref)), level=0.95)[2,4]),
  c(coef(outm_IDE_m2)[3],
    coefci(outm_IDE_m2, vcov.=vcovCL(outm_IDE_m2,factor(mdt_m2$zip_pref)), level=0.90)[3,],
    coefci(outm_IDE_m2, vcov.=vcovCL(outm_IDE_m2,factor(mdt_m2$zip_pref)), level=0.95)[3,],
    coeftest(outm_IDE_m2, vcov.=vcovCL(outm_IDE_m2,factor(mdt_m2$zip_pref)), level=0.95)[3,4]),
  c(-medoutm_IDE_m2$d0,-quantile(medoutm_IDE_m2$d0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE_m2$d0.p),
  c(-medoutm_IDE_m2$z0,-quantile(medoutm_IDE_m2$z0.sims,probs=c(0.95,0.05,0.975,0.025)),medoutm_IDE_m2$z0.p)
))
colnames(coefdt) <- c("est","lci90","uci90","lci95","uci95","p")

coefdt$gender <- factor(rep(c("Female","Male"),each=4),levels=c("Female","Male"))
coefdt$mod <- c("Treat. => Med.\n(OLS Coefficient)","Med. => Out.\n(Logit Coefficient)",
                "Treat. => Med. => Out.\n(ACME: Probs. Diff.)","Treat. =>  Out.\n(ADE: Probs. Diff.)")
coefdt$mod <- factor(coefdt$mod, levels=unique(coefdt$mod))
coefdt$lambda <- rep(c("Original", 
                       "Matched without Distance Adj.",
                       "Matched with Lambda = 350km", 
                       "Matched with Lambda = 200km", 
                       "Matched with Lambda = 100km", 
                       "Matched with Lambda = 50km"), each=8)
coefdt$lambda <- factor(coefdt$lambda, levels=unique(coefdt$lambda))

require(ggplot2)
p <- ggplot(coefdt, aes(x=gender, y=est)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=lambda), #linetype=pstar 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) + 
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=lambda),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=lambda, colour=lambda), 
             position=position_dodge(width=-0.7), size=3) + 
  facet_grid(gender ~ mod, scales = "free") + 
  scale_shape_discrete(name="Model/\nDataset", 
                       labels=c("No Matching (with Controls)",
                                "Matched without Distance Adj.",
                                bquote("Matched with" ~ lambda ~ "= 350km"),
                                bquote("Matched with" ~ lambda ~ "= 200km"),
                                bquote("Matched with" ~ lambda ~ "= 100km"),
                                bquote("Matched with" ~ lambda ~ "= 50km")
                       ) 
  ) + 
  scale_color_manual(name="Model/\nDataset", 
                     values=rep("black", 6),
                     labels=c("No Matching (with Controls)",
                              "Matched without Distance Adj.",
                              bquote("Matched with" ~ lambda ~ "= 350km"),
                              bquote("Matched with" ~ lambda ~ "= 200km"),
                              bquote("Matched with" ~ lambda ~ "= 100km"),
                              bquote("Matched with" ~ lambda ~ "= 50km")
                     )) + 
  #scale_linetype_manual(name="Significance",values=c("solid","longdash","dotted")) + 
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") + 
  xlab(NULL) + 
  labs(caption="Treatment: University education or more (1), Senior High School or less (0). \nMediatior: Conservative Ideology (rescaled to 0-1 with 1 being the most conservative). Model is estimated by OLS. \nOutcome: Rather agree or agree with granting suffrage to permanent residents (1), else (0). Model is estimated by logit.") + 
  coord_flip() + theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot_IDE.png"),p,width=8,height=5)

require(ggplot2)
p <- ggplot(coefdt[coefdt$mod!="Treat. =>  Out.\n(ADE: Probs. Diff.)",], aes(x=gender, y=est)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=lambda), #linetype=pstar 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) + 
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=lambda),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=lambda, colour=lambda), 
             position=position_dodge(width=-0.7), size=3) + 
  facet_grid(gender ~ mod, scales = "free") + 
  scale_shape_discrete(name="Model/\nDataset", 
                       labels=c("No Matching (with Controls)",
                                "Matched without Distance Adj.",
                                bquote("Matched with" ~ lambda ~ "= 350km"),
                                bquote("Matched with" ~ lambda ~ "= 200km"),
                                bquote("Matched with" ~ lambda ~ "= 100km"),
                                bquote("Matched with" ~ lambda ~ "= 50km")
                       ) 
  ) + 
  scale_color_manual(name="Model/\nDataset", 
                     values=rep("black", 6),
                     labels=c("No Matching (with Controls)",
                              "Matched without Distance Adj.",
                              bquote("Matched with" ~ lambda ~ "= 350km"),
                              bquote("Matched with" ~ lambda ~ "= 200km"),
                              bquote("Matched with" ~ lambda ~ "= 100km"),
                              bquote("Matched with" ~ lambda ~ "= 50km")
                     )) + 
  #scale_linetype_manual(name="Significance",values=c("solid","longdash","dotted")) + 
  ylab("(Thin Line = 95% CI; Thick Line 90% CI)") + 
  xlab(NULL) + 
  labs(caption="Treatment: University education or more (1), Senior High School or less (0). \nMediatior: Conservative Ideology (rescaled to 0-1 with 1 being the most conservative). Model is estimated by OLS. \nOutcome: Rather agree or agree with granting suffrage to permanent residents (1), else (0). Model is estimated by logit.") + 
  coord_flip() + theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

ggsave(paste0(projdir,"/out/mediationplot2_IDE.png"),p,width=8,height=5)

## Save Image

save.image(paste0(projdir,"/out/analysis_4_mediationIDE_v4.RData"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/analysis_4_mediationIDE_v4.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
# In Terminal, move to src directory and run:
# Rscript -e "rmarkdown::render('analysis_4_mediationIDE_v4.R', 'github_document', clean=FALSE)"
