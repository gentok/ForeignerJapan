#' ---
#' title: "Analysis 1: Main Analysis with Unmatched Data"
#' author: "Fan Lu & Gento Kato"
#' date: "Jan 1, 2020"
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

## Find Data Directory
datadir1 <- paste0(projdir, "/data/sifcct_latest_v4.rds")
datadir2 <- paste0(projdir, "/data/utas_latest_v4.rds")

## Import Original Data
sifcct <- readRDS(datadir1)
utas <- readRDS(datadir2)

## packages
# devtools::install_github("gentok/estvis")
require(estvis)
require(multiwayvcov)
require(sandwich)
require(lmtest)
require(MASS)
# devtools::install_github("tidyverse/ggplot2") # Need development version (as of Dec 31, 2019)
library(ggplot2)
require(texreg)
require(brant)
require(VGAM)

#'
#' # Regression Formulas
#'

# Baseline
basemodA <- formula(  ~ edu * agecat + knowledge + polint + employed
                      + evecon + income + lvpr + as.factor(wave)) # sifcct
basemodB <- formula(  ~ edu * agecat + knowledge + employed + evecon + as.factor(year)) # utas

#'
#' # Dropping NAs from Data 
#'

## Drop missing cases
sifcct <- sifcct[complete.cases(sifcct[,c("foreignsuff",
                                          "familialityFT_KOR",
                                          "ideology",
                                          "zip_pref",all.vars(basemodA))]),]
utas <- utas[complete.cases(utas[,c("foreignsuff",
                                    "add_pref",all.vars(basemodB))]),]

## Limit Education to University or over and SHS or under
table(sifcct$edu)
sifcct <- sifcct[which(sifcct$edu!=">SHS & <College(4yr)"),]
sifcct$edu <- ifelse(sifcct$edu==">=College(4yr)",1,0)
table(utas$edu)
utas <- utas[which(utas$edu!=">SHS & <College(4yr)"),]
utas$edu <- ifelse(utas$edu==">=College(4yr)",1,0)

## Factor Variables for Brant Test
sifcct$foreignsuff_fac <- as.factor(sifcct$foreignsuff)
utas$foreignsuff_fac <- as.factor(utas$foreignsuff)
sifcct$wave_fac <- as.factor(sifcct$wave)
utas$year_fac <- as.factor(utas$year)
sifcct$edu_agecat <- paste(sifcct$edu,sifcct$agecat,sep="_")
utas$edu_agecat <- paste(utas$edu,utas$agecat,sep="_")

#'
#' # Suffrage Models
#' 
#' ## SIFCCT
#'

## Ordered Logit
# Female
smof_1 <- polr(update(as.factor(foreignsuff) ~ ., basemodA), data=sifcct[which(sifcct$female==1),], Hess=TRUE)
coeftest(smof_1, vcov.=vcovCL(smof_1,factor(sifcct[which(sifcct$female==1),]$zip_pref)))
# Male
smom_1 <- polr(update(as.factor(foreignsuff) ~ ., basemodA), data=sifcct[which(sifcct$female==0),], Hess=TRUE)
coeftest(smom_1, vcov.=vcovCL(smom_1,factor(sifcct[which(sifcct$female==0),]$zip_pref)))

## Linear Models for robustness Check
# Female
smof_2 <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$female==1),])
coeftest(smof_2, vcov.=vcovCL(smof_2,factor(sifcct[which(sifcct$female==1),]$zip_pref)))
# Male
smom_2 <- lm(update(foreignsuff ~ ., basemodA), data=sifcct[which(sifcct$female==0),])
coeftest(smom_2, vcov.=vcovCL(smom_2,factor(sifcct[which(sifcct$female==0),]$zip_pref)))

## Brant Test for Parallel Regression Assumption (Violation Found)
require(brant)
smof_1b <- polr(foreignsuff_fac ~ edu_agecat + knowledge + polint + 
                  employed + evecon + income + lvpr + wave_fac, 
               data=sifcct[which(sifcct$female==1),], Hess=TRUE)
(smof_1bt <- brant(smof_1b, by.var=T))
smom_1b <- polr(foreignsuff_fac ~ edu_agecat + knowledge + polint + 
                  employed + evecon + income + lvpr + wave_fac, 
               data=sifcct[which(sifcct$female==0),], Hess=TRUE)
(smom_1bt <- brant(smom_1b, by.var=T))

## Generalized Ordered Logit of Foreigner's Suffrage
require(VGAM)
# Female
smof_3 <- vglm(update(foreignsuff*4+1 ~ ., basemodA), 
               data=sifcct[which(sifcct$female==1),], 
               family=cumulative(link="logit", 
                                 parallel=TRUE ~ -1 + employed + income + as.factor(wave), 
                                 reverse=TRUE))
smof_3_sum <- summary(smof_3) 
smof_3_sum
# Male
smom_3 <- vglm(update(foreignsuff*4+1 ~ ., basemodA), 
               data=sifcct[which(sifcct$female==0),], 
               family=cumulative(link="logit", 
                                 parallel=TRUE ~ -1 + employed + income + as.factor(wave), 
                                 reverse=TRUE))
smom_3_sum <- summary(smom_3) 
smom_3_sum

## Logit (>=Rather Agree)
# Female
smof_4 <- glm(update(foreignsuff>=0.75 ~ ., basemodA), 
              data=sifcct[which(sifcct$female==1),],
              family = binomial("logit"))
coeftest(smof_4, vcov.=vcovCL(smof_4,factor(sifcct[which(sifcct$female==1),]$zip_pref)))
# Male
smom_4 <- glm(update(foreignsuff>=0.75 ~ ., basemodA), 
              data=sifcct[which(sifcct$female==0),],
             family = binomial("logit"))
coeftest(smom_4, vcov.=vcovCL(smom_4,factor(sifcct[which(sifcct$female==0),]$zip_pref)))

#' 
#' ## UTAS
#'

## Ordered Logit
# Female
umof_1 <- polr(update(as.factor(foreignsuff) ~ ., basemodB), 
               data=utas[which(utas$female==1),], Hess=TRUE)
nobs(umof_1)
coeftest(umof_1, vcov.=vcovCL(umof_1,factor(utas[which(utas$female==1),]$add_pref)))
# Male
umom_1 <- polr(update(as.factor(foreignsuff) ~ ., basemodB), 
               data=utas[which(utas$female==0),], Hess=TRUE)
nobs(umom_1)
coeftest(umom_1, vcov.=vcovCL(umom_1,factor(utas[which(utas$female==0),]$add_pref)))

## Linear Models for Robustness Check
# Female
umof_2 <- lm(update(foreignsuff ~ ., basemodB), data=utas[which(utas$female==1),])
nobs(umof_2)
coeftest(umof_2, vcov.=vcovCL(umof_2,factor(utas[which(utas$female==1),]$add_pref)))
# Male
umom_2 <- lm(update(foreignsuff ~ ., basemodB), data=utas[which(utas$female==0),])
nobs(umom_2)
coeftest(umom_2, vcov.=vcovCL(umom_2,factor(utas[which(utas$female==0),]$add_pref)))

## Brant Test for Parallel Regression Assumption (Violation Found)
require(brant)
umof_1b <- polr(foreignsuff_fac ~ edu_agecat + knowledge + employed + evecon + year_fac, 
               data=utas[which(utas$female==1),], Hess=TRUE)
(umof_1bt <- brant(umof_1b, by.var=T))
umom_1b <- polr(foreignsuff_fac ~ edu_agecat + knowledge + employed + evecon + year_fac, 
               data=utas[which(utas$female==0),], Hess=TRUE)
(umom_1bt <- brant(umom_1b, by.var=T))

## Then Generalized Ordered Logit (Education Only Works to >=Rather Agree)
require(VGAM)
# Female
umof_3 <- vglm(update(foreignsuff*4+1 ~ ., basemodB), 
               data=utas[which(utas$female==1),], 
               family=cumulative(link="logit", 
                                 parallel=TRUE ~ -1 + employed + as.factor(year), 
                                 reverse=TRUE))
umof_3_sum <- summary(umof_3)
umof_3_sum
# Male
umom_3 <- vglm(update(foreignsuff*4+1 ~ ., basemodB), 
               data=utas[which(utas$female==0),], 
               family=cumulative(link="logit", 
                                 parallel=TRUE ~ -1 + employed + as.factor(year), 
                                 reverse=TRUE))
umom_3_sum <- summary(umom_3)
umom_3_sum

## Logit (>=Rather Agree)
# Female
umof_4 <- glm(update(foreignsuff>=0.75 ~ ., basemodB), 
              data=utas[which(utas$female==1),],
              family = binomial("logit"))
nobs(umof_2)
coeftest(umof_4, vcov.=vcovCL(umof_4,factor(utas[which(utas$female==1),]$add_pref)))
# Male
umom_4 <- glm(update(foreignsuff>=0.75 ~ ., basemodB), 
              data=utas[which(utas$female==0),],
             family = binomial("logit"))
nobs(umom_4)
coeftest(umom_4, vcov.=vcovCL(umom_4,factor(utas[which(utas$female==0),]$add_pref)))

#'
#' # Regression Tables
#'
#' ## Prepartion
#'

# Modifying extract function of texreg to export GOLogit table
extract.vglm <- function (model, 
                          include.aic = TRUE,
                          include.bic = TRUE,
                          include.loglik = TRUE, 
                          include.df = FALSE, 
                          include.nobs = TRUE,
                          beside = TRUE,
                          resp.names = NA,
                          ...)
{
  s <- summary(model)
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    gof <- c(gof, AIC(model))
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    gof <- c(gof, BIC(model))
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    gof <- c(gof, VGAM::logLik.vlm(model))
    gof.names <- c(gof.names, "Log Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.df == TRUE) {
    gof <- c(gof, df <- s@df[2])
    gof.names <- c(gof.names, "DF")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, nobs(s))
    gof.names <- c(gof.names, "Num.\\ obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  besidereq <- nrow(s@coef3) > 
    length(s@extra$colnames.y) - 1 + length(all.vars(s@terms$terms)[-1])

  if (beside == TRUE & besidereq==TRUE) {
    trlist <- list()
    
    respcol <- seq(1, length(s@extra$colnames.y), by=1)
    if (is.na(resp.names)) resp.names <- respcol
    if (length(resp.names)!=length(respcol)) {
      warning("resp.names length does not match with number of response categories")
      resp.names <- respcol
    }
    
    for (i in 1:(length(respcol)-1)) {
      names <- rownames(coef(s))
      resploc <- grep(paste0(":",respcol[i],"$"),names)
      names <- gsub(paste0(":",respcol[i],"$"),"",names[resploc])
      co <- s@coef3[resploc, 1]
      se <- s@coef3[resploc, 2]
      pval <- s@coef3[resploc, 4]
      if (i==1) {
        tr <- createTexreg(coef.names = names, coef = co, se = se, 
                           pvalues = pval, gof.names = gof.names, 
                           gof = gof, gof.decimal = gof.decimal,
                           model.name = paste(resp.names[i],resp.names[i+1],sep="|"))
      } else {
        tr <- createTexreg(coef.names = names, coef = co, se = se, 
                           pvalues = pval, gof.names = character(), 
                           gof = numeric(), gof.decimal = logical(),
                           model.name = paste(resp.names[i],resp.names[i+1],sep="|"))
      }
      trlist <- c(trlist, tr)
    }
    if (length(trlist) == 1) {
      return(trlist[[1]])
    }
    else {
      return(trlist)
    }
  }
  else {
    names <- rownames(coef(s))
    co <- s@coef3[, 1]
    se <- s@coef3[, 2]
    pval <- s@coef3[, 4]
    tr <- createTexreg(coef.names = names, coef = co, se = se, 
                       pvalues = pval, gof.names = gof.names, gof = gof, gof.decimal = gof.decimal)
    return(tr)
  }
  
}
setMethod("extract", signature = className("vglm"), definition = extract.vglm)

# Modifying extract function of texreg to export brant test table
extract.brant <- function (model, 
                          ...)
{
  createTexreg(coef.names = rownames(model), coef = model[,1], se = numeric(), 
               pvalues = model[,3])
}
setMethod("extract", signature = "brant", definition = extract.brant)

## Variable Names Assignment
coef_assign_sifcct <- list("(Intercept)"="(Intercept)",
                           'edu'='University Education',
                           'agecatMiddle Aged (40-50s)'='Middle Aged (40-50s)',
                           'agecatElder (>=60s)'='Elder (>=60s)',
                           'edu:agecatMiddle Aged (40-50s)'='University*Middle Aged',
                           'edu:agecatElder (>=60s)'='University*Elder',
                           'knowledge'='Knowledge',
                           'polint'='Political Interest',
                           'employed'='Employed',
                           'evecon'='Economic Evaluation',
                           'income'='Income',
                           'lvpr'='Length of Residence')
coef_assign_sifcct_ol <- coef_assign_sifcct[-which(names(coef_assign_sifcct)%in%c('(Intercept)'))]
coef_assign_sifcct_notpr <- coef_assign_sifcct[-which(names(coef_assign_sifcct)%in%c('employed','income'))]
coef_assign_sifcct_pr <- coef_assign_sifcct[which(names(coef_assign_sifcct)%in%c('employed','income'))]
coef_assign_utas <- coef_assign_sifcct[-which(names(coef_assign_sifcct)%in%c('polint','income','lvpr'))]
coef_assign_utas_ol <- coef_assign_utas[-which(names(coef_assign_utas)%in%c('(Intercept)'))]
coef_assign_utas_notpr <- coef_assign_sifcct[-which(names(coef_assign_sifcct)%in%c('employed','polint','income','lvpr'))]
coef_assign_utas_pr <- coef_assign_sifcct[which(names(coef_assign_sifcct)%in%c('employed'))]
coef_assign_brant <- c(list('Omnibus'=NA,'edu_agecat'="University*Age Category"),
                       coef_assign_sifcct[-c(1,2,3,4,5)],
                       list('wave_fac'='SIFCCT Waves','year_fac'='UTAS Years'))

## Footnote
golnote_sifcct <- "%stars. Wave fixed effects omitted from the output.\nResponse ranges from 1 = disagree to 5 = agree for supporting permanent resident's local suffrage.\nThe model is estimated by partial proportional odds model (logit) through \\texttt{vglm} function in \\texttt{VGAM} package of \\texttt{R}."
golnote_utas <- gsub("Wave","Year",golnote_sifcct)
olnote_sifcct <- gsub("partial proportional odds model (logit) through \\texttt{vglm} function in \\texttt{VGAM} package of \\texttt{R}",
                      "ordered logit through \\texttt{polr} function in \\texttt{MASS}",
                      golnote_sifcct, fixed=TRUE)
olnote_utas <- gsub("Wave","Year",olnote_sifcct)
olnote_brant <- "%stars. Null Hypothesis is H0: parallel regression assumtion holds."
lmnote_sifcct <- gsub("partial proportional odds model (logit) through \\texttt{vglm} function in \\texttt{VGAM} package of \\texttt{R}",
                      "OLS regression, standard errors are clustered by prefectures.",
                      golnote_sifcct, fixed=TRUE)
lmnote_utas <- gsub("Wave","Year",lmnote_sifcct)
lgnote_sifcct <- gsub("partial proportional odds model (logit) through \\texttt{vglm} function in \\texttt{VGAM} package of \\texttt{R}",
                      "logit, standard errors are clustered by prefectures.",
                      golnote_sifcct, fixed=TRUE)
lgnote_utas <- gsub("Wave","Year",lgnote_sifcct)

#'
#' ## Partial Propotional Odds Model Tables
#'

# SIFCCT GOLogit Female
goltab_smof <- texreg(list(smof_3), single.row = FALSE, 
                   caption = "Education and the Support for Foreigner's Local Suffrage (Female, SIFCCT): Variables with Partial Proportional Odds Assumption Relaxed.", 
                   label = "goltab_smof",
                   stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                   booktabs = TRUE, use.packages = FALSE,
                   custom.coef.map = coef_assign_sifcct_notpr,
                   custom.note = gsub("\n","LINEBREAK",golnote_sifcct, fixed=TRUE),
                   caption.above = TRUE, digits = 3)
goltab_smof <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", goltab_smof)
goltab_smof <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{5\\}\\{l\\}\\{\\\\scriptsize\\{", goltab_smof)
goltab_smof
goltab_smof_pr <- texreg(list(smof_3), single.row = TRUE, beside = FALSE, 
                      caption = "Education and the Support for Foreigner's Local Suffrage (Female, SIFCCT): Variables with Partial Proportional Odds Assumption Retained.", 
                      custom.model.names = "Proportional Odds Coefficients",
                      label = "goltab_smof_pr",
                      stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                      booktabs = TRUE, use.packages = FALSE,
                      custom.coef.map = coef_assign_sifcct_pr,
                      custom.note = gsub("\n","LINEBREAK",golnote_sifcct, fixed=TRUE), 
                      caption.above = TRUE, digits = 3)
goltab_smof_pr <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", goltab_smof_pr)
goltab_smof_pr <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{2\\}\\{l\\}\\{\\\\scriptsize\\{", goltab_smof_pr)
goltab_smof_pr

# SIFCCT GOLogit Male
goltab_smom <- texreg(list(smom_3), single.row = FALSE, 
                      caption = "Education and the Support for Foreigner's Local Suffrage (Male, SIFCCT): Variables with Partial Proportional Odds Assumption Relaxed.", 
                      label = "goltab_smom",
                      stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                      booktabs = TRUE, use.packages = FALSE,
                      custom.coef.map = coef_assign_sifcct_notpr,
                      custom.note = gsub("\n","LINEBREAK",golnote_sifcct, fixed=TRUE),
                      caption.above = TRUE, digits = 3)
goltab_smom <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", goltab_smom)
goltab_smom <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{5\\}\\{l\\}\\{\\\\scriptsize\\{", goltab_smom)
goltab_smom
goltab_smom_pr <- texreg(list(smom_3), single.row = TRUE, beside = FALSE, 
                      caption = "Education and the Support for Foreigner's Local Suffrage (Male, SIFCCT): Variables with Partial Proportional Odds Assumption Retained.", 
                      label = "goltab_smom_pr",
                      custom.model.names = "Proportional Odds Coefficients",
                      stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                      booktabs = TRUE, use.packages = FALSE,
                      custom.coef.map = coef_assign_sifcct_pr,
                      custom.note = gsub("\n","LINEBREAK",golnote_sifcct, fixed=TRUE),
                      caption.above = TRUE, digits = 3)
goltab_smom_pr <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", goltab_smom_pr)
goltab_smom_pr <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{2\\}\\{l\\}\\{\\\\scriptsize\\{", goltab_smom_pr)
goltab_smom_pr
# UTAS GOLogit Female
goltab_umof <- texreg(list(umof_3), single.row = FALSE, 
                      caption = "Education and the Support for Foreigner's Local Suffrage (Female, UTAS): Variables with Partial Proportional Odds Assumption Relaxed.", 
                      label = "goltab_umof",
                      stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                      booktabs = TRUE, use.packages = FALSE,
                      custom.coef.map = coef_assign_utas_notpr,
                      custom.note = gsub("\n","LINEBREAK",golnote_utas, fixed=TRUE),
                      caption.above = TRUE, digits = 3)
goltab_umof <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", goltab_umof)
goltab_umof <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{5\\}\\{l\\}\\{\\\\scriptsize\\{", goltab_umof)
goltab_umof
goltab_umof_pr <- texreg(list(umof_3), single.row = FALSE, beside = FALSE,
                      caption = "Education and the Support for Foreigner's Local Suffrage (Female, UTAS): Variables with Partial Proportional Odds Assumption Retained.", 
                      label = "goltab_umof_pr",
                      stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                      booktabs = TRUE, use.packages = FALSE,
                      custom.coef.map = coef_assign_utas_pr,
                      custom.note = gsub("\n","LINEBREAK",golnote_utas, fixed=TRUE),
                      caption.above = TRUE, digits = 3)
goltab_umof_pr <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", goltab_umof_pr)
goltab_umof_pr <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{2\\}\\{l\\}\\{\\\\scriptsize\\{", goltab_umof_pr)
goltab_umof_pr

# UTAS GOLogit Male
goltab_umom <- texreg(list(umom_3), single.row = FALSE, 
                      caption = "Education and the Support for Foreigner's Local Suffrage (Male, UTAS): Variables with Partial Proportional Odds Assumption Relaxed.", 
                      label = "goltab_umom",
                      stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                      booktabs = TRUE, use.packages = FALSE,
                      custom.coef.map = coef_assign_utas_notpr,
                      custom.note = gsub("\n","LINEBREAK",golnote_utas, fixed=TRUE),
                      caption.above = TRUE, digits = 3)
goltab_umom <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", goltab_umom)
goltab_umom <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{5\\}\\{l\\}\\{\\\\scriptsize\\{", goltab_umom)
goltab_umom

goltab_umom_pr <- texreg(list(umom_3), single.row = FALSE, beside = FALSE,
                      caption = "Education and the Support for Foreigner's Local Suffrage (Male, UTAS): Variables with Partial Proportional Odds Assumption Retained.", 
                      label = "goltab_umom_pr",
                      stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                      booktabs = TRUE, use.packages = FALSE,
                      custom.coef.map = coef_assign_utas_pr,
                      custom.note = gsub("\n","LINEBREAK",golnote_utas, fixed=TRUE),
                      caption.above = TRUE, digits = 3)
goltab_umom_pr <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", goltab_umom_pr)
goltab_umom_pr <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{2\\}\\{l\\}\\{\\\\scriptsize\\{", goltab_umom_pr)
goltab_umom_pr

# Save Tex Files for GOLogit Tables
writeLines(goltab_smof, paste0(projdir, "/out/goltab_smof.tex"), useBytes=T)
writeLines(goltab_smom, paste0(projdir, "/out/goltab_smom.tex"), useBytes=T)
writeLines(goltab_umof, paste0(projdir, "/out/goltab_umof.tex"), useBytes=T)
writeLines(goltab_umom, paste0(projdir, "/out/goltab_umom.tex"), useBytes=T)
writeLines(goltab_smof_pr, paste0(projdir, "/out/goltab_smof_pr.tex"), useBytes=T)
writeLines(goltab_smom_pr, paste0(projdir, "/out/goltab_smom_pr.tex"), useBytes=T)
writeLines(goltab_umof_pr, paste0(projdir, "/out/goltab_umof_pr.tex"), useBytes=T)
writeLines(goltab_umom_pr, paste0(projdir, "/out/goltab_umom_pr.tex"), useBytes=T)

#'
#' ## Ordered Logit Tables
#'

smof_1_test <- coeftest(smof_1, vcov.=vcovCL(smof_1,factor(sifcct[which(sifcct$female==1),]$zip_pref)))
smom_1_test <- coeftest(smom_1, vcov.=vcovCL(smom_1,factor(sifcct[which(sifcct$female==0),]$zip_pref)))
umof_1_test <- coeftest(umof_1, vcov.=vcovCL(umof_1,factor(utas[which(utas$female==1),]$add_pref)))
umom_1_test <- coeftest(umom_1, vcov.=vcovCL(umom_1,factor(utas[which(utas$female==0),]$add_pref)))

# SIFCCT OLogit Table
oltab_smo <- texreg(list(smof_1,smom_1), single.row = TRUE, 
                    caption = "Education and the Support for Foreigner's Local Suffrage (Ordered Logit, SIFCCT)", 
                    label = "oltab_smo",
                    custom.model.names = c("Female","Male"),
                    # include.thresholds = TRUE,
                    override.se = list(smof_1_test[,2],smom_1_test[,2]),
                    override.pvalues =  list(smof_1_test[,4],smom_1_test[,4]),
                    stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                    booktabs = TRUE, use.packages = FALSE,
                    custom.coef.map = coef_assign_sifcct_ol,
                    custom.note = gsub("\n","LINEBREAK",olnote_sifcct, fixed=TRUE),
                    caption.above = TRUE, digits = 3)
oltab_smo <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", oltab_smo)
oltab_smo <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{3\\}\\{l\\}\\{\\\\scriptsize\\{", oltab_smo)
oltab_smo

# UTAS OLogit Table
oltab_umo <- texreg(list(umof_1,umom_1), single.row = TRUE, 
                    caption = "Education and the Support for Foreigner's Local Suffrage (Ordered Logit, UTAS)", 
                    label = "oltab_umo",
                    custom.model.names = c("Female","Male"),
                    # include.thresholds = TRUE,
                    override.se = list(umof_1_test[,2],umom_1_test[,2]),
                    override.pvalues =  list(umof_1_test[,4],umom_1_test[,4]),
                    stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                    booktabs = TRUE, use.packages = FALSE,
                    custom.coef.map = coef_assign_utas_ol,
                    custom.note = gsub("\n","LINEBREAK",olnote_utas, fixed=TRUE),
                    caption.above = TRUE, digits = 3)
oltab_umo <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", oltab_umo)
oltab_umo <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{3\\}\\{l\\}\\{\\\\scriptsize\\{", oltab_umo)
oltab_umo

# Brant Test
class(smof_1bt) <- class(smom_1bt) <- class(umof_1bt) <- class(umom_1bt) <- "brant"
oltab_brant <- texreg(list(smof_1bt,smom_1bt,umof_1bt,umom_1bt), single.row = TRUE, 
                      caption = "Brant Test for Ordered Logit Models", 
                      label = "oltab_brant",
                      custom.model.names = c("Female (SIFCCT)","Male (SIFCCT)","Female (UTAS)","Male (UTAS)"),
                      stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                      booktabs = TRUE, use.packages = FALSE,
                      custom.coef.map = coef_assign_brant,
                      custom.note = gsub("\n","LINEBREAK",olnote_brant, fixed=TRUE),
                      caption.above = TRUE, digits = 3)
oltab_brant <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", oltab_brant)
oltab_brant<- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{5\\}\\{l\\}\\{\\\\scriptsize\\{", oltab_brant)
oltab_brant

# Save Tex Files for OLogit Tables
writeLines(oltab_smo, paste0(projdir, "/out/oltab_smo.tex"), useBytes=T)
writeLines(oltab_umo, paste0(projdir, "/out/oltab_umo.tex"), useBytes=T)
writeLines(oltab_brant, paste0(projdir, "/out/oltab_brant.tex"), useBytes=T)

#'
#' ## OLS Linear Regression Tables
#'

smof_2_test <- coeftest(smof_2, vcov.=vcovCL(smof_2,factor(sifcct[which(sifcct$female==1),]$zip_pref)))
smom_2_test <- coeftest(smom_2, vcov.=vcovCL(smom_2,factor(sifcct[which(sifcct$female==0),]$zip_pref)))
umof_2_test <- coeftest(umof_2, vcov.=vcovCL(umof_2,factor(utas[which(utas$female==1),]$add_pref)))
umom_2_test <- coeftest(umom_2, vcov.=vcovCL(umom_2,factor(utas[which(utas$female==0),]$add_pref)))

# SIFCCT OLS Table
lmtab_smo <- texreg(list(smof_2,smom_2), single.row = TRUE, 
                    caption = "Education and the Support for Foreigner's Local Suffrage (OLS Regression, SIFCCT)", 
                    label = "lmtab_smo",
                    custom.model.names = c("Female","Male"),
                    override.se = list(smof_2_test[,2],smom_2_test[,2]),
                    override.pvalues =  list(smof_2_test[,4],smom_2_test[,4]),
                    stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                    booktabs = TRUE, use.packages = FALSE,
                    custom.coef.map = coef_assign_sifcct,
                    custom.note = gsub("\n","LINEBREAK",lmnote_sifcct, fixed=TRUE),
                    caption.above = TRUE, digits = 3)
lmtab_smo <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", lmtab_smo)
lmtab_smo <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{3\\}\\{l\\}\\{\\\\scriptsize\\{", lmtab_smo)
lmtab_smo

# UTAS OLS Table
lmtab_umo <- texreg(list(umof_2,umom_2), single.row = TRUE, 
                    caption = "Education and the Support for Foreigner's Local Suffrage (OLS Regression, UTAS)", 
                    label = "lmtab_umo",
                    override.se = list(umof_2_test[,2],umom_2_test[,2]),
                    override.pvalues =  list(umof_2_test[,4],umom_2_test[,4]),
                    stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                    booktabs = TRUE, use.packages = FALSE,
                    custom.coef.map = coef_assign_utas,
                    custom.note = gsub("\n","LINEBREAK",lmnote_utas, fixed=TRUE),
                    caption.above = TRUE, digits = 3)
lmtab_umo <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", lmtab_umo)
lmtab_umo <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{3\\}\\{l\\}\\{\\\\scriptsize\\{", lmtab_umo)
lmtab_umo

# Save Tex Files for OLS Tables
writeLines(lmtab_smo, paste0(projdir, "/out/lmtab_smo.tex"), useBytes=T)
writeLines(lmtab_umo, paste0(projdir, "/out/lmtab_umo.tex"), useBytes=T)

#'
#' ## Logit Tables
#'

smof_4_test <- coeftest(smof_4, vcov.=vcovCL(smof_4,factor(sifcct[which(sifcct$female==1),]$zip_pref)))
smom_4_test <- coeftest(smom_4, vcov.=vcovCL(smom_4,factor(sifcct[which(sifcct$female==0),]$zip_pref)))
umof_4_test <- coeftest(umof_4, vcov.=vcovCL(umof_4,factor(utas[which(utas$female==1),]$add_pref)))
umom_4_test <- coeftest(umom_4, vcov.=vcovCL(umom_4,factor(utas[which(utas$female==0),]$add_pref)))

# SIFCCT Logit Table
lgtab_smo <- texreg(list(smof_4,smom_4), single.row = TRUE, 
                    caption = "Education and the Support for Foreigner's Local Suffrage (Logit, SIFCCT)", 
                    label = "lgtab_smo",
                    custom.model.names = c("Female","Male"),
                    override.se = list(smof_4_test[,2],smom_4_test[,2]),
                    override.pvalues =  list(smof_4_test[,4],smom_4_test[,4]),
                    stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                    booktabs = TRUE, use.packages = FALSE,
                    custom.coef.map = coef_assign_sifcct,
                    custom.note = gsub("\n","LINEBREAK",lgnote_sifcct, fixed=TRUE),
                    caption.above = TRUE, digits = 3)
lgtab_smo <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", lgtab_smo)
lgtab_smo <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{3\\}\\{l\\}\\{\\\\scriptsize\\{", lgtab_smo)
lgtab_smo

# UTAS Logit Table
lgtab_umo <- texreg(list(umof_4,umom_4), single.row = TRUE, 
                    caption = "Education and the Support for Foreigner's Local Suffrage (Logit, UTAS)", 
                    label = "lgtab_umo",
                    override.se = list(umof_4_test[,2],umom_4_test[,2]),
                    override.pvalues =  list(umof_4_test[,4],umom_4_test[,4]),
                    stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
                    booktabs = TRUE, use.packages = FALSE,
                    custom.coef.map = coef_assign_utas,
                    custom.note = gsub("\n","LINEBREAK",lgnote_utas, fixed=TRUE),
                    caption.above = TRUE, digits = 3)
lgtab_umo <- gsub("\\{dagger\\}","\\{\\\\dagger\\}", lgtab_umo)
lgtab_umo <- gsub("LINEBREAK","\\}\\} \\\\\\\\ \\\\multicolumn\\{3\\}\\{l\\}\\{\\\\scriptsize\\{", lgtab_umo)
lgtab_umo

# Save Tex Files for OLS Tables
writeLines(lgtab_smo, paste0(projdir, "/out/lgtab_smo.tex"), useBytes=T)
writeLines(lgtab_umo, paste0(projdir, "/out/lgtab_umo.tex"), useBytes=T)

#'
#' # Coefficient Plot of Logit
#'

## Logit with Different Age Base Category
# Middle Aged As Base Category
sifcct$agecat <- factor(sifcct$agecat, levels=c("Middle Aged (40-50s)",
                                                "Young (<=30s)",
                                                "Elder (>=60s)"))
smof_4m <- glm(update(foreignsuff>=0.75 ~ ., basemodA), 
              data=sifcct[which(sifcct$female==1),],
              family = binomial("logit"))
smom_4m <- glm(update(foreignsuff>=0.75 ~ ., basemodA), 
              data=sifcct[which(sifcct$female==0),],
              family = binomial("logit"))
# Elder As Base Category
sifcct$agecat <- factor(sifcct$agecat, levels=c("Elder (>=60s)",
                                                "Young (<=30s)",
                                                "Middle Aged (40-50s)"))
smof_4e <- glm(update(foreignsuff>=0.75 ~ ., basemodA), 
               data=sifcct[which(sifcct$female==1),],
               family = binomial("logit"))
smom_4e <- glm(update(foreignsuff>=0.75 ~ ., basemodA), 
               data=sifcct[which(sifcct$female==0),],
               family = binomial("logit"))
## Put All Back
sifcct$agecat <- factor(sifcct$agecat, levels=c("Young (<=30s)",
                                                "Middle Aged (40-50s)",
                                                "Elder (>=60s)"))

# Middle Aged As Base Category
utas$agecat <- factor(utas$agecat, levels=c("Middle Aged (40-50s)",
                                                "Young (<=30s)",
                                                "Elder (>=60s)"))
umof_4m <- glm(update(foreignsuff>=0.75 ~ ., basemodB), 
               data=utas[which(utas$female==1),],
               family = binomial("logit"))
umom_4m <- glm(update(foreignsuff>=0.75 ~ ., basemodB), 
               data=utas[which(utas$female==0),],
               family = binomial("logit"))
# Elder As Base Category
utas$agecat <- factor(utas$agecat, levels=c("Elder (>=60s)",
                                                "Young (<=30s)",
                                                "Middle Aged (40-50s)"))
umof_4e <- glm(update(foreignsuff>=0.75 ~ ., basemodB), 
               data=utas[which(utas$female==1),],
               family = binomial("logit"))
umom_4e <- glm(update(foreignsuff>=0.75 ~ ., basemodB), 
               data=utas[which(utas$female==0),],
               family = binomial("logit"))
## Put All Back
utas$agecat <- factor(utas$agecat, levels=c("Young (<=30s)",
                                                "Middle Aged (40-50s)",
                                                "Elder (>=60s)"))

smof_4m_test <- coeftest(smof_4m, vcov.=vcovCL(smof_4m,factor(sifcct[which(sifcct$female==1),]$zip_pref)))
smom_4m_test <- coeftest(smom_4m, vcov.=vcovCL(smom_4m,factor(sifcct[which(sifcct$female==0),]$zip_pref)))
umof_4m_test <- coeftest(umof_4m, vcov.=vcovCL(umof_4m,factor(utas[which(utas$female==1),]$add_pref)))
umom_4m_test <- coeftest(umom_4m, vcov.=vcovCL(umom_4m,factor(utas[which(utas$female==0),]$add_pref)))
smof_4e_test <- coeftest(smof_4e, vcov.=vcovCL(smof_4e,factor(sifcct[which(sifcct$female==1),]$zip_pref)))
smom_4e_test <- coeftest(smom_4e, vcov.=vcovCL(smom_4e,factor(sifcct[which(sifcct$female==0),]$zip_pref)))
umof_4e_test <- coeftest(umof_4e, vcov.=vcovCL(umof_4e,factor(utas[which(utas$female==1),]$add_pref)))
umom_4e_test <- coeftest(umom_4e, vcov.=vcovCL(umom_4e,factor(utas[which(utas$female==0),]$add_pref)))

smof_4_ci90 <- coefci(smof_4, vcov.=vcovCL(smof_4,factor(sifcct[which(sifcct$female==1),]$zip_pref)), level=0.90)
smom_4_ci90 <- coefci(smom_4, vcov.=vcovCL(smom_4,factor(sifcct[which(sifcct$female==0),]$zip_pref)), level=0.90)
umof_4_ci90 <- coefci(umof_4, vcov.=vcovCL(umof_4,factor(utas[which(utas$female==1),]$add_pref)), level=0.90)
umom_4_ci90 <- coefci(umom_4, vcov.=vcovCL(umom_4,factor(utas[which(utas$female==0),]$add_pref)), level=0.90)
smof_4_ci95 <- coefci(smof_4, vcov.=vcovCL(smof_4,factor(sifcct[which(sifcct$female==1),]$zip_pref)), level=0.95)
smom_4_ci95 <- coefci(smom_4, vcov.=vcovCL(smom_4,factor(sifcct[which(sifcct$female==0),]$zip_pref)), level=0.95)
umof_4_ci95 <- coefci(umof_4, vcov.=vcovCL(umof_4,factor(utas[which(utas$female==1),]$add_pref)), level=0.95)
umom_4_ci95 <- coefci(umom_4, vcov.=vcovCL(umom_4,factor(utas[which(utas$female==0),]$add_pref)), level=0.95)
smof_4m_ci90 <- coefci(smof_4m, vcov.=vcovCL(smof_4m,factor(sifcct[which(sifcct$female==1),]$zip_pref)), level=0.90)
smom_4m_ci90 <- coefci(smom_4m, vcov.=vcovCL(smom_4m,factor(sifcct[which(sifcct$female==0),]$zip_pref)), level=0.90)
umof_4m_ci90 <- coefci(umof_4m, vcov.=vcovCL(umof_4m,factor(utas[which(utas$female==1),]$add_pref)), level=0.90)
umom_4m_ci90 <- coefci(umom_4m, vcov.=vcovCL(umom_4m,factor(utas[which(utas$female==0),]$add_pref)), level=0.90)
smof_4m_ci95 <- coefci(smof_4m, vcov.=vcovCL(smof_4m,factor(sifcct[which(sifcct$female==1),]$zip_pref)), level=0.95)
smom_4m_ci95 <- coefci(smom_4m, vcov.=vcovCL(smom_4m,factor(sifcct[which(sifcct$female==0),]$zip_pref)), level=0.95)
umof_4m_ci95 <- coefci(umof_4m, vcov.=vcovCL(umof_4m,factor(utas[which(utas$female==1),]$add_pref)), level=0.95)
umom_4m_ci95 <- coefci(umom_4m, vcov.=vcovCL(umom_4m,factor(utas[which(utas$female==0),]$add_pref)), level=0.95)
smof_4e_ci90 <- coefci(smof_4e, vcov.=vcovCL(smof_4e,factor(sifcct[which(sifcct$female==1),]$zip_pref)), level=0.90)
smom_4e_ci90 <- coefci(smom_4e, vcov.=vcovCL(smom_4e,factor(sifcct[which(sifcct$female==0),]$zip_pref)), level=0.90)
umof_4e_ci90 <- coefci(umof_4e, vcov.=vcovCL(umof_4e,factor(utas[which(utas$female==1),]$add_pref)), level=0.90)
umom_4e_ci90 <- coefci(umom_4e, vcov.=vcovCL(umom_4e,factor(utas[which(utas$female==0),]$add_pref)), level=0.90)
smof_4e_ci95 <- coefci(smof_4e, vcov.=vcovCL(smof_4e,factor(sifcct[which(sifcct$female==1),]$zip_pref)), level=0.95)
smom_4e_ci95 <- coefci(smom_4e, vcov.=vcovCL(smom_4e,factor(sifcct[which(sifcct$female==0),]$zip_pref)), level=0.95)
umof_4e_ci95 <- coefci(umof_4e, vcov.=vcovCL(umof_4e,factor(utas[which(utas$female==1),]$add_pref)), level=0.95)
umom_4e_ci95 <- coefci(umom_4e, vcov.=vcovCL(umom_4e,factor(utas[which(utas$female==0),]$add_pref)), level=0.95)

lgcfdt <- as.data.frame(rbind(
  c(smof_4_test[2,],smof_4_ci90[2,],smof_4_ci95[2,]),
  c(smof_4m_test[2,],smof_4m_ci90[2,],smof_4m_ci95[2,]),
  c(smof_4e_test[2,],smof_4e_ci90[2,],smof_4e_ci95[2,]),
  c(smom_4_test[2,],smom_4_ci90[2,],smom_4_ci95[2,]),
  c(smom_4m_test[2,],smom_4m_ci90[2,],smom_4m_ci95[2,]),
  c(smom_4e_test[2,],smom_4e_ci90[2,],smom_4e_ci95[2,]),
  c(umof_4_test[2,],umof_4_ci90[2,],umof_4_ci95[2,]),
  c(umof_4m_test[2,],umof_4m_ci90[2,],umof_4m_ci95[2,]),
  c(umof_4e_test[2,],umof_4e_ci90[2,],umof_4e_ci95[2,]),
  c(umom_4_test[2,],umom_4_ci90[2,],umom_4_ci95[2,]),
  c(umom_4m_test[2,],umom_4m_ci90[2,],umom_4m_ci95[2,]),
  c(umom_4e_test[2,],umom_4e_ci90[2,],umom_4e_ci95[2,])
  ))
colnames(lgcfdt) <- c("est","se","z","p","lci90","uci90","lci95","uci95")

lgcfdt$gender <- rep(c("Female","Male"), each=3)
lgcfdt$gender <- factor(lgcfdt$gender, levels=unique(lgcfdt$gender))
lgcfdt$agecat <- c("Young\n(<=30s)","Middle Aged\n(40-50s)","Elder\n(>=60s)")
lgcfdt$agecat <- factor(lgcfdt$agecat, levels=unique(lgcfdt$agecat))
lgcfdt$data <- factor(rep(c("SIFCCT","UTAS"),each=6),levels=c("SIFCCT","UTAS"))

cfpnote <- gsub("^%stars.*\\n|\\}","",lgnote_sifcct)
cfpnote <- gsub("\\texttt{","",cfpnote,fixed=TRUE)
cfpnote <- gsub("The model is","Models are", cfpnote)
cfpnote <- paste0(cfpnote,
                  "\nEach model is estimated within each gender subset of each dataset. All models include knowledge, political interest (only SIFCCT), employment,",
                  "\neconomic evaluation, income (only SIFCCT), and wave/year fixed effects as controls. See Appendix for the detailed tables.")

require(ggplot2)
p <- ggplot(lgcfdt, aes(x=agecat, y=exp(est))) + 
  geom_hline(aes(yintercept=1), linetype=2) + 
  geom_errorbar(aes(ymin=exp(lci95),ymax=exp(uci95),colour=data), #linetype=pstar 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) + 
  geom_errorbar(aes(ymin=exp(lci90),ymax=exp(uci90),colour=data),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=data, colour=data), 
             position=position_dodge(width=-0.7), size=3) + 
  facet_grid(agecat ~ gender, scales = "free_y") + 
  scale_shape_discrete(name="Dataset") + 
  scale_color_brewer(name="Dataset",type="qual", palette = 2) + 
  #scale_linetype_manual(name="Significance",values=c("solid","longdash","dotted")) + 
  ylab(bquote(atop(bold("Odds Ratio of Attaining University Education"),
                   "Confidence Interval: Thin Line = 95%, Thick Line = 90%"))) + 
  xlab(NULL) + 
  labs(caption=cfpnote, subtitle = "DV: The Agreement with Granting Local Suffrage to Permanent Residents") + 
  coord_flip() + theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        axis.text.y.left = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust=0),
        plot.caption.position = "plot",
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_text(size=10, color="black"))
p

ggsave(paste0(projdir,"/out/lgcoefplot_smoumo_3.png"),p,width=8,height=5)


#'
#' # Coefficient Plot of GOLogit
#'


# Extracting Coefficients
estlist_smof_3 <- list()
for (i in 1:4) {
  cf1 <- smof_3_sum@coef3[grep(paste0("^(edu).*:",i,"$"),rownames(smof_3_sum@coef3)),]
  rownames(cf1) <- gsub(paste0(":",i,"$"),"",rownames(cf1))
  vcov1 <- smof_3_sum@cov.unscaled[grep(paste0("^(edu).*:",i,"$"),rownames(smof_3_sum@cov.unscaled)),
                                   grep(paste0("^(edu).*:",i,"$"),colnames(smof_3_sum@cov.unscaled))]
  rownames(vcov1) <- gsub(paste0(":",i,"$"),"",rownames(vcov1))
  colnames(vcov1) <- gsub(paste0(":",i,"$"),"",colnames(vcov1))
  estlist_smof_3[[i]] <- list(cf1,vcov1)
}
estlist_smof_3

estlist_smom_3 <- list()
for (i in 1:4) {
  cf1 <- smom_3_sum@coef3[grep(paste0("^(edu).*:",i,"$"),rownames(smom_3_sum@coef3)),]
  rownames(cf1) <- gsub(paste0(":",i,"$"),"",rownames(cf1))
  vcov1 <- smom_3_sum@cov.unscaled[grep(paste0("^(edu).*:",i,"$"),rownames(smom_3_sum@cov.unscaled)),
                                   grep(paste0("^(edu).*:",i,"$"),colnames(smom_3_sum@cov.unscaled))]
  rownames(vcov1) <- gsub(paste0(":",i,"$"),"",rownames(vcov1))
  colnames(vcov1) <- gsub(paste0(":",i,"$"),"",colnames(vcov1))
  estlist_smom_3[[i]] <- list(cf1,vcov1)
}
estlist_smom_3

estlist_umof_3 <- list()
for (i in 1:4) {
  cf1 <- umof_3_sum@coef3[grep(paste0("^(edu).*:",i,"$"),rownames(umof_3_sum@coef3)),]
  rownames(cf1) <- gsub(paste0(":",i,"$"),"",rownames(cf1))
  vcov1 <- umof_3_sum@cov.unscaled[grep(paste0("^(edu).*:",i,"$"),rownames(umof_3_sum@cov.unscaled)),
                                   grep(paste0("^(edu).*:",i,"$"),colnames(umof_3_sum@cov.unscaled))]
  rownames(vcov1) <- gsub(paste0(":",i,"$"),"",rownames(vcov1))
  colnames(vcov1) <- gsub(paste0(":",i,"$"),"",colnames(vcov1))
  estlist_umof_3[[i]] <- list(cf1,vcov1)
}
estlist_umof_3

estlist_umom_3 <- list()
for (i in 1:4) {
  cf1 <- umom_3_sum@coef3[grep(paste0("^(edu).*:",i,"$"),rownames(umom_3_sum@coef3)),]
  rownames(cf1) <- gsub(paste0(":",i,"$"),"",rownames(cf1))
  vcov1 <- umom_3_sum@cov.unscaled[grep(paste0("^(edu).*:",i,"$"),rownames(umom_3_sum@cov.unscaled)),
                                   grep(paste0("^(edu).*:",i,"$"),colnames(umom_3_sum@cov.unscaled))]
  rownames(vcov1) <- gsub(paste0(":",i,"$"),"",rownames(vcov1))
  colnames(vcov1) <- gsub(paste0(":",i,"$"),"",colnames(vcov1))
  estlist_umom_3[[i]] <- list(cf1,vcov1)
}
estlist_umom_3

# Extracting Coefficients and SE

coefdt <- data.frame(data=factor(rep(c("SIFCCT","UTAS"),each=24),levels=c("SIFCCT","UTAS")),
                     cat=factor(rep(rep(c(">=Rather Disagree",">=Neither",
                                      ">=Rather Agree","Agree"), each=3),4),
                                levels=c(">=Rather Disagree",">=Neither",
                                         ">=Rather Agree","Agree")),
                     gender=factor(rep(rep(c("Female","Male"),each=12),2), levels=c("Female","Male")),
                     agecat=factor(rep(c("Young\n(<=30s)","Middle Aged\n(40-50s)","Elder\n(>=60s)"),16),
                                   levels=c("Young\n(<=30s)","Middle Aged\n(40-50s)","Elder\n(>=60s)"))
                     )

coefdt$est <- 
  c(estlist_smof_3[[1]][[1]][1,1],
  estlist_smof_3[[1]][[1]][1,1] + estlist_smof_3[[1]][[1]][2,1],
  estlist_smof_3[[1]][[1]][1,1] + estlist_smof_3[[1]][[1]][3,1],
  estlist_smof_3[[2]][[1]][1,1],
  estlist_smof_3[[2]][[1]][1,1] + estlist_smof_3[[2]][[1]][2,1],
  estlist_smof_3[[2]][[1]][1,1] + estlist_smof_3[[2]][[1]][3,1],
  estlist_smof_3[[3]][[1]][1,1],
  estlist_smof_3[[3]][[1]][1,1] + estlist_smof_3[[3]][[1]][2,1],
  estlist_smof_3[[3]][[1]][1,1] + estlist_smof_3[[3]][[1]][3,1],
  estlist_smof_3[[4]][[1]][1,1],
  estlist_smof_3[[4]][[1]][1,1] + estlist_smof_3[[4]][[1]][2,1],
  estlist_smof_3[[4]][[1]][1,1] + estlist_smof_3[[4]][[1]][3,1],
  estlist_smom_3[[1]][[1]][1,1],
  estlist_smom_3[[1]][[1]][1,1] + estlist_smom_3[[1]][[1]][2,1],
  estlist_smom_3[[1]][[1]][1,1] + estlist_smom_3[[1]][[1]][3,1],
  estlist_smom_3[[2]][[1]][1,1],
  estlist_smom_3[[2]][[1]][1,1] + estlist_smom_3[[2]][[1]][2,1],
  estlist_smom_3[[2]][[1]][1,1] + estlist_smom_3[[2]][[1]][3,1],
  estlist_smom_3[[3]][[1]][1,1],
  estlist_smom_3[[3]][[1]][1,1] + estlist_smom_3[[3]][[1]][2,1],
  estlist_smom_3[[3]][[1]][1,1] + estlist_smom_3[[3]][[1]][3,1],
  estlist_smom_3[[4]][[1]][1,1],
  estlist_smom_3[[4]][[1]][1,1] + estlist_smom_3[[4]][[1]][2,1],
  estlist_smom_3[[4]][[1]][1,1] + estlist_smom_3[[4]][[1]][3,1],
  estlist_umof_3[[1]][[1]][1,1],
  estlist_umof_3[[1]][[1]][1,1] + estlist_umof_3[[1]][[1]][2,1],
  estlist_umof_3[[1]][[1]][1,1] + estlist_umof_3[[1]][[1]][3,1],
  estlist_umof_3[[2]][[1]][1,1],
  estlist_umof_3[[2]][[1]][1,1] + estlist_umof_3[[2]][[1]][2,1],
  estlist_umof_3[[2]][[1]][1,1] + estlist_umof_3[[2]][[1]][3,1],
  estlist_umof_3[[3]][[1]][1,1],
  estlist_umof_3[[3]][[1]][1,1] + estlist_umof_3[[3]][[1]][2,1],
  estlist_umof_3[[3]][[1]][1,1] + estlist_umof_3[[3]][[1]][3,1],
  estlist_umof_3[[4]][[1]][1,1],
  estlist_umof_3[[4]][[1]][1,1] + estlist_umof_3[[4]][[1]][2,1],
  estlist_umof_3[[4]][[1]][1,1] + estlist_umof_3[[4]][[1]][3,1],
  estlist_umom_3[[1]][[1]][1,1],
  estlist_umom_3[[1]][[1]][1,1] + estlist_umom_3[[1]][[1]][2,1],
  estlist_umom_3[[1]][[1]][1,1] + estlist_umom_3[[1]][[1]][3,1],
  estlist_umom_3[[2]][[1]][1,1],
  estlist_umom_3[[2]][[1]][1,1] + estlist_umom_3[[2]][[1]][2,1],
  estlist_umom_3[[2]][[1]][1,1] + estlist_umom_3[[2]][[1]][3,1],
  estlist_umom_3[[3]][[1]][1,1],
  estlist_umom_3[[3]][[1]][1,1] + estlist_umom_3[[3]][[1]][2,1],
  estlist_umom_3[[3]][[1]][1,1] + estlist_umom_3[[3]][[1]][3,1],
  estlist_umom_3[[4]][[1]][1,1],
  estlist_umom_3[[4]][[1]][1,1] + estlist_umom_3[[4]][[1]][2,1],
  estlist_umom_3[[4]][[1]][1,1] + estlist_umom_3[[4]][[1]][3,1])

coefdt$se <- 
c(sqrt(estlist_smof_3[[1]][[2]][1,1]),
  sqrt(estlist_smof_3[[1]][[2]][1,1] + estlist_smof_3[[1]][[2]][2,2] + 2*estlist_smof_3[[1]][[2]][2,1]),
  sqrt(estlist_smof_3[[1]][[2]][1,1] + estlist_smof_3[[1]][[2]][3,3] + 2*estlist_smof_3[[1]][[2]][3,1]),
  sqrt(estlist_smof_3[[2]][[2]][1,1]),
  sqrt(estlist_smof_3[[2]][[2]][1,1] + estlist_smof_3[[2]][[2]][2,2] + 2*estlist_smof_3[[2]][[2]][2,1]),
  sqrt(estlist_smof_3[[2]][[2]][1,1] + estlist_smof_3[[2]][[2]][3,3] + 2*estlist_smof_3[[2]][[2]][3,1]),
  sqrt(estlist_smof_3[[3]][[2]][1,1]),
  sqrt(estlist_smof_3[[3]][[2]][1,1] + estlist_smof_3[[3]][[2]][2,2] + 2*estlist_smof_3[[3]][[2]][2,1]),
  sqrt(estlist_smof_3[[3]][[2]][1,1] + estlist_smof_3[[3]][[2]][3,3] + 2*estlist_smof_3[[3]][[2]][3,1]),
  sqrt(estlist_smof_3[[4]][[2]][1,1]),
  sqrt(estlist_smof_3[[4]][[2]][1,1] + estlist_smof_3[[4]][[2]][2,2] + 2*estlist_smof_3[[4]][[2]][2,1]),
  sqrt(estlist_smof_3[[4]][[2]][1,1] + estlist_smof_3[[4]][[2]][3,3] + 2*estlist_smof_3[[4]][[2]][3,1]),
  sqrt(estlist_smom_3[[1]][[2]][1,1]),
  sqrt(estlist_smom_3[[1]][[2]][1,1] + estlist_smom_3[[1]][[2]][2,2] + 2*estlist_smom_3[[1]][[2]][2,1]),
  sqrt(estlist_smom_3[[1]][[2]][1,1] + estlist_smom_3[[1]][[2]][3,3] + 2*estlist_smom_3[[1]][[2]][3,1]),
  sqrt(estlist_smom_3[[2]][[2]][1,1]),
  sqrt(estlist_smom_3[[2]][[2]][1,1] + estlist_smom_3[[2]][[2]][2,2] + 2*estlist_smom_3[[2]][[2]][2,1]),
  sqrt(estlist_smom_3[[2]][[2]][1,1] + estlist_smom_3[[2]][[2]][3,3] + 2*estlist_smom_3[[2]][[2]][3,1]),
  sqrt(estlist_smom_3[[3]][[2]][1,1]),
  sqrt(estlist_smom_3[[3]][[2]][1,1] + estlist_smom_3[[3]][[2]][2,2] + 2*estlist_smom_3[[3]][[2]][2,1]),
  sqrt(estlist_smom_3[[3]][[2]][1,1] + estlist_smom_3[[3]][[2]][3,3] + 2*estlist_smom_3[[3]][[2]][3,1]),
  sqrt(estlist_smom_3[[4]][[2]][1,1]),
  sqrt(estlist_smom_3[[4]][[2]][1,1] + estlist_smom_3[[4]][[2]][2,2] + 2*estlist_smom_3[[4]][[2]][2,1]),
  sqrt(estlist_smom_3[[4]][[2]][1,1] + estlist_smom_3[[4]][[2]][3,3] + 2*estlist_smom_3[[4]][[2]][3,1]),
  sqrt(estlist_umof_3[[1]][[2]][1,1]),
  sqrt(estlist_umof_3[[1]][[2]][1,1] + estlist_umof_3[[1]][[2]][2,2] + 2*estlist_umof_3[[1]][[2]][2,1]),
  sqrt(estlist_umof_3[[1]][[2]][1,1] + estlist_umof_3[[1]][[2]][3,3] + 2*estlist_umof_3[[1]][[2]][3,1]),
  sqrt(estlist_umof_3[[2]][[2]][1,1]),
  sqrt(estlist_umof_3[[2]][[2]][1,1] + estlist_umof_3[[2]][[2]][2,2] + 2*estlist_umof_3[[2]][[2]][2,1]),
  sqrt(estlist_umof_3[[2]][[2]][1,1] + estlist_umof_3[[2]][[2]][3,3] + 2*estlist_umof_3[[2]][[2]][3,1]),
  sqrt(estlist_umof_3[[3]][[2]][1,1]),
  sqrt(estlist_umof_3[[3]][[2]][1,1] + estlist_umof_3[[3]][[2]][2,2] + 2*estlist_umof_3[[3]][[2]][2,1]),
  sqrt(estlist_umof_3[[3]][[2]][1,1] + estlist_umof_3[[3]][[2]][3,3] + 2*estlist_umof_3[[3]][[2]][3,1]),
  sqrt(estlist_umof_3[[4]][[2]][1,1]),
  sqrt(estlist_umof_3[[4]][[2]][1,1] + estlist_umof_3[[4]][[2]][2,2] + 2*estlist_umof_3[[4]][[2]][2,1]),
  sqrt(estlist_umof_3[[4]][[2]][1,1] + estlist_umof_3[[4]][[2]][3,3] + 2*estlist_umof_3[[4]][[2]][3,1]),
  sqrt(estlist_umom_3[[1]][[2]][1,1]),
  sqrt(estlist_umom_3[[1]][[2]][1,1] + estlist_umom_3[[1]][[2]][2,2] + 2*estlist_umom_3[[1]][[2]][2,1]),
  sqrt(estlist_umom_3[[1]][[2]][1,1] + estlist_umom_3[[1]][[2]][3,3] + 2*estlist_umom_3[[1]][[2]][3,1]),
  sqrt(estlist_umom_3[[2]][[2]][1,1]),
  sqrt(estlist_umom_3[[2]][[2]][1,1] + estlist_umom_3[[2]][[2]][2,2] + 2*estlist_umom_3[[2]][[2]][2,1]),
  sqrt(estlist_umom_3[[2]][[2]][1,1] + estlist_umom_3[[2]][[2]][3,3] + 2*estlist_umom_3[[2]][[2]][3,1]),
  sqrt(estlist_umom_3[[3]][[2]][1,1]),
  sqrt(estlist_umom_3[[3]][[2]][1,1] + estlist_umom_3[[3]][[2]][2,2] + 2*estlist_umom_3[[3]][[2]][2,1]),
  sqrt(estlist_umom_3[[3]][[2]][1,1] + estlist_umom_3[[3]][[2]][3,3] + 2*estlist_umom_3[[3]][[2]][3,1]),
  sqrt(estlist_umom_3[[4]][[2]][1,1]),
  sqrt(estlist_umom_3[[4]][[2]][1,1] + estlist_umom_3[[4]][[2]][2,2] + 2*estlist_umom_3[[4]][[2]][2,1]),
  sqrt(estlist_umom_3[[4]][[2]][1,1] + estlist_umom_3[[4]][[2]][3,3] + 2*estlist_umom_3[[4]][[2]][3,1]))

coefdt$p <- round((1-pnorm(abs(coefdt$est/coefdt$se)))*2,3)
coefdt$pstar <- ifelse(coefdt$p<0.05, "p < 0.05", ifelse(coefdt$p<0.1, "p < 0.10", "n.s."))
coefdt$pstar <- factor(coefdt$pstar, levels=c("p < 0.05","p < 0.10","n.s."))

coefdt$lci95 <- coefdt$est - coefdt$se*qnorm(0.975)
coefdt$uci95 <- coefdt$est + coefdt$se*qnorm(0.975)
coefdt$lci90 <- coefdt$est - coefdt$se*qnorm(0.95)
coefdt$uci90 <- coefdt$est + coefdt$se*qnorm(0.95)

cfpnote <- gsub("^%stars.*\\n|\\}","",golnote_sifcct)
cfpnote <- gsub("\\texttt{","",cfpnote,fixed=TRUE)
cfpnote <- gsub("The model is","Models are", cfpnote)
cfpnote <- paste0(cfpnote,
                  "\nEach model is estimated within each gender subset of each dataset. All models include knowledge, political interest (only SIFCCT), employment,",
                  "\neconomic evaluation, income (only SIFCCT), and wave/year fixed effects as controls. Parallel regression assumption retained for employment,",
                  "\nincome and wave/year fixed effects, relaxed for all other variables. See Appendix for the detailed tables.")
require(ggplot2)
p <- ggplot(coefdt, aes(x=cat, y=est)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lci95,ymax=uci95,colour=data), #linetype=pstar 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) + 
  geom_errorbar(aes(ymin=lci90,ymax=uci90,colour=data),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(shape=data, colour=data), 
             position=position_dodge(width=-0.7), size=3) + 
  facet_grid(agecat ~ gender) + 
  scale_shape_discrete(name="Dataset") + 
  scale_color_brewer(name="Dataset",type="qual", palette = 2) + 
  #scale_linetype_manual(name="Significance",values=c("solid","longdash","dotted")) + 
  ylab("Partial Propotional Odds Model (Logit) Coefficient\n(Thin Line = 95% CI; Thick Line = 90% CI)") + 
  xlab("Granting Local Suffrage to Pemanent Residents") + 
  labs(caption=cfpnote) + 
  coord_flip() + theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.caption.position = "plot",
        axis.text.y = element_text(size=10, color="black"))
p

ggsave(paste0(projdir,"/out/golcoefplot_smoumo_3.png"),p,width=8,height=5)

#'
#' # Predicted Probabilities from GOLogit
#'

# Footnote Description
golprfootnote_sifcct <-  
"Predicted probabilities are generated from the models presented in Figure X.
Employment is fixed at employed (mode). All other predictors are fixed at median. Wave is fixed at 15 (December 2012)."
golprfootnote_utas <- gsub("Wave is fixed at 15 (December 2012)",
                           "Year is fixed at 2012", golprfootnote_sifcct, fixed=TRUE)
#'
#' ## SIFCCT
#'  
sifcct_newdt <- data.frame(edu = rep(c(0,1),each=3), 
                           agecat = rep(c("Young (<=30s)","Middle Aged (40-50s)","Elder (>=60s)"),2), 
                           knowledge = median(sifcct$knowledge),
                           polint = median(sifcct$polint),
                           employed = median(sifcct$employed),
                           evecon = median(sifcct$evecon),
                           income = median(sifcct$income),
                           lvpr = median(sifcct$lvpr),
                           wave = 15)

sifcct_prdt <- rbind(sifcct_newdt,sifcct_newdt,sifcct_newdt,sifcct_newdt,sifcct_newdt)
sifcct_prdt$cat <- rep(c("Disagree","Rather Disagree","Neither","Rather Agree","Agree"),each=6)
sifcct_prdt$cat <- factor(sifcct_prdt$cat, levels=unique(sifcct_prdt$cat))
sifcct_prdt <- rbind(sifcct_prdt,sifcct_prdt)
sifcct_prdt$gender <- rep(c("Female","Male"),each=30)
sifcct_prdt$agecat <- factor(sifcct_prdt$agecat, levels=c("Young (<=30s)","Middle Aged (40-50s)","Elder (>=60s)"))

sifcct_prdt2 <- rbind(sifcct_newdt,sifcct_newdt)
sifcct_prdt2$gender <- rep(c("Female","Male"),each=6)
sifcct_prdt2$agecat <- factor(sifcct_prdt2$agecat, levels=c("Young (<=30s)","Middle Aged (40-50s)","Elder (>=60s)"))

require(faraway)
smof_pred <- predict(smof_3, newdata = sifcct_newdt, se.fit=TRUE)
f_est <- c(1 - ilogit(smof_pred$fitted.values[,1]),
           ilogit(smof_pred$fitted.values[,1]) - ilogit(smof_pred$fitted.values[,2]),
           ilogit(smof_pred$fitted.values[,2]) - ilogit(smof_pred$fitted.values[,3]),
           ilogit(smof_pred$fitted.values[,3]) - ilogit(smof_pred$fitted.values[,4]),
           ilogit(smof_pred$fitted.values[,4]))
f_est_12 <- ilogit(smof_pred$fitted.values[,1])
f_lci_12 <- ilogit(smof_pred$fitted.values[,1] - qnorm(0.975)*smof_pred$se.fit[,1])
f_uci_12 <- ilogit(smof_pred$fitted.values[,1] + qnorm(0.975)*smof_pred$se.fit[,1])
f_est_23 <- ilogit(smof_pred$fitted.values[,2])
f_lci_23 <- ilogit(smof_pred$fitted.values[,2] - qnorm(0.975)*smof_pred$se.fit[,2])
f_uci_23 <- ilogit(smof_pred$fitted.values[,2] + qnorm(0.975)*smof_pred$se.fit[,2])
f_est_34 <- ilogit(smof_pred$fitted.values[,3])
f_lci_34 <- ilogit(smof_pred$fitted.values[,3] - qnorm(0.975)*smof_pred$se.fit[,3])
f_uci_34 <- ilogit(smof_pred$fitted.values[,3] + qnorm(0.975)*smof_pred$se.fit[,3])
f_est_45 <- ilogit(smof_pred$fitted.values[,4])
f_lci_45 <- ilogit(smof_pred$fitted.values[,4] - qnorm(0.975)*smof_pred$se.fit[,4])
f_uci_45 <- ilogit(smof_pred$fitted.values[,4] + qnorm(0.975)*smof_pred$se.fit[,4])

smom_pred <- predict(smom_3, newdata = sifcct_newdt, se.fit=TRUE)
m_est <- c(1 - ilogit(smom_pred$fitted.values[,1]),
           ilogit(smom_pred$fitted.values[,1]) - ilogit(smom_pred$fitted.values[,2]),
           ilogit(smom_pred$fitted.values[,2]) - ilogit(smom_pred$fitted.values[,3]),
           ilogit(smom_pred$fitted.values[,3]) - ilogit(smom_pred$fitted.values[,4]),
           ilogit(smom_pred$fitted.values[,4]))
m_est_12 <- ilogit(smom_pred$fitted.values[,1])
m_lci_12 <- ilogit(smom_pred$fitted.values[,1] - qnorm(0.975)*smom_pred$se.fit[,1])
m_uci_12 <- ilogit(smom_pred$fitted.values[,1] + qnorm(0.975)*smom_pred$se.fit[,1])
m_est_23 <- ilogit(smom_pred$fitted.values[,2])
m_lci_23 <- ilogit(smom_pred$fitted.values[,2] - qnorm(0.975)*smom_pred$se.fit[,2])
m_uci_23 <- ilogit(smom_pred$fitted.values[,2] + qnorm(0.975)*smom_pred$se.fit[,2])
m_est_34 <- ilogit(smom_pred$fitted.values[,3])
m_lci_34 <- ilogit(smom_pred$fitted.values[,3] - qnorm(0.975)*smom_pred$se.fit[,3])
m_uci_34 <- ilogit(smom_pred$fitted.values[,3] + qnorm(0.975)*smom_pred$se.fit[,3])
m_est_45 <- ilogit(smom_pred$fitted.values[,4])
m_lci_45 <- ilogit(smom_pred$fitted.values[,4] - qnorm(0.975)*smom_pred$se.fit[,4])
m_uci_45 <- ilogit(smom_pred$fitted.values[,4] + qnorm(0.975)*smom_pred$se.fit[,4])

sifcct_prdt$est <- c(f_est,m_est)
sifcct_prdt2$est_12 <- c(f_est_12,m_est_12)
sifcct_prdt2$lci_12 <- c(f_lci_12,m_lci_12)
sifcct_prdt2$uci_12 <- c(f_uci_12,m_uci_12)
sifcct_prdt2$est_23 <- c(f_est_23,m_est_23)
sifcct_prdt2$lci_23 <- c(f_lci_23,m_lci_23)
sifcct_prdt2$uci_23 <- c(f_uci_23,m_uci_23)
sifcct_prdt2$est_34 <- c(f_est_34,m_est_34)
sifcct_prdt2$lci_34 <- c(f_lci_34,m_lci_34)
sifcct_prdt2$uci_34 <- c(f_uci_34,m_uci_34)
sifcct_prdt2$est_45 <- c(f_est_45,m_est_45)
sifcct_prdt2$lci_45 <- c(f_lci_45,m_lci_45)
sifcct_prdt2$uci_45 <- c(f_uci_45,m_uci_45)

require(ggplot2)
p <- ggplot(sifcct_prdt, aes(x=1-edu)) + 
  geom_col(aes(fill=cat, y=est), position=position_stack()) + 
  geom_errorbar(data=sifcct_prdt2, aes(x=1-edu-0.1,ymin=lci_12,ymax=uci_12),width=0.15) +
  geom_point(data=sifcct_prdt2, aes(x=1-edu-0.1,y=est_12)) +
  geom_errorbar(data=sifcct_prdt2, aes(x=1-edu+0.1,ymin=lci_23,ymax=uci_23),width=0.15,position=position_dodge(width=0.1)) +
  geom_point(data=sifcct_prdt2, aes(x=1-edu+0.1,y=est_23)) +
  geom_errorbar(data=sifcct_prdt2, aes(x=1-edu+0.1,ymin=lci_34,ymax=uci_34),width=0.15) +
  geom_point(data=sifcct_prdt2, aes(x=1-edu+0.1,y=est_34)) +
  geom_errorbar(data=sifcct_prdt2, aes(x=1-edu-0.1,ymin=lci_45,ymax=uci_45),width=0.15,position=position_dodge(width=0.1)) +
  geom_point(data=sifcct_prdt2, aes(x=1-edu-0.1,y=est_45)) +
  coord_flip() + 
  facet_grid(agecat ~ gender, ) + 
  #geom_area(aes(fill=cat)) + 
  scale_fill_brewer(name="Granting Local Suffrage\nto Permanent Residents", type="div") +
  ylab("Cummulative Predicted Probability") + 
  xlab("Education") + 
  #ggtitle("Cummulative Predicted Probability from Ordinal Logit") + 
  scale_x_continuous(breaks=c(1,0),labels=c("High School\nor Less","University\nor More")) + # No Expansion of axis
  scale_y_continuous() + 
  labs(caption=golprfootnote_sifcct) + 
  guides(fill = guide_legend(reverse = TRUE)) +  
  theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.caption.position = "plot",
        axis.text.y = element_text(size=10))
p

ggsave(paste0(projdir,"/out/golpredplot_smo_3.png"),p,width=8,height=5)

#'
#' ## UTAS
#'  

utas_newdt <- data.frame(edu = rep(c(0,1),each=3), 
                         agecat = rep(c("Young (<=30s)","Middle Aged (40-50s)","Elder (>=60s)"),2), 
                         knowledge = median(utas$knowledge),
                         employed = median(utas$employed),
                         evecon = median(utas$evecon),
                         year = 2012)

utas_prdt <- rbind(utas_newdt,utas_newdt,utas_newdt,utas_newdt,utas_newdt)
utas_prdt$cat <- rep(c("Disagree","Rather Disagree","Neither","Rather Agree","Agree"),each=6)
utas_prdt$cat <- factor(utas_prdt$cat, levels=unique(utas_prdt$cat))
utas_prdt <- rbind(utas_prdt,utas_prdt)
utas_prdt$gender <- rep(c("Female","Male"),each=30)
utas_prdt$agecat <- factor(utas_prdt$agecat, levels=c("Young (<=30s)","Middle Aged (40-50s)","Elder (>=60s)"))

utas_prdt2 <- rbind(utas_newdt,utas_newdt)
utas_prdt2$gender <- rep(c("Female","Male"),each=6)
utas_prdt2$agecat <- factor(utas_prdt2$agecat, levels=c("Young (<=30s)","Middle Aged (40-50s)","Elder (>=60s)"))


require(faraway)
umof_pred <- predict(umof_3, newdata = utas_newdt, se.fit=TRUE)
umof_pred
f_est <- c(1 - ilogit(umof_pred$fitted.values[,1]),
           ilogit(umof_pred$fitted.values[,1]) - ilogit(umof_pred$fitted.values[,2]),
           ilogit(umof_pred$fitted.values[,2]) - ilogit(umof_pred$fitted.values[,3]),
           ilogit(umof_pred$fitted.values[,3]) - ilogit(umof_pred$fitted.values[,4]),
           ilogit(umof_pred$fitted.values[,4]))
f_est_12 <- ilogit(umof_pred$fitted.values[,1])
f_lci_12 <- ilogit(umof_pred$fitted.values[,1] - qnorm(0.975)*umof_pred$se.fit[,1])
f_uci_12 <- ilogit(umof_pred$fitted.values[,1] + qnorm(0.975)*umof_pred$se.fit[,1])
f_est_23 <- ilogit(umof_pred$fitted.values[,2])
f_lci_23 <- ilogit(umof_pred$fitted.values[,2] - qnorm(0.975)*umof_pred$se.fit[,2])
f_uci_23 <- ilogit(umof_pred$fitted.values[,2] + qnorm(0.975)*umof_pred$se.fit[,2])
f_est_34 <- ilogit(umof_pred$fitted.values[,3])
f_lci_34 <- ilogit(umof_pred$fitted.values[,3] - qnorm(0.975)*umof_pred$se.fit[,3])
f_uci_34 <- ilogit(umof_pred$fitted.values[,3] + qnorm(0.975)*umof_pred$se.fit[,3])
f_est_45 <- ilogit(umof_pred$fitted.values[,4])
f_lci_45 <- ilogit(umof_pred$fitted.values[,4] - qnorm(0.975)*umof_pred$se.fit[,4])
f_uci_45 <- ilogit(umof_pred$fitted.values[,4] + qnorm(0.975)*umof_pred$se.fit[,4])

umom_pred <- predict(umom_3, newdata = utas_newdt, se.fit=TRUE)
m_est <- c(1 - ilogit(umom_pred$fitted.values[,1]),
           ilogit(umom_pred$fitted.values[,1]) - ilogit(umom_pred$fitted.values[,2]),
           ilogit(umom_pred$fitted.values[,2]) - ilogit(umom_pred$fitted.values[,3]),
           ilogit(umom_pred$fitted.values[,3]) - ilogit(umom_pred$fitted.values[,4]),
           ilogit(umom_pred$fitted.values[,4]))
m_est_12 <- ilogit(umom_pred$fitted.values[,1])
m_lci_12 <- ilogit(umom_pred$fitted.values[,1] - qnorm(0.975)*umom_pred$se.fit[,1])
m_uci_12 <- ilogit(umom_pred$fitted.values[,1] + qnorm(0.975)*umom_pred$se.fit[,1])
m_est_23 <- ilogit(umom_pred$fitted.values[,2])
m_lci_23 <- ilogit(umom_pred$fitted.values[,2] - qnorm(0.975)*umom_pred$se.fit[,2])
m_uci_23 <- ilogit(umom_pred$fitted.values[,2] + qnorm(0.975)*umom_pred$se.fit[,2])
m_est_34 <- ilogit(umom_pred$fitted.values[,3])
m_lci_34 <- ilogit(umom_pred$fitted.values[,3] - qnorm(0.975)*umom_pred$se.fit[,3])
m_uci_34 <- ilogit(umom_pred$fitted.values[,3] + qnorm(0.975)*umom_pred$se.fit[,3])
m_est_45 <- ilogit(umom_pred$fitted.values[,4])
m_lci_45 <- ilogit(umom_pred$fitted.values[,4] - qnorm(0.975)*umom_pred$se.fit[,4])
m_uci_45 <- ilogit(umom_pred$fitted.values[,4] + qnorm(0.975)*umom_pred$se.fit[,4])

utas_prdt$est <- c(f_est,m_est)
utas_prdt2$est_12 <- c(f_est_12,m_est_12)
utas_prdt2$lci_12 <- c(f_lci_12,m_lci_12)
utas_prdt2$uci_12 <- c(f_uci_12,m_uci_12)
utas_prdt2$est_23 <- c(f_est_23,m_est_23)
utas_prdt2$lci_23 <- c(f_lci_23,m_lci_23)
utas_prdt2$uci_23 <- c(f_uci_23,m_uci_23)
utas_prdt2$est_34 <- c(f_est_34,m_est_34)
utas_prdt2$lci_34 <- c(f_lci_34,m_lci_34)
utas_prdt2$uci_34 <- c(f_uci_34,m_uci_34)
utas_prdt2$est_45 <- c(f_est_45,m_est_45)
utas_prdt2$lci_45 <- c(f_lci_45,m_lci_45)
utas_prdt2$uci_45 <- c(f_uci_45,m_uci_45)

require(ggplot2)
p <- ggplot(utas_prdt, aes(x=1-edu)) + 
  geom_col(aes(fill=cat, y=est), position=position_stack()) + 
  geom_errorbar(data=utas_prdt2, aes(x=1-edu-0.1,ymin=lci_12,ymax=uci_12),width=0.15) +
  geom_point(data=utas_prdt2, aes(x=1-edu-0.1,y=est_12)) +
  geom_errorbar(data=utas_prdt2, aes(x=1-edu+0.1,ymin=lci_23,ymax=uci_23),width=0.15,position=position_dodge(width=0.1)) +
  geom_point(data=utas_prdt2, aes(x=1-edu+0.1,y=est_23)) +
  geom_errorbar(data=utas_prdt2, aes(x=1-edu+0.1,ymin=lci_34,ymax=uci_34),width=0.15) +
  geom_point(data=utas_prdt2, aes(x=1-edu+0.1,y=est_34)) +
  geom_errorbar(data=utas_prdt2, aes(x=1-edu-0.1,ymin=lci_45,ymax=uci_45),width=0.15,position=position_dodge(width=0.1)) +
  geom_point(data=utas_prdt2, aes(x=1-edu-0.1,y=est_45)) +
  coord_flip() + 
  facet_grid(agecat ~ gender, ) + 
  #geom_area(aes(fill=cat)) + 
  scale_fill_brewer(name="Granting Local Suffrage\nto Permanent Residents", type="div") +
  ylab("Cummulative Predicted Probability") + 
  xlab("Education") + 
  #ggtitle("Cummulative Predicted Probability from Ordinal Logit") + 
  scale_x_continuous(breaks=c(1,0),labels=c("High School\nor Less","University\nor More")) + # No Expansion of axis
  scale_y_continuous() + 
  labs(caption=golprfootnote_utas) + 
  guides(fill = guide_legend(reverse = TRUE)) +  
  theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.caption.position = "plot",
        axis.text.y = element_text(size=10))
p

ggsave(paste0(projdir,"/out/golpredplot_umo_3.png"),p,width=8,height=5)

#'
#' # Save Image
#'

save.image(file=paste0(projdir,"/out/analysis_main_v4.RData"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/analysis_1_original_v4.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
# In Terminal, move to src directory and run:
# Rscript -e "rmarkdown::render('./src/analysis_1_original_v4.R', 'github_document', clean=FALSE)"
