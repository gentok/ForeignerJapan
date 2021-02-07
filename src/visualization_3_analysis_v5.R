#' ---
#' title: "Visualization 3: Analysis Results"
#' author: "Fan Lu & Gento Kato"
#' date: "January 26, 2020"
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

## Directories for Main Effect Data
visdtdir <- paste0(projdir, "/out/visdt.rds")
visdtmdir <- paste0(projdir, "/out/visdtm.rds")
visdtalldir <- paste0(projdir, "/out/visdtall.rds")
visdtxdir <- paste0(projdir, "/out/visdtx.rds")
visdtxmdir <- paste0(projdir, "/out/visdtxm.rds")
visdtxalldir <- paste0(projdir, "/out/visdtxall.rds")

## Directories for Mediation Effect Data
coefdtdir0 <- paste0(projdir,"/out/medoutcoefdt_unmatched_v5.rds")
coefdtdir1 <- paste0(projdir,"/out/medoutcoefdt_matchednoL_v5.rds")
coefdtdir2 <- paste0(projdir,"/out/medoutcoefdt_matchedL50_v5.rds")
coefdtdir3 <- paste0(projdir,"/out/medoutcoefdt_matchedL100_v5.rds")
coefdtdir4 <- paste0(projdir,"/out/medoutcoefdt_matchedL200_v5.rds")
coefdtdir5 <- paste0(projdir,"/out/medoutcoefdt_matchedL350_v5.rds")

## Packages
require(ggplot2)

#'
#' # Main Effects
#' 

## Import Required Data
visdt <- readRDS(visdtdir)
visdtm <- readRDS(visdtmdir)
visdtall <- readRDS(visdtalldir)

#' 
#' ## OLS
#' 

require(ggplot2)
p <- ggplot(visdt, aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ data) +
  scale_y_continuous(breaks = c(-0.1,-0.05,0.00,0.05)) + 
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  ylab("OLS Coefficient\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/maineffectplot1.png"),p,width=8,height=5)
ggsave(paste0(projdir,"/out/maineffectplot1.pdf"),p,width=8,height=5)

require(ggplot2)
p <- ggplot(visdt[which(visdt$data%in%c("Unmatched",
                                        "Matched without \nDistance Adj.",
                                        "Matched with \nLambda = 100km")),], 
            aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ data) +
  scale_y_continuous(breaks = c(-0.1,-0.05,0.00,0.05)) + 
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  ylab("OLS Coefficient\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/maineffectplot2.png"),p,width=8,height=5)
ggsave(paste0(projdir,"/out/maineffectplot2.pdf"),p,width=8,height=5)

#' 
#' ## Multinomial Logit (Disagree vs. Agree)
#' 


require(ggplot2)
p <- ggplot(visdtm, aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ data) +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  ylab("Multinomial Logit Coefficient: Agree over Disagree\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/maineffectplot1m.png"),p,width=8,height=5)
ggsave(paste0(projdir,"/out/maineffectplot1m.pdf"),p,width=8,height=5)

require(ggplot2)
p <- ggplot(visdtm[which(visdtm$data%in%c("Unmatched",
                                          "Matched without \nDistance Adj.",
                                          "Matched with \nLambda = 100km")),], 
            aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ data) +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  ylab("Multinomial Logit Coefficient: Agree over Disagree\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/maineffectplot2m.png"),p,width=8,height=5)
ggsave(paste0(projdir,"/out/maineffectplot2m.pdf"),p,width=8,height=5)

#'
#' ## Compare OLS and Multinomial Logit
#'

visdtsub <- subset(visdtall, data=="Unmatched")

visdtsub$method <- factor(gsub("Multinomial Logit\nAgree vs. Disagree",
                               "Multinomial Logit\nDisagree vs. Agree",
                               visdtsub$method), 
                          levels = c("OLS","Multinomial Logit\nDisagree vs. Agree"))

dummy <- data.frame(est = c(range(c(subset(visdtall, method=="OLS")$lci95,
                                    subset(visdtall, method=="OLS")$uci95),
                                  na.rm = TRUE),
                            range(c(subset(visdtall, method!="OLS")$lci95,
                                    subset(visdtall, method!="OLS")$uci95),
                                  na.rm = TRUE)),
                    gender = "Female", age = 45,
                    method = factor(rep(levels(visdtsub$method), each=2),
                                    levels = levels(visdtsub$method)))

require(ggplot2)
p <- ggplot(visdtsub, aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  geom_blank(data=dummy) + 
  facet_grid(gender ~ method, scales = "free_x") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop = FALSE) +
  labs(caption="Check Table 2 for the full results with coefficient values.") +
  xlab("Age") +
  labs(caption="Outcome: Agreement with granting suffrage to permanent residents \n(OLS: Five categories, rescaled to 0-1; Multinomial logit: Three categories, disagree, neigher, and agree).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/maineffectcompareolsmultinom.png"),p,width=8,height=5)
ggsave(paste0(projdir,"/out/maineffectcompareolsmultinom.pdf"),p,width=8,height=5)

#'
#' ## For Robustness Check
#'

visdtsub <- subset(visdtall, data%in%c("Matched without \nDistance Adj.",
                                       "Matched with \nLambda = 200km",
                                       "Mail-in"))

visdtsub$data2 <- factor(visdtsub$data, 
                          labels = c("Standard \nMatching",
                                     "Distance Adjusted \nMatching",
                                     "Mail-in \n(CI omitted)"))


visdtsub$method <- factor(gsub("Multinomial Logit\nAgree vs. Disagree",
                               "Multinomial Logit\nDisagree vs. Agree",
                               visdtsub$method), 
                          levels = c("OLS","Multinomial Logit\nDisagree vs. Agree"))

dummy <- data.frame(est = c(range(c(subset(visdtall, method=="OLS")$lci95,
                                    subset(visdtall, method=="OLS")$uci95),
                                  na.rm = TRUE),
                            range(c(subset(visdtall, method!="OLS")$lci95,
                                    subset(visdtall, method!="OLS")$uci95),
                                  na.rm = TRUE)),
                    gender = "Female", age = 45,
                    method = factor(rep(levels(visdtsub$method), each=2),
                                    levels = levels(visdtsub$method)))

require(ggplot2)
p <- ggplot(visdtsub, aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,alpha=pstar, color=data2), 
                position=position_dodge(width=-0.9), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,alpha=pstar, color=data2),
                position=position_dodge(width=-0.9), size=1.5, width=0.0) +
  geom_point(aes(alpha=pstar, shape=data2, color=data2),
             position=position_dodge(width=-0.9), size=3) +
  geom_blank(data=dummy) + 
  facet_grid(gender ~ method, scales = "free_x") +
  scale_color_manual(name="Data", values = rep("black", 3)) + 
  scale_shape_discrete(name="Data") + 
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop = FALSE) +
  ylab("University Education (1:Attained, 0:Not Attained) Coefficient\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Check Online Appendix for the full results with coefficient values. CI omitted for mail-in survey results since they are too wide.") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/maineffectrobustnesscheck.png"),p,width=8,height=5)
ggsave(paste0(projdir,"/out/maineffectrobustnesscheck.pdf"),p,width=8,height=5)

#'
#' # Main Effects (Movers)
#' 

## Import Required Data
visdtx <- readRDS(visdtxdir)
visdtxm <- readRDS(visdtxmdir)
visdtxall <- readRDS(visdtxalldir)

#' 
#' ## OLS
#' 

require(ggplot2)
p <- ggplot(visdtx, aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ data) +
  scale_y_continuous(breaks = c(-0.1,-0.05,0.00,0.05)) + 
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  ylab("OLS Coefficient\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/maineffectplotx.png"),p,width=8,height=5)
ggsave(paste0(projdir,"/out/maineffectplotx.pdf"),p,width=8,height=5)

#' 
#' ## Multinomial Logit (Disagree vs. Agree)
#' 


require(ggplot2)
p <- ggplot(visdtxm, aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  facet_grid(gender ~ data) +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop=FALSE) +
  ylab("Multinomial Logit Coefficient: Agree over Disagree\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). \nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1).") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/maineffectplotxm.png"),p,width=8,height=5)
ggsave(paste0(projdir,"/out/maineffectplotxm.pdf"),p,width=8,height=5)

#'
#' ## Compare OLS and Multinomial Logit
#'

visdtxsub <- subset(visdtxall, data=="Unmatched")

visdtxsub$method <- factor(gsub("Multinomial Logit\nAgree vs. Disagree",
                               "Multinomial Logit\nDisagree vs. Agree",
                               visdtxsub$method), 
                          levels = c("OLS","Multinomial Logit\nDisagree vs. Agree"))

dummy <- data.frame(est = c(range(c(subset(visdtxall, method=="OLS")$lci95,
                                    subset(visdtxall, method=="OLS")$uci95),
                                  na.rm = TRUE),
                            range(c(subset(visdtxall, method!="OLS")$lci95,
                                    subset(visdtxall, method!="OLS")$uci95),
                                  na.rm = TRUE)),
                    gender = "Female", age = 45,
                    method = factor(rep(levels(visdtxsub$method), each=2),
                                    levels = levels(visdtxsub$method)))

require(ggplot2)
p <- ggplot(visdtxsub, aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,alpha=pstar), 
                position=position_dodge(width=-0.7), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,alpha=pstar),
                position=position_dodge(width=-0.7), size=1.5, width=0.0) +
  geom_point(aes(alpha=pstar),
             position=position_dodge(width=-0.7), size=3) +
  geom_blank(data = dummy) + 
  facet_grid(gender ~ method, scales = "free_x") +
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop = FALSE) +
  ylab("University Education (1:Attained, 0:Not Attained) Coefficient\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Check Online Appendix for the full results with coefficient values.") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/maineffectcompareolsmultinomx.png"),p,width=8,height=5)
ggsave(paste0(projdir,"/out/maineffectcompareolsmultinomx.pdf"),p,width=8,height=5)

#'
#' ## For Robustness Check
#'

visdtxsub <- subset(visdtxall, data%in%c("Matched without \nDistance Adj.",
                                       "Mail-in"))

visdtxsub$data2 <- factor(visdtxsub$data, 
                          labels = c("Standard\nMatching",
                                     "Mail-in \n(CI omitted)"))

visdtxsub$method <- factor(gsub("Multinomial Logit\nAgree vs. Disagree",
                               "Multinomial Logit\nDisagree vs. Agree",
                               visdtxsub$method), 
                          levels = c("OLS","Multinomial Logit\nDisagree vs. Agree"))

dummy <- data.frame(est = c(range(c(subset(visdtxall, method=="OLS")$lci95,
                                    subset(visdtxall, method=="OLS")$uci95),
                                  na.rm = TRUE),
                            range(c(subset(visdtxall, method!="OLS")$lci95,
                                    subset(visdtxall, method!="OLS")$uci95),
                                  na.rm = TRUE)),
                    gender = "Female", age = 45,
                    method = factor(rep(levels(visdtxsub$method), each=2),
                                    levels = levels(visdtxsub$method)))

require(ggplot2)
p <- ggplot(visdtxsub, aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,alpha=pstar, color=data2), 
                position=position_dodge(width=-0.9), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,alpha=pstar, color=data2),
                position=position_dodge(width=-0.9), size=1.5, width=0.0) +
  geom_point(aes(alpha=pstar, shape=data2, color=data2),
             position=position_dodge(width=-0.9), size=3) +
  geom_blank(data=dummy) + 
  facet_grid(gender ~ method, scales = "free_x") +
  scale_color_manual(name="Data", values = rep("black", 3)) + 
  scale_shape_discrete(name="Data") + 
  scale_alpha_manual(name="Significance",values=c(1,0.5,0.2), drop = FALSE) +
  ylab("University Education (1:Attained, 0:Not Attained) Coefficient\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Check Online Appendix for the full results with coefficient values. CI omitted for mail-in survey results since they are too wide.") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=11),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/maineffectrobustnesscheckx.png"),p,width=8,height=5)
ggsave(paste0(projdir,"/out/maineffectrobustnesscheckx.pdf"),p,width=8,height=5)

#'
#' # Mediation Effects
#'
#'
#' ## Function to Subset Data (Except for knowledge)
#'

gencoefdts <- function(coefdt) {
  
  coefdt$med <- factor(coefdt$med, levels=c("income","knowledge","ideology","ldpdpjft",
                                            "familiarityFT_KOR","familiarityFT_CHN",
                                            "familiarityFT_USA"),
                       labels = c("Income\n(Percentile)",
                                  "Political\nKnowledge",
                                  "Political\nIdeology",
                                  "LDP - DPJ\nFeeling\nThermometer",
                                  "South Korea\nFeeling\nThermometer",
                                  "China\nFeeling\nThermometer",
                                  "United States\nFeeling\nThermometer"))
  
  
  coefdts <- subset(coefdt, med!="Political\nKnowledge" & 
                      mod!="Treatment => Outcome\n(ADE)" & 
                      age %in% c(25,45,65))
  
  return(coefdts)

}

#'
#' ## Unmatched
#'

coefdts <- gencoefdts(readRDS(coefdtdir0))

require(ggplot2)
p <- ggplot(coefdts, 
            aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,color=gender,alpha=pstar), #linetype=pstar
                position=position_dodge(width=-0.9), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,color=gender,alpha=pstar),
                position=position_dodge(width=-0.9), size=1.5, width=0.0) +
  geom_point(aes(shape=gender,alpha=pstar),
             position=position_dodge(width=-0.9), size=3) +
  facet_grid(med ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance (Transparency)",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Gender (Point Shape)") + 
  scale_color_manual(name="Gender (Point Shape)", values = rep("black",2)) + 
  ylab("Effect Size\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). Mediatiors: All rescaled to 0=minimum and 1=maximum.\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1). All models are estimated by OLS.") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/mediationplot_all_unmatched_v5.png"),p,width=10,height=7)
ggsave(paste0(projdir,"/out/mediationplot_all_unmatched_v5.pdf"),p,width=10,height=7)

#'
#' ## Matched without Distance Adjustment
#'

coefdts <- gencoefdts(readRDS(coefdtdir1))

require(ggplot2)
p <- ggplot(coefdts, 
            aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,color=gender,alpha=pstar), #linetype=pstar
                position=position_dodge(width=-0.9), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,color=gender,alpha=pstar),
                position=position_dodge(width=-0.9), size=1.5, width=0.0) +
  geom_point(aes(shape=gender,alpha=pstar),
             position=position_dodge(width=-0.9), size=3) +
  facet_grid(med ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance (Transparency)",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Gender (Point Shape)") + 
  scale_color_manual(name="Gender (Point Shape)", values = rep("black",2)) + 
  ylab("Effect Size\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). Mediatiors: All rescaled to 0=minimum and 1=maximum.\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1). All models are estimated by OLS.") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/mediationplot_all_matchednoL_v5.png"),p,width=10,height=7)
ggsave(paste0(projdir,"/out/mediationplot_all_matchednoL_v5.pdf"),p,width=10,height=7)

#'
#' ## Matched with Lambda = 50km
#'

coefdts <- gencoefdts(readRDS(coefdtdir2))

require(ggplot2)
p <- ggplot(coefdts, 
            aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,color=gender,alpha=pstar), #linetype=pstar
                position=position_dodge(width=-0.9), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,color=gender,alpha=pstar),
                position=position_dodge(width=-0.9), size=1.5, width=0.0) +
  geom_point(aes(shape=gender,alpha=pstar),
             position=position_dodge(width=-0.9), size=3) +
  facet_grid(med ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance (Transparency)",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Gender (Point Shape)") + 
  scale_color_manual(name="Gender (Point Shape)", values = rep("black",2)) + 
  ylab("Effect Size\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). Mediatiors: All rescaled to 0=minimum and 1=maximum.\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1). All models are estimated by OLS.") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/mediationplot_all_matchedL50_v5.png"),p,width=10,height=7)
ggsave(paste0(projdir,"/out/mediationplot_all_matchedL50_v5.pdf"),p,width=10,height=7)

#'
#' ## Matched with Lambda = 100km
#'

coefdts <- gencoefdts(readRDS(coefdtdir3))

require(ggplot2)
p <- ggplot(coefdts, 
            aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,color=gender,alpha=pstar), #linetype=pstar
                position=position_dodge(width=-0.9), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,color=gender,alpha=pstar),
                position=position_dodge(width=-0.9), size=1.5, width=0.0) +
  geom_point(aes(shape=gender,alpha=pstar),
             position=position_dodge(width=-0.9), size=3) +
  facet_grid(med ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance (Transparency)",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Gender (Point Shape)") + 
  scale_color_manual(name="Gender (Point Shape)", values = rep("black",2)) + 
  ylab("Effect Size\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). Mediatiors: All rescaled to 0=minimum and 1=maximum.\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1). All models are estimated by OLS.") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/mediationplot_all_matchedL100_v5.png"),p,width=10,height=7)
ggsave(paste0(projdir,"/out/mediationplot_all_matchedL100_v5.pdf"),p,width=10,height=7)

#'
#' ## Matched with Lambda = 200km
#'

coefdts <- gencoefdts(readRDS(coefdtdir4))

require(ggplot2)
p <- ggplot(coefdts, 
            aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,color=gender,alpha=pstar), #linetype=pstar
                position=position_dodge(width=-0.9), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,color=gender,alpha=pstar),
                position=position_dodge(width=-0.9), size=1.5, width=0.0) +
  geom_point(aes(shape=gender,alpha=pstar),
             position=position_dodge(width=-0.9), size=3) +
  facet_grid(med ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance (Transparency)",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Gender (Point Shape)") + 
  scale_color_manual(name="Gender (Point Shape)", values = rep("black",2)) + 
  ylab("Effect Size\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). Mediatiors: All rescaled to 0=minimum and 1=maximum.\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1). All models are estimated by OLS.") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/mediationplot_all_matchedL200_v5.png"),p,width=10,height=7)
ggsave(paste0(projdir,"/out/mediationplot_all_matchedL200_v5.pdf"),p,width=10,height=7)

#'
#' ## Matched with Lambda = 100km
#'

coefdts <- gencoefdts(readRDS(coefdtdir5))

require(ggplot2)
p <- ggplot(coefdts, 
            aes(x=factor(age, levels=rev(names(table(age)))), y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lci95,ymax=uci95,color=gender,alpha=pstar), #linetype=pstar
                position=position_dodge(width=-0.9), size=0.5, width=0.3) +
  geom_errorbar(aes(ymin=lci90,ymax=uci90,color=gender,alpha=pstar),
                position=position_dodge(width=-0.9), size=1.5, width=0.0) +
  geom_point(aes(shape=gender,alpha=pstar),
             position=position_dodge(width=-0.9), size=3) +
  facet_grid(med ~ mod, scales = "free") +
  scale_alpha_manual(name="Significance (Transparency)",values=c(1,0.5,0.2), drop=FALSE) +
  scale_shape_discrete(name="Gender (Point Shape)") + 
  scale_color_manual(name="Gender (Point Shape)", values = rep("black",2)) + 
  ylab("Effect Size\n(Thin Line = 95% CI; Thick Line 90% CI)") +
  xlab("Age") +
  labs(caption="Treatment: University education (1:attained, 0:not attained). Mediatiors: All rescaled to 0=minimum and 1=maximum.\nOutcome: Agreement with granting suffrage to permanent residents (rescaled to 0-1). All models are estimated by OLS.") +
  coord_flip() + theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=9),
        strip.text.y = element_text(angle=0,size=11),
        strip.background = element_rect(fill=NA,color=NA),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(hjust=0.5))
p

ggsave(paste0(projdir,"/out/mediationplot_all_matchedL350_v5.png"),p,width=10,height=7)
ggsave(paste0(projdir,"/out/mediationplot_all_matchedL350_v5.pdf"),p,width=10,height=7)

#'
#' # Extra Multinomial Logit Table
#'

## Load Analysis Data

load(paste0(projdir,"/out/heavy/analysis_2_matched_v5.RData"))

## Set Working Directory (Automatically) ##
require(rstudioapi); require(rprojroot)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)

require(texreg)
require(lmtest)
require(sandwich)
require(mlogit)

#+ eval = FALSE
texreg(list(s0mo_1C,s0mo2_1C), digits = 4, single.row = T,
       override.se = list(coeftest(s0mo_1C,vcov.=vcovHC(s0mo_1C))[,2],
                          coeftest(s0mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1C))),2],
                          coeftest(s0mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1C))),2]),
       override.pvalues = list(coeftest(s0mo_1C,vcov.=vcovHC(s0mo_1C))[,4],
                               coeftest(s0mo2_1C,vcov=sandwich)[grep(":Neither",names(coef(s0mo2_1C))),4],
                               coeftest(s0mo2_1C,vcov=sandwich)[grep(":Agree",names(coef(s0mo2_1C))),4]),
       beside = T,
       omit.coef = "(wave)",stars = c(0.1,0.05,0.01,0.001), symbol = "\\dagger",
       custom.coef.map = vnmap,
       custom.model.names = c(" ", "vs. Agree", "vs. Neither"),
       custom.header = list("OLS"=1, "Multinomial logit"=2:3),
       custom.note = '%stars. Robust standard errors in parentheses. Survey month fixed effects ommited from the output. For multinomial logit, the baseline category is "disagree". The table is exported using \\texttt{texreg} R package \\citep{Leifeld2013teco}.',
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE, threeparttable = TRUE, fontsize = "scriptsize",
       caption = "The effect of university education on the support for granting suffrage to permanent residents in Japan",
       caption.above = TRUE, label = "table:s0mo_1_article", float.pos = "t!",
       file = paste0(projdir,"/out/s0mo_1_tabular_article.tex"))
tmptab <- gsub("{dagger","{\\dagger",
               readLines(paste0(projdir,"/out/s0mo_1_tabular_article.tex")),fixed=T)
tmptab
tmptab <- gsub("16618.2864                   & 16618.2864", "\\multicolumn{2}{D{.}{.}{5.4}}{16618.2864}", tmptab, fixed=T)
tmptab <- gsub("-8239.1432                   & -8239.1432", "\\multicolumn{2}{D{.}{.}{5.4}}{-8239.1432}", tmptab, fixed=T)
tmptab <- gsub("7827                      & 7827                         & 7827", "7827 & \\multicolumn{2}{D{.}{.}{5.4}}{7827}", tmptab, fixed=T)
tmptab <- gsub("3                            & 3", "\\multicolumn{2}{D{.}{.}{5.4}}{3}", tmptab, fixed=T)
tmptab
writeLines(tmptab,paste0(projdir,"/out/s0mo_1_tabular_article.tex"), useBytes = T)

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/visualization_3_analysis_v5.R', rmarkdown::pdf_document(latex_engine="xelatex", extra_dependencies = list(bookmark=NULL, xltxtra=NULL, zxjatype=NULL, zxjafont=c("ipa"))), encoding = 'UTF-8')
# rmarkdown::render('./src/visualization_3_analysis_v5.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
