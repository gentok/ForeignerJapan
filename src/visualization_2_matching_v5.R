#' ---
#' title: "Visualization 2: Matching Results"
#' author: "Fan Lu & Gento Kato"
#' date: "January 26, 2021"
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

## Import Matched Data

d <- readRDS(paste0(projdir, "/data/sifcct_zip_latest_v5.rds"))
dy <- readRDS(paste0(projdir, "/data/sifcct_unmatched_v5.rds"))
dym1 <- readRDS(paste0(projdir, "/data/sifcct_matched_1_all_v5.rds"))
dym2 <- readRDS(paste0(projdir, "/data/sifcct_matched_2_all_v5.rds"))
dym3 <- readRDS(paste0(projdir, "/data/sifcct_matched_3_all_v5.rds"))
dym4 <- readRDS(paste0(projdir, "/data/sifcct_matched_4_all_v5.rds"))
dym5 <- readRDS(paste0(projdir, "/data/sifcct_matched_5_all_v5.rds"))

## Fix Pair ID 
updatepairid <- function(dym1) {
  dym1$pair_id[which(dym1$femalebefore==1)] <- paste0("fb_",dym1$pair_id[which(dym1$femalebefore==1)]) 
  dym1$pair_id[which(dym1$femaleafter==1)] <- paste0("fa_",dym1$pair_id[which(dym1$femaleafter==1)]) 
  dym1$pair_id[which(dym1$malebefore==1)] <- paste0("mb_",dym1$pair_id[which(dym1$malebefore==1)]) 
  dym1$pair_id[which(dym1$maleafter==1)] <- paste0("ma_",dym1$pair_id[which(dym1$maleafter==1)]) 
  return(dym1)
}
dym1 <- updatepairid(dym1)
dym2 <- updatepairid(dym2)
dym3 <- updatepairid(dym3)
dym4 <- updatepairid(dym4)
dym5 <- updatepairid(dym5)

## Packages
library(lmtest) # For Statistical Test
library(sandwich) # Cluster Robust Standard Error
library(ggplot2) # Plotting 
library(grid) # Plotting  
library(gridExtra) # Plotting
library(sf) # Plotting Map
library(ggimage) # Plotting Map
library(jpndistrict) # Plotting Japanese Map
library(magrittr) # Data Management/Plotting
library(purrr) # Data Management
library(pbapply) # Apply with Progress Bar
require(ebal) # Matching Balance
require(Matching) # Matching Balance

#'
#' # Plotting Individual-Level Predictors Balance
#'

## Import Data
bal_dy_unmatched <- readRDS(paste0(projdir, "/data/sifcct_unmatched_balance_v5.rds")) # Young, No Distance Adjustment
bal_dy_matched_1 <- readRDS(paste0(projdir, "/data/sifcct_matched_1_balance_v5.rds")) # Young, No Distance Adjustment
bal_dy_matched_2 <- readRDS(paste0(projdir, "/data/sifcct_matched_2_balance_v5.rds")) # Young, Distance Adjusted 50km
bal_dy_matched_3  <- readRDS(paste0(projdir, "/data/sifcct_matched_3_balance_v5.rds")) # Young, Distance Adjusted 100km
bal_dy_matched_4  <- readRDS(paste0(projdir, "/data/sifcct_matched_4_balance_v5.rds")) # Young, Distance Adjusted 200km
bal_dy_matched_5  <- readRDS(paste0(projdir, "/data/sifcct_matched_5_balance_v5.rds")) # Young, Distance Adjusted 350km

## Raw data balance

## Matching Function
source(paste0(projdir,"/src/findmatch.R"))

fmbal = formula(edu2 ~ female + age + bornyr + lvlen + lvpr + 
                  c10_sreg_fper + I(sqrt(c10_sreg_fper)) + c10_sreg_foreignN + c10_sreg_pop + 
                  c10_sreg_edu_ugsP + c10_sreg_edu_ugs + c10_sreg_edu_graduated + 
                  c10_mun_fper + I(sqrt(c10_mun_fper)) + c10_mun_foreignN + c10_mun_pop + 
                  c10_mun_edu_ugsP + c10_mun_edu_ugs + c10_mun_edu_graduated + 
                  zip_did + didper + wave + after + panel)
vnbal = c("Gender","Age","Born Year","Livng Length","Living Proportion",
          "Foreigner Percentage (zip)", "Foreigner Percentage sqrt. (zip)", 
          "Foreigner Population (zip)", "Population (zip)",
          "University Percentage (zip)",  
          "University Population (zip)", "Graduated Population (zip)",
          "Foreigner Percentage (mun.)", "Foreigner Percentage sqrt. (mun.)", 
          "Foreigner Population (mun.)", "Population (mun.)",
          "University Percentage (mun.)",  
          "University Population (mun.)", "Graduated Population (mun.)",
          "DID Residence","DID Proportion","Wave","Aug. 2012 or After","Panel")

### Female
balf_dy_unmatched <- findbalance(dy[dy$female==1,], fmbal, vnbal)
round(balf_dy_unmatched,3)[,1:7]

### Male
balm_dy_unmatched <- findbalance(dy[dy$male==1,], fmbal, vnbal)
round(balm_dy_unmatched,3)[,1:7]

## Matched Sample Proportions
matchprdt <- data.frame(
  labs = c("Unmatched",
           "Matched without Distance Adjustment",
           "Matched with Lambda = 350km",
           "Matched with Lambda = 200km",
           "Matched with Lambda = 100km",
           "Matched with Lambda = 50km"),
  notF = c(table(dy[dy$female==1,]$edu2)[1],
           table(dym1[dym1$female==1,]$edu2)[1],
           table(dym5[dym5$female==1,]$edu2)[1],
           table(dym4[dym4$female==1,]$edu2)[1],
           table(dym3[dym3$female==1,]$edu2)[1],
           table(dym2[dym2$female==1,]$edu2)[1]),
  treatedF = c(table(dy[dy$female==1,]$edu2)[2],
               table(dym1[dym1$female==1,]$edu2)[2],
               table(dym5[dym5$female==1,]$edu2)[2],
               table(dym4[dym4$female==1,]$edu2)[2],
               table(dym3[dym3$female==1,]$edu2)[2],
               table(dym2[dym2$female==1,]$edu2)[2]), 
  notM = c(table(dy[dy$female==0,]$edu2)[1],
           table(dym1[dym1$female==0,]$edu2)[1],
           table(dym5[dym5$female==0,]$edu2)[1],
           table(dym4[dym4$female==0,]$edu2)[1],
           table(dym3[dym3$female==0,]$edu2)[1],
           table(dym2[dym2$female==0,]$edu2)[1]),  
  treatedM = c(table(dy[dy$female==0,]$edu2)[2],
               table(dym1[dym1$female==0,]$edu2)[2],
               table(dym5[dym5$female==0,]$edu2)[2],
               table(dym4[dym4$female==0,]$edu2)[2],
               table(dym3[dym3$female==0,]$edu2)[2],
               table(dym2[dym2$female==0,]$edu2)[2]))
matchprdt$prF <- round((matchprdt$notF/table(dy[dy$female==1,]$edu2)[2])*100,1)
matchprdt$prM <- round((matchprdt$notM/table(dy[dy$female==0,]$edu2)[1])*100,1)
matchprdt <- matchprdt[,c("labs","notF","treatedF","prF","notM","treatedM","prM")]

matchprdt <- as.matrix(matchprdt)
colnames(matchprdt) <- c("", "No Univ.", "Univ.", "% Matched", 
                         "No Univ.", "Univ.", "% Matched")

### Table of Data Sizes
require(knitr)
require(kableExtra)
tmp <- add_header_above(kable(matchprdt,"latex", booktabs = TRUE, linesep = ""), 
                        c(" ", "Female"=3, "Male"=3))
cat(tmp)
writeLines(tmp, paste0(projdir, "/out/matchedsizes.tex"))

### Balance Data
baldt <- as.data.frame(rbind(balf_dy_unmatched,
               bal_dy_matched_1$f,
               bal_dy_matched_5$f,
               bal_dy_matched_4$f,
               bal_dy_matched_3$f,
               bal_dy_matched_2$f,
               balm_dy_unmatched,
               bal_dy_matched_1$m,
               bal_dy_matched_5$m,
               bal_dy_matched_4$m,
               bal_dy_matched_3$m,
               bal_dy_matched_2$m))
baldt <- data.frame(stat = c(baldt$sdiff,baldt$`T pval`))
baldt$data <- rep(c("Unmatched",
                "Matched without Distance Adjustment",
                "Matched with Lambda = 350km",
                "Matched with Lambda = 200km",
                "Matched with Lambda = 100km",
                "Matched with Lambda = 50km"), each=nrow(balf_dy_unmatched))
baldt$data <- factor(baldt$data, levels=unique(baldt$data))
baldt$vn <- factor(rownames(balf_dy_unmatched), 
                   levels=rev(rownames(balf_dy_unmatched)))
baldt$stat_cat <- rep(c("Standardized Difference in Means",
                        "p-value: Difference of Means Test"),
                      each = nrow(balf_dy_unmatched)*12)
baldt$stat_cat <- factor(baldt$stat_cat, levels=unique(baldt$stat_cat))
baldt$gender <- rep(c("Female","Male"), each=nrow(balf_dy_unmatched)*6)
baldt$gender <- factor(baldt$gender, levels=unique(baldt$gender))


require(ggplot2)
p <- ggplot(baldt, aes(x=vn,y=stat)) + 
  geom_hline(aes(yintercept=0), size=0.25, linetype=1) + 
  geom_point(aes(alpha=gender, shape=data), color="black",
             position=position_dodge(width=-0.5), size=2) + 
  facet_grid( ~ stat_cat, scales="free_x", switch="x") + 
  scale_shape_discrete(name="Matching\nStatus", 
                       labels = c("Unmatched",
                                  "Matched without Distance Adjustment",
                                  bquote("Matched with"~lambda~"= 350km"),
                                  bquote("Matched with"~lambda~"= 200km"),
                                  bquote("Matched with"~lambda~"= 100km"),
                                  bquote("Matched with"~lambda~"= 50km"))) + 
  scale_alpha_manual(name="Gender", values=c("Female"=1,"Male"=0.5)) + 
  coord_flip() +
  ylab(NULL) + xlab(NULL) + 
  guides(alpha = guide_legend(nrow = 2), 
         shape = guide_legend(nrow = 3)) + 
  theme_bw() +  
  theme(legend.position="bottom",
        axis.text.y = element_text(color="black"),
        strip.background.x = element_blank(),
        strip.text.y = element_text(angle=0,size=10),
        strip.placement = "outside")
p

#+ eval=FALSE
ggsave(paste0(projdir,"/out/matchbalanceplot_sifcct_v5.pdf"),p,width=8,height=5)

#'
#' # Plotting Geographic Distance Balance
#'
#' ## Prepare Japanese Map Data
#'

## Referenced from https://uribo.hatenablog.com/entry/2017/12/08/144549
## All w/o Tokyo and Okinawa
alljp_no1347 <- do.call("rbind", pblapply(seq(1,47)[-c(13,47)], 
                                          function(k) jpn_pref(pref_code = k, district=FALSE)))
## Tokyo 
tokyo13 <- jpn_pref(pref_code = 13, district = FALSE) %>% 
  st_simplify(dTolerance = 0.01)
## (Excluding Southern Islands) # Deprecated
# tokyo13 <- jpn_pref(pref_code = 13, district = TRUE) %>% 
#   st_simplify(dTolerance = 0.01) %>% 
#   mutate(city_code = as.numeric(city_code)) %>% 
#   filter(city_code != 13421) %>% st_union() %>% 
#   as.data.frame() %>% mutate(jis_code = "13", 
#                              prefecture = "東京都") %>% magrittr::set_names(c("geometry", 
#                                                                            "jis_code", "prefecture")) %>% st_as_sf()
## Okinawa 
okinawa47 <- jpn_pref(pref_code = 47, district = FALSE)
okinawa47 <- okinawa47 %>% st_set_crs(value = 4326)

#'
#' ## Prepare Respondents Data
#'

# Sample N Respondents
N = 200
set.seed(3451)
dymap1 <- dym1[which(dym1$pair_id%in%sample(dym1$pair_id,N)),]
set.seed(5412)
dymap2 <- dym2[which(dym2$pair_id%in%sample(dym2$pair_id,N)),]
set.seed(5241)
dymap3 <- dym3[which(dym3$pair_id%in%sample(dym3$pair_id,N)),]
set.seed(5441)
dymap4 <- dym4[which(dym4$pair_id%in%sample(dym4$pair_id,N)),]
set.seed(5141)
dymap5 <- dym5[which(dym5$pair_id%in%sample(dym5$pair_id,N)),]

# Move Okinawa location to left-upper corner (not done for now) 
# okinawa47$geometry <- okinawa47$geometry %>% magrittr::add(c(5.6, 17.5))
# dymap1$zip_lon[which(dymap1$zip_pref=="沖縄県")] <- dymap1$zip_lon[which(dymap1$zip_pref=="沖縄県")] + 5.6
# dymap1$zip_lat[which(dymap1$zip_pref=="沖縄県")] <- dymap1$zip_lat[which(dymap1$zip_pref=="沖縄県")] + 17.5
# dymap2$zip_lon[which(dymap2$zip_pref=="沖縄県")] <- dymap2$zip_lon[which(dymap2$zip_pref=="沖縄県")] + 5.6
# dymap2$zip_lat[which(dymap2$zip_pref=="沖縄県")] <- dymap2$zip_lat[which(dymap2$zip_pref=="沖縄県")] + 17.5
# dmmap1$zip_lon[which(dmmap1$zip_pref=="沖縄県")] <- dmmap1$zip_lon[which(dmmap1$zip_pref=="沖縄県")] + 5.6
# dmmap1$zip_lat[which(dmmap1$zip_pref=="沖縄県")] <- dmmap1$zip_lat[which(dmmap1$zip_pref=="沖縄県")] + 17.5
# dmmap2$zip_lon[which(dmmap2$zip_pref=="沖縄県")] <- dmmap2$zip_lon[which(dmmap2$zip_pref=="沖縄県")] + 5.6
# dmmap2$zip_lat[which(dmmap2$zip_pref=="沖縄県")] <- dmmap2$zip_lat[which(dmmap2$zip_pref=="沖縄県")] + 17.5
# demap1$zip_lon[which(demap1$zip_pref=="沖縄県")] <- demap1$zip_lon[which(demap1$zip_pref=="沖縄県")] + 5.6
# demap1$zip_lat[which(demap1$zip_pref=="沖縄県")] <- demap1$zip_lat[which(demap1$zip_pref=="沖縄県")] + 17.5
# demap2$zip_lon[which(demap2$zip_pref=="沖縄県")] <- demap2$zip_lon[which(demap2$zip_pref=="沖縄県")] + 5.6
# demap2$zip_lat[which(demap2$zip_pref=="沖縄県")] <- demap2$zip_lat[which(demap2$zip_pref=="沖縄県")] + 17.5

#'
#' ## Plot
#'

p1 <- ggplot() + 
  geom_sf(data=alljp_no1347 %>% st_simplify(dTolerance = 0.01), size=0.3) + 
  geom_sf(data = tokyo13, inherit.aes = TRUE, size=0.3) + 
  geom_sf(data = okinawa47 %>% st_simplify(dTolerance = 0.01), inherit.aes = TRUE, size=0.3) +   
  # geom_segment(aes(x = round(st_bbox(alljp_no1347)[1], 0), xend = 132.5, y = 40, yend = 40)) + 
  # geom_segment(aes(x = 132.5, xend = 138, y = 40, yend = 42)) + 
  # geom_segment(aes(x = 138, xend = 138, y = 42, yend = round(st_bbox(alljp_no1347)[4],0))) + 
  geom_point(data = dymap1, aes(x=zip_lon,y=zip_lat, color=as.factor(1-female)), alpha=0.5, size=0.4) + 
  geom_path(data = dymap1, aes(x=zip_lon,y=zip_lat, group=pair_id, color=as.factor(1-female)), alpha=0.65, size=0.3) + 
  scale_color_manual(name="Gender", values=c("darkred","navyblue")) + 
  coord_sf(xlim=c(124.5,148.5)) + 
  #coord_sf(xlim=c(128,148.5),ylim=c(27,46)) + 
  labs(x=paste0(table(dym1[dym1$female==1,]$treated)[1],"/",table(dy[dy$female==1,]$edu2)[2]," Female and ",
                table(dym1[dym1$female==0,]$treated)[1],"/",table(dy[dy$female==0,]$edu2)[1],
                " Male Matched Pairs Found"),
       y=NULL,title="No Distance Adjustment") + theme_light() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.background = element_rect(color="black",fill="white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        line = element_blank(), 
        axis.title.x = element_text(size=10),
        legend.position = "none")
p1

p2 <- ggplot() + 
  geom_sf(data=alljp_no1347 %>% st_simplify(dTolerance = 0.01), size=0.3) + 
  geom_sf(data = tokyo13, inherit.aes = TRUE, size=0.3) + 
  geom_sf(data = okinawa47 %>% st_simplify(dTolerance = 0.01), inherit.aes = TRUE, size=0.3) +   
  # geom_segment(aes(x = round(st_bbox(alljp_no1347)[1], 0), xend = 132.5, y = 40, yend = 40)) + 
  # geom_segment(aes(x = 132.5, xend = 138, y = 40, yend = 42)) + 
  # geom_segment(aes(x = 138, xend = 138, y = 42, yend = round(st_bbox(alljp_no1347)[4],0))) + 
  geom_point(data = dymap2, aes(x=zip_lon,y=zip_lat, color=as.factor(1-female)), alpha=0.5, size=0.4) + 
  geom_path(data = dymap2, aes(x=zip_lon,y=zip_lat, group=pair_id, color=as.factor(1-female)), alpha=0.8, size=0.3) + 
  scale_color_manual(name="Gender", values=c("darkred","navyblue")) + 
  coord_sf(xlim=c(124.5,148.5)) + 
  #coord_sf(xlim=c(128,148.5),ylim=c(27,46)) + 
  labs(x=paste0(table(dym2[dym2$female==1,]$treated)[1],"/",table(dy[dy$female==1,]$edu2)[2]," Female and ",
                table(dym2[dym2$female==0,]$treated)[1],"/",table(dy[dy$female==0,]$edu2)[1],
                " Male Matched Pairs Found"),
       y=NULL,title=bquote("Distance Adjusted ("~lambda~" = 50km)")) + theme_light() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.background = element_rect(color="black",fill="white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        line = element_blank(), 
        axis.title.x = element_text(size=10),
        legend.position = "none")
p2

p3 <- ggplot() + 
  geom_sf(data=alljp_no1347 %>% st_simplify(dTolerance = 0.01), size=0.3) + 
  geom_sf(data = tokyo13, inherit.aes = TRUE, size=0.3) + 
  geom_sf(data = okinawa47 %>% st_simplify(dTolerance = 0.01), inherit.aes = TRUE, size=0.3) +   
  # geom_segment(aes(x = round(st_bbox(alljp_no1347)[1], 0), xend = 132.5, y = 40, yend = 40)) + 
  # geom_segment(aes(x = 132.5, xend = 138, y = 40, yend = 42)) + 
  # geom_segment(aes(x = 138, xend = 138, y = 42, yend = round(st_bbox(alljp_no1347)[4],0))) + 
  geom_point(data = dymap3, aes(x=zip_lon,y=zip_lat, color=as.factor(1-female)), alpha=0.5, size=0.4) + 
  geom_path(data = dymap3, aes(x=zip_lon,y=zip_lat, group=pair_id, color=as.factor(1-female)), alpha=0.8, size=0.3) + 
  scale_color_manual(name="Gender", values=c("darkred","navyblue")) + 
  coord_sf(xlim=c(124.5,148.5)) + 
  #coord_sf(xlim=c(128,148.5),ylim=c(27,46)) + 
  labs(x=paste0(table(dym3[dym3$female==1,]$treated)[1],"/",table(dy[dy$female==1,]$edu2)[2]," Female and ",
                table(dym3[dym3$female==0,]$treated)[1],"/",table(dy[dy$female==0,]$edu2)[1],
                " Male Matched Pairs Found"),
       y=NULL,title=bquote("Distance Adjusted ("~lambda~" = 100km)")) + theme_light() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.background = element_rect(color="black",fill="white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        line = element_blank(), 
        axis.title.x = element_text(size=10),
        legend.position = "none")
p3

p4 <- ggplot() + 
  geom_sf(data=alljp_no1347 %>% st_simplify(dTolerance = 0.01), size=0.3) + 
  geom_sf(data = tokyo13, inherit.aes = TRUE, size=0.3) + 
  geom_sf(data = okinawa47 %>% st_simplify(dTolerance = 0.01), inherit.aes = TRUE, size=0.3) +   
  # geom_segment(aes(x = round(st_bbox(alljp_no1347)[1], 0), xend = 132.5, y = 40, yend = 40)) + 
  # geom_segment(aes(x = 132.5, xend = 138, y = 40, yend = 42)) + 
  # geom_segment(aes(x = 138, xend = 138, y = 42, yend = round(st_bbox(alljp_no1347)[4],0))) + 
  geom_point(data = dymap4, aes(x=zip_lon,y=zip_lat, color=as.factor(1-female)), alpha=0.5, size=0.4) + 
  geom_path(data = dymap4, aes(x=zip_lon,y=zip_lat, group=pair_id, color=as.factor(1-female)), alpha=0.8, size=0.3) + 
  scale_color_manual(name="Gender", values=c("darkred","navyblue")) + 
  coord_sf(xlim=c(124.5,148.5)) + 
  #coord_sf(xlim=c(128,148.5),ylim=c(27,46)) + 
  labs(x=paste0(table(dym4[dym4$female==1,]$treated)[1],"/",table(dy[dy$female==1,]$edu2)[2]," Female and ",
                table(dym4[dym4$female==0,]$treated)[1],"/",table(dy[dy$female==0,]$edu2)[1],
                " Male Matched Pairs Found"),
       y=NULL,title=bquote("Distance Adjusted ("~lambda~" = 200km)")) + theme_light() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.background = element_rect(color="black",fill="white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        line = element_blank(), 
        axis.title.x = element_text(size=10),
        legend.position = "none")
p4

p5 <- ggplot() + 
  geom_sf(data=alljp_no1347 %>% st_simplify(dTolerance = 0.01), size=0.3) + 
  geom_sf(data = tokyo13, inherit.aes = TRUE, size=0.3) + 
  geom_sf(data = okinawa47 %>% st_simplify(dTolerance = 0.01), inherit.aes = TRUE, size=0.3) +   
  # geom_segment(aes(x = round(st_bbox(alljp_no1347)[1], 0), xend = 132.5, y = 40, yend = 40)) + 
  # geom_segment(aes(x = 132.5, xend = 138, y = 40, yend = 42)) + 
  # geom_segment(aes(x = 138, xend = 138, y = 42, yend = round(st_bbox(alljp_no1347)[4],0))) + 
  geom_point(data = dymap5, aes(x=zip_lon,y=zip_lat, color=as.factor(1-female)), alpha=0.5, size=0.4) + 
  geom_path(data = dymap5, aes(x=zip_lon,y=zip_lat, group=pair_id, color=as.factor(1-female)), alpha=0.8, size=0.3) + 
  scale_color_manual(name="Gender", values=c("darkred","navyblue")) + 
  coord_sf(xlim=c(124.5,148.5)) + 
  #coord_sf(xlim=c(128,148.5),ylim=c(27,46)) + 
  labs(x=paste0(table(dym5[dym5$female==1,]$treated)[1],"/",table(dy[dy$female==1,]$edu2)[2]," Female and ",
                table(dym5[dym5$female==0,]$treated)[1],"/",table(dy[dy$female==0,]$edu2)[1],
                " Male Matched Pairs Found"),
       y=NULL,title=bquote("Distance Adjusted ("~lambda~" = 350km)")) + theme_light() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.background = element_rect(color="black",fill="white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        line = element_blank(), 
        axis.title.x = element_text(size=10),
        legend.position = "none")
p5

#'
#' ## Export Map Plots
#'

foottxt <- paste0("Dots represent randomly sampled ", N, " matched respondent pairs",
                  " and lines connect two matched pairs ",
                  "on the map (red = female, \nblue = male). The left panel shows the matching outcome ",
                  "without geographic distance adjustment and ",
                  "the right panel shows \nthe outcome of matching with geographic distance adjustment.")

p <- arrangeGrob(p1,p2, nrow=1,
                 bottom=textGrob(foottxt, vjust=0.5,just="left",x=unit(0.05,"npc"),
                                 gp=gpar(fontsize=9)))
grid.arrange(p)

#+ eval=FALSE
ggsave(paste0(projdir,"/out/geomatchplot_l50_sifcct_v5.pdf"),p,width=8,height=5)

#+
p <- arrangeGrob(p1,p3, nrow=1,
                 bottom=textGrob(foottxt,
                                 vjust=0.5,just="left",x=unit(0.05,"npc"),
                                 gp=gpar(fontsize=9)))
grid.arrange(p)

#+ eval=FALSE
ggsave(paste0(projdir,"/out/geomatchplot_l100_sifcct_v5.pdf"),p,width=8,height=5)

#+
p <- arrangeGrob(p1,p4, nrow=1,
                 bottom=textGrob(foottxt, vjust=0.5,just="left",x=unit(0.05,"npc"),
                                 gp=gpar(fontsize=9)))
grid.arrange(p)

#+ eval=FALSE
ggsave(paste0(projdir,"/out/geomatchplot_l200_sifcct_v5.pdf"),p,width=8,height=5)

#+
p <- arrangeGrob(p1,p5, nrow=1,
                 bottom=textGrob(foottxt, vjust=0.5,just="left",x=unit(0.05,"npc"),
                                 gp=gpar(fontsize=9)))
grid.arrange(p)

#+ eval=FALSE
ggsave(paste0(projdir,"/out/geomatchplot_l350_sifcct_v5.pdf"),p,width=8,height=5)

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/visualization_2_matching_v5.R', rmarkdown::pdf_document(latex_engine="xelatex", extra_dependencies = list(bookmark=NULL, xltxtra=NULL, zxjatype=NULL, zxjafont=c("ipa"))), encoding = 'UTF-8')
# rmarkdown::render('./src/visualization_2_matching_v5.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
