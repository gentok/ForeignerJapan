#' ---
#' title: "UTAS Recodes"
#' author: "Fan Lu & Gento Kato"
#' date: "Dec 30, 2019"
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
datadir1 <- paste(projdir,"data/original/2009_2010utas130816.sav",sep="/")
datadir2 <- paste(projdir,"data/original/2012-2013UTASV131129.csv",sep="/")
datadir3 <- paste(projdir,"data/original/2014_2016UTASV20161004.csv",sep="/")

## Import Original Data
require(foreign)
require(readr)
do09 <- read.spss(datadir1, use.value.labels=FALSE, to.data.frame=TRUE)
do12 <- read_csv(datadir2, locale=locale(encoding="CP932"))
do14 <- read_csv(datadir3, locale=locale(encoding="CP932"))
# do12 <- read.csv(datadir2, encoding="CP932", stringsAsFactors = FALSE)
# do14 <- read.csv(datadir3, encoding="CP932", stringsAsFactors = FALSE)

#'
#' # Data Manipulation
#'

# Initiate New Data Set
d <- data.frame(id = c(do09$ID, do12$ID, do14$ID),
                year = c(rep(2009,nrow(do09)),rep(2012,nrow(do12)),rep(2014,nrow(do14))))
# d <- data.frame(id=do12$ID, year=2012)

# Year Variable
#table(d$year, useNA="always")

#'
#' ## DEPENDENT variables of (potential) interest
#' 
#' ### The local election suffrage should be granted to permanent resident foreigners.
#' 
#' * Original: 1=Agree 5= Disagree
#' * Recoded: 0=Strongly disagree, 0.5=Neither, 1=Strongly agree, Missing=NA

# Original Variable
tmp <- c(do09$Q013816, do12$Q013016, do14$W1Q16_12) 
# tmp <- do12$Q013016
table(tmp, useNA="always")
# Recoded Variable
d$foreignsuff <- ifelse(tmp==99, NA, 5 - tmp)/4
table(d$foreignsuff, useNA="always")
d$foreignsuff3 <- ifelse(tmp==99, NA, ifelse(tmp%in%c(1,2),3,
                                             ifelse(tmp==3, 1, 2)))
d$foreignsuff3 <- factor(d$foreignsuff3, labels=c("Neither","Disagree","Agree"))
table(d$foreignsuff3, useNA="always")

#'
#' ### Support increase immigrants.
#' 
#' * Original: 1=Increase immigrants 5= Reduce immigrants
#' * Recoded: 1=Increase immigrants, 0.5=Neither/DK, 0=Reduce immigrants, Missing=NA

# Original
tmp <- c(do09$Q013817, do12$Q013017, do14$W1Q16_13)
#tmp <- do12$Q013017
table(tmp, useNA="always")
# Recoded Variable
d$immigincrease <- ifelse(tmp==99, NA, 5 - tmp)/4
table(d$immigincrease, useNA="always")
d$immigincrease3 <- ifelse(tmp==99, NA, ifelse(tmp%in%c(1,2),3,
                                             ifelse(tmp==3, 1, 2)))
d$immigincrease3 <- factor(d$immigincrease3, labels=c("Neither","Decrease","Increase"))
table(d$immigincrease3, useNA="always")

#'
#' ### Familiality with Foreign Countries (Only in 2012)
#'

tmp2 <- do12$Q013202
tmp2 <- ifelse(tmp2==99, NA, 5 - tmp2)/4 
table(tmp2, useNA="always")
d$familialityFT_CHN <- NA
d$familialityFT_CHN[d$year==2012] <- tmp2

tmp4 <- do12$Q013204
tmp4 <- ifelse(tmp4==99, NA, 5 - tmp4)/4 
table(tmp4, useNA="always")
d$familialityFT_KOR <- NA
d$familialityFT_KOR[d$year==2012] <- tmp4

#'
#' ### Self-Assessed Knowledge
#'
#' * Original: 1 Know a lot 5 don't know much
#'

# Original
tmp <- c(do09$Q013100, do12$Q012100, do14$W2Q8)
# tmp <- do12$Q012100
table(tmp, useNA="always") 
# Recoded Variable
d$knowledge <- ifelse(tmp==99, NA, 5 - tmp)/4
table(d$knowledge, useNA="always")

#'
#' ## PREDICTORS
#' 
#' ### Education (Ordinal)
#' 
#' * Original: 1= primary/junior-high school, 2=High School, 
#' 3=Junior College, 4=Vocational School, 5= College 6=Grad School, 7=Others
#' * Recoded: 1= "<=SHS", 2="Junior College/Vocational School", 3=">=College" 
#' 

# Original
tmp <- c(do09$Q014800, do12$Q014300, do14$W1F3)
# tmp <- do12$Q014300
tmp <- ifelse(tmp%in%c(7,99), NA, 
              ifelse(tmp%in%c(1,2), 1,
                     ifelse(tmp%in%c(3,4), 2, 3)))
table(tmp, useNA="always")
# Recoded
d$edu <- tmp
# Make it a Factor
d$edu <- factor(d$edu, labels = c("<=SHS",
                                  ">SHS & <College(4yr)",
                                  ">=College(4yr)"))
table(d$edu, useNA="always")

#' 
#' ### Gender
#' 
#' * Recoded: 0=male, 1=female
#' 

# Original
tmp <- c(do09$Q014600, do12$Q014100, do14$W1F1)
# tmp <- do12$Q014100
tmp <- ifelse(tmp==99, NA, tmp - 1)
table(tmp, useNA="always")
# Recoded
d$female <- tmp
table(d$female, useNA="always")
d$male <- 1 - d$female

#'
#' ### Age
#'
#' * Recoded (Categorical):
#' 

# Original
tmp <- c(do09$Q014700, do12$Q014200, do14$W1F2)
table(tmp, useNA="always")
d$age <- ifelse(tmp==99, NA, ifelse(tmp==1, 25, 
                                    ifelse(tmp==2,35,
                                           ifelse(tmp==3,45,
                                                  ifelse(tmp==4,55,
                                                         ifelse(tmp==5,65,
                                                                ifelse(tmp==6,75,NA)))))))
table(d$age, useNA="always")
tmp <- ifelse(tmp==99, NA, ifelse(tmp%in%c(1,2), 1, ifelse(tmp%in%c(3,4), 2, 3)))
table(tmp, useNA="always")
# Recoded
d$agecat <- tmp
table(d$agecat, useNA="always")
## Make it a factor
d$agecat <- factor(d$agecat, labels=c("Young (<=30s)",
                                      "Middle Aged (40-50s)",
                                      "Elder (>=60s)"))
table(d$agecat, useNA="always") 

#'
#' ### Employment
#' 
#' * Recoded: 0=not empoyed, 1=employed
#' 

# Original
tmp <- c(do09$Q015100, do12$Q014500, do14$W1F4)
table(tmp, useNA="always")
# Recoded
d$employed <- ifelse(tmp==99,NA,ifelse(tmp%in%c(6,7,8),0,1))
table(d$employed)

#'
#' ### assessment of current japanese economy. 
#' 
#' * Original: 1=good 5=bad
#' * Recoded: 0=bad, 0.5=Neither, 1=good, NA=NA
#'

# Original
tmp <- c(do09$Q012400, do12$Q012300, do14$W1Q11)
# tmp <- do12$Q012300
table(tmp, useNA="always")
# Recoded
d$evecon <- ifelse(tmp==99, NA, 5 - tmp)/4
table(d$evecon, useNA="always")

#'
#' ### party support 
#' 
#' Recoded: 
#' 1=Mutoha(No Party)
#' 2=Democratic Party of Japan (DPJ), 
#' 3=Liberal Democratic Party (LDP),
#' 4=New Komeito (CGP), 
#' 5=Japanese Communist Party (JCP) 
#' 6= Social Democratic Party (SDP)
#' 7=Your Party (YP)
#' 8=Japan Restoration Party (JRP)
#' 9=others 
#'

# Original
tmp09 <- do09$Q014100
tmp09 <- ifelse(tmp09==9, 1,
                ifelse(tmp09==1, 3,
                       ifelse(tmp09==2, 2,
                              ifelse(tmp09==3, 4,
                                     ifelse(tmp09==4, 5,
                                            ifelse(tmp09==5, 6,
                                                   ifelse(tmp09==7, 7,
                                                          ifelse(tmp09%in%c(6,8), 9, NA))))))))
table(tmp09, useNA="always")
tmp12 <- do12$Q013700
tmp12 <- ifelse(tmp12==14, 1,
                ifelse(tmp12==1, 2, 
                       ifelse(tmp12==2, 3,
                              ifelse(tmp12==4, 4,
                                     ifelse(tmp12==6, 5, 
                                            ifelse(tmp12==8, 6, 
                                                   ifelse(tmp12==7, 7, 
                                                          ifelse(tmp12==5, 8, 
                                                                 ifelse(tmp12%in%c(3,9,10,11,12,13), 9, NA)))))))))
table(tmp12, useNA="always")
tmp14 <- do14$W1Q18_1
tmp14 <- ifelse(tmp14==11, 1,
                ifelse(tmp14==2, 2,
                       ifelse(tmp14==1, 3,
                              ifelse(tmp14==4, 4,
                                     ifelse(tmp14==6, 5,
                                            ifelse(tmp14==8, 6,
                                                   ifelse(tmp14==3, 8,
                                                          ifelse(tmp14%in%c(5,7,9), 9, NA))))))))
table(tmp14, useNA="always")
## Combine
tmp <- c(tmp09, tmp12, tmp14)
# tmp <- tmp12
table(tmp, useNA="always")
d$psup <- factor(tmp,
                 labels=c("None","DPJ","LDP","CGP(Komei)",
                          "JCP","SDP","YP","JRP","Other"))
table(d$psup, useNA="always")

d$psup_original <- d$psup
d$psup <- ifelse(d$psup_original%in%c("DPJ","CGP(Komei)","JCP","SDP"),
                 "Left",ifelse(d$psup_original%in%c("LDP","YP","JRP"),
                               "Right",
                               ifelse(d$psup_original%in%c("None","Other"),"None/Other",NA)))
d$psup <- factor(d$psup,
                 levels=c("None/Other","Left","Right"))
table(d$psup, useNA="always")

d$left <- ifelse(d$psup%in%"Left",1,0)
d$right <- ifelse(d$psup%in%"Right",1,0)

#'
#' # LDP FT
#'

tmp <- c(do09$Q021902,do12$Q012202,do14$W1Q15_1)
table(tmp)
d$ldpft <- ifelse(tmp==999,0.5,tmp/100)
d$ldpft[which(is.na(d$ldpft))] <- 0.5
table(d$ldpft,d$year, useNA="always")

#'
#' ## Address Location
#'

table(do09$PREF)
head(do09$CITY)
table(do12$PREFNAME)
head(do12$CITY)
table(do14$PREFNAME)
head(do14$CITY)

temp <- apply(table(do12$PREFEC,do12$PREFNAME),1, 
      function(k) colnames(table(do12$PREFEC,do12$PREFNAME))[which.max(k)])
do09PREF <- temp[match(as.character(do09$PREF),names(temp))]

library(stringr)
d$add_pref <- str_squish(c(do09PREF,do12$PREFNAME,do14$PREFNAME))
d$add_city <- str_squish(c(as.character(do09$CITY),do12$CITY,do14$CITY))
d$add_all <- paste0(d$add_pref,d$add_city)
d$add_all[grep("NA",d$add_all)] <- NA
table(is.na(d$add_all))

# ## 654 unique addresses recovered.
# addvec <- unique(d$add_all)
# # 
# library(RCurl)
# library(RJSONIO)
# library(pbapply)
# appid <- readLines("/home/gentok/Documents/yahoo_appid.txt") ## Application ID for API (Need Yahoo!JAPAN ID)
# query_prefix <- paste0("https://map.yahooapis.jp/search/zip/V1/zipCodeSearch?appid=",appid,"&output=json&detail=full&al=2&sort=address2&query=")
# ## sort=address2 implies that the result is returned in "popular" order.
# ## The most "popular" address searched for the given keywords is returned first.
# ## This means that the most "popular" address (supposedly the city center) within the municipality is returned.
# 
# # Make Query (Done on Dec 15, 2019, 17:03 PST)
# adddt1 <- pblapply(addvec, function(k) fromJSON(getURL(paste0(query_prefix,k))))
# names(adddt1) <- addvec
# # Missing Places Replaced Because of Change in Municipality
# names(adddt1)[which(sapply(adddt1, function(k) k[["ResultInfo"]][["Count"]][1])==0)]
# adddt1[["岩手県東磐井郡藤沢町"]] <- fromJSON(getURL(paste0(query_prefix,"岩手県一関市藤沢町")))
# adddt1[["埼玉県鳩ケ谷市"]] <- fromJSON(getURL(paste0(query_prefix,"埼玉県川口市三ツ和"))) # The location of former city hall
# adddt1[["埼玉県北葛飾郡鷲宮町"]] <- fromJSON(getURL(paste0(query_prefix,"埼玉県久喜市鷲宮")))　# The location of former town hall
# adddt1[["愛知県幡豆郡一色町"]] <- fromJSON(getURL(paste0(query_prefix,"愛知県西尾市一色町")))
# adddt1[["長崎県北松浦郡江迎町"]] <- fromJSON(getURL(paste0(query_prefix,"長崎県佐世保市江迎町"))) 
# saveRDS(adddt1, "./data/utas_address/adddt1.rds")
# adddt1 <- readRDS("./data/utas_address/adddt1.rds")
# 
# addloc <- data.frame(add=as.character(names(adddt1)))
# addloc$add <- as.character(addloc$add)
# addloc$coord = sapply(adddt1, function(k) ifelse(is.null(k$Feature[[1]]$Geometry[2]),"NA,NA",
#                                                  k$Feature[[1]]$Geometry[2]))
# addloc$pref = sapply(adddt1, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[1]][1]),NA,
#                                                 k$Feature[[1]]$Property$AddressElement[[1]][1]))
# addloc$pref_kana = sapply(adddt1, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[1]][2]),NA,
#                                                      k$Feature[[1]]$Property$AddressElement[[1]][2]))
# addloc$muni = sapply(adddt1, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[2]][1]),NA,
#                                                 k$Feature[[1]]$Property$AddressElement[[2]][1]))
# addloc$muni_kana = sapply(adddt1, function(k) ifelse(is.null(k$Feature[[1]]$Property$AddressElement[[2]][2]),NA,
#                                                      k$Feature[[1]]$Property$AddressElement[[2]][2]))
# addloc$full = sapply(adddt1, function(k) ifelse(is.null(k$Feature[[1]]$Property$Address[1]),NA,
#                                                 k$Feature[[1]]$Property$Address[1]))
# coordtmp <- str_split(addloc$coord,",")
# addloc$lon <- as.numeric(sapply(coordtmp, function(k) k[1]))
# addloc$lat <- as.numeric(sapply(coordtmp, function(k) k[2]))
# 
# saveRDS(addloc, "./data/utas_address/addloc.rds")
# rm(adddt1, addloc)

# Longitude, Latitude, Prefecture, and Municipality from add Code
addloc <- readRDS(paste0(projdir,"/data/utas_address/addloc.rds"))
d$add_lon <- d$add_lat <- NA
d$add_lon[which(!is.na(d$add_all))] <- addloc$lon[match(d$add_all[which(!is.na(d$add_all))],addloc$add)]
d$add_lat[which(!is.na(d$add_all))] <- addloc$lat[match(d$add_all[which(!is.na(d$add_all))],addloc$add)]
d$add_pref <- d$add_pref_kana <- NA
d$add_pref[which(!is.na(d$add_all))] <- addloc$pref[match(d$add_all[which(!is.na(d$add_all))],addloc$add)]
d$add_pref_kana[which(!is.na(d$add_all))] <- addloc$pref_kana[match(d$add_all[which(!is.na(d$add_all))],addloc$add)]
d$add_muni <- d$add_muni_kana <- NA
d$add_muni[which(!is.na(d$add_all))] <- addloc$muni[match(d$add_all[which(!is.na(d$add_all))],addloc$add)]
d$add_muni_kana[which(!is.na(d$add_all))] <- addloc$muni_kana[match(d$add_all[which(!is.na(d$add_all))],addloc$add)]

#'
#' ## Subsetting Data
#'

# Only 2012 Data
#d <- d[d$year%in%c(2012),]

#'
#' ## Saving Data
#'

#+ eval=FALSE
saveRDS(d, paste0(projdir, "/data/utas_latest_v4.rds"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/data_utas_1_recode_v4.R', 'pdf_document', encoding = 'UTF-8')
# rmarkdown::render('./src/data_utas_1_recode_v4.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$|\\.log$|\\.tex$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
# In Terminal, move to src directory and run:
# Rscript -e "rmarkdown::render('data_utas_1_recode_v4.R', 'pdf_document', encoding = 'UTF-8')"
# Rscript -e "rmarkdown::render('data_utas_1_recode_v4.R', 'github_document', clean=FALSE)"

