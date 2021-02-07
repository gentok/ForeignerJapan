#' ---
#' title: "SIFCCT Mail Recoding Ver 5 (Additional Recode)"
#' author: "Fan Lu & Gento Kato"
#' date: "December 25, 2020"
#' ---
#' 
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

## Find Data Directory
datadir <- paste(projdir,"data/mail_latest_v5.rds",sep="/")
datadir1 <- paste(projdir,"data/original/gaikokujintokei_2012.xlsx",sep="/")
datadir2 <- paste(projdir,"data/original/jinkochosa_2012.xls",sep="/")
datadir3 <- paste(projdir,"data/original/did_data_2010/A16-10_00_DID.shp",sep="/")
datadir4 <- paste(projdir,"data/original/FEH_00200521_201114091011.xlsx",sep="/")
datadir5 <- paste(projdir,"data/original/FEH_00200521_201114090614.xlsx",sep="/")
datadir6 <- paste(projdir,"data/original/addloc_grand.rds", sep="/")
datadir7 <- paste(projdir,"data/original/sreg_content_2010/table02.rds", sep="/")
datadir8 <- paste(projdir,"data/original/sreg_content_2010/table03.rds", sep="/")
datadir9 <- paste(projdir,"data/original/sreg_content_2010/table04.rds", sep="/")
datadir10 <- paste(projdir,"data/original/sreg_content_2010/table05.rds", sep="/")
datadir11 <- paste(projdir,"data/original/sreg_content_2010/table08.rds", sep="/")
datadir12 <- paste(projdir,"data/original/sreg_content_2010/table10.rds", sep="/")
# datadir13 <- paste(projdir,"data/original/sreg_content_2010/table12.rds", sep="/") # labor 
datadir14 <- paste(projdir,"data/original/sreg_content_2010/table13.rds", sep="/")
datadir15 <- paste(projdir,"data/original/sreg_content_2010/table14.rds", sep="/")

## Import Version 5 Data
d <- readRDS(datadir)
### Limit to Cases with ZIP data
dzip <- d[complete.cases(d$zip_lon),]
dzip$zip_prefmuni = paste0(dzip$zip_pref,dzip$zip_muni)

## Import Relevant Geographic Data
require(readxl)

### Foreigner Population Statistics
fs <- read_xlsx(datadir1, skip=4, 
                col_names = c("pref","muni1","muni2","total",
                              "china","taiwan","korea","philippines",
                              "brazil","vietnam","peru","us","others"))
### Adjust Missing Cells (Prefecture)
fs$pref <- sapply(1:nrow(fs), function(k) {
  if (is.na(fs$pref[k])) {
    na.omit(fs$pref[1:k])[length(na.omit(fs$pref[1:k]))]
  } else {
    fs$pref[k]
  }
})
### Adjust Missing Cells (Municipality 1)
fs$muni1 <- sapply(1:nrow(fs), function(k) {
  if (is.na(fs$muni1[k])) {
    na.omit(fs$muni1[1:k])[length(na.omit(fs$muni1[1:k]))]
  } else {
    fs$muni1[k]
  }
})
### Deleting Non-standard Categorization of Geographic Names
fs$muni1[fs$muni1=="特別区"] <- ""
fs$muni1[fs$muni1=="中川郡(天塩)"] <- "中川郡"
fs$muni1[fs$muni1=="中川郡(十勝)"] <- "中川郡"
fs$muni1[fs$muni1=="上川郡(天塩)"] <- "上川郡"
fs$muni1[fs$muni1=="上川郡(十勝)"] <- "上川郡"
fs$muni1[fs$muni1=="上川郡(石狩)"] <- "上川郡"
### Adjust Missing Cells (Municipality 2)
fs$muni2[is.na(fs$muni2)] <- ""
### Create Municipality Variable
fs$muni <- paste0(fs$muni1,fs$muni2)
### Create Prefecture-Municipality
fs$prefmuni <- paste0(fs$pref,fs$muni)
### Reorder Columns
fs <- fs[,c("pref","muni1","muni2","muni","prefmuni","total",
      "china","taiwan","korea","philippines",
      "brazil","vietnam","peru","us","others")]
head(fs, 20)

### Population Statistics
ps <- read_xls(datadir2, skip=4, 
                col_names = c("code","pref","muni","male","female","total","households",
                              "inmigrants","births","inothers","intotal",
                              "outmigrants","deaths","outothers","outtotal",
                              "inoutdif","inoutrate",
                              "naturalincrease","naturalincreaserate",
                              "socialincrease","socialincreaserate"))
head(ps)
### Create Prefecture-Municipality
require(stringr)
ps$muni <- str_replace(ps$muni,"[[:blank:]]","")
ps$prefmuni <- paste0(ps$pref,ps$muni)
### Reorder Columns
ps <- ps[,c("code","pref","muni","prefmuni","male","female","total","households",
            "inmigrants","births","inothers","intotal",
            "outmigrants","deaths","outothers","outtotal",
            "inoutdif","inoutrate",
            "naturalincrease","naturalincreaserate",
            "socialincrease","socialincreaserate")]
ps$codex <- floor(as.numeric(ps$code)/10); max(table(ps$codex))
head(ps, 20)

## DID Data
require(sf)   # for read_sf(...)
require(rgeos)   # for gContains(...)
require(ggplot2)
### Read Data
did <- read_sf(datadir3, options="ENCODING=CP932")

## Population Data (2010)
ps10 <- read_xlsx(datadir4, skip=10, n_max=4499,
                  col_names = c("code","name","skip","pop","kumikae",
                                "inoutdif","inoutrate","size","popdens")
                  )
ps10$pop <- as.numeric(ps10$pop)
ps10$kumikae <- as.numeric(ps10$kumikae)
ps10$inoutdif <- as.numeric(ps10$inoutdif)
ps10$inoutrate <- as.numeric(ps10$inoutrate)
ps10$size <- as.numeric(ps10$size)
ps10$popdens <- as.numeric(ps10$size)
ps10$codex <- as.numeric(ps10$code)
tail(ps10)

## Municipality & Prefecture Names Borrowed from ps data
ps10$pref <- ps$pref[match(ps10$codex, ps$codex)]
ps10$pref[is.na(ps10$codex)] <- NA
ps10$muni <- ps$muni[match(ps10$codex, ps$codex)]
ps10$muni[is.na(ps10$codex)] <- NA

## Check municipalities missing in ps data
ps$muni[which(is.na(match(ps$codex, ps10$codex)))]
ps$codex[which(is.na(match(ps$codex, ps10$codex)))]
# 17344 -> 野々市市, 石川県 
# 23304 -> 長久手市, 愛知県
ps10$pref[which(ps10$codex==17344)] <- "石川県"
ps10$muni[which(ps10$codex==17344)] <- "野々市市"
ps10$pref[which(ps10$codex==23304)] <- "愛知県"
ps10$muni[which(ps10$codex==23304)] <- "長久手市"

## Prefecture plus municiparity
ps10$prefmuni <- paste0(ps10$pref,ps10$muni)

# Check municipalities missing in ps10 data 
tmp1 <- ps10$name[which(is.na(match(ps10$codex, ps$codex)))]
tmp1[!grepl("市部$|郡部$|区部$|旧 |振興局|支庁|全国|郡$", tmp1)]
tmp2 <- ps10$code[which(is.na(match(ps10$codex, ps$codex)))]
tmp2[!grepl("市部$|郡部$|区部$|旧 |振興局|支庁|全国|郡$", tmp1)]


# 藤沢町 (codex=3422) is part of 一関市 (codex=3209) in 2012
ps10$pop[which(ps10$codex==3209)] <- ps10$pop[which(ps10$codex==3209)] + ps10$pop[which(ps10$codex==3422)]
ps10$size[which(ps10$codex==3209)] <- ps10$size[which(ps10$codex==3209)] + ps10$size[which(ps10$codex==3422)]
ps10$popdens[which(ps10$codex==3209)] <- ps10$pop[which(ps10$codex==3209)]/ps10$size[which(ps10$codex==3209)]
# 西方町 (codex=9321) is part of 栃木市 (codex=9203) in 2012
ps10$pop[which(ps10$codex==9203)] <- ps10$pop[which(ps10$codex==9203)] + ps10$pop[which(ps10$codex==9321)]
ps10$size[which(ps10$codex==9203)] <- ps10$size[which(ps10$codex==9203)] + ps10$size[which(ps10$codex==9321)]
ps10$popdens[which(ps10$codex==9203)] <- ps10$pop[which(ps10$codex==9203)]/ps10$size[which(ps10$codex==9203)]
# 鳩ケ谷市 (codex=11226) is part of 川口市 (codex=11203) in 2012
ps10$pop[which(ps10$codex==11203)] <- ps10$pop[which(ps10$codex==11203)] + ps10$pop[which(ps10$codex==11226)]
ps10$size[which(ps10$codex==11203)] <- ps10$size[which(ps10$codex==11203)] + ps10$size[which(ps10$codex==11226)]
ps10$popdens[which(ps10$codex==11203)] <- ps10$pop[which(ps10$codex==11203)]/ps10$size[which(ps10$codex==11203)]
# 一色町 (codex=23481), 吉良町 (codex=23482), 幡豆町 (codex=23483) are part of 西尾市 (codex=23213) in 2012
ps10$pop[which(ps10$codex==23213)] <- ps10$pop[which(ps10$codex==23213)] + 
  ps10$pop[which(ps10$codex==23481)] + ps10$pop[which(ps10$codex==23482)] + ps10$pop[which(ps10$codex==23483)]
ps10$size[which(ps10$codex==23213)] <- ps10$size[which(ps10$codex==23213)] + 
  ps10$size[which(ps10$codex==23481)] + ps10$size[which(ps10$codex==23482)] + ps10$size[which(ps10$codex==23483)]
ps10$popdens[which(ps10$codex==23213)] <- ps10$pop[which(ps10$codex==23213)]/ps10$size[which(ps10$codex==23213)]
# 東出雲町 (codex=32304) is part of 松江市 (codex=32201) in 2012
ps10$pop[which(ps10$codex==32201)] <- ps10$pop[which(ps10$codex==32201)] + ps10$pop[which(ps10$codex==32304)]
ps10$size[which(ps10$codex==32201)] <- ps10$size[which(ps10$codex==32201)] + ps10$size[which(ps10$codex==32304)]
ps10$popdens[which(ps10$codex==32201)] <- ps10$pop[which(ps10$codex==32201)]/ps10$size[which(ps10$codex==32201)]
# 斐川町(codex=32401) is part of 出雲市 (codex=32203) in 2012
ps10$pop[which(ps10$codex==32203)] <- ps10$pop[which(ps10$codex==32203)] + ps10$pop[which(ps10$codex==32401)]
ps10$size[which(ps10$codex==32203)] <- ps10$size[which(ps10$codex==32203)] + ps10$size[which(ps10$codex==32401)]
ps10$popdens[which(ps10$codex==32203)] <- ps10$pop[which(ps10$codex==32203)]/ps10$size[which(ps10$codex==32203)]

## Municipality DID Data (2010)
did10 <- read_xlsx(datadir5, skip=10, na=c("","***"), n_max=4499,
                  col_names = c("code","name","skip","pop_did","kumikae_did",
                                "inoutdif_did","inoutrate_did","size_did","popdens_did")
)
tail(did10)
all(did10$code==did10$code)
did10$pop_did <- ifelse(is.na(did10$pop_did),0,as.numeric(did10$pop_did))
did10$kumikae_did <- ifelse(is.na(did10$kumikae_did),0,as.numeric(did10$kumikae_did))
did10$inoutdif_did <- ifelse(is.na(did10$inoutdif_did),0,as.numeric(did10$inoutdif_did))
did10$inoutrate_did <- ifelse(is.na(did10$inoutrate_did),0,as.numeric(did10$inoutrate_did))
did10$size_did <- ifelse(is.na(did10$size_did),0,as.numeric(did10$size_did))
did10$popdens_did <- as.numeric(did10$size_did)
did10$codex <- as.numeric(did10$code)
tail(did10)

did10$pop_did[which(did10$codex==3422)]
did10$pop_did[which(did10$codex==9321)]
did10$pop_did[which(did10$codex==11226)]
did10$pop_did[which(did10$codex==23481)]
did10$pop_did[which(did10$codex==23482)]
did10$pop_did[which(did10$codex==23483)]
did10$pop_did[which(did10$codex==32304)]

# 藤沢町 (codex=3422) is part of 一関市 (codex=3209) in 2012
# did10$pop_did[which(did10$codex==3209)] <- did10$pop_did[which(did10$codex==3209)] + did10$pop_did[which(did10$codex==3422)]
# 西方町 (codex=9321) is part of 栃木市 (codex=9203) in 2012
# did10$pop_did[which(did10$codex==9203)] <- did10$pop_did[which(did10$codex==9203)] + did10$pop_did[which(did10$codex==9321)]
# 鳩ケ谷市 (codex=11226) is part of 川口市 (codex=11203) in 2012
did10$pop_did[which(did10$codex==11203)] <- did10$pop_did[which(did10$codex==11203)] + did10$pop_did[which(did10$codex==11226)]
did10$size_did[which(did10$codex==11203)] <- did10$size_did[which(did10$codex==11203)] + did10$size_did[which(did10$codex==11226)]
did10$popdens_did[which(did10$codex==11203)] <- did10$pop_did[which(did10$codex==11203)]/did10$size_did[which(did10$codex==11203)]
# 一色町 (codex=23481), 吉良町 (codex=23482), 幡豆町 (codex=23483) are part of 西尾市 (codex=23213) in 2012
did10$pop_did[which(did10$codex==23213)] <- did10$pop_did[which(did10$codex==23213)] + 
  did10$pop_did[which(did10$codex==23481)] + did10$pop_did[which(did10$codex==23482)] + did10$pop_did[which(did10$codex==23483)]
did10$size_did[which(did10$codex==23213)] <- did10$size_did[which(did10$codex==23213)] + 
  did10$size_did[which(did10$codex==23481)] + did10$size_did[which(did10$codex==23482)] + did10$size_did[which(did10$codex==23483)]
did10$pop_did[which(did10$codex==23213)] <- did10$pop_did[which(did10$codex==23213)]/did10$size_did[which(did10$codex==23213)]
# 東出雲町 (codex=32304) is part of 松江市 (codex=32201) in 2012
# did10$pop_did[which(did10$codex==32201)] <- did10$pop_did[which(did10$codex==32201)] + did10$pop_did[which(did10$codex==32304)]
# 斐川町(codex=32401) is part of 出雲市 (codex=32203) in 2012
# did10$pop_did[which(did10$codex==32203)] <- did10$pop_did[which(did10$codex==32203)] + did10$pop_did[which(did10$codex==32401)]

# Combine DID data with Population Data (2010)
ps10 <- cbind(ps10[,-which(colnames(ps10)%in%"skip")], 
              did10[,-which(colnames(did10)%in%c("code","codex","name","skip"))])
ps10 <- ps10[!is.na(ps10$muni),]
nrow(ps10)
head(ps10)

# Additional Packages
require(pbapply)

#'
#' # Data Manipulation
#' 
#' ## Adjusting Municipality Names
#'

## For fs
table(is.na(match(dzip$zip_prefmuni,fs$prefmuni)))
tmploc <- which(is.na(match(dzip$zip_prefmuni,fs$prefmuni)))
unique(dzip$zip_prefmuni[tmploc])
# fs$prefmuni[grep("黒川郡", fs$prefmuni)]

## For ps
table(is.na(match(dzip$zip_prefmuni,ps$prefmuni)))
tmploc <- which(is.na(match(dzip$zip_prefmuni,ps$prefmuni)))
unique(dzip$zip_prefmuni[tmploc])
# ps$prefmuni[grep("白岡", ps$prefmuni)]

## Shiraoka-shi was not a city in 2011
ps$prefmuni[which(ps$prefmuni=="埼玉県南埼玉郡白岡町")] <- "埼玉県白岡市"
ps$prefmuni[which(ps$prefmuni=="福岡県糟屋郡須惠町")] <- "福岡県糟屋郡須恵町"
ps10$prefmuni[which(ps10$prefmuni=="埼玉県南埼玉郡白岡町")] <- "埼玉県白岡市"
ps10$prefmuni[which(ps10$prefmuni=="福岡県糟屋郡須惠町")] <- "福岡県糟屋郡須恵町"

### Adjust for Specific Changes in Names in dzip
dzip$zip_prefmuni[which(dzip$zip_prefmuni=="千葉県大網白里市")] <- "千葉県山武郡大網白里町"
dzip$zip_prefmuni[which(dzip$zip_prefmuni=="福岡県那珂川市")] <- "福岡県筑紫郡那珂川町"
dzip$zip_prefmuni[which(dzip$zip_prefmuni=="福岡県糟屋郡須惠町")] <- "福岡県糟屋郡須恵町"
dzip$zip_prefmuni[which(dzip$zip_prefmuni=="兵庫県丹波篠山市")] <- "兵庫県篠山市"
dzip$zip_prefmuni[which(dzip$zip_prefmuni=="兵庫県丹波篠山市")] <- "兵庫県篠山市"
dzip$zip_prefmuni[which(dzip$zip_prefmuni=="東京都八丈町")] <- "東京都八丈島八丈町"
dzip$zip_prefmuni[which(dzip$zip_prefmuni=="宮城県富谷市")] <- "宮城県黒川郡富谷町"
### Kumamoto was not a seirei-shitei toshi in 2011
dzip$zip_prefmuni[grep("熊本県熊本市",dzip$zip_prefmuni)] <- "熊本県熊本市"

table(is.na(match(dzip$zip_prefmuni,fs$prefmuni)))
table(is.na(match(dzip$zip_prefmuni,ps$prefmuni)))
table(is.na(match(dzip$zip_prefmuni,ps10$prefmuni)))

#'
#' ## Foreigner Population
#'

dzip$fpop <- fs$total[match(dzip$zip_prefmuni,fs$prefmuni)] 
dzip$fpop_china <- fs$china[match(dzip$zip_prefmuni,fs$prefmuni)] 
dzip$fpop_taiwan <- fs$taiwan[match(dzip$zip_prefmuni,fs$prefmuni)] 
dzip$fpop_korea <- fs$korea[match(dzip$zip_prefmuni,fs$prefmuni)] 
dzip$fpop_philippines <- fs$philippines[match(dzip$zip_prefmuni,fs$prefmuni)] 
dzip$fpop_brazil <- fs$brazil[match(dzip$zip_prefmuni,fs$prefmuni)] 
dzip$fpop_vietnam <- fs$vietnam[match(dzip$zip_prefmuni,fs$prefmuni)] 
dzip$fpop_peru <- fs$peru[match(dzip$zip_prefmuni,fs$prefmuni)] 
dzip$fpop_us <- fs$us[match(dzip$zip_prefmuni,fs$prefmuni)] 
dzip$fpop_others <- fs$others[match(dzip$zip_prefmuni,fs$prefmuni)] 

#'
#' ## Other Municipality Level Population Statistics
#'

dzip$pop <- ps$total[match(dzip$zip_prefmuni,ps$prefmuni)]
dzip$pop_male <- ps$male[match(dzip$zip_prefmuni,ps$prefmuni)]
dzip$pop_female <- ps$female[match(dzip$zip_prefmuni,ps$prefmuni)]
dzip$pop_households <- ps$households[match(dzip$zip_prefmuni,ps$prefmuni)]

dzip$pop10 <- ps10$pop[match(dzip$zip_prefmuni,ps10$prefmuni)]
dzip$size10 <- ps10$size[match(dzip$zip_prefmuni,ps10$prefmuni)]
dzip$popdens10 <- ps10$popdens[match(dzip$zip_prefmuni,ps10$prefmuni)]

dzip$pop10_did <- ps10$pop_did[match(dzip$zip_prefmuni,ps10$prefmuni)]
dzip$size10_did <- ps10$size_did[match(dzip$zip_prefmuni,ps10$prefmuni)]
dzip$popdens10_did <- ps10$popdens_did[match(dzip$zip_prefmuni,ps10$prefmuni)]

#'
#' ## DID Identifier
#'

### Function to Check if DID
ifDID <- function(lonlat) {
  tmp <- suppressMessages(as.numeric(st_intersects(st_point(lonlat, dim="XY"), did, sparse = FALSE)))
  if (any(tmp==1)) {
    return(1)
  } else if (all(tmp==0)) {
    return(0)
  } else {
    return(NA)
  }
}

## Using dzip
dzip$zip_did <- NA
# #+ eval=FALSE
# dzip$zip_did <- pbapply(dzip[,c("zip_lon","zip_lat")],1,ifDID)
# #+ eval=FALSE, echo=FALSE
# saveRDS(dzip$zip_did, paste0(projdir,"/data/mail_zip_did_v5.rds"))
#+ echo=FALSE
dzip$zip_did <- readRDS(paste0(projdir,"/data/mail_zip_did_v5.rds"))
#+ 
table(dzip$zip_did, useNA="always")

#'
#' ## Proportion Variables
#'

## Foreigner Percentage
dzip$fper <- dzip$fpop/dzip$pop * 100
dzip$fper_china <- dzip$fpop_china/dzip$pop * 100 
dzip$fper_taiwan <- dzip$fpop_taiwan/dzip$pop * 100
dzip$fper_korea <- dzip$fpop_korea/dzip$pop * 100
dzip$fper_philippines <- dzip$fpop_philippines/dzip$pop * 100
dzip$fper_brazil <- dzip$fpop_brazil/dzip$pop * 100 
dzip$fper_vietnam <- dzip$fpop_vietnam/dzip$pop * 100
dzip$fper_peru <- dzip$fpop_peru/dzip$pop * 100 
dzip$fper_us <- dzip$fpop_us/dzip$pop * 100
dzip$fper_others <- dzip$fpop_others/dzip$pop * 100
summary(dzip$fper)
hist(dzip$fper)
unique(dzip$zip_prefmuni[dzip$fper>20])

## DID Population Percentage
dzip$didper <- dzip$pop10_did/dzip$pop10
cor(dzip$didper,dzip$zip_did)
cor(dzip$popdens10,dzip$zip_did)

#'
#' # Using 2010 Census Small Region Data
#' 

addloc <- readRDS(datadir6)

dzip$c10_key_mun <- as.character(addloc$key_mun[match(dzip$zip,addloc$zip)])
dzip$c10_key_sreg <- as.character(addloc$key_sreg[match(dzip$zip,addloc$zip)])

## Population
t <- readRDS(datadir7) 
colnames(t)
for(i in 11:ncol(t)) t[,i][is.na(t[,i])] <- 0
t1 <- subset(t, t$地域識別番号==1)
t2 <- subset(t, t$地域識別番号==2)
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2] <- as.character(paste0("00",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5])) 
dzip$c10_key_mun <- as.character(addloc$key_mun[match(dzip$zip,addloc$zip)])
dzip$c10_key_sreg <- as.character(addloc$key_sreg[match(dzip$zip,addloc$zip)])
tmp1 <- paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
tmp2 <- paste0(t2$市区町村コード,substr(t2$秘匿先情報,1,4))[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
for(k in 1:length(tmp1)) {
  dzip$c10_key_sreg[dzip$c10_key_sreg%in%as.character(tmp1[k])] <- as.character(tmp2[k])
}

## No longer have secret
paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]

dzip$c10_mun_pop <- t1$`総数（男女別）`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_pop_m <- t1$男[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_pop_f <- t1$女[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_famN <- t1$世帯数[match(dzip$c10_key_mun,t1$市区町村コード)]

dzip$c10_sreg_pop <- t2$`総数（男女別）`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_pop_m <- t2$男[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_pop_f <- t2$女[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_famN <- t2$世帯数[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]

t <- readRDS(datadir8) 
colnames(t)
for(i in 11:ncol(t)) t[,i][is.na(t[,i])] <- 0
t1 <- subset(t, t$地域識別番号==1)
t2 <- subset(t, t$地域識別番号==2)
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2] <- as.character(paste0("00",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5])) 

dzip$c10_key_mun <- addloc$key_mun[match(dzip$zip,addloc$zip)]
dzip$c10_key_sreg <- addloc$key_sreg[match(dzip$zip,addloc$zip)]
tmp1 <- paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
tmp2 <- paste0(t2$市区町村コード,substr(t2$秘匿先情報,1,4))[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
for(k in 1:length(tmp1)) {
  dzip$c10_key_sreg[dzip$c10_key_sreg%in%as.character(tmp1[k])] <- as.character(tmp2[k])
}
## No longer have secret
paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]

dzip$c10_mun_avage <- t1$平均年齢[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_avage_m <- t1$平均年齢_男[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_avage_f <- t1$平均年齢_女[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_foreignN <- t1$`（再掲）外国人 1)`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_foreignN_m <- t1$`（再掲）外国人 1)_男`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_foreignN_f <- t1$`（再掲）外国人 1)_女`[match(dzip$c10_key_mun,t1$市区町村コード)]

dzip$c10_sreg_avage <- t2$平均年齢[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_avage_m <- t2$平均年齢_男[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_avage_f <- t2$平均年齢_女[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_foreignN <- t2$`（再掲）外国人 1)`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_foreignN_m <- t2$`（再掲）外国人 1)_男`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_foreignN_f <- t2$`（再掲）外国人 1)_女`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]

t <- readRDS(datadir9) 
colnames(t)
for(i in 11:ncol(t)) t[,i][is.na(t[,i])] <- 0
t1 <- subset(t, t$地域識別番号==1)
t2 <- subset(t, t$地域識別番号==2)
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2] <- as.character(paste0("00",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5])) 

dzip$c10_key_mun <- addloc$key_mun[match(dzip$zip,addloc$zip)]
dzip$c10_key_sreg <- addloc$key_sreg[match(dzip$zip,addloc$zip)]
tmp1 <- paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
tmp2 <- paste0(t2$市区町村コード,substr(t2$秘匿先情報,1,4))[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
for(k in 1:length(tmp1)) {
  dzip$c10_key_sreg[dzip$c10_key_sreg%in%as.character(tmp1[k])] <- as.character(tmp2[k])
}
## No longer have secret
paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]

dzip$c10_mun_pop_15ov <- t1$`総数（配偶関係）`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_pop_15ov_m <- t1$`総数（配偶関係）_男`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_pop_15ov_f <- t1$`総数（配偶関係）_女`[match(dzip$c10_key_mun,t1$市区町村コード)]

dzip$c10_sreg_pop_15ov <- t2$`総数（配偶関係）`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_pop_15ov_m <- t2$`総数（配偶関係）_男`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_pop_15ov_f <- t2$`総数（配偶関係）_女`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]

t <- readRDS(datadir10) 
for(i in 11:ncol(t)) t[,i][is.na(t[,i])] <- 0
colnames(t)
t1 <- subset(t, t$地域識別番号==1)
t2 <- subset(t, t$地域識別番号==2)
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2] <- as.character(paste0("00",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5])) 

dzip$c10_key_mun <- addloc$key_mun[match(dzip$zip,addloc$zip)]
dzip$c10_key_sreg <- addloc$key_sreg[match(dzip$zip,addloc$zip)]
tmp1 <- paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
tmp2 <- paste0(t2$市区町村コード,substr(t2$秘匿先情報,1,4))[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
for(k in 1:length(tmp1)) {
  dzip$c10_key_sreg[dzip$c10_key_sreg%in%as.character(tmp1[k])] <- as.character(tmp2[k])
}
## No longer have secret
paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]

dzip$c10_mun_famsize <- t1$`（一般世帯）1世帯当たり人員`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_sreg_famsize <- t2$`（一般世帯）1世帯当たり人員`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]

t <- readRDS(datadir11) 
colnames(t)
for(i in 11:ncol(t)) t[,i][is.na(t[,i])] <- 0
t1 <- subset(t, t$地域識別番号==1)
t2 <- subset(t, t$地域識別番号==2)
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2] <- as.character(paste0("00",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5])) 

dzip$c10_key_mun <- addloc$key_mun[match(dzip$zip,addloc$zip)]
dzip$c10_key_sreg <- addloc$key_sreg[match(dzip$zip,addloc$zip)]
tmp1 <- paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
tmp2 <- paste0(t2$市区町村コード,substr(t2$秘匿先情報,1,4))[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
for(k in 1:length(tmp1)) {
  dzip$c10_key_sreg[dzip$c10_key_sreg%in%as.character(tmp1[k])] <- as.character(tmp2[k])
}
## No longer have secret
paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]

dzip$c10_mun_houseN <- t1$`総数（住宅の建て方）　1)`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_singlehouseN <- t1$一戸建[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_singlehouseP <- (dzip$c10_mun_singlehouseN/dzip$c10_mun_houseN)*100

dzip$c10_sreg_houseN <- t2$`総数（住宅の建て方）　1)`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_singlehouseN <- t2$一戸建[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_singlehouseP <- (dzip$c10_sreg_singlehouseN/dzip$c10_sreg_houseN)*100

t <- readRDS(datadir12) 
colnames(t)
for(i in 11:ncol(t)) t[,i][is.na(t[,i])] <- 0
t1 <- subset(t, t$地域識別番号==1)
t2 <- subset(t, t$地域識別番号==2)
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2] <- as.character(paste0("00",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5])) 

dzip$c10_key_mun <- addloc$key_mun[match(dzip$zip,addloc$zip)]
dzip$c10_key_sreg <- addloc$key_sreg[match(dzip$zip,addloc$zip)]
tmp1 <- paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
tmp2 <- paste0(t2$市区町村コード,substr(t2$秘匿先情報,1,4))[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
for(k in 1:length(tmp1)) {
  dzip$c10_key_sreg[dzip$c10_key_sreg%in%as.character(tmp1[k])] <- as.character(tmp2[k])
}
## No longer have secret
paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]

dzip$c10_mun_pop_15ov_labor <- t1$労働力人口[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_pop_15ov_labor_m <- t1$労働力人口_男[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_pop_15ov_labor_f <- t1$労働力人口_女[match(dzip$c10_key_mun,t1$市区町村コード)]

dzip$c10_sreg_15ov_labor <- t2$労働力人口[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_15ov_labor_m <- t2$労働力人口_男[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_15ov_labor_f <- t2$労働力人口_女[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]


# 
# t <- readRDS(datadir13) 
# colnames(t)
# for(i in 11:ncol(t)) t[,i][is.na(t[,i])] <- 0
# t1 <- subset(t, t$地域識別番号==1)
# t2 <- subset(t, t$地域識別番号==2)
# 
# dzip$c10_key_mun <- addloc$key_mun[match(dzip$zip,addloc$zip)]
# dzip$c10_key_sreg <- addloc$key_sreg[match(dzip$zip,addloc$zip)]
# tmp1 <- paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
# tmp2 <- paste0(t2$市区町村コード,substr(t2$秘匿先情報,1,4))[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
# for(k in 1:length(tmp1)) {
#   dzip$c10_key_sreg[dzip$c10_key_sreg%in%as.character(tmp1[k])] <- as.character(tmp2[k])
# }
# ## No longer have secret
# paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
#

t <- readRDS(datadir14) 
colnames(t)
for(i in 11:ncol(t)) t[,i][is.na(t[,i])] <- 0
t1 <- subset(t, t$地域識別番号==1)
t2 <- subset(t, t$地域識別番号==2)
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2] <- as.character(paste0("00",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5])) 

dzip$c10_key_mun <- addloc$key_mun[match(dzip$zip,addloc$zip)]
dzip$c10_key_sreg <- addloc$key_sreg[match(dzip$zip,addloc$zip)]
tmp1 <- paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
tmp2 <- paste0(t2$市区町村コード,substr(t2$秘匿先情報,1,4))[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
for(k in 1:length(tmp1)) {
  dzip$c10_key_sreg[dzip$c10_key_sreg%in%as.character(tmp1[k])] <- as.character(tmp2[k])
}
## No longer have secret
paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]

dzip$c10_mun_sincebornN <- t1$出生時から[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_sincebornN_m <- t1$出生時から_男[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_sincebornN_f <- t1$出生時から_女[match(dzip$c10_key_mun,t1$市区町村コード)]

dzip$c10_sreg_sincebornN <- t2$出生時から[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_sincebornN_m <- t2$出生時から_男[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_sincebornN_f <- t2$出生時から_女[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]

t <- readRDS(datadir15) 
colnames(t)
for(i in 11:ncol(t)) t[,i][is.na(t[,i])] <- 0
t1 <- subset(t, t$地域識別番号==1)
t2 <- subset(t, t$地域識別番号==2)
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2] <- as.character(paste0("00",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%2])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%3])) 
t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5] <- as.character(paste0("0",t2$秘匿先情報[nchar(t2$秘匿先情報)%in%5])) 

dzip$c10_key_mun <- addloc$key_mun[match(dzip$zip,addloc$zip)]
dzip$c10_key_sreg <- addloc$key_sreg[match(dzip$zip,addloc$zip)]
tmp1 <- paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
tmp2 <- paste0(t2$市区町村コード,substr(t2$秘匿先情報,1,4))[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]
for(k in 1:length(tmp1)) {
  dzip$c10_key_sreg[dzip$c10_key_sreg%in%as.character(tmp1[k])] <- as.character(tmp2[k])
}
## No longer have secret
paste0(t2$市区町村コード,t2$町丁字コード)[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))][which(t2$秘匿処理[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]%in%"秘匿地域")]

dzip$c10_mun_edu_graduated <- t1$卒業者[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_graduated_m <- t1$卒業者_男[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_graduated_f <- t1$卒業者_女[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_esjhs <- t1$`小学校・中学校`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_esjhs_m <- t1$`小学校・中学校_男`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_esjhs_f <- t1$`小学校・中学校_女`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_hs <- t1$`高校・旧中 2)3)`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_hs_m <- t1$`高校・旧中 2)3)_男`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_hs_f <- t1$`高校・旧中 2)3)_女`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_tsjc <- t1$`短大・高専 2)`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_tsjc_m <- t1$`短大・高専 2)_男`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_tsjc_f <- t1$`短大・高専 2)_女`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_ugs <- t1$`大学・大学院 2)`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_ugs_m <- t1$`大学・大学院 2)_男`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_ugs_f <- t1$`大学・大学院 2)_女`[match(dzip$c10_key_mun,t1$市区町村コード)]
dzip$c10_mun_edu_ugsP <- dzip$c10_mun_edu_ugs/dzip$c10_mun_edu_graduated*100
dzip$c10_mun_edu_ugsP_m <- dzip$c10_mun_edu_ugs_m/dzip$c10_mun_edu_graduated_m*100
dzip$c10_mun_edu_ugsP_f <- dzip$c10_mun_edu_ugs_f/dzip$c10_mun_edu_graduated_f*100

dzip$c10_sreg_edu_graduated <- t2$卒業者[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_graduated_m <- t2$卒業者_男[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_graduated_f <- t2$卒業者_女[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_esjhs <- t2$`小学校・中学校`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_esjhs_m <- t2$`小学校・中学校_男`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_esjhs_f <- t2$`小学校・中学校_女`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_hs <- t2$`高校・旧中 2)3)`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_hs_m <- t2$`高校・旧中 2)3)_男`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_hs_f <- t2$`高校・旧中 2)3)_女`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_tsjc <- t2$`短大・高専 2)`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_tsjc_m <- t2$`短大・高専 2)_男`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_tsjc_f <- t2$`短大・高専 2)_女`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_ugs <- t2$`大学・大学院 2)`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_ugs_m <- t2$`大学・大学院 2)_男`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_ugs_f <- t2$`大学・大学院 2)_女`[match(dzip$c10_key_sreg,paste0(t2$市区町村コード,t2$町丁字コード))]
dzip$c10_sreg_edu_ugsP <- dzip$c10_sreg_edu_ugs/dzip$c10_sreg_edu_graduated*100
dzip$c10_sreg_edu_ugsP_m <- dzip$c10_sreg_edu_ugs_m/dzip$c10_sreg_edu_graduated_m*100
dzip$c10_sreg_edu_ugsP_f <- dzip$c10_sreg_edu_ugs_f/dzip$c10_sreg_edu_graduated_f*100

## Define Names
dzip$c10_key_mun <- addloc$key_mun[match(dzip$zip,addloc$zip)]
dzip$c10_key_sreg <- addloc$key_sreg[match(dzip$zip,addloc$zip)]
dzip$c10_name_pref <- addloc$pref_name[match(dzip$zip,addloc$zip)]
dzip$c10_name_mun <- addloc$city_name[match(dzip$zip,addloc$zip)]
dzip$c10_name_sreg <- addloc$s_name[match(dzip$zip,addloc$zip)]

#'
#' # Saving Data
#'

#+ eval=FALSE
saveRDS(dzip, paste0(projdir, "/data/mail_zip_latest_v5.rds"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/data_mail_1x_recode_v5.R', rmarkdown::pdf_document(latex_engine="xelatex", extra_dependencies = list(bookmark=NULL, xltxtra=NULL, zxjatype=NULL, zxjafont=c("ipa"))), encoding = 'UTF-8')
# rmarkdown::render('./src/data_mail_1x_recode_v5.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))

