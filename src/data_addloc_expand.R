#' ---
#' title: "Creating Expanded Postal Code Level Data"
#' author: "Fan Lu & Gento Kato"
#' date: "Dec 22, 2020"
#' ---
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

## Zipangu Package
require(zipangu)
data("jpnprefs")

## Import Current Postal Code Level Data
addloc <- rbind(readRDS(paste0(projdir,"/data/original/sifcct_address/addloc.rds")),
                readRDS(paste0(projdir,"/data/original/sifcct_address/addloc_ext.rds")))

## Additional Prefecture Codes
addloc$pref_jis <- jpnprefs$jis_code[match(addloc$pref,jpnprefs$prefecture_kanji)]
addloc$pref_en <- gsub("-.*$", "", jpnprefs$prefecture[match(addloc$pref,jpnprefs$prefecture_kanji)])

## Some Adjustments
addloc$lat[addloc$zip=="2590201"] <- 35.15310

## Extract Data from SHP file
require(sf) 
require(pbapply)

# ### Extraction Function
# getshp <- function(lonlat) {
#   
#   tmploc <- suppressMessages(which(st_intersects(st_point(lonlat, dim="XY"), prefshp, 
#                                                  sparse = FALSE)))
#   
#   if (length(tmploc)>1) stop("more than 1 target found!")
#   
#   as.data.frame(
#     prefshp[tmploc,tolower(c("KEY_CODE","KEYCODE1","KEYCODE2","PREF","CITY","KIHON1","KIHON2",
#                              "PREF_NAME","CITY_NAME","S_NAME","AREA","AREA_MAX_F",
#                              "KIGO_D","KIGO_E","KIGO_I"))]
#   )
# }
# 
# ### For Each Prefecture
# 
# cn <- tolower(c("KEY_CODE","KEYCODE1","KEYCODE2","PREF","CITY","KIHON1","KIHON2",
#                 "PREF_NAME","CITY_NAME","S_NAME","AREA","AREA_MAX_F",
#                 "KIGO_D","KIGO_E","KIGO_I","ZIP"))
# shpdata <- as.data.frame(matrix(ncol=length(cn),nrow=0))
# colnames(shpdata) <- cn
# 
# for (k in 1:47) {
# 
#   cat(paste0(jpnprefs$jis_code[k],":",jpnprefs$prefecture[k]," "))
#   
#   prefshp <- read_sf(paste0(projdir,"/data/original/sreg_data_2010/",
#                             as.character(jpnprefs$jis_code[k]),"_",
#                             gsub("-.*$", "", jpnprefs$prefecture[k]),"/",
#                             "h22ka",as.character(jpnprefs$jis_code[k]),".shp"), 
#                      options="ENCODING=SHIFT_JIS")
#   colnames(prefshp) <- tolower(colnames(prefshp))
#   
#   dtmp <- pbapply(addloc[which(addloc$pref_jis==jpnprefs$jis_code[k]),c("lon","lat")],1,getshp)
#   dtmp <- do.call("rbind", dtmp)
#   dtmp$zip <- addloc$zip[which(addloc$pref_jis==jpnprefs$jis_code[k])]
#   
#   shpdata <- rbind(shpdata, dtmp)
#   
#   saveRDS(shpdata, paste0(projdir,"/data/original/shpdata_tmp.rds"))
# 
#   Sys.sleep(1)
#   
# }
# 
# head(shpdata$key_code)
# shpdata$key_sreg <- substr(shpdata$key_code,1,9)
# shpdata$key_mun <- substr(shpdata$key_code,1,5)
# saveRDS(shpdata, paste0(projdir,"/data/original/shpdata.rds"))

shpdata <- readRDS(paste0(projdir,"/data/original/shpdata.rds"))

## Add Variable to ddloc 
addloc$key_mun <- shpdata$key_mun[match(addloc$zip,shpdata$zip)]
addloc$key_sreg <- shpdata$key_sreg[match(addloc$zip,shpdata$zip)]
addloc$pref_name <- shpdata$pref_name[match(addloc$zip,shpdata$zip)]
addloc$city_name <- shpdata$city_name[match(addloc$zip,shpdata$zip)]
addloc$s_name <- shpdata$s_name[match(addloc$zip,shpdata$zip)]

saveRDS(addloc, paste0(projdir,"/data/original/addloc_grand.rds"))
