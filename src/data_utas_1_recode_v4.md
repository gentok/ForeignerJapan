UTAS Recodes
================
Fan Lu & Gento Kato
Dec 30, 2019

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
    ##  /home/gentok/Documents/Projects/ForeignerJapan

``` r
setwd(projdir)

## Find Data Directory
datadir1 <- paste(projdir,"data/original/2009_2010utas130816.sav",sep="/")
datadir2 <- paste(projdir,"data/original/2012-2013UTASV131129.csv",sep="/")
datadir3 <- paste(projdir,"data/original/2014_2016UTASV20161004.csv",sep="/")

## Import Original Data
require(foreign)
require(readr)
do09 <- read.spss(datadir1, use.value.labels=FALSE, to.data.frame=TRUE)
```

    ## Warning in read.spss(datadir1, use.value.labels = FALSE, to.data.frame = TRUE): /home/gentok/Documents/Projects/ForeignerJapan/
    ## data/original/2009_2010utas130816.sav: Very long string record(s) found (record type 7, subtype 14), each will be imported in
    ## consecutive separate variables

``` r
do12 <- read_csv(datadir2, locale=locale(encoding="CP932"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   PREFNAME = col_character(),
    ##   CITY = col_character(),
    ##   Q0102FA = col_character(),
    ##   Q0104FA = col_character(),
    ##   Q0106FA = col_character(),
    ##   Q0119FA = col_logical(),
    ##   Q0120FA = col_character(),
    ##   Q0125FA = col_character(),
    ##   Q0137FA = col_logical(),
    ##   Q0138FA = col_character(),
    ##   Q0140FA = col_character(),
    ##   Q0145FA = col_character(),
    ##   Q0202FA = col_character(),
    ##   Q0204FA = col_character(),
    ##   Q0205FA = col_character(),
    ##   Q0208FA = col_character(),
    ##   Q0220FA = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
do14 <- read_csv(datadir3, locale=locale(encoding="CP932"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   PREFNAME = col_character(),
    ##   CITY = col_character(),
    ##   W1Q2FA = col_character(),
    ##   W1Q4FA = col_character(),
    ##   W1Q5FA = col_character(),
    ##   W1Q6FA = col_character(),
    ##   W1Q14_2FA = col_character(),
    ##   W1Q18FA = col_character(),
    ##   W1F3FA = col_character(),
    ##   W1F4FA = col_character(),
    ##   W2Q2FA = col_character(),
    ##   W2Q4FA = col_character(),
    ##   W2Q7FA = col_character(),
    ##   W2Q16FA = col_character(),
    ##   W2Q17FA = col_character(),
    ##   W2Q22_2FA = col_character(),
    ##   W2Q22_3FA = col_character(),
    ##   W2Q23FA = col_character(),
    ##   W2Q26FA = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
# do12 <- read.csv(datadir2, encoding="CP932", stringsAsFactors = FALSE)
# do14 <- read.csv(datadir3, encoding="CP932", stringsAsFactors = FALSE)
```

# Data Manipulation

``` r
# Initiate New Data Set
d <- data.frame(id = c(do09$ID, do12$ID, do14$ID),
                year = c(rep(2009,nrow(do09)),rep(2012,nrow(do12)),rep(2014,nrow(do14))))
# d <- data.frame(id=do12$ID, year=2012)

# Year Variable
#table(d$year, useNA="always")
```

## DEPENDENT variables of (potential) interest

### The local election suffrage should be granted to permanent resident foreigners.

  - Original: 1=Agree 5= Disagree
  - Recoded: 0=Strongly disagree, 0.5=Neither, 1=Strongly agree,
    Missing=NA

<!-- end list -->

``` r
# Original Variable
tmp <- c(do09$Q013816, do12$Q013016, do14$W1Q16_12) 
# tmp <- do12$Q013016
table(tmp, useNA="always")
```

    ## tmp
    ##    1    2    3    4    5   99 <NA> 
    ##  829 1602 1908  551  627  114  168

``` r
# Recoded Variable
d$foreignsuff <- ifelse(tmp==99, NA, 5 - tmp)/4
table(d$foreignsuff, useNA="always")
```

    ## 
    ##    0 0.25  0.5 0.75    1 <NA> 
    ##  627  551 1908 1602  829  282

``` r
d$foreignsuff3 <- ifelse(tmp==99, NA, ifelse(tmp%in%c(1,2),3,
                                             ifelse(tmp==3, 1, 2)))
d$foreignsuff3 <- factor(d$foreignsuff3, labels=c("Neither","Disagree","Agree"))
table(d$foreignsuff3, useNA="always")
```

    ## 
    ##  Neither Disagree    Agree     <NA> 
    ##     1908     1178     2431      282

### Support increase immigrants.

  - Original: 1=Increase immigrants 5= Reduce immigrants
  - Recoded: 1=Increase immigrants, 0.5=Neither/DK, 0=Reduce immigrants,
    Missing=NA

<!-- end list -->

``` r
# Original
tmp <- c(do09$Q013817, do12$Q013017, do14$W1Q16_13)
#tmp <- do12$Q013017
table(tmp, useNA="always")
```

    ## tmp
    ##    1    2    3    4    5   99 <NA> 
    ##  367 1068 2458 1104  504  126  172

``` r
# Recoded Variable
d$immigincrease <- ifelse(tmp==99, NA, 5 - tmp)/4
table(d$immigincrease, useNA="always")
```

    ## 
    ##    0 0.25  0.5 0.75    1 <NA> 
    ##  504 1104 2458 1068  367  298

``` r
d$immigincrease3 <- ifelse(tmp==99, NA, ifelse(tmp%in%c(1,2),3,
                                             ifelse(tmp==3, 1, 2)))
d$immigincrease3 <- factor(d$immigincrease3, labels=c("Neither","Decrease","Increase"))
table(d$immigincrease3, useNA="always")
```

    ## 
    ##  Neither Decrease Increase     <NA> 
    ##     2458     1608     1435      298

### Familiality with Foreign Countries (Only in 2012)

``` r
tmp2 <- do12$Q013202
tmp2 <- ifelse(tmp2==99, NA, 5 - tmp2)/4 
table(tmp2, useNA="always")
```

    ## tmp2
    ##    0 0.25  0.5 0.75    1 <NA> 
    ##  695  599  377  138   35   56

``` r
d$familialityFT_CHN <- NA
d$familialityFT_CHN[d$year==2012] <- tmp2

tmp4 <- do12$Q013204
tmp4 <- ifelse(tmp4==99, NA, 5 - tmp4)/4 
table(tmp4, useNA="always")
```

    ## tmp4
    ##    0 0.25  0.5 0.75    1 <NA> 
    ##  311  400  569  462  105   53

``` r
d$familialityFT_KOR <- NA
d$familialityFT_KOR[d$year==2012] <- tmp4
```

### Self-Assessed Knowledge

  - Original: 1 Know a lot 5 don’t know much

<!-- end list -->

``` r
# Original
tmp <- c(do09$Q013100, do12$Q012100, do14$W2Q8)
# tmp <- do12$Q012100
table(tmp, useNA="always") 
```

    ## tmp
    ##    1    2    3    4    5   99 <NA> 
    ##  181 1133 2130 1367  483   28  477

``` r
# Recoded Variable
d$knowledge <- ifelse(tmp==99, NA, 5 - tmp)/4
table(d$knowledge, useNA="always")
```

    ## 
    ##    0 0.25  0.5 0.75    1 <NA> 
    ##  483 1367 2130 1133  181  505

## PREDICTORS

### Education (Ordinal)

  - Original: 1= primary/junior-high school, 2=High School, 3=Junior
    College, 4=Vocational School, 5= College 6=Grad School, 7=Others
  - Recoded: 1= “\<=SHS”, 2=“Junior College/Vocational School”,
    3=“\>=College”

<!-- end list -->

``` r
# Original
tmp <- c(do09$Q014800, do12$Q014300, do14$W1F3)
# tmp <- do12$Q014300
tmp <- ifelse(tmp%in%c(7,99), NA, 
              ifelse(tmp%in%c(1,2), 1,
                     ifelse(tmp%in%c(3,4), 2, 3)))
table(tmp, useNA="always")
```

    ## tmp
    ##    1    2    3 <NA> 
    ## 3162 1171 1343  123

``` r
# Recoded
d$edu <- tmp
# Make it a Factor
d$edu <- factor(d$edu, labels = c("<=SHS",
                                  ">SHS & <College(4yr)",
                                  ">=College(4yr)"))
table(d$edu, useNA="always")
```

    ## 
    ##                <=SHS >SHS & <College(4yr)       >=College(4yr)                 <NA> 
    ##                 3162                 1171                 1343                  123

### Gender

  - Recoded: 0=male, 1=female

<!-- end list -->

``` r
# Original
tmp <- c(do09$Q014600, do12$Q014100, do14$W1F1)
# tmp <- do12$Q014100
tmp <- ifelse(tmp==99, NA, tmp - 1)
table(tmp, useNA="always")
```

    ## tmp
    ##    0    1 <NA> 
    ## 2762 3003   34

``` r
# Recoded
d$female <- tmp
table(d$female, useNA="always")
```

    ## 
    ##    0    1 <NA> 
    ## 2762 3003   34

``` r
d$male <- 1 - d$female
```

### Age

  - Recoded (Categorical):

<!-- end list -->

``` r
# Original
tmp <- c(do09$Q014700, do12$Q014200, do14$W1F2)
table(tmp, useNA="always")
```

    ## tmp
    ##    1    2    3    4    5    6   99 <NA> 
    ##  514  796  926  994 1212 1321   20   16

``` r
d$age <- ifelse(tmp==99, NA, ifelse(tmp==1, 25, 
                                    ifelse(tmp==2,35,
                                           ifelse(tmp==3,45,
                                                  ifelse(tmp==4,55,
                                                         ifelse(tmp==5,65,
                                                                ifelse(tmp==6,75,NA)))))))
table(d$age, useNA="always")
```

    ## 
    ##   25   35   45   55   65   75 <NA> 
    ##  514  796  926  994 1212 1321   36

``` r
tmp <- ifelse(tmp==99, NA, ifelse(tmp%in%c(1,2), 1, ifelse(tmp%in%c(3,4), 2, 3)))
table(tmp, useNA="always")
```

    ## tmp
    ##    1    2    3 <NA> 
    ## 1310 1920 2533   36

``` r
# Recoded
d$agecat <- tmp
table(d$agecat, useNA="always")
```

    ## 
    ##    1    2    3 <NA> 
    ## 1310 1920 2533   36

``` r
## Make it a factor
d$agecat <- factor(d$agecat, labels=c("Young (<=30s)",
                                      "Middle Aged (40-50s)",
                                      "Elder (>=60s)"))
table(d$agecat, useNA="always") 
```

    ## 
    ##        Young (<=30s) Middle Aged (40-50s)        Elder (>=60s)                 <NA> 
    ##                 1310                 1920                 2533                   36

### Employment

  - Recoded: 0=not empoyed, 1=employed

<!-- end list -->

``` r
# Original
tmp <- c(do09$Q015100, do12$Q014500, do14$W1F4)
table(tmp, useNA="always")
```

    ## tmp
    ##    1    2    3    4    5    6    7    8    9   99 <NA> 
    ## 1563  227  590  219  773 1003   83  951  310   50   30

``` r
# Recoded
d$employed <- ifelse(tmp==99,NA,ifelse(tmp%in%c(6,7,8),0,1))
table(d$employed)
```

    ## 
    ##    0    1 
    ## 2037 3682

### assessment of current japanese economy.

  - Original: 1=good 5=bad
  - Recoded: 0=bad, 0.5=Neither, 1=good, NA=NA

<!-- end list -->

``` r
# Original
tmp <- c(do09$Q012400, do12$Q012300, do14$W1Q11)
# tmp <- do12$Q012300
table(tmp, useNA="always")
```

    ## tmp
    ##    1    2    3    4    5   99 <NA> 
    ##   20  326  891 2305 2182   55   20

``` r
# Recoded
d$evecon <- ifelse(tmp==99, NA, 5 - tmp)/4
table(d$evecon, useNA="always")
```

    ## 
    ##    0 0.25  0.5 0.75    1 <NA> 
    ## 2182 2305  891  326   20   75

### party support

Recoded: 1=Mutoha(No Party) 2=Democratic Party of Japan (DPJ), 3=Liberal
Democratic Party (LDP), 4=New Komeito (CGP), 5=Japanese Communist Party
(JCP) 6= Social Democratic Party (SDP) 7=Your Party (YP) 8=Japan
Restoration Party (JRP) 9=others

``` r
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
```

    ## tmp09
    ##    1    2    3    4    5    6    7    9 <NA> 
    ##  459  508  814   75   66   38   25   17   84

``` r
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
```

    ## tmp12
    ##    1    2    3    4    5    6    7    8    9 <NA> 
    ##  447  188  663   96   45   26  103  241   34   57

``` r
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
```

    ## tmp14
    ##    1    2    3    4    5    6    8    9 <NA> 
    ##  468  218  731   99   76   23  155   20   23

``` r
## Combine
tmp <- c(tmp09, tmp12, tmp14)
# tmp <- tmp12
table(tmp, useNA="always")
```

    ## tmp
    ##    1    2    3    4    5    6    7    8    9 <NA> 
    ## 1374  914 2208  270  187   87  128  396   71  164

``` r
d$psup <- factor(tmp,
                 labels=c("None","DPJ","LDP","CGP(Komei)",
                          "JCP","SDP","YP","JRP","Other"))
table(d$psup, useNA="always")
```

    ## 
    ##       None        DPJ        LDP CGP(Komei)        JCP        SDP         YP        JRP      Other       <NA> 
    ##       1374        914       2208        270        187         87        128        396         71        164

``` r
d$psup_original <- d$psup
d$psup <- ifelse(d$psup_original%in%c("DPJ","CGP(Komei)","JCP","SDP"),
                 "Left",ifelse(d$psup_original%in%c("LDP","YP","JRP"),
                               "Right",
                               ifelse(d$psup_original%in%c("None","Other"),"None/Other",NA)))
d$psup <- factor(d$psup,
                 levels=c("None/Other","Left","Right"))
table(d$psup, useNA="always")
```

    ## 
    ## None/Other       Left      Right       <NA> 
    ##       1445       1458       2732        164

``` r
d$left <- ifelse(d$psup%in%"Left",1,0)
d$right <- ifelse(d$psup%in%"Right",1,0)
```

# LDP FT

``` r
tmp <- c(do09$Q021902,do12$Q012202,do14$W1Q15_1)
table(tmp)
```

    ## tmp
    ##    0    1    2    5   10   15   18   20   21   23   25   28   30   35   39   40   41   45   47   48   49   50   51   52   53 
    ##  285    8    1    9  106   15    1  116    1    1  347    1  220   18    1  282    2   59    1    8   23 1534   38    7    3 
    ##   54   55   56   58   59   60   64   65   67   68   70   73   74   75   78   80   85   88   90   95  100  999 
    ##    1   63    1    1    3  391    1   40    2    1  273    1    1  444    3  293   16    1   92    2  123  405

``` r
d$ldpft <- ifelse(tmp==999,0.5,tmp/100)
d$ldpft[which(is.na(d$ldpft))] <- 0.5
table(d$ldpft,d$year, useNA="always")
```

    ##       
    ##        2009 2012 2014 <NA>
    ##   0      86  122   77    0
    ##   0.01    2    3    3    0
    ##   0.02    0    0    1    0
    ##   0.05    0    4    5    0
    ##   0.1    22   37   47    0
    ##   0.15    5    6    4    0
    ##   0.18    1    0    0    0
    ##   0.2    47   33   36    0
    ##   0.21    1    0    0    0
    ##   0.23    0    0    1    0
    ##   0.25  162  115   70    0
    ##   0.28    1    0    0    0
    ##   0.3    85   64   71    0
    ##   0.35    6    4    8    0
    ##   0.39    1    0    0    0
    ##   0.4   101   91   90    0
    ##   0.41    0    1    1    0
    ##   0.45   27   18   14    0
    ##   0.47    0    0    1    0
    ##   0.48    4    1    3    0
    ##   0.49    8    7    8    0
    ##   0.5  1077  731  685    0
    ##   0.51    9   10   19    0
    ##   0.52    2    0    5    0
    ##   0.53    0    2    1    0
    ##   0.54    0    0    1    0
    ##   0.55   22   23   18    0
    ##   0.56    0    1    0    0
    ##   0.58    0    0    1    0
    ##   0.59    0    2    1    0
    ##   0.6    92  125  174    0
    ##   0.64    0    0    1    0
    ##   0.65   10   14   16    0
    ##   0.67    0    1    1    0
    ##   0.68    1    0    0    0
    ##   0.7    57   71  145    0
    ##   0.73    1    0    0    0
    ##   0.74    0    1    0    0
    ##   0.75  150  207   87    0
    ##   0.78    0    0    3    0
    ##   0.8    57  106  130    0
    ##   0.85    4    6    6    0
    ##   0.88    0    1    0    0
    ##   0.9    15   33   44    0
    ##   0.95    0    2    0    0
    ##   1      30   58   35    0
    ##   <NA>    0    0    0    0

## Address Location

``` r
table(do09$PREF)
```

    ## 
    ##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32 
    ##  86  22  28  38  20  24  31  37  36  32 114  94 209 147  46  19  19  14  16  42  37  72 117  33  27  46 135  88  21  21  12  15 
    ##  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47 
    ##  31  41  29  14  15  19  13  83   8  26  24  21  18  28  17

``` r
head(do09$CITY)
```

    ## [1] 札幌市中央区                                札幌市中央区                               
    ## [3] 札幌市中央区                                札幌市中央区                               
    ## [5] 札幌市中央区                                札幌市西区                                 
    ## 326 Levels:                                                  ... 黒川郡大郷町

``` r
table(do12$PREFNAME)
```

    ## 
    ##   三重県   京都府   佐賀県   兵庫県   北海道   千葉県 和歌山県   埼玉県   大分県   大阪府   奈良県   宮城県   宮崎県   富山県 
    ##       30       37       10       79       81       90       15      112       22      128       19       34       16       19 
    ##   山口県   山形県   山梨県   岐阜県   岡山県   岩手県   島根県   広島県   徳島県   愛媛県   愛知県   新潟県   東京都   栃木県 
    ##       23       21       12       33       27       25       11       41       13       18      117       37      187       26 
    ##   沖縄県   滋賀県   熊本県   石川県 神奈川県   福井県   福岡県   福島県   秋田県   群馬県   茨城県   長崎県   長野県   青森県 
    ##       19       19       25       17      137       14       63       32       21       33       48       20       34       21 
    ##   静岡県   香川県   高知県   鳥取県 鹿児島県 
    ##       52       18       12        8       24

``` r
head(do12$CITY)
```

    ## [1] "札幌市豊平区" "札幌市豊平区" "札幌市豊平区" "札幌市豊平区" "札幌市豊平区" "札幌市豊平区"

``` r
table(do14$PREFNAME)
```

    ## 
    ##   三重県   京都府   佐賀県   兵庫県   北海道   千葉県 和歌山県   埼玉県   大分県   大阪府   奈良県   宮城県   宮崎県   富山県 
    ##       20       37       11       84       80       79       15      105       17      123       21       31       13       16 
    ##   山口県   山形県   山梨県   岐阜県   岡山県   岩手県   島根県   広島県   徳島県   愛媛県   愛知県   新潟県   東京都   栃木県 
    ##       19       18       12       31       35       24       12       34       12       20      110       36      174       34 
    ##   沖縄県   滋賀県   熊本県   石川県 神奈川県   福井県   福岡県   福島県   秋田県   群馬県   茨城県   長崎県   長野県   青森県 
    ##       15       26       24       15      118       14       68       28       18       31       43       19       34       23 
    ##   静岡県   香川県   高知県   鳥取県 鹿児島県 
    ##       57       15       11        6       21

``` r
head(do14$CITY)
```

    ## [1] "札幌市中央区" "札幌市中央区" "札幌市中央区" "札幌市中央区" "札幌市中央区" "札幌市中央区"

``` r
temp <- apply(table(do12$PREFEC,do12$PREFNAME),1, 
      function(k) colnames(table(do12$PREFEC,do12$PREFNAME))[which.max(k)])
do09PREF <- temp[match(as.character(do09$PREF),names(temp))]

library(stringr)
d$add_pref <- str_squish(c(do09PREF,do12$PREFNAME,do14$PREFNAME))
d$add_city <- str_squish(c(as.character(do09$CITY),do12$CITY,do14$CITY))
d$add_all <- paste0(d$add_pref,d$add_city)
d$add_all[grep("NA",d$add_all)] <- NA
table(is.na(d$add_all))
```

    ## 
    ## FALSE  TRUE 
    ##  5794     5

``` r
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
```

## Subsetting Data

``` r
# Only 2012 Data
#d <- d[d$year%in%c(2012),]
```

## Saving Data

``` r
saveRDS(d, paste0(projdir, "/data/utas_latest_v4.rds"))
```
