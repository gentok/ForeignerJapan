#' ---
#' title: "SIFCCT Matching (Young)"
#' author: "Fan Lu & Gento Kato"
#' date: "Dec 30, 2019"
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

## Import Packages
library(designmatch) # Need GLPK Optimizer Installed. 
library(gurobi) # Need Gurobi Optimizer Installed.
library(fields)

#'
#' # Data Preparation
#'

## Import Data
d <- readRDS(paste0(projdir, "/data/sifcct_latest_v4.rds"))
table(d$edu,d$female,d$agecat)

## Focus on Young Generation

dy <- d[which(d$agecat=="Young (<=30s)"),]
table(is.na(dy$zip_lat), useNA="always")
dy <- dy[,c("id","foreignsuff","familialityFT_KOR","ideology",
            "female","edu","age","agecat","knowledge","polint",
            "employed","evecon","lvpr","income","wave",
            "zip","zip_pref","zip_lat","zip_lon")]
dy <- na.omit(dy)
dy <- dy[dy$edu!=">SHS & <College(4yr)",]

## Treatment 
table(dy$edu,dy$female)
dy$treated <- ifelse(dy$edu=="<=SHS",0,1)

## Standardized Covariates
dy$stdage <- (dy$age - mean(dy$age))/sd(dy$age)
hist(dy$stdage)
dy$stdknowledge <- (dy$knowledge - mean(dy$knowledge))/sd(dy$knowledge)
table(dy$stdknowledge)
dy$stdpolint <- (dy$polint - mean(dy$polint))/sd(dy$polint)
table(dy$stdpolint)
dy$stdevecon <- (dy$evecon - mean(dy$evecon))/sd(dy$evecon)
hist(dy$stdevecon)
dy$stdincome <- (dy$income - mean(dy$income))/sd(dy$income)
hist(dy$stdincome)
dy$stdlvpr <- (dy$lvpr - mean(dy$lvpr))/sd(dy$lvpr)
hist(dy$stdlvpr)

#+ eval=FALSE
## Save Data
saveRDS(dy, paste0(projdir, "/data/sifcct_young_unmatched.rds"))

#+ eval=FALSE
##############################################
# Match 1
# Optimal subset matching: yes
# Penalties on the distance matrix: no, *distance matrix based on covariates*
# Explicit balancing of the covariates:  
#   mom_covs =  cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
#   mom_tols = c(0.03, 0.01, 0.02, 0.02, 0.02, 0.02)
#   exact_covs = as.matrix(employed) AND female by construct
#   near_exact_covs = as.matrix(wave) 
#   near_exact_devs = 1
##############################################

dy_matched_1 = rep(NA, 41)
pair_id_count = 0
for (j in 0:1) {	
  
  dy2 = dy[dy$female==j, ] # Gender Subset
  dy2 = dy2[order(dy2$treated, decreasing=FALSE), ] # Not Treated First
  attach(dy2, warn.conflicts=FALSE)
  
  # Treatment indicator (Not treated as 1)
  t_ind = 1-treated
  
  # Matrix of covariates (moment covariates, Age and House price)
  X_mat = cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
  
  # Distance matrix (designmatch. Mahalanobis distance)
  dist_mat = distmat(t_ind, X_mat)
  # Whether all the treated units need to be used
  subset_weight = 1
  
  # Number of matches
  n_matches = 1
  
  # Moment covariates
  mom_covs = cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
  mom_tols = c(0.03, 0.01, 0.02, 0.02, 0.02, 0.02)
  mom_weights = NULL #c(0,0,0)
  
  # Covariates for exact, near-exact, fine, and near-fine balance
  exact_covs = as.matrix(employed)
  near_exact_covs = as.matrix(wave)
  near_exact_devs = 1
  fine_covs = NULL
  near_fine_covs =  NULL
  near_fine_devs = NULL
  
  detach(dy2)
  
  # Find the match
  out = bmatch(t_ind=t_ind, dist_mat=dist_mat, n_controls=n_matches,
               subset_weight = subset_weight,
               mom = list(covs=mom_covs,tols=mom_tols),
               exact = list(covs=exact_covs),
               near_exact = list(covs=near_exact_covs,devs=near_exact_devs),
               fine = list(covs=fine_covs),
               near_fine = list(covs=near_fine_covs,devs=near_fine_devs),
               solver = list(name="gurobi", approximate=0, t_max=600, trace=0))
  t_id = out$t_id
  c_id = out$c_id
  pair_id = out$group_id+pair_id_count
  pair_id_count = pair_id_count+length(t_id)
  
  # Save	
  d_aux = dy2[c(t_id, c_id), ]
  d_aux = cbind(d_aux, pair_id)
  dy_matched_1 = rbind(dy_matched_1, d_aux)
  
}
dy_matched_1 = dy_matched_1[-1, ]
saveRDS(dy_matched_1, file = paste0(projdir,"/data/sifcct_young_matched_1.rds"))

#+
dy_matched_1 <- readRDS(paste0(projdir,"/data/sifcct_young_matched_1.rds"))

table(dy_matched_1$treated)
min(table(dy_matched_1$pair_id))
max(table(dy_matched_1$pair_id))
table(is.na(dy_matched_1$pair_id))
length(unique(dy_matched_1$id))

## Balance
require(ebal)
require(Matching)
### Female
paste(table(dy_matched_1[dy_matched_1$female==1,]$treated)[1],
      table(dy[dy$female==1,]$treated)[1],sep="/") # N of Matched Found
balf_dy_matched_1 <- baltest.collect(MatchBalance(treated ~ age + knowledge + polint + employed + evecon + income + lvpr,
                                                  data = dy_matched_1[dy_matched_1$female==1,], paired=FALSE, print.level=0), 
                                     var.names = c("Age","Knowledge","Political Interest","Employed", 
                                                   "Economic Evaluation","Income","Length of Residence"),
                                     after = FALSE)
round(balf_dy_matched_1,3)[,1:7]
### Male
paste(table(dy_matched_1[dy_matched_1$female==0,]$treated)[1],
      table(dy[dy$female==0,]$treated)[1],sep="/") # N of Matched Found
balm_dy_matched_1 <- baltest.collect(MatchBalance(treated ~ age + knowledge + polint + employed + evecon + income + lvpr,
                                                  data = dy_matched_1[dy_matched_1$female==0,], paired=FALSE, print.level=0), 
                                     var.names = c("Age","Knowledge","Political Interest","Employed", 
                                                   "Economic Evaluation","Income","Length of Residence"),
                                     after = FALSE)
round(balm_dy_matched_1,3)[,1:7]

#+ eval=FALSE
## Save Balance Data
saveRDS(list(f = balf_dy_matched_1, m = balm_dy_matched_1), 
        file = paste0(projdir,"/data/sifcct_young_matched_1_balance.rds"))

#+ eval=FALSE
##############################################
# Match 2
# Optimal subset matching: yes
# Penalties on the distance matrix: yes (lambda = 50km)
# Explicit balancing of the covariates:  
#   mom_covs =  cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
#   mom_tols = c(0.03, 0.01, 0.02, 0.02, 0.02, 0.02)
#   exact_covs = as.matrix(employed) AND female by construct
#   near_exact_covs = as.matrix(wave) 
#   near_exact_devs = 1
##############################################

dy_matched_2 = rep(NA, 41)
pair_id_count = 0
for (j in 0:1) {	
  
  dy2 = dy[dy$female==j, ]
  dy2 = dy2[order(dy2$treated, decreasing=FALSE), ]
  attach(dy2, warn.conflicts=FALSE)
  
  # Treatment indicator 
  t_ind = 1-treated
  
  # Distance matrix
  lat_t = abs(zip_lat[t_ind==1])
  lat_c = abs(zip_lat[t_ind==0])
  lon_t = abs(zip_lon[t_ind==1])
  lon_c = abs(zip_lon[t_ind==0])
  coords_t = cbind(lon_t, lat_t)
  coords_c = cbind(lon_c, lat_c) 	
  dist_mat_orig = rdist.earth(coords_t, coords_c, miles=FALSE)
  dist_mat = round(dist_mat_orig/50, 4)
  # dist_mat = round(dist_mat_orig/median(dist_mat_orig), 2)
  dist_mat = dist_mat+100*(dist_mat_orig>350)
  # Whether all the treated units need to be used
  subset_weight = 1
  
  # Number of matches
  n_matches = 1
  
  # Moment covariates
  mom_covs = cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
  mom_tols = c(0.03, 0.01, 0.02, 0.02, 0.02, 0.02)
  mom_weights = NULL
  
  # Covariates for exact, near-exact, fine, and near-fine balance
  exact_covs = as.matrix(employed)
  near_exact_covs = as.matrix(wave)
  near_exact_devs = 1
  fine_covs = NULL
  near_fine_covs =  NULL
  near_fine_devs = NULL

  detach(dy2)
  
  # Find the match
  out = bmatch(t_ind=t_ind, dist_mat=dist_mat, n_controls=n_matches,
               subset_weight = subset_weight,
               mom = list(covs=mom_covs,tols=mom_tols),
               exact = list(covs=exact_covs),
               near_exact = list(covs=near_exact_covs,devs=near_exact_devs),
               fine = list(covs=fine_covs),
               near_fine = list(covs=near_fine_covs,devs=near_fine_devs),
               solver = list(name="gurobi", approximate=0, t_max=600, trace=0))
  t_id = out$t_id
  c_id = out$c_id
  pair_id = out$group_id+pair_id_count
  pair_id_count = pair_id_count+length(t_id)
  
  # Save	
  d_aux = dy2[c(t_id, c_id), ]
  d_aux = cbind(d_aux, pair_id)
  dy_matched_2 = rbind(dy_matched_2, d_aux)
  
}
dy_matched_2 = dy_matched_2[-1, ]
saveRDS(dy_matched_2, file = paste0(projdir,"/data/sifcct_young_matched_2.rds"))

#+
dy_matched_2 <- readRDS(paste0(projdir,"/data/sifcct_young_matched_2.rds"))

table(dy_matched_2$treated)
min(table(dy_matched_2$pair_id))
max(table(dy_matched_2$pair_id))
table(is.na(dy_matched_2$pair_id))
length(unique(dy_matched_2$id))

## Balance
require(ebal)
require(Matching)
### Female
paste(table(dy_matched_2[dy_matched_2$female==1,]$treated)[1],
      table(dy[dy$female==1,]$treated)[1],sep="/") # N of Matched Found
balf_dy_matched_2 <- baltest.collect(MatchBalance(treated ~ age + knowledge + polint + employed + evecon + income + lvpr,
                                                  data = dy_matched_2[dy_matched_2$female==1,], paired=FALSE, print.level=0), 
                                     var.names = c("Age","Knowledge","Political Interest","Employed", 
                                                   "Economic Evaluation","Income","Length of Residence"),
                                     after = FALSE)
round(balf_dy_matched_2,3)[,1:7]
### Male
paste(table(dy_matched_2[dy_matched_2$female==0,]$treated)[1],
      table(dy[dy$female==0,]$treated)[1],sep="/") # N of Matched Found
balm_dy_matched_2 <- baltest.collect(MatchBalance(treated ~ age + knowledge + polint + employed + evecon + income + lvpr,
                                                  data = dy_matched_2[dy_matched_2$female==0,], paired=FALSE, print.level=0), 
                                     var.names = c("Age","Knowledge","Political Interest","Employed", 
                                                   "Economic Evaluation","Income","Length of Residence"),
                                     after = FALSE)
round(balm_dy_matched_2,3)[,1:7]

#+ eval=FALSE
## Save Balance Data
saveRDS(list(f = balf_dy_matched_2, m = balm_dy_matched_2), 
        file = paste0(projdir,"/data/sifcct_young_matched_2_balance.rds"))

#+ eval=FALSE
##############################################
# Match 3
# Optimal subset matching: yes
# Penalties on the distance matrix: yes (lambda = 100km)
# Explicit balancing of the covariates:  
#   mom_covs =  cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
#   mom_tols = c(0.03, 0.01, 0.02, 0.02, 0.02, 0.02)
#   exact_covs = as.matrix(employed) AND female by construct
#   near_exact_covs = as.matrix(wave) 
#   near_exact_devs = 1
##############################################

dy_matched_3 = rep(NA, 41)
pair_id_count = 0
for (j in 0:1) {	
  
  dy2 = dy[dy$female==j, ]
  dy2 = dy2[order(dy2$treated, decreasing=FALSE), ]
  attach(dy2, warn.conflicts=FALSE)
  
  # Treatment indicator 
  t_ind = 1-treated
  
  # Distance matrix
  lat_t = abs(zip_lat[t_ind==1])
  lat_c = abs(zip_lat[t_ind==0])
  lon_t = abs(zip_lon[t_ind==1])
  lon_c = abs(zip_lon[t_ind==0])
  coords_t = cbind(lon_t, lat_t)
  coords_c = cbind(lon_c, lat_c) 	
  dist_mat_orig = rdist.earth(coords_t, coords_c, miles=FALSE)
  dist_mat = round(dist_mat_orig/100, 4)
  # dist_mat = round(dist_mat_orig/median(dist_mat_orig), 2)
  dist_mat = dist_mat+100*(dist_mat_orig>350)
  # Whether all the treated units need to be used
  subset_weight = 1
  
  # Number of matches
  n_matches = 1
  
  # Moment covariates
  mom_covs = cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
  mom_tols = c(0.03, 0.01, 0.02, 0.02, 0.02, 0.02)
  mom_weights = NULL
  
  # Covariates for exact, near-exact, fine, and near-fine balance
  exact_covs = as.matrix(employed)
  near_exact_covs = as.matrix(wave)
  near_exact_devs = 1
  fine_covs = NULL
  near_fine_covs =  NULL
  near_fine_devs = NULL
  
  detach(dy2)
  
  # Find the match
  out = bmatch(t_ind=t_ind, dist_mat=dist_mat, n_controls=n_matches,
               subset_weight = subset_weight,
               mom = list(covs=mom_covs,tols=mom_tols),
               exact = list(covs=exact_covs),
               near_exact = list(covs=near_exact_covs,devs=near_exact_devs),
               fine = list(covs=fine_covs),
               near_fine = list(covs=near_fine_covs,devs=near_fine_devs),
               solver = list(name="gurobi", approximate=0, t_max=600, trace=0))
  t_id = out$t_id
  c_id = out$c_id
  pair_id = out$group_id+pair_id_count
  pair_id_count = pair_id_count+length(t_id)
  
  # Save	
  d_aux = dy2[c(t_id, c_id), ]
  d_aux = cbind(d_aux, pair_id)
  dy_matched_3 = rbind(dy_matched_3, d_aux)
  
}
dy_matched_3 = dy_matched_3[-1, ]
saveRDS(dy_matched_3, file = paste0(projdir,"/data/sifcct_young_matched_3.rds"))

#+
dy_matched_3 <- readRDS(paste0(projdir,"/data/sifcct_young_matched_3.rds"))

table(dy_matched_3$treated)
min(table(dy_matched_3$pair_id))
max(table(dy_matched_3$pair_id))
table(is.na(dy_matched_3$pair_id))
length(unique(dy_matched_3$id))

## Balance
require(ebal)
require(Matching)
### Female
paste(table(dy_matched_3[dy_matched_3$female==1,]$treated)[1],
      table(dy[dy$female==1,]$treated)[1],sep="/") # N of Matched Found
balf_dy_matched_3 <- baltest.collect(MatchBalance(treated ~ age + knowledge + polint + employed + evecon + income + lvpr,
                                                  data = dy_matched_3[dy_matched_3$female==1,], paired=FALSE, print.level=0), 
                                     var.names = c("Age","Knowledge","Political Interest","Employed", 
                                                   "Economic Evaluation","Income","Length of Residence"),
                                     after = FALSE)
round(balf_dy_matched_3,3)[,1:7]
### Male
paste(table(dy_matched_3[dy_matched_3$female==0,]$treated)[1],
      table(dy[dy$female==0,]$treated)[1],sep="/") # N of Matched Found
balm_dy_matched_3 <- baltest.collect(MatchBalance(treated ~ age + knowledge + polint + employed + evecon + income + lvpr,
                                                  data = dy_matched_3[dy_matched_3$female==0,], paired=FALSE, print.level=0), 
                                     var.names = c("Age","Knowledge","Political Interest","Employed", 
                                                   "Economic Evaluation","Income","Length of Residence"),
                                     after = FALSE)
round(balm_dy_matched_3,3)[,1:7]

#+ eval=FALSE
## Save Balance Data
saveRDS(list(f = balf_dy_matched_3, m = balm_dy_matched_3), 
        file = paste0(projdir,"/data/sifcct_young_matched_3_balance.rds"))

#+ eval=FALSE
##############################################
# Match 4
# Optimal subset matching: yes
# Penalties on the distance matrix: yes (lambda = 200km)
# Explicit balancing of the covariates:  
#   mom_covs =  cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
#   mom_tols = c(0.03, 0.01, 0.02, 0.02, 0.02, 0.02)
#   exact_covs = as.matrix(employed) AND female by construct
#   near_exact_covs = as.matrix(wave) 
#   near_exact_devs = 1
##############################################

dy_matched_4 = rep(NA, 41)
pair_id_count = 0
for (j in 0:1) {	
  
  dy2 = dy[dy$female==j, ]
  dy2 = dy2[order(dy2$treated, decreasing=FALSE), ]
  attach(dy2, warn.conflicts=FALSE)
  
  # Treatment indicator 
  t_ind = 1-treated
  
  # Distance matrix
  lat_t = abs(zip_lat[t_ind==1])
  lat_c = abs(zip_lat[t_ind==0])
  lon_t = abs(zip_lon[t_ind==1])
  lon_c = abs(zip_lon[t_ind==0])
  coords_t = cbind(lon_t, lat_t)
  coords_c = cbind(lon_c, lat_c) 	
  dist_mat_orig = rdist.earth(coords_t, coords_c, miles=FALSE)
  dist_mat = round(dist_mat_orig/200, 4)
  # dist_mat = round(dist_mat_orig/median(dist_mat_orig), 2)
  dist_mat = dist_mat+100*(dist_mat_orig>350)
  # Whether all the treated units need to be used
  subset_weight = 1
  
  # Number of matches
  n_matches = 1
  
  # Moment covariates
  mom_covs = cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
  mom_tols = c(0.03, 0.01, 0.02, 0.02, 0.02, 0.02)
  mom_weights = NULL
  
  # Covariates for exact, near-exact, fine, and near-fine balance
  exact_covs = as.matrix(employed)
  near_exact_covs = as.matrix(wave)
  near_exact_devs = 1
  fine_covs = NULL
  near_fine_covs =  NULL
  near_fine_devs = NULL
  
  detach(dy2)
  
  # Find the match
  out = bmatch(t_ind=t_ind, dist_mat=dist_mat, n_controls=n_matches,
               subset_weight = subset_weight,
               mom = list(covs=mom_covs,tols=mom_tols),
               exact = list(covs=exact_covs),
               near_exact = list(covs=near_exact_covs,devs=near_exact_devs),
               fine = list(covs=fine_covs),
               near_fine = list(covs=near_fine_covs,devs=near_fine_devs),
               solver = list(name="gurobi", approximate=0, t_max=600, trace=0))
  t_id = out$t_id
  c_id = out$c_id
  pair_id = out$group_id+pair_id_count
  pair_id_count = pair_id_count+length(t_id)
  
  # Save	
  d_aux = dy2[c(t_id, c_id), ]
  d_aux = cbind(d_aux, pair_id)
  dy_matched_4 = rbind(dy_matched_4, d_aux)
  
}
dy_matched_4 = dy_matched_4[-1, ]
saveRDS(dy_matched_4, file = paste0(projdir,"/data/sifcct_young_matched_4.rds"))

#+
dy_matched_4 <- readRDS(paste0(projdir,"/data/sifcct_young_matched_4.rds"))

table(dy_matched_4$treated)
min(table(dy_matched_4$pair_id))
max(table(dy_matched_4$pair_id))
table(is.na(dy_matched_4$pair_id))
length(unique(dy_matched_4$id))

## Balance
require(ebal)
require(Matching)
### Female
paste(table(dy_matched_4[dy_matched_4$female==1,]$treated)[1],
      table(dy[dy$female==1,]$treated)[1],sep="/") # N of Matched Found
balf_dy_matched_4 <- baltest.collect(MatchBalance(treated ~ age + knowledge + polint + employed + evecon + income + lvpr,
                                                  data = dy_matched_4[dy_matched_4$female==1,], paired=FALSE, print.level=0), 
                                     var.names = c("Age","Knowledge","Political Interest","Employed", 
                                                   "Economic Evaluation","Income","Length of Residence"),
                                     after = FALSE)
round(balf_dy_matched_4,3)[,1:7]
### Male
paste(table(dy_matched_4[dy_matched_4$female==0,]$treated)[1],
      table(dy[dy$female==0,]$treated)[1],sep="/") # N of Matched Found
balm_dy_matched_4 <- baltest.collect(MatchBalance(treated ~ age + knowledge + polint + employed + evecon + income + lvpr,
                                                  data = dy_matched_4[dy_matched_4$female==0,], paired=FALSE, print.level=0), 
                                     var.names = c("Age","Knowledge","Political Interest","Employed", 
                                                   "Economic Evaluation","Income","Length of Residence"),
                                     after = FALSE)
round(balm_dy_matched_4,3)[,1:7]

#+ eval=FALSE
## Save Balance Data
saveRDS(list(f = balf_dy_matched_4, m = balm_dy_matched_4), 
        file = paste0(projdir,"/data/sifcct_young_matched_4_balance.rds"))

#+ eval=FALSE
##############################################
# Match 5
# Optimal subset matching: yes
# Penalties on the distance matrix: yes (lambda = 350km)
# Explicit balancing of the covariates:  
#   mom_covs =  cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
#   mom_tols = c(0.03, 0.01, 0.02, 0.02, 0.02, 0.02)
#   exact_covs = as.matrix(employed) AND female by construct
#   near_exact_covs = as.matrix(wave) 
#   near_exact_devs = 1
##############################################

dy_matched_5 = rep(NA, 41)
pair_id_count = 0
for (j in 0:1) {	
  
  dy2 = dy[dy$female==j, ]
  dy2 = dy2[order(dy2$treated, decreasing=FALSE), ]
  attach(dy2, warn.conflicts=FALSE)
  
  # Treatment indicator 
  t_ind = 1-treated
  
  # Distance matrix
  lat_t = abs(zip_lat[t_ind==1])
  lat_c = abs(zip_lat[t_ind==0])
  lon_t = abs(zip_lon[t_ind==1])
  lon_c = abs(zip_lon[t_ind==0])
  coords_t = cbind(lon_t, lat_t)
  coords_c = cbind(lon_c, lat_c) 	
  dist_mat_orig = rdist.earth(coords_t, coords_c, miles=FALSE)
  dist_mat = round(dist_mat_orig/350, 4)
  # dist_mat = round(dist_mat_orig/median(dist_mat_orig), 2)
  dist_mat = dist_mat+100*(dist_mat_orig>350)
  # Whether all the treated units need to be used
  subset_weight = 1
  
  # Number of matches
  n_matches = 1
  
  # Moment covariates
  mom_covs = cbind(stdage, stdknowledge, stdpolint, stdevecon, stdincome, stdlvpr)
  mom_tols = c(0.03, 0.01, 0.02, 0.02, 0.02, 0.02)
  mom_weights = NULL
  
  # Covariates for exact, near-exact, fine, and near-fine balance
  exact_covs = as.matrix(employed)
  near_exact_covs = as.matrix(wave)
  near_exact_devs = 1
  fine_covs = NULL
  near_fine_covs =  NULL
  near_fine_devs = NULL
  
  detach(dy2)
  
  # Find the match
  out = bmatch(t_ind=t_ind, dist_mat=dist_mat, n_controls=n_matches,
               subset_weight = subset_weight,
               mom = list(covs=mom_covs,tols=mom_tols),
               exact = list(covs=exact_covs),
               near_exact = list(covs=near_exact_covs,devs=near_exact_devs),
               fine = list(covs=fine_covs),
               near_fine = list(covs=near_fine_covs,devs=near_fine_devs),
               solver = list(name="gurobi", approximate=0, t_max=600, trace=0))
  t_id = out$t_id
  c_id = out$c_id
  pair_id = out$group_id+pair_id_count
  pair_id_count = pair_id_count+length(t_id)
  
  # Save	
  d_aux = dy2[c(t_id, c_id), ]
  d_aux = cbind(d_aux, pair_id)
  dy_matched_5 = rbind(dy_matched_5, d_aux)
  
}
dy_matched_5 = dy_matched_5[-1, ]
saveRDS(dy_matched_5, file = paste0(projdir,"/data/sifcct_young_matched_5.rds"))

#+
dy_matched_5 <- readRDS(paste0(projdir,"/data/sifcct_young_matched_5.rds"))

table(dy_matched_5$treated)
min(table(dy_matched_5$pair_id))
max(table(dy_matched_5$pair_id))
table(is.na(dy_matched_5$pair_id))
length(unique(dy_matched_5$id))

## Balance
require(ebal)
require(Matching)
### Female
paste(table(dy_matched_5[dy_matched_5$female==1,]$treated)[1],
      table(dy[dy$female==1,]$treated)[1],sep="/") # N of Matched Found
balf_dy_matched_5 <- baltest.collect(MatchBalance(treated ~ age + knowledge + polint + employed + evecon + income + lvpr,
                                                  data = dy_matched_5[dy_matched_5$female==1,], paired=FALSE, print.level=0), 
                                     var.names = c("Age","Knowledge","Political Interest","Employed", 
                                                   "Economic Evaluation","Income","Length of Residence"),
                                     after = FALSE)
round(balf_dy_matched_5,3)[,1:7]
### Male
paste(table(dy_matched_5[dy_matched_5$female==0,]$treated)[1],
      table(dy[dy$female==0,]$treated)[1],sep="/") # N of Matched Found
balm_dy_matched_5 <- baltest.collect(MatchBalance(treated ~ age + knowledge + polint + employed + evecon + income + lvpr,
                                                  data = dy_matched_5[dy_matched_5$female==0,], paired=FALSE, print.level=0), 
                                     var.names = c("Age","Knowledge","Political Interest","Employed", 
                                                   "Economic Evaluation","Income","Length of Residence"),
                                     after = FALSE)
round(balm_dy_matched_5,3)[,1:7]

#+ eval=FALSE
## Save Balance Data
saveRDS(list(f = balf_dy_matched_5, m = balm_dy_matched_5), 
        file = paste0(projdir,"data/sifcct_young_matched_5_balance.rds"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./src/data_sifcct_2_matching_young_v4.R', 'pdf_document', encoding = 'UTF-8')
# rmarkdown::render('./src/data_sifcct_2_matching_young_v4.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$|\\.log$|\\.tex$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
# In Terminal, move to src directory and run:
# Rscript -e "rmarkdown::render('data_sifcct_2_matching_young_v4.R', 'pdf_document', encoding = 'UTF-8')"
# Rscript -e "rmarkdown::render('data_sifcct_2_matching_young_v4.R', 'github_document', clean=FALSE)"
