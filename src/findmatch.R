library(designmatch) # Need GLPK Optimizer Installed. 
library(gurobi) # Need Gurobi Optimizer Installed.
library(fields) # For rdist.earth function

findmatch <- function(dt, ## Dataset
                      ## Subsetting Dataset 
                      subset_var = "female", # Subset by this variable 
                      subset_val = 1,
                      treated_var = "edu2", # Treatment Indicator Variable Name
                      treated_val = 1, # A value that should be considered treated
                      ## Distance Covariates 
                      dist_covs = NULL, # If this is NULL, use dist_coord_covs instead.
                      dist_coords_covs = c("zip_lon","zip_lat"),
                      dist_lambda = 50,
                      dist_penalize = 350,
                      ## Number of matches to find
                      n_matches = 1,
                      # Whether all the treated units need to be used
                      subset_weight = 1, 
                      # Moment covariates
                      mom_covs = NULL,
                      mom_tols = NULL,
                      # Kolmogorov-Smirnov balance covariates
                      ks_covs = NULL,
                      ks_tols = NULL,
                      # Exact balance covariaes (i.e., exact match beween pairs)
                      exact_covs = NULL,
                      # Near exact balance covariates (exact matched within devs)
                      near_exact_covs = NULL, 
                      near_exact_devs = NULL,
                      # Fine balance covariates (distribution match exactly)
                      fine_covs = NULL,
                      # Near fine covariates (distribution matched wthnin devs)
                      near_fine_covs = NULL,
                      near_fine_devs = NULL, 
                      # Solver (using Gurobi, but also GLPK is possible)
                      solver = list(name="gurobi", approximate=0, t_max=600, trace=1),
                      ...) {
  
  
  ## Subset data
  if (!is.null(subset_var)) dt <- dt[which(dt[,subset_var]==subset_val),]
  dt$treated <- ifelse(dt[,treated_var]==treated_val,1,0)
  dt = dt[order(dt$treated, decreasing=TRUE), ] # Treated First
  
  # Distance matrix
  if (!is.null(dist_covs)) { # (moment covariates)
    
    X_mat = as.matrix(dt[,dist_covs])
    dist_mat = distmat(dt$treated, X_mat)
    
  } else { # (geographical distance)
    
    lon_t = abs(dt[dt$treated==1,dist_coords_covs[1]])
    lon_c = abs(dt[dt$treated==0,dist_coords_covs[1]])
    lat_t = abs(dt[dt$treated==1,dist_coords_covs[2]])
    lat_c = abs(dt[dt$treated==0,dist_coords_covs[2]])
    coords_t = cbind(lon_t, lat_t)
    coords_c = cbind(lon_c, lat_c) 	
    dist_mat_orig = rdist.earth(coords_t, coords_c, miles=FALSE)
    dist_mat = round(dist_mat_orig/dist_lambda, 4)
    dist_mat = dist_mat+100*(dist_mat_orig>dist_penalize)
    
  }
  
  # Moment balance covariates
  if(!is.null(mom_covs)) mom_covs = list(covs=as.matrix(dt[,mom_covs]),tols=mom_tols)

  # Kolmogorov-Smirnov balance covariates
  if(!is.null(ks_covs)) ks_covs = list(covs=as.matrix(dt[,ks_covs]),tols=ks_tols)
  
  # Covariates for exact, near-exact, fine, and near-fine balance
  if(!is.null(exact_covs)) exact_covs = list(covs=as.matrix(dt[,exact_covs]))
  if(!is.null(near_exact_covs)) near_exact_covs = list(covs=as.matrix(dt[,near_exact_covs]),devs=near_exact_devs)
  if(!is.null(fine_covs)) fine_covs = list(covs=as.matrix(dt[,fine_covs]))
  if(!is.null(near_fine_covs)) near_fine_covs = list(covs=as.matrix(dt[,near_fine_covs]), devs=near_fine_devs)
  
  # Find the match
  set.seed(123456789)
  out = bmatch(t_ind=dt$treated, 
               dist_mat=dist_mat, 
               n_controls=n_matches,
               subset_weight = subset_weight,
               mom = mom_covs, 
               ks = ks_covs,
               exact = exact_covs,
               near_exact = near_exact_covs, 
               fine = fine_covs,
               near_fine = near_fine_covs,
               solver = solver, ...)

  # Output	
  d_aux = dt[c(out$t_id, out$c_id), ]
  d_aux$pair_id  = out$group_id
  
  return(d_aux)

}

## Balance
require(ebal)
require(Matching)

findbalance <- function(ds_matched, fm, vn) {
  
  balf_ds_matched <- baltest.collect(MatchBalance(fm, data = ds_matched, paired=FALSE, print.level=0), 
                                       var.names = vn, after = FALSE)
  return(balf_ds_matched)

}

