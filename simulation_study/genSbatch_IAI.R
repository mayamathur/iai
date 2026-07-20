# PRELIMINARIES -----------------------------------------

path = "/home/groups/manishad/IAI"
setwd(path)
source("helper_IAI.R")

allPackages = c("here",
                "crayon",
                "dplyr",
                "foreach",
                "doParallel",
                "data.table",
                "purrr",
                "tidyr",
                "tibble",
                "testthat",
                "Hmisc",
                "stringr")

( packagesNeeded = allPackages[ !( allPackages %in% installed.packages()[,"Package"] ) ] )
if( length(packagesNeeded) > 0 ) install.packages(packagesNeeded)

# load all packages
lapply( allPackages,
        require,
        character.only = TRUE)

#**you need to see all "TRUE" printed by this in order for the package to actually be loaded



# SET SIMULATION PARAMETERS -----------------------------------------

scen.params = tidyr::expand_grid(
  
  model = c("OLS", "logistic"),   
  coef_of_interest = "A",
  N = c(10000),
  
  # MICE parameters
  # as on cluster
  imp_m = 50,
  imp_maxit = 100,
  mice_method = NA,  # let MICE use its defaults
  
  # AF4 parameters
  boot_reps_af4 = 0,  # only needed for CIs; if set to 0, won't give CIs
  
  dag_name = c("5-MNAR-cont", "5-MAR-cont",
               "5-MNAR-bin", "5-MAR-bin",
               "6-MNAR-cont", "6-MAR-cont",
               "6-MNAR-bin", "6-MAR-bin"),
  
  # ~~ W BLOCK -----------------------------------------------
  # W_dim = 1  -> legacy single binary auxiliary D (reproduces prior runs)
  # W_dim = 10 -> high-dimensional correlated mixed-type W
  W_dim = c(1, 10) )


# ~~ W-block parameters (constant across scens; edit here to vary) ----------

scen.params = scen.params %>%
  mutate(
    W_n_cont          = ifelse( W_dim == 1, 0, 5 ),   # 5 continuous, 5 binary when W_dim = 10
    
    # W^+ / W^- split. W_nc (complete) vs W_c (incomplete), type-balanced.
    # NB: with W_dim = 10 and 5 complete, W != W^-, so the 5-* DAGs satisfy
    # NEITHER hypothesis of Thm 6 (W = W^- fails, and V^- = A != Y).
    # Set W_n_cont_complete = W_n_bin_complete = 0 for an all-incomplete arm.
    W_n_cont_complete = ifelse( W_dim == 1, 0, 3 ),
    W_n_bin_complete  = ifelse( W_dim == 1, 0, 2 ),
    
    W_rho             = ifelse( W_dim == 1, 0, 0.4 ),  # LATENT-scale correlation
    W_cor_type        = "exch",                        # "exch" or "ar1"
    W_bin_prob        = 0.5,
    
    # target marginal P(R_Wj = 0) for incomplete components. The legacy value is
    # 0.4252 (= what expit(-1 + 3*D1) implies); that is fine for W_dim = 1 but
    # leaves ~3.6% complete cases at W_dim = 10, so the high-dim arms use 0.10.
    W_miss_rate       = ifelse( W_dim == 1, 1 - 0.5748, 0.10 ) )


# ~~ Methods, per scenario --------------------------------------------------
# IPW-nm's JAGS model builds one parameter block per observed missingness
# pattern, so it is not viable once W has 10 separately-missing components.
# Drop it there and lean on MICE. NB: this means the W_dim = 1 and W_dim = 10
# scens no longer share a comparator set -- the IPW-nm column will be empty for
# the high-dim scens.

scen.params = scen.params %>%
  mutate( rep.methods = ifelse( W_dim == 1,
                                "gold ; CC ; MICE-std ; genloc ; IPW-nm",
                                "gold ; CC ; MICE-std ; genloc" ) ) %>%
  relocate(rep.methods)


# The MAR arms have no auxiliary at all (their sim_data branches never call
# gen_W_block), so W_dim does not apply: keep one copy of each and record
# W_dim = 0 rather than a misleading W_dim = 1.
scen.params = scen.params %>%
  filter( !( grepl("-MAR-", dag_name) & W_dim == 10 ) ) %>%
  mutate( W_dim = ifelse( grepl("-MAR-", dag_name), 0, W_dim ) )


# remove nonsensical combinations of parameters:
# i.e., logistic regression when outcome is continuous
scen.params = scen.params %>% filter( !(model == "logistic" & !grepl("-bin", dag_name) ) )


# # FULL SET
# scen.params = tidyr::expand_grid(
#   
#   rep.methods = "gold ; IPW-nm ; af4-np ; af4-sp",
#   
#   model = "OLS",  # FOR CONTINUOUS OUTCOME
#   
#   coef_of_interest = "A",
#   N = c(10000),
#   
#   # MICE parameters
#   # as on cluster
#   imp_m = 50,  
#   imp_maxit = 100,
#   mice_method = NA,
#   
#   # AF4 parameters
#   boot_reps_af4 = 1000,  # only needed for CIs; if set to 0, won't give CIs
#   
#   dag_name = c("1A", "1B", "1C",
#                "2A", "2B",
#                "3A", "3B")
#   
#   )


# add scen numbers
start.at = 1
scen.params = scen.params %>% add_column( scen = start.at : ( nrow(scen.params) + (start.at - 1) ),
                                          .before = 1 )


( n.scen = nrow(scen.params) )
# look at it
head( as.data.frame(scen.params) )

# write the csv file of params (to Sherlock)
setwd(path)
write.csv( scen.params, "scen_params.csv", row.names = FALSE )


########################### GENERATE SBATCHES ###########################

# load functions for generating sbatch files
source("helper_IAI.R")

# number of sbatches to generate (i.e., iterations within each scenario)
n.reps.per.scen = 500
n.reps.in.doParallel = 5
#n.reps.per.scen = 100
#n.reps.in.doParallel = 1
( n.files = ( n.reps.per.scen / n.reps.in.doParallel ) * n.scen )


path = "/home/groups/manishad/IAI"

scen.name = rep( scen.params$scen, each = ( n.files / n.scen ) )
jobname = paste("job", 1:n.files, sep="_")
outfile = paste("/home/groups/manishad/IAI/rmfiles/rm_", 1:n.files, ".out", sep="")
errorfile = paste("/home/groups/manishad/IAI/rmfiles/rm_", 1:n.files, ".err", sep="")
write_path = paste(path, "/sbatch_files/", 1:n.files, ".sbatch", sep="")
runfile_path = paste(path, "/testRunFile.R", sep="")

sbatch_params <- data.frame(jobname,
                            outfile,
                            errorfile,
                            #jobtime = "01:00:00",  # with IPW-nm
                            #jobtime = "00:30:00",  # with only MICE
                            jobtime = "02:00:00",
                            quality = "normal",
                            node_number = 1,
                            mem_per_node = 64000,
                            mailtype =  "NONE",
                            user_email = "mmathur@stanford.edu",
                            tasks_per_node = 16,
                            cpus_per_task = 1,
                            path_to_r_script = paste(path, "/doParallel_IAI.R", sep=""),
                            args_to_r_script = paste("--args", jobname, scen.name, sep=" "),
                            write_path,
                            stringsAsFactors = F,
                            server_sbatch_path = NA)

generateSbatch(sbatch_params, runfile_path)

n.files

# run just the first one
# sbatch -p qsu,owners,normal /home/groups/manishad/IAI/sbatch_files/1.sbatch

path = "/home/groups/manishad/IAI"
setwd( paste(path, "/sbatch_files", sep="") )
for (i in 1:n.files) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/IAI/sbatch_files/", i, ".sbatch", sep="") )
}





######## If Running Only Some Jobs To Fill Gaps ########

# run in Sherlock ml load R
path = "/home/groups/manishad/IAI"
setwd(path)
source("helper_IAI.R")

missed.nums = sbatch_not_run( "/home/groups/manishad/IAI/long_results",
                              "/home/groups/manishad/IAI/long_results",
                              .name.prefix = "long_results",
                              .max.sbatch.num = n.files )



setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/IAI/sbatch_files/", i, ".sbatch", sep="") )
}