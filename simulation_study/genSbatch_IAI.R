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
  
  rep.methods = c("gold ; CC ; mia-pkg-sp ; mia-pkg-ice ; IPW-nm"),
  model = "OLS",  # OLS or logistic
  coef_of_interest = "A",
  N = c(200, 500, 1000, 10e3),
  #N = 500,
  
  # MICE parameters (as on cluster)
  imp_m = 50,
  imp_maxit = 100,
  mice_method = NA,  # let MICE use its defaults
  
  # AF4 / MIA parameters
  boot_reps_af4 = 1000,  # only needed for CIs; if 0, no CIs
  mia_n_mc = 10e3,      # Monte Carlo draws for mia-pkg-sp
  
  dag_name = c("5A", "5B", "5C", "5D",
               "1A", "1B", "1C",
               "2A", "2B",
               "3A", "3B"),
  
  # dag_name = c("1A", "1B", "1C",
  #              "2A", "2B",
  #              "3A", "3B",
  #              "5A", "5B", "5C", "5D"),
  
  # ~~ W BLOCK -----------------------------------------------
  # W_dim = 1  -> legacy single binary auxiliary D (reproduces prior runs)
  # W_dim = 10 -> high-dimensional correlated mixed-type W
  W_dim = c(1, 10) )

# ~~ W-block parameters (constant across scens; edit here to vary) ----------
scen.params = scen.params %>%
  mutate(
    W_n_cont          = ifelse( W_dim == 1, 0, 5 ),   # 5 continuous, 5 binary when W_dim = 10
    
    # W^+ / W^- split: complete vs incomplete components, type-balanced.
    # Set W_n_cont_complete = W_n_bin_complete = 0 for an all-incomplete arm.
    W_n_cont_complete = ifelse( W_dim == 1, 0, 3 ),
    W_n_bin_complete  = ifelse( W_dim == 1, 0, 2 ),
    
    W_rho             = ifelse( W_dim == 1, 0, 0.4 ),  # LATENT-scale correlation
    W_cor_type        = "exch",                        # "exch" or "ar1"
    W_bin_prob        = 0.5,                            # marginal P(W_binary = 1)
    
    # target marginal P(R_Wj = 0) for incomplete components. Legacy value is
    # 0.4252 (what expit(-1 + 3*D1) implies); fine at W_dim = 1 but leaves ~3.6%
    # complete cases at W_dim = 10, so the high-dim arms use 0.10.
    W_miss_rate       = ifelse( W_dim == 1, 1 - 0.5748, 0.10 ),
    
    # required by the W-block generator (their absence caused the crash):
    W_parent_coef     = 1,     # strength of the W parent (X2 or Y, per DAG) -> W
    W_n_inter         = 3,     # # of W_j*W_k interaction terms in S_R (needs 2*W_n_inter <= W_dim)
    W_inter_coef      = 1 )    # coefficient on each interaction term


# remove bad combos:
# 5-series must have W_dim=1
scen.params = scen.params %>% filter( !(dag_name %in% c("5A", "5B", "5C", "5D") & W_dim > 1 ) )
# check it 
table(scen.params$dag_name, scen.params$W_dim)

# replace rep.methods string to not include IPW-nm when W_dim > 1
rm_IPW_nm = function(string) paste(setdiff(strsplit(string, "\\s*;\\s*")[[1]], "IPW-nm"), collapse = " ; ")
# example: rm_IPW_nm("gold ; CC ; mia-pkg-sp ; mia-pkg-ice ; IPW-nm")
scen.params = scen.params %>% rowwise() %>%
  mutate( rep.methods = ifelse( W_dim > 1, rm_IPW_nm(rep.methods), rep.methods ) )


# add scen numbers
start.at = 1
scen.params = scen.params %>% add_column( scen = start.at : ( nrow(scen.params) + (start.at - 1) ),
                                          .before = 1 )
# check
table(scen.params$W_dim, scen.params$rep.methods)



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
n.reps.per.scen = 1000; n.reps.in.doParallel = 500
#n.reps.per.scen = 1; n.reps.in.doParallel = 1
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
                            jobtime = "00:30:00",  
                            #jobtime = "02:00:00",
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