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


# FULL SET FOR STUDY 3 - 2026-08-03
scen.params = tidyr::expand_grid(

  rep.methods = c("gold ; mia-pkg-ice ; mia-tmle ; IPW-nm"),
  #rep.methods = c("gold ; mia-tmle"),

  model = "OLS",  # OLS or logistic

  coef_of_interest = "A",

  N = c( 200, 500, 1000, 5000, 10e3 ),

  # # MICE parameters (as on cluster)
  # imp_m = 50,
  # imp_maxit = 100,
  # mice_method = NA,  # let MICE use its defaults

  # # mia-pkg-ice parameters
  boot_reps_mia_ice = 1000,  # only needed for CIs; if 0, no CIs
  # #boot_reps_mia_ice = 0,

  # mia-pkg-sp parameters
  # mia_n_mc = 10e3,      # Monte Carlo draws for mia-pkg-sp

  # mia-tmle parameters
  calculate_tmle_CIs = TRUE,

  dag_name = c("5A", "5B", "5C", "5D",
               "6A", "6B", "6C", "6D"),

  # ~~ W BLOCK -----------------------------------------------
  W_dim = c(1)
  #W_dim = 10
)


# # FULL SET FOR STUDIES 1-2 - 2026-07-30
# scen.params = tidyr::expand_grid(
#   
#   rep.methods = c("gold ; CC ; mia-pkg-ice ; mia-tmle ; IPW-nm"),
#   #rep.methods = c("gold ; mia-tmle"),
#   
#   model = "OLS",  # OLS or logistic
#   
#   coef_of_interest = "A",
#   
#   # later only N <= 1000 will be retained for W_dim=1 scens,
#   #  and only N > 1000 for w_dim > 1 scens
#   N = c(200, 500, 1000,
#         2000, 5000, 10000),
#   
#   # # MICE parameters (as on cluster)
#   # imp_m = 50,
#   # imp_maxit = 100,
#   # mice_method = NA,  # let MICE use its defaults
#   
#   # # mia-pkg-ice parameters
#   boot_reps_mia_ice = 1000,  # only needed for CIs; if 0, no CIs
#   # #boot_reps_mia_ice = 0,
#   
#   # mia-pkg-sp parameters
#   # mia_n_mc = 10e3,      # Monte Carlo draws for mia-pkg-sp
#   
#   # mia-tmle parameters
#   calculate_tmle_CIs = TRUE,
#   
#   dag_name = c("1A", "1B", "1C",
#                "2A", "2B",
#                "3A", "3B"),
#   # dag_name = c("1A", "1B", "1C",
#   #              "2A", "2B",
#   #              "3A", "3B",
#   #              "5A", "5B", "5C", "5D", "5E"),
#   
#   # ~~ W BLOCK -----------------------------------------------
#   W_dim = c(1, 10)
#   #W_dim = 10
# )



# add W-block parameters
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

# END OF SCEN PARAMS FOR FULL SIMS


# remove bad combos:
# W_dim=10 and W_dim = 1 use different sample sizes
# However, for Study 3 (violations of assumptions; DAGs 5 and 6), don't apply any restrictions
#scen.params = scen.params %>% filter( !( N > 5000 & W_dim == 1 ) )
scen.params = scen.params %>% filter( startsWith(as.character(dag_name), "5") | startsWith(as.character(dag_name), "6") | !( N < 1000 & W_dim > 1 ) )
# check it
table(scen.params$N, scen.params$W_dim)
# both W_dim columns should be nonempty
stopifnot( all( table(scen.params$W_dim) > 0 ) )

# 5-series must have W_dim=1
scen.params = scen.params %>% filter( startsWith(as.character(dag_name), "5") | startsWith(as.character(dag_name), "6") & W_dim > 1 ) )
# check it
table(scen.params$dag_name, scen.params$W_dim)

# replace rep.methods string to not include IPW-nm when W_dim > 1
rm_IPW_nm = function(string) paste(setdiff(strsplit(string, "\\s*;\\s*")[[1]], "IPW-nm"), collapse = " ; ")
# example: rm_IPW_nm("gold ; CC ; mia-pkg-sp ; mia-pkg-ice ; IPW-nm") returns ""gold ; CC ; mia-pkg-sp ; mia-pkg-ice"
scen.params = scen.params %>% rowwise() %>%
  mutate( rep.methods = ifelse( W_dim > 1, rm_IPW_nm(rep.methods), rep.methods ) ) %>%
  ungroup()  
# check 
table(scen.params$W_dim, scen.params$rep.methods)

# add scen numbers
start.at = 1
scen.params = scen.params %>% add_column( scen = start.at : ( nrow(scen.params) + (start.at - 1) ),
                                          .before = 1 )

# look at it
head( as.data.frame(scen.params) )

# write the csv file of params (to Sherlock)
setwd(path)
write.csv( scen.params, "scen_params.csv", row.names = FALSE )



( n.scen = length(unique(scen.params$scen)) )


########################## GENERATE SBATCHES ##########################

# load functions for generating sbatch files
source("helper_IAI.R")

# ~~ PER-SCENARIO JOB SIZING ---------------------------------------------------
# Some scens have longer runtimes and need to be broken up into more sbatches, so
# reps-per-job is set per scenario rather than globally. Jobs are sized so that every
# scenario still gets exactly n.reps.per.scen reps in total.
# 2026-07-24 - for sims with N <= 1,000 including all methods, sim.reps=250 with job
#   time 4:00:00 worked well (these are now the W_dim = 1 scens).
n.reps.per.scen = 1000  # **temp: I used 1000 for real sims

# reps per doParallel job, one entry per row of scen.params.
reps.in.doParallel = ifelse( (scen.params$W_dim == 1 & scen.params$N < 10e3), 250, 10 )

# job resources, also per scenario (currently constant across arms; split these if the
# W_dim > 1 scens turn out to need more wall time or memory)
jobtime.by.scen      = ifelse( scen.params$W_dim == 1, "02:00:00", "08:00:00" )
# useful for running missed jobs

mem_per_node.by.scen = rep( 64000, n.scen )

# ******* DEBUGGING ONLY
if (FALSE) {
  n.reps.per.scen = 1  # debugging
  jobtime.by.scen      = ifelse( scen.params$W_dim == 1, "00:20:00", "00:20:00" )
}

# useful for running missed jobs
if(FALSE){
  #reps.in.doParallel = rep(10, nrow(scen.params) )
  jobtime.by.scen = rep("12:00:00", nrow(scen.params) )
}



# split n.reps.per.scen into chunks of AT MOST max.per.chunk, as evenly as possible, so
# the chunks sum to exactly n.reps.per.scen (no over- or under-run when the numbers
# don't divide evenly)
chunk_sizes = function(total, max.per.chunk) {
  n.chunks = ceiling(total / max.per.chunk)
  base = floor(total / n.chunks)
  rem  = total - base * n.chunks
  base + c( rep(1, rem), rep(0, n.chunks - rem) )
}

reps.by.file    = lapply( 1:n.scen,
                          function(i) chunk_sizes(n.reps.per.scen, reps.in.doParallel[i]) )
n.files.by.scen = sapply(reps.by.file, length)

# expand scenario-level vectors to file level
scen.name        = rep( scen.params$scen, times = n.files.by.scen )
n.reps.this.file = unlist(reps.by.file)
jobtime          = rep( jobtime.by.scen,      times = n.files.by.scen )
mem_per_node     = rep( mem_per_node.by.scen, times = n.files.by.scen )
( n.files = length(n.reps.this.file) )

# sanity checks
stopifnot( all( tapply(n.reps.this.file, scen.name, sum) == n.reps.per.scen ) )
stopifnot( length(scen.name) == n.files )

# sanity check
print( scen.params %>%
         mutate( reps.per.job = reps.in.doParallel,
                 n.jobs       = n.files.by.scen ) %>%
         count(W_dim, N, reps.per.job, n.jobs) %>%
         as.data.frame() )
cat("\nTotal scens:", n.scen, "  Total sbatch files:", n.files, "\n")


# ~~ BUILD SBATCH PARAMS -------------------------------------------------------

jobname = paste("job", 1:n.files, sep="_")
outfile = paste("/home/groups/manishad/IAI/rmfiles/rm_", 1:n.files, ".out", sep="")
errorfile = paste("/home/groups/manishad/IAI/rmfiles/rm_", 1:n.files, ".err", sep="")
write_path = paste(path, "/sbatch_files/", 1:n.files, ".sbatch", sep="")
runfile_path = paste(path, "/testRunFile.R", sep="")

sbatch_params <- data.frame(jobname,
                            outfile,
                            errorfile,
                            jobtime = jobtime,             
                            quality = "normal",
                            node_number = 1,
                            mem_per_node = mem_per_node,    
                            mailtype =  "NONE",
                            user_email = "mmathur@stanford.edu",
                            tasks_per_node = 16,
                            cpus_per_task = 1,
                            path_to_r_script = paste(path, "/doParallel_IAI.R", sep=""),
                            ### CHANGED: 3rd arg is now file-specific, not a global constant
                            args_to_r_script = paste("--args", jobname, scen.name,
                                                     n.reps.this.file, sep=" "),
                            write_path,
                            stringsAsFactors = F,
                            # left NA because the submission loop below (which passes -p)
                            # is what actually submits; testRunFile.R goes unused
                            server_sbatch_path = NA)

generateSbatch(sbatch_params, runfile_path)

n.files

# run just the first one
# sbatch -p qsu,owners,normal /home/groups/manishad/IAI/sbatch_files/1.sbatch


path = "/home/groups/manishad/IAI"
partition = "qsu,owners,normal"
setwd( paste(path, "/sbatch_files", sep="") )

# 928
for (i in 1:928) {
  system( paste("sbatch -p ", partition, " /home/groups/manishad/IAI/sbatch_files/", i, ".sbatch", sep="") )
}


######## If Running Only Some Jobs To Fill Gaps ########

# run in Sherlock ml load R
path = "/home/groups/manishad/IAI"
setwd(path)
source("helper_IAI.R")

missed.nums = sbatch_not_run( "/home/groups/manishad/IAI/long_results",
                              "/home/groups/manishad/IAI/long_results",
                              .name.prefix = "long_results",
                              .max.sbatch.num = 928 )

setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p ", partition, " /home/groups/manishad/IAI/sbatch_files/", i, ".sbatch", sep="") )
}