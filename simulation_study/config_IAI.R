# CONFIGURATION FOR THE SIMULATION STUDY -------------------------------------------
#
# Every user-adjustable setting lives in this file: directory layout, the random
# seed, cluster resources, and the scenario grids for each simulation study. All
# other scripts source this file and should not need editing.
#
# All paths are relative to the root of the simulation_study directory. Scripts
# must therefore be run with that directory as the working directory; the
# generated sbatch files `cd` into it before calling R. To run from elsewhere,
# set the environment variable IAI_ROOT to the simulation_study directory.


# PATHS ----------------------------------------------------------------------------

root.dir = normalizePath( Sys.getenv("IAI_ROOT", unset = getwd()), mustWork = TRUE )

# directories specific to one study (scenario numbers restart at 1 in each study,
#  so each study's files are kept separate)
study_dirs = function(study) {
  base = file.path(root.dir, "results", study)
  list(
    base         = base,
    scen.params  = file.path(base, "scen_params.csv"),  # written by genSbatch_IAI.R
    long.results = file.path(base, "long_results"),     # one file per sbatch job
    stitched     = file.path(base, "stitched"),         # stitched.csv and agg.csv
    sbatch       = file.path(base, "sbatch_files"),     # generated sbatch files
    logs         = file.path(base, "logs")              # SLURM .out and .err files
  )
}

make_study_dirs = function(study) {
  d = study_dirs(study)
  for ( x in d[ names(d) != "scen.params" ] ) dir.create(x, recursive = TRUE, showWarnings = FALSE)
  invisible(d)
}


# STUDIES --------------------------------------------------------------------------

# Labels of the simulation studies. "study12" produces Studies 1 and 2 of the
# paper (DAGs 1A-3B); "study3" produces Study 3 (violations of assumptions;
# DAGs 5A-6D). See make_scen_params() below and the README.
all.studies = c("study12", "study3")

check_study = function(study) {
  if ( !study %in% all.studies ) {
    stop( "Unknown study '", study, "'. Must be one of: ", paste(all.studies, collapse = ", ") )
  }
}


# R PACKAGES -----------------------------------------------------------------------

# packages needed to simulate and analyze data (loaded by load_sim_packages())
sim.packages = c("dplyr",
                 "tidyr",
                 "tibble",
                 "data.table",
                 "MASS",      # multivariate normal draws for the W block
                 "R2jags",    # IPW-nm (requires JAGS to be installed)
                 "boot",      # bootstrap CIs for mia-pkg-ice
                 "miapack",   # mia-pkg-ice
                 "tmle")      # mia-tmle

# packages installed from GitHub rather than CRAN, as package = "user/repo"
github.packages = c(miapack = "stmcg/miapack")

# Loads the given packages, first installing any that are missing (from CRAN, or
#  from GitHub for those in github.packages). Install once, e.g., on a cluster login
#  node via record_package_versions_IAI.R, before submitting jobs: many jobs
#  installing into the same library at once can corrupt it.
# R2jags additionally requires JAGS itself (on Sherlock: ml load jags/4.3.1).
load_sim_packages = function(pkgs = sim.packages,
                             repos = "https://cloud.r-project.org") {
  
  missing = pkgs[ !sapply( pkgs, requireNamespace, quietly = TRUE ) ]
  
  if ( length(missing) > 0 ) {
    
    # install into the first writable library (on a cluster, usually the user library)
    lib = .libPaths()[ file.access( .libPaths(), mode = 2 ) == 0 ][1]
    if ( is.na(lib) ) {
      lib = Sys.getenv("R_LIBS_USER")
      dir.create(lib, recursive = TRUE, showWarnings = FALSE)
      .libPaths( c(lib, .libPaths()) )
    }
    
    cran.missing = setdiff( missing, names(github.packages) )
    gh.missing   = intersect( missing, names(github.packages) )
    
    if ( length(cran.missing) > 0 ) {
      message( "Installing from CRAN: ", paste(cran.missing, collapse = ", ") )
      install.packages(cran.missing, lib = lib, repos = repos)
    }
    
    if ( length(gh.missing) > 0 ) {
      if ( !requireNamespace("remotes", quietly = TRUE) ) install.packages("remotes", lib = lib, repos = repos)
      for ( pkg in gh.missing ) {
        message( "Installing from GitHub: ", github.packages[[pkg]] )
        remotes::install_github( github.packages[[pkg]], lib = lib, upgrade = "never" )
      }
    }
  }
  
  ok = sapply( pkgs, function(pkg) suppressPackageStartupMessages( require(pkg, character.only = TRUE) ) )
  if ( !all(ok) ) stop( "Could not install or load package(s): ", paste(pkgs[!ok], collapse = ", "),
                        ". If R2jags failed, check that JAGS is installed." )
  invisible(TRUE)
}


# RANDOM SEEDS ---------------------------------------------------------------------

# Each sbatch job j of study s uses seed job_seed(s, j). Within a job, doRNG
# gives every simulation rep its own independent L'Ecuyer-CMRG stream derived
# from that seed, so results do not depend on the number of cores.
base.seed = 20241020

job_seed = function(study, job.num) {
  base.seed + 1e5 * match(study, all.studies) + as.integer(job.num)
}


# SIMULATION SIZE ------------------------------------------------------------------

# total simulation reps per scenario, split across sbatch jobs by genSbatch_IAI.R
n.reps.per.scen = 1000


# CLUSTER SETTINGS (SLURM) ---------------------------------------------------------

# These reflect Stanford's Sherlock cluster and will need to be changed for
# other systems.
cluster = list(
  partition      = "qsu,owners,normal",
  cores          = 16,  # cores per job; also passed to registerDoParallel()
  mem_per_node   = 64000,  # MB
  user_email     = "",     # set to receive SLURM emails
  mailtype       = "NONE",
  # environment modules loaded before running R
  modules        = c("v8", "openblas/0.3.20", "jags/4.3.1", "R/4.3.2")
)

# reps per sbatch job and wall time, as functions of a scenario's parameters
reps_per_job = function(scen.params) {
  ifelse( scen.params$W_dim == 1 & scen.params$N < 10e3, 250, 10 )
}

jobtime_per_scen = function(scen.params) {
  ifelse( scen.params$W_dim == 1, "02:00:00", "08:00:00" )
}


# SCENARIO GRIDS -------------------------------------------------------------------

# Returns one row per scenario, with the scenario number in column `scen`.
#
# Method labels in rep.methods (see doParallel_IAI.R):
#   gold        = benchmark analysis of the full data (no missingness)
#   CC          = complete-case analysis
#   mia-pkg-ice = MIA plug-in (iterative conditional expectation) estimator, via miapack
#   mia-tmle    = MIA targeted maximum likelihood estimator
#   IPW-nm      = inverse-probability weighting under a no-self-censoring model (Sun et al.)
#
# DAG labels: see the mapping to the paper's DAGs at the top of helper_IAI.R.

make_scen_params = function(study) {
  
  check_study(study)
  
  if ( study == "study12" ) {
    scen.params = tidyr::expand_grid(
      rep.methods        = "gold ; CC ; mia-pkg-ice ; mia-tmle ; IPW-nm",
      model              = "OLS",
      coef_of_interest   = "A",
      N                  = c(200, 500, 1000, 2000, 5000, 10000),
      boot_reps_mia_ice  = 1000,  # bootstrap reps for mia-pkg-ice CIs (0 = no CIs)
      calculate_tmle_CIs = TRUE,
      dag_name           = c("1A", "1B", "1C", "2A", "2B", "3A", "3B"),
      W_dim              = c(1, 10) )
  }
  
  if ( study == "study3" ) {
    scen.params = tidyr::expand_grid(
      rep.methods        = "gold ; mia-pkg-ice ; mia-tmle ; IPW-nm",
      model              = "OLS",
      coef_of_interest   = "A",
      N                  = c(200, 500, 1000, 5000, 10000),
      boot_reps_mia_ice  = 1000,
      calculate_tmle_CIs = TRUE,
      dag_name           = c("5A", "5B", "5C", "5D", "6A", "6B", "6C", "6D"),
      W_dim              = 1 )
  }
  
  # parameters of the auxiliary block W (helper_IAI_Wblock.R)
  scen.params = scen.params %>%
    dplyr::mutate(
      # number of continuous components (the rest are binary)
      W_n_cont          = ifelse( W_dim == 1, 0, 5 ),
      # numbers of always-observed continuous and binary components
      W_n_cont_complete = ifelse( W_dim == 1, 0, 3 ),
      W_n_bin_complete  = ifelse( W_dim == 1, 0, 2 ),
      # latent-scale correlation among components, and its structure ("exch" or "ar1")
      W_rho             = ifelse( W_dim == 1, 0, 0.4 ),
      W_cor_type        = "exch",
      # marginal P(W_j = 1) for binary components
      W_bin_prob        = 0.5,
      # marginal P(R_Wj = 0) for incomplete components; 0.4252 is the value implied
      #  by expit(-1 + 3*D1) when W_dim = 1; W_dim = 10 uses 0.10 so that complete
      #  cases are not too rare
      W_miss_rate       = ifelse( W_dim == 1, 1 - 0.5748, 0.10 ),
      # coefficient of W's parent (X2 or Y, depending on the DAG) on W
      W_parent_coef     = 1,
      # number and coefficient of W_j*W_k interaction terms in the missingness model
      #  (requires 2 * W_n_inter <= W_dim)
      W_n_inter         = 3,
      W_inter_coef      = 1 )
  
  # Study 1-2: W_dim = 10 is run only for N >= 1000
  scen.params = scen.params %>% dplyr::filter( !( N < 1000 & W_dim > 1 ) )
  
  # IPW-nm is run only when W_dim = 1
  rm_IPW_nm = function(string) paste( setdiff( strsplit(string, "\\s*;\\s*")[[1]], "IPW-nm" ),
                                      collapse = " ; " )
  scen.params$rep.methods = ifelse( scen.params$W_dim > 1,
                                    vapply(scen.params$rep.methods, rm_IPW_nm, character(1)),
                                    scen.params$rep.methods )
  
  scen.params %>% tibble::add_column( scen = seq_len( nrow(scen.params) ), .before = 1 )
}