# STITCH AND AGGREGATE SIMULATION RESULTS ------------------------------------------
#
# Step 3 of the simulation pipeline (see README). After all sbatch jobs for a study
# have finished, this script:
#   (1) combines the per-job results files in results/<study>/long_results into
#       results/<study>/stitched/stitched.csv (one row per scenario x rep x method); and
#   (2) summarizes performance by scenario and method in
#       results/<study>/stitched/agg.csv (bias, RMSE, CI coverage, and CI width).
#
# Usage (from the simulation_study directory):
#   Rscript stitch_IAI.R <study>
# where <study> is "study12" or "study3". It also reports any jobs that did not
# write results; these can be rerun with `Rscript genSbatch_IAI.R <study> resubmit_missed`.


# PRELIMINARIES --------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

source("config_IAI.R")
source("helper_IAI.R")


# FUNCTION ----------------------------------------------------------------------

stitch = function(study) {
  
  check_study(study)
  d = make_study_dirs(study)
  
  
  # ~ Stitch per-job files ---------------------------
  
  keepers = list.files(d$long.results, pattern = "^long_results_job", full.names = TRUE)
  if ( length(keepers) == 0 ) stop("No results files found in ", d$long.results)
  
  # bind_rows fills with NA any columns that are absent from some files (e.g., a
  #  method that errored in every rep of a job)
  s = bind_rows( lapply( keepers, function(x) fread(x, colClasses = c(dag_name = "character")) ) )
  s = s %>% filter( !is.na(scen.name) )
  
  cat("\nRows:", nrow(s), "  Scenarios:", nuni(s$scen.name), "\n")
  
  # report any jobs that did not write results
  n.files = length( list.files(d$sbatch, pattern = "\\.sbatch$") )
  sbatch_not_run( .results.singles.path = d$long.results,
                  .results.write.path = d$base,
                  .name.prefix = "long_results",
                  .max.sbatch.num = if ( n.files > 0 ) n.files else NA )
  
  fwrite( s, file.path(d$stitched, "stitched.csv") )
  
  
  # ~ Estimands ---------------------------
  
  # beta is the true contrast when sim_data() supplies it analytically; otherwise
  #  it is estimated by the mean of the benchmark ("gold") estimates across reps.
  #  The true intercept (reference-level mean) is always the benchmark mean.
  truth_emp = s %>%
    filter(method == "gold") %>%
    group_by(scen.name) %>%
    summarise( beta_emp = meanNA(bhat),
               int      = meanNA(inthat),
               .groups  = "drop" )
  
  s2 = s %>%
    left_join(truth_emp, by = "scen.name") %>%
    mutate( beta = coalesce(beta, beta_emp) ) %>%
    select(-beta_emp)
  
  s2$method = factor( s2$method, levels = c("gold", "CC", "IPW-nm", "mia-pkg-ice", "mia-tmle") )
  
  
  # ~ Aggregate by scenario and method ---------------------------
  
  # scenario-level variables: those constant within every scenario
  n.distinct = s2 %>%
    group_by(scen.name) %>%
    summarise( across( everything(), ~ n_distinct(., na.rm = FALSE) ), .groups = "drop" ) %>%
    select(-scen.name)
  scenario_vars = names(n.distinct)[ sapply(n.distinct, function(x) all(x == 1)) ]
  scenario_vars = union("scen.name", scenario_vars)
  
  agg = s2 %>%
    group_by( across( all_of(scenario_vars) ), method ) %>%
    summarise(
      reps      = n(),
      PropNA    = mean( is.na(bhat) ),
      Bhat      = meanNA(bhat),
      BhatBias  = meanNA(bhat - beta),
      BhatRMSE  = sqrt( meanNA( (bhat - beta)^2 ) ),
      BhatWidth = meanNA(bhat_hi - bhat_lo),
      BhatCover = meanNA( covers(truth = beta, lo = bhat_lo, hi = bhat_hi) ),
      IntHat    = meanNA(inthat),
      IntBias   = meanNA(inthat - int),
      IntRMSE   = sqrt( meanNA( (inthat - int)^2 ) ),
      IntWidth  = meanNA(int_hi - int_lo),
      IntCover  = meanNA( covers(truth = int, lo = int_lo, hi = int_hi) ),
      .groups = "drop" ) %>%
    mutate_if( is.numeric, function(x) round(x, 2) )
  
  fwrite( agg, file.path(d$stitched, "agg.csv") )
  cat("\nWrote", file.path(d$stitched, "stitched.csv"), "and agg.csv\n")
  
  invisible(agg)
}


# RUN ------------------------------------------------------------------------------

# runs only when called via Rscript, not when this file is sourced
if ( sys.nframe() == 0 ) {
  args = commandArgs(trailingOnly = TRUE)
  stitch( if ( length(args) >= 1 ) args[1] else "study3" )
}
