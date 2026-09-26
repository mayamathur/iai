# GENERATE AND SUBMIT SBATCH FILES -------------------------------------------------
#
# Step 1 of the simulation pipeline (see README). For one simulation study, this
# script:
#   (1) writes the scenario grid defined in config_IAI.R to results/<study>/scen_params.csv;
#   (2) splits each scenario's n.reps.per.scen reps into sbatch jobs;
#   (3) writes one sbatch file per job to results/<study>/sbatch_files; and
#   (4) optionally submits all jobs, or only those that have not yet written results.
#
# Usage (from the simulation_study directory, on the cluster):
#   Rscript genSbatch_IAI.R <study> [submit | resubmit_missed]
# where <study> is "study12" or "study3". Without a second argument, the sbatch
# files are written but not submitted. "resubmit_missed" submits only jobs whose
# results file is absent (e.g., because a job exceeded its wall time).
#
# The sbatch files are specific to SLURM; cluster settings are in config_IAI.R.


# PRELIMINARIES --------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
})

source("config_IAI.R")
source("helper_IAI.R")

args   = commandArgs(trailingOnly = TRUE)
study  = if ( length(args) >= 1 ) args[1] else "study3"
action = if ( length(args) >= 2 ) args[2] else "write_only"
check_study(study)
stopifnot( action %in% c("write_only", "submit", "resubmit_missed") )

d = make_study_dirs(study)


# RESUBMIT MISSED JOBS -------------------------------------------------------------

if ( action == "resubmit_missed" ) {
  n.files = length( list.files(d$sbatch, pattern = "\\.sbatch$") )
  missed.nums = sbatch_not_run( .results.singles.path = d$long.results,
                                .results.write.path = d$base,
                                .name.prefix = "long_results",
                                .max.sbatch.num = n.files )
  for (i in missed.nums) {
    system( paste0( "sbatch -p ", cluster$partition, " ", file.path(d$sbatch, paste0(i, ".sbatch")) ) )
  }
  quit(save = "no")
}


# SCENARIO PARAMETERS --------------------------------------------------------------

scen.params = make_scen_params(study)
n.scen = nrow(scen.params)

# check the grid
print( table(scen.params$dag_name, scen.params$W_dim) )
print( table(scen.params$N, scen.params$W_dim) )
print( table(scen.params$W_dim, scen.params$rep.methods) )

write.csv( scen.params, d$scen.params, row.names = FALSE )


# SPLIT REPS INTO JOBS -------------------------------------------------------------

# Split `total` reps into chunks of at most `max.per.chunk`, as evenly as possible,
#  so that the chunks sum to exactly `total`.
chunk_sizes = function(total, max.per.chunk) {
  n.chunks = ceiling(total / max.per.chunk)
  base = floor(total / n.chunks)
  rem  = total - base * n.chunks
  base + c( rep(1, rem), rep(0, n.chunks - rem) )
}

reps.by.file    = lapply( reps_per_job(scen.params),
                          function(m) chunk_sizes(n.reps.per.scen, m) )
n.files.by.scen = sapply(reps.by.file, length)

# expand scenario-level quantities to job level
scen.name        = rep( scen.params$scen, times = n.files.by.scen )
n.reps.this.file = unlist(reps.by.file)
jobtime          = rep( jobtime_per_scen(scen.params), times = n.files.by.scen )
n.files          = length(n.reps.this.file)

stopifnot( all( tapply(n.reps.this.file, scen.name, sum) == n.reps.per.scen ) )

cat("\nTotal scenarios:", n.scen, "  Total sbatch files:", n.files, "\n")


# WRITE SBATCH FILES ---------------------------------------------------------------

# remove sbatch files from any previous run of this study
unlink( list.files(d$sbatch, pattern = "\\.sbatch$", full.names = TRUE) )

jobname = paste("job", 1:n.files, sep = "_")

sbatch_params = data.frame(
  jobname,
  outfile          = file.path(d$logs, paste0("rm_", 1:n.files, ".out")),
  errorfile        = file.path(d$logs, paste0("rm_", 1:n.files, ".err")),
  jobtime          = jobtime,
  quality          = "normal",
  node_number      = 1,
  mem_per_node     = cluster$mem_per_node,
  mailtype         = cluster$mailtype,
  user_email       = cluster$user_email,
  tasks_per_node   = cluster$cores,
  cpus_per_task    = 1,
  partition        = cluster$partition,
  module_loads     = paste( paste("ml load", cluster$modules), collapse = "\n" ),
  work_dir         = root.dir,
  path_to_r_script = "doParallel_IAI.R",
  # arguments: study, job name, scenario number, number of reps in this job
  args_to_r_script = paste("--args", study, jobname, scen.name, n.reps.this.file),
  write_path       = file.path(d$sbatch, paste0(1:n.files, ".sbatch")),
  stringsAsFactors = FALSE )

invisible( generateSbatch( sbatch_params,
                extra_placeholders = c( PARTITION    = "partition",
                                        MODULE_LOADS = "module_loads",
                                        WORK_DIR     = "work_dir" ) ) )


# SUBMIT ---------------------------------------------------------------------------

if ( action == "submit" ) {
  for (i in 1:n.files) {
    system( paste0( "sbatch -p ", cluster$partition, " ", sbatch_params$write_path[i] ) )
  }
}
