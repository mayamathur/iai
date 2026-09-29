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
#   Rscript genSbatch_IAI.R <study> [submit | check_missed | resubmit_missed [time.mult]]
# where <study> is "study12" or "study3". Without a second argument, the sbatch
# files are written but not submitted. "check_missed" reports how many jobs have
# finished and which are missing, without submitting anything. "resubmit_missed"
# does the same, then resubmits jobs that have no results file and are not still
# queued or running (e.g., because a job exceeded its wall time); jobs retired by
# split_resubmit_IAI.R (listed in results/<study>/retired_jobs.csv) are never
# counted as missing or resubmitted. The optional
# time.mult multiplies each resubmitted job's wall time, e.g.,
#   Rscript genSbatch_IAI.R study12 resubmit_missed 2
# resubmits with double the original wall time (capped at cluster$max_hours).
#
# The sbatch files are specific to SLURM; cluster settings are in config_IAI.R.


# PRELIMINARIES --------------------------------------------------------------------

source("config_IAI.R")
load_sim_packages( c("dplyr", "tidyr", "tibble") )
source("helper_IAI.R")

args   = commandArgs(trailingOnly = TRUE)
study  = if ( length(args) >= 1 ) args[1] else "study3"
action = if ( length(args) >= 2 ) args[2] else "write_only"
# for resubmit_missed: factor by which to multiply each resubmitted job's wall time
time.mult = if ( length(args) >= 3 ) as.numeric(args[3]) else 1
check_study(study)
stopifnot( action %in% c("write_only", "submit", "check_missed", "resubmit_missed") )

d = make_study_dirs(study)


# CHECK OR RESUBMIT MISSED JOBS ----------------------------------------------------

# "check_missed" only reports progress; "resubmit_missed" reports, then resubmits
#  jobs that have no results file and are not still in the queue.
if ( action %in% c("check_missed", "resubmit_missed") ) {
  
  # expected jobs: one per sbatch file written for this study
  n.files = length( list.files(d$sbatch, pattern = "\\.sbatch$") )
  if ( n.files == 0 ) stop("No sbatch files in ", d$sbatch)
  
  # finished jobs: those that wrote a results file
  finished = list.files(d$long.results, pattern = "^long_results_job_[0-9]+_\\.csv$")
  finished.nums = sort( as.integer( sub( ".*_job_([0-9]+)_.*", "\\1", finished ) ) )
  
  # jobs still pending or running. SLURM job names are "<study>_job_<number>"; plain
  #  "job_<number>" names come from sbatch files written by earlier versions of this
  #  script and cannot be attributed to a study, so they are counted for any study.
  queue = tryCatch( system( "squeue -u $USER -h -o %j", intern = TRUE ), error = function(e) character(0) )
  pattern = paste0( "^(", study, "_)?job_[0-9]+$" )
  queued.nums = as.integer( sub( ".*job_", "", grep( pattern, queue, value = TRUE ) ) )
  
  # jobs retired by split_resubmit_IAI.R (replaced by smaller jobs with new numbers)
  retired.path = file.path(d$base, "retired_jobs.csv")
  retired.nums = if ( file.exists(retired.path) ) unique( read.csv(retired.path)$old.job ) else integer(0)
  
  missed.nums = setdiff( setdiff( setdiff( 1:n.files, finished.nums ), queued.nums ), retired.nums )
  
  # summarize runs of consecutive numbers, e.g., "1-3, 7, 10-12"
  as_ranges = function(x) {
    if ( length(x) == 0 ) return("none")
    x = sort(x); breaks = c(0, which(diff(x) != 1), length(x))
    paste( sapply( seq_len(length(breaks) - 1), function(k) {
      lo = x[breaks[k] + 1]; hi = x[breaks[k + 1]]
      if ( lo == hi ) lo else paste0(lo, "-", hi)
    } ), collapse = ", " )
  }
  
  cat( "\nStudy:                    ", study,
       "\nExpected jobs (sbatch):   ", n.files,
       "\nFinished (results file):  ", length(finished.nums),
       "\nMax finished job number:  ", if ( length(finished.nums) ) max(finished.nums) else NA,
       "\nStill queued or running:  ", length( intersect(queued.nums, 1:n.files) ),
       "\nRetired (split into new): ", length(retired.nums),
       "\nMissing, not in queue:    ", length(missed.nums),
       "\n  job numbers:            ", as_ranges(missed.nums), "\n\n" )
  
  if ( length(missed.nums) > 0 ) {
    write.csv( data.frame(job = missed.nums), file.path(d$base, "missed_job_nums.csv"), row.names = FALSE )
  }
  
  if ( action == "resubmit_missed" ) {
    for (i in missed.nums) {
      f = file.path(d$sbatch, paste0(i, ".sbatch"))
      time.arg = ""
      if ( time.mult != 1 ) {
        # original wall time from the sbatch file, as HH:MM:SS
        orig = sub( ".*--time=", "", grep( "^#SBATCH --time=", readLines(f), value = TRUE ) )
        hms = as.numeric( strsplit(orig, ":")[[1]] )
        mins = min( ceiling( (hms[1] * 60 + hms[2] + hms[3] / 60) * time.mult ), cluster$max_hours * 60 )
        new.time = sprintf( "%02d:%02d:00", mins %/% 60, mins %% 60 )
        time.arg = paste0(" --time=", new.time)  # overrides the #SBATCH line in the file
        cat("Job", i, ": wall time", orig, "->", new.time, "\n")
      }
      system( paste0( "sbatch -p ", cluster$partition, time.arg, " ", f ) )
    }
    cat("Resubmitted", length(missed.nums), "jobs\n")
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

# Regenerating renumbers every job (and would erase jobs written by
#  split_resubmit_IAI.R), so refuse once a study has any results; start over with
#  `bash run_all_IAI.sh clean <study>` if that is really intended.
if ( length( list.files(d$long.results) ) > 0 || file.exists( file.path(d$base, "retired_jobs.csv") ) ) {
  stop( study, " already has results in ", d$long.results, " (or split jobs); not regenerating its sbatch files. ",
        "Use check_missed / resubmit_missed / split_resubmit_IAI.R, or clean the study first." )
}

# remove sbatch files from any previous run of this study
unlink( list.files(d$sbatch, pattern = "\\.sbatch$", full.names = TRUE) )

jobname = paste("job", 1:n.files, sep = "_")

sbatch_params = data.frame(
  # SLURM job name includes the study, so jobs of different studies can be told apart
  jobname          = paste(study, jobname, sep = "_"),
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