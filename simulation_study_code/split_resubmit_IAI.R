# SPLIT AND RESUBMIT JOBS THAT KEEP TIMING OUT --------------------------------------
#
# Replaces sbatch jobs that did not write a results file with several smaller jobs
#  that run the same scenario and together run the same number of reps, so no other
#  jobs in the study need to be rerun.
#
# Usage (from $ROOT, with the modules loaded):
#   Rscript split_resubmit_IAI.R <study> <new reps per job> <job nums | missed> [HH:MM:SS] [write]
# e.g.
#   Rscript split_resubmit_IAI.R study12 50 missed              # dry run: prints the plan
#   Rscript split_resubmit_IAI.R study12 50 missed 24:00:00 write
#   Rscript split_resubmit_IAI.R study12 50 1001,1002,1003 write
# "missed" = every job number with an sbatch file but no results file, minus jobs
#  already retired by an earlier split. Omit HH:MM:SS (or pass NA) to keep each old
#  job's wall time.
#
# Bookkeeping:
#  - Each piece gets a NEW job number above every sbatch file, results file, and
#    retired job, hence its own seed from job_seed(); reusing the old number for
#    several pieces would make their reps identical under %dorng%.
#  - The old sbatch file is the template (partition, modules, paths carry over). The
#    job name <study>_job_<k> and log files rm_<k>.out/.err get the new number, and
#    the doParallel line gets the new jobname and n.reps.
#  - genSbatch_IAI.R resubmit_missed passes a longer --time on the sbatch command
#    line without editing the file, so the old files still hold the ORIGINAL wall
#    time; pass HH:MM:SS explicitly unless that original time is enough.
#  - New files are written as <new num>.sbatch in the study's sbatch_files dir, so
#    submit_throttled_IAI.sh and the range loop in the cheat sheet can find them.
#  - Refuses to split a job that has a results file or is still in the queue.
#  - Appends old -> new job numbers to results/<study>/retired_jobs.csv. Stitching
#    needs no change (it reads whatever results files exist), but anything that
#    lists "missed" jobs must exclude the retired ones (sbatch_not_run(.retired.path)).

source("config_IAI.R")
source("helper_IAI.R")

split_resubmit = function(study, old.job.nums, new.reps.per.job,
                          new.jobtime = NA, write = FALSE) {
  
  check_study(study)
  d = study_dirs(study)
  retired.path = file.path(d$base, "retired_jobs.csv")
  retired = if ( file.exists(retired.path) ) read.csv(retired.path) else
    data.frame(old.job = integer(0), new.job = integer(0))
  
  sb.files = list.files(d$sbatch, pattern = "^[0-9]+\\.sbatch$")
  sb.nums  = as.integer( sub("\\.sbatch$", "", sb.files) )
  res.nums = suppressWarnings( as.integer( sub( ".*_job_([0-9]+)_\\.csv$", "\\1",
                                                list.files(d$long.results, pattern = "_job_[0-9]+_\\.csv$") ) ) )
  
  if ( identical(old.job.nums, "missed") ) {
    old.job.nums = setdiff( sb.nums, c(res.nums, retired$old.job) )
  }
  old.job.nums = sort( as.integer(old.job.nums) )
  if ( length(old.job.nums) == 0 ) { cat("\nNo jobs to split.\n"); return(invisible(NULL)) }
  if ( any(old.job.nums %in% retired$old.job) ) {
    stop( "Already retired by an earlier split: ", paste( intersect(old.job.nums, retired$old.job), collapse = ", " ) )
  }
  
  # jobs still queued or running must not be split (their results may yet arrive)
  queued = tryCatch( system( "squeue -u $USER -h -o %j", intern = TRUE ), error = function(e) character(0) )
  
  next.num = max( c(sb.nums, res.nums, retired$old.job, retired$new.job), na.rm = TRUE ) + 1
  
  plan = list(); new.files = list()
  
  for ( k in old.job.nums ) {
    
    f = file.path( d$sbatch, paste0(k, ".sbatch") )
    if ( !file.exists(f) ) stop( "No sbatch file ", f )
    if ( k %in% res.nums ) stop( "Job ", k, " already has a results file; not splitting it." )
    x = readLines(f, warn = FALSE)
    
    jn = sub( "^#SBATCH --job-name=", "", grep( "^#SBATCH --job-name=", x, value = TRUE )[1] )
    if ( !is.na(jn) && jn %in% queued ) stop( "Job ", k, " (", jn, ") is still in the queue; cancel it first or wait." )
    
    r.line = grep( "doParallel_IAI\\.R.*--args", x )
    if ( length(r.line) != 1 ) stop( "No unique doParallel args line in ", f )
    args = strsplit( trimws( sub( ".*--args", "", x[r.line] ) ), "\\s+" )[[1]]
    if ( length(args) != 4 || args[1] != study || args[2] != paste0("job_", k) ) {
      stop( "Unexpected args in ", f, ": ", paste(args, collapse = " ") )
    }
    scen = as.integer(args[3]); old.n.reps = as.integer(args[4])
    old.time = sub( "^#SBATCH --time=", "", grep( "^#SBATCH --time=", x, value = TRUE )[1] )
    
    sizes = c( rep( new.reps.per.job, old.n.reps %/% new.reps.per.job ),
               if ( old.n.reps %% new.reps.per.job > 0 ) old.n.reps %% new.reps.per.job )
    if ( length(sizes) == 1 ) warning( "Job ", k, " has only ", old.n.reps, " reps; splitting into 1 piece does nothing but change its seed." )
    
    for ( s in sizes ) {
      m = next.num
      if ( m >= 1e5 ) stop( "Job number ", m, " would reach into the next study's seed range (job_seed uses 1e5 per study)." )
      y = x
      # only the job-number tokens: "job_<k>" in the job name and "rm_<k>." in the log
      #  paths (a bare number match could hit e.g. the "12" in "study12")
      sb.lines = grep( "^#SBATCH --(job-name|output|error)=", y )
      y[sb.lines] = gsub( paste0("job_", k, "(?![0-9])"), paste0("job_", m), y[sb.lines], perl = TRUE )
      y[sb.lines] = gsub( paste0("rm_", k, "\\."), paste0("rm_", m, "."), y[sb.lines] )
      y[r.line]   = sub( paste0("--args\\s+", study, "\\s+job_", k, "\\s+", scen, "\\s+", old.n.reps, "\\b"),
                         paste("--args", study, paste0("job_", m), scen, s), y[r.line] )
      if ( !grepl( paste0("job_", m, " ", scen, " ", s), y[r.line] ) ) stop( "Failed to rewrite the args line for job ", k )
      if ( !is.na(new.jobtime) ) y = sub( "^#SBATCH --time=.*", paste0("#SBATCH --time=", new.jobtime), y )
      if ( sum( y[sb.lines] != x[sb.lines] ) != 3 ) stop( "Job ", k, ": expected to rename the job name and both log files; check ", f )
      
      new.path = file.path( d$sbatch, paste0(m, ".sbatch") )
      if ( file.exists(new.path) ) stop( new.path, " already exists" )
      new.files[[new.path]] = y
      plan[[length(plan) + 1]] = data.frame( old.job = k, new.job = m, scen = scen,
                                             old.n.reps = old.n.reps, new.n.reps = s,
                                             old.time = old.time,
                                             new.time = if ( is.na(new.jobtime) ) old.time else new.jobtime )
      next.num = next.num + 1
    }
  }
  plan = do.call(rbind, plan)
  
  # every old job's reps are accounted for exactly
  tot = aggregate( new.n.reps ~ old.job + old.n.reps, data = plan, FUN = sum )
  stopifnot( all( tot$new.n.reps == tot$old.n.reps ) )
  # seeds of the new jobs are distinct from each other and from every existing job
  seeds.new = job_seed( study, plan$new.job )
  seeds.old = job_seed( study, unique( c(sb.nums, res.nums, retired$new.job) ) )
  stopifnot( !anyDuplicated(seeds.new), !any(seeds.new %in% seeds.old) )
  
  cat( "\nPlan:\n" ); print( plan, row.names = FALSE )
  
  if ( !write ) {
    cat( "\nDry run: nothing written. Add 'write' as the last argument to write the sbatch files.\n" )
    return( invisible(plan) )
  }
  
  for ( p in names(new.files) ) writeLines( new.files[[p]], p )
  write.table( plan[, c("old.job", "new.job", "scen", "old.n.reps", "new.n.reps")], retired.path,
               sep = ",", row.names = FALSE,
               col.names = !file.exists(retired.path), append = file.exists(retired.path) )
  
  cat( "\nWrote", nrow(plan), "sbatch files (", min(plan$new.job), "-", max(plan$new.job), ") to", d$sbatch,
       "\nand appended the mapping to", retired.path, "\n" )
  invisible(plan)
}

# COMMAND LINE -------------------------------------------------------------------

if ( !interactive() ) {
  a = commandArgs(trailingOnly = TRUE)
  if ( length(a) < 3 ) stop( "Usage: Rscript split_resubmit_IAI.R <study> <reps per job> <job nums | missed> [HH:MM:SS] [write]" )
  jobs  = if ( a[3] == "missed" ) "missed" else as.integer( strsplit(a[3], ",")[[1]] )
  write = "write" %in% a[-(1:3)]
  rest  = setdiff( a[-(1:3)], "write" )
  jt    = if ( length(rest) == 0 || rest[1] == "NA" ) NA else rest[1]
  split_resubmit( a[1], jobs, as.integer(a[2]), new.jobtime = jt, write = write )
}
