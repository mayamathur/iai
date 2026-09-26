# RUN ONE CLUSTER JOB OF THE SIMULATION STUDY ---------------------------------------
#
# Step 2 of the simulation pipeline (see README). Runs a given number of simulation
# reps for one scenario, in parallel across cores, and writes one results file with
# one row per rep and estimation method. The work for each rep is done by
# sim_one_rep() in sim_one_rep_IAI.R.
#
# This script is called by the sbatch files that genSbatch_IAI.R writes:
#   R -f doParallel_IAI.R --args <study> <jobname> <scen> <n.reps>
# and writes results/<study>/long_results/long_results_<jobname>_.csv.
# To run a scenario on a personal computer instead, use run_one_scenario_local_IAI.R.


# PRELIMINARIES --------------------------------------------------------------------

source("config_IAI.R")
load_sim_packages( c(sim.packages, "foreach", "doParallel", "doRNG") )
select = dplyr::select
source("helper_IAI.R")
source("sim_one_rep_IAI.R")

args = commandArgs(trailingOnly = TRUE)
cat("\n\nArguments received:", args)

study   = args[1]
jobname = args[2]
scen    = as.integer(args[3])
n.reps  = as.integer(args[4])
check_study(study)

d = study_dirs(study)
scen.params = read.csv(d$scen.params)
if ( !scen %in% scen.params$scen ) stop("Scenario ", scen, " is not in ", d$scen.params)
p = scen.params[ scen.params$scen == scen, names(scen.params) != "scen" ]

cat("\n\nSCENARIO PARAMETERS:\n")
print( as.data.frame(p) )

job.seed = job_seed( study, as.integer( sub("job_", "", jobname) ) )
out.file = file.path( d$long.results, paste( "long_results", jobname, ".csv", sep = "_" ) )

registerDoParallel(cores = cluster$cores)


# RUN SIMULATION REPS --------------------------------------------------------------

# %dorng% gives each rep an independent random-number stream determined by job.seed,
#  so results are reproducible regardless of the number of cores.
doParallel.seconds = system.time({
  rs = foreach( i = 1:n.reps, .combine = bind_rows, .options.RNG = job.seed ) %dorng% {
    sim_one_rep(p, verbose = TRUE) %>%
      add_column( job.name = jobname, study = study, job.seed = job.seed,
                  scen.name = scen, rep.name = i, .before = 1 )
  }
})


# WRITE RESULTS --------------------------------------------------------------------

rs$doParallel.seconds = doParallel.seconds[["elapsed"]]
fwrite(rs, out.file)
cat("\n\nWrote results to", out.file, "\n")
