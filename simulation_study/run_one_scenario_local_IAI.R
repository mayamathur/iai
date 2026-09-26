# RUN ONE SCENARIO LOCALLY ----------------------------------------------------------
#
# A standalone way to run a few simulation reps of one scenario on a personal
# computer, without a cluster. It uses the same data-generating mechanisms and
# estimation methods as the full simulation study (via sim_one_rep_IAI.R), but runs
# the reps sequentially. Useful for checking the installation or exploring a scenario.
#
# Usage: edit the settings below, set the working directory to simulation_study, and
# run the script (e.g., `Rscript run_one_scenario_local_IAI.R`). It prints a summary
# by method and writes all results to results/local_test/.


# SETTINGS -------------------------------------------------------------------------

study  = "study12"  # "study12" (paper's Studies 1-2) or "study3" (Study 3)
scen   = 15         # row of the study's scenario grid; see make_scen_params() in config_IAI.R
n.reps = 5         # number of simulation reps
seed   = 1

# If TRUE, skips the 1000 bootstrap reps used for mia-pkg-ice CIs, which dominate
#  the run time. Point estimates are unaffected, but mia-pkg-ice then has no CIs.
fast.mode = TRUE


# PRELIMINARIES --------------------------------------------------------------------

source("config_IAI.R")
load_sim_packages()
select = dplyr::select
source("helper_IAI.R")
source("sim_one_rep_IAI.R")

scen.params = make_scen_params(study)
p = scen.params[ scen.params$scen == scen, names(scen.params) != "scen" ]
if ( fast.mode ) p$boot_reps_mia_ice = 0

# Any scenario parameter can also be changed here, e.g., p$N = 2000

cat("\nSCENARIO PARAMETERS:\n")
print( as.data.frame(p) )


# RUN SIMULATION REPS --------------------------------------------------------------

set.seed(seed)
rs = bind_rows( lapply( 1:n.reps, function(i) {
  cat("\n\n~~~~~~~~ Sim rep", i, "of", n.reps, "~~~~~~~~\n")
  sim_one_rep(p, verbose = FALSE) %>% add_column( rep.name = i, .before = 1 )
} ) )


# SUMMARIZE AND SAVE ---------------------------------------------------------------

# for DAGs whose true beta is not available analytically, use the benchmark mean
if ( all( is.na(rs$beta) ) ) rs$beta = meanNA( rs$bhat[ rs$method == "gold" ] )

summary = rs %>%
  group_by(method) %>%
  summarise( reps      = n(),
             PropNA    = mean( is.na(bhat) ),
             Bhat      = meanNA(bhat),
             BhatBias  = meanNA(bhat - beta),
             BhatCover = meanNA( covers(truth = beta, lo = bhat_lo, hi = bhat_hi) ),
             .groups = "drop" ) %>%
  mutate_if( is.numeric, function(x) round(x, 2) )
print( as.data.frame(summary) )

out.dir = file.path(root.dir, "results", "local_test")
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
fwrite( rs, file.path(out.dir, paste0(study, "_scen", scen, ".csv")) )
