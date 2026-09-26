## Overview

This repository contains all code required to reproduce the applied example and simulation studies reported in:

*Mathur MB, Seaman S, Zhang W, McGrath S, Shpitser I (under review). Estimating conditional means under missingness-not-at-random with incomplete auxiliary variables. [Preprint link.](https://www.researchgate.net/publication/401123077_Estimating_conditional_means_under_missingness-not-at-random_with_incomplete_auxiliary_variables?channel=doi&linkId=699ccbc17247bc6473e365d5&showFulltext=true.)*

## Applied example

The dataset on sexual identity can be accessed after an approved ethics application to the [Steering Committee of the Stockholm Public Health Cohort](https://www.ces.regionstockholm.se/projekt-och-uppdrag/halsa-stockholm/SPHC-data). Code to reproduce this example is [here](https://github.com/willizhang/incomplete-auxiliary-variable-in-imputation/tree/main).

## Simulation study

### What the code reproduces

The simulation study has no separate analysis stage. The results reported in the paper and supplement are scenario-level summaries of estimator performance (bias, RMSE, 95% CI coverage, and CI width), computed by the aggregation in `stitch_IAI.R` and written to `agg.csv`:

| Paper results | Study label in code | Output file |
|---|---|---|
| Simulation Studies 1 and 2 | `study12` | `results/study12/stitched/agg.csv` |
| Simulation Study 3 | `study3` | `results/study3/stitched/agg.csv` |

The per-job results files written by the cluster (one row per simulation rep and method) are very large, so they were not saved locally and are not included here. Rerunning the pipeline below regenerates them, along with `stitched.csv` and `agg.csv`.

### Directory structure

All code is in `simulation_study/`; scripts must be run with that directory as the working directory. Results are written to `simulation_study/results/<study>/`, which the scripts create.

| File | Purpose |
|---|---|
| `config_IAI.R` | All user-adjustable settings: scenario grids for each study, number of reps, random seed, directory layout, cluster resources, and R packages. |
| `helper_IAI.R` | Data-generating mechanisms for each DAG (`sim_data()`), the benchmark, complete-case, and IPW-nm estimators, and cluster utilities. |
| `helper_IAI_Wblock.R` | Generation and calibration of the high-dimensional auxiliary block W. |
| `sim_one_rep_IAI.R` | `sim_one_rep()`: simulates one dataset and applies every estimation method, including the MIA estimators. |
| `genSbatch_IAI.R` | Step 1: writes the scenario grid and one SLURM sbatch file per job, and optionally submits them. |
| `doParallel_IAI.R` | Step 2: run by each sbatch job; runs a batch of reps of one scenario in parallel. |
| `stitch_IAI.R` | Step 3: combines per-job results and computes the performance metrics reported in the paper. |
| `run_all_IAI.sh` | Master script that runs the steps above in order. |
| `run_one_scenario_local_IAI.R` | Standalone script to run a few reps of one scenario on a personal computer. |
| `record_package_versions_IAI.R` | Records the R, package, and JAGS versions of the computing environment. |

In the code, the variables X1, X2, W1, and Y of the paper are named C, A, D (or W01), and B, respectively; `helper_IAI.R` lists the correspondence between DAG labels in the code and in the paper. Estimation methods are labeled `gold` (benchmark analysis of the full data before missingness), `CC` (complete-case analysis), `IPW-nm` (inverse-probability weighting under a no-self-censoring model), `mia-pkg-ice` (MIA plug-in estimator), and `mia-tmle` (MIA targeted maximum likelihood estimator).

### Software

The simulations were run in R 4.3.2 on Stanford's Sherlock cluster (SLURM). Required R packages are dplyr, tidyr, tibble, data.table, foreach, doParallel, doRNG, MASS, R2jags, boot, miapack, and tmle; IPW-nm also requires [JAGS](https://mcmc-jags.sourceforge.io/). The miapack package is installed from GitHub (`remotes::install_github("stmcg/miapack")`). The exact versions used, including the miapack commit, are listed in `simulation_study/package_versions.csv` and `simulation_study/session_info.txt`, which `record_package_versions_IAI.R` writes.

### How to rerun the simulation study

The full study is computationally intensive: each scenario has 1,000 reps, and the MIA plug-in estimator uses 1,000 bootstrap reps per dataset for its CIs. Studies 1–2 comprise 3,640 sbatch jobs and Study 3 comprises 928, each using 16 cores for up to 2 hours (W of dimension 1) or 8 hours (W of dimension 10). `genSbatch_IAI.R` is specific to SLURM; cluster settings (partition, modules, memory, and wall time) are in `config_IAI.R` and would need to be adapted for other systems.

From `simulation_study/` on the cluster:

1. `bash run_all_IAI.sh submit all` records package versions, then writes and submits the sbatch jobs for both studies. For each study, this writes `results/<study>/scen_params.csv` (the scenario grid) and `results/<study>/sbatch_files/`. Each job writes one file to `results/<study>/long_results/` and its SLURM logs to `results/<study>/logs/`.
2. After all jobs have finished, `bash run_all_IAI.sh stitch all` writes `results/<study>/stitched/stitched.csv` (one row per scenario, rep, and method) and `results/<study>/stitched/agg.csv` (one row per scenario and method; the results reported in the paper). It also lists any jobs that did not write results in `results/<study>/missed_job_nums.csv`; these can be rerun with `Rscript genSbatch_IAI.R <study> resubmit_missed`, after which step 2 should be repeated.

Each step can also be run for a single study, e.g., `bash run_all_IAI.sh submit study3`.

### Random seeds

Each sbatch job uses a seed determined by its study and job number (`job_seed()` in `config_IAI.R`), and doRNG gives each rep within a job an independent random-number stream, so results do not depend on the number of cores. The seed is recorded in each results row (`job.seed`).

### Running one scenario locally

`run_one_scenario_local_IAI.R` runs a few reps of one scenario sequentially, without a cluster, and prints bias and coverage by method. It uses the same data-generating mechanisms and estimators as the full study. Choose the study, scenario, and number of reps in the settings at the top of the script, then run `Rscript run_one_scenario_local_IAI.R` from `simulation_study/` (equivalently, `bash run_all_IAI.sh test`). By default it skips the bootstrap CIs for the MIA plug-in estimator, which dominate run time; set `fast.mode = FALSE` to include them.
