## Overview

This repository contains all code required to reproduce the applied example and simulation studies reported in:

*Mathur MB, Seaman S, Zhang W, McGrath S, Shpitser I (under review). Estimating conditional means under missingness-not-at-random with incomplete auxiliary variables. [Preprint link.](https://www.researchgate.net/publication/401123077_Estimating_conditional_means_under_missingness-not-at-random_with_incomplete_auxiliary_variables?channel=doi&linkId=699ccbc17247bc6473e365d5&showFulltext=true.)*

## Applied example

The dataset on sexual identity can be accessed after an approved ethics application to the [Steering Committee of the Stockholm Public Health Cohort](https://www.ces.regionstockholm.se/projekt-och-uppdrag/halsa-stockholm/SPHC-data). Code to reproduce this example is [here](https://github.com/willizhang/incomplete-auxiliary-variable-in-imputation/tree/main).

## Simulation study

### What the code reproduces

The simulation study has no separate analysis stage. The results reported in the paper and supplement are scenario-level summaries of estimator performance (bias, RMSE, 95% CI coverage, and CI width), computed by the aggregation in `stitch_IAI.R`. The results of each study are provided as a zip file in `simulation_study_results/`:

| Paper results | Study label in code | Results provided | Output file when rerun |
|---|---|---|---|
| Simulation Studies 1 and 2 | `study12` | `simulation_study_results/Studies 1-2.zip` | `results/study12/stitched/agg.csv` |
| Simulation Study 3 | `study3` | `simulation_study_results/Study 3.zip` | `results/study3/stitched/agg.csv` |

Each zip file contains two files:

- `stitched.csv`: one row per scenario, simulation rep, and method (the rep-level results).
- `agg.csv` (named with the study and date, e.g., `2026-09-29 - agg study12.csv`): one row per scenario and method. These are the results reported in the paper and supplement.

`DATA_DICTIONARY.md` defines the variables in both files. The per-job results files written by the cluster are not included, because `stitched.csv` contains all of their rows. Rerunning the pipeline below regenerates the per-job files, `stitched.csv`, and `agg.csv`.

### Directory structure

All code is in `simulation_study/`; scripts must be run with that directory as the working directory. Results are written to `simulation_study/results/<study>/`, which the scripts create.

| File | Purpose |
|---|---|
| `config_IAI.R` | All user-adjustable settings: scenario grids for each study, number of reps, random seed, directory layout, cluster resources, and R packages. |
| `helper_IAI.R` | Data-generating mechanisms for each DAG (`sim_data()`), the benchmark, complete-case, and IPW-nm estimators, and cluster utilities. |
| `helper_IAI_Wblock.R` | Generation and calibration of the high-dimensional auxiliary block W. |
| `sim_one_rep_IAI.R` | `sim_one_rep()`: simulates one dataset and applies every estimation method, including the MIA estimators. |
| `genSbatch_IAI.R` | Step 1: writes the scenario grid and one SLURM sbatch file per job, and optionally submits them. Also reports and resubmits jobs that did not finish. |
| `doParallel_IAI.R` | Step 2: run by each sbatch job; runs a batch of reps of one scenario in parallel. |
| `stitch_IAI.R` | Step 3: combines per-job results and computes the performance metrics reported in the paper. |
| `split_resubmit_IAI.R` | Replaces jobs that repeatedly exceed their wall time with several smaller jobs (see "Jobs that time out"). |
| `run_all_IAI.sh` | Master script that runs the steps above in order. |
| `run_one_scenario_local_IAI.R` | Standalone script to run a few reps of one scenario on a personal computer. |
| `record_package_versions_IAI.R` | Installs any missing packages and records the R, package, and JAGS versions of the computing environment. |
| `DATA_DICTIONARY.md` | Defines the variables in the results files `agg.csv` and `stitched.csv`. |

In the code, the variables X1, X2, W1, and Y of the paper are named C, A, D (or W01), and B, respectively; `helper_IAI.R` lists the correspondence between DAG labels in the code and in the paper. Estimation methods are labeled `gold` (benchmark analysis of the full data before missingness), `CC` (complete-case analysis), `MICE-std` (multiple imputation by chained equations), `IPW-nm` (inverse-probability weighting under a no-self-censoring model), `mia-pkg-ice` (MIA plug-in estimator), and `mia-tmle` (MIA targeted maximum likelihood estimator). The code also implements `Am-std` (multiple imputation under a joint normal model via Amelia), which is not run in the paper's scenarios.

### Software

The simulations were run in R 4.3.2 on Stanford's Sherlock cluster (SLURM), with JAGS 4.3.1 (required by R2jags for IPW-nm; [download](https://mcmc-jags.sourceforge.io/)). Package versions:

| Package | Version |
|---|---|
| dplyr | 1.1.4 |
| tidyr | 1.3.1 |
| tibble | 3.2.1 |
| data.table | 1.15.4 |
| foreach | 1.5.2 |
| doParallel | 1.0.17 |
| doRNG | 1.8.6.3 |
| MASS | 7.3-60.0.1 |
| R2jags | 0.8-9 |
| rjags | 4-17 |
| boot | 1.3-28.1 |
| miapack | 0.2.0 (GitHub commit `06fd66a88e38ab3ae63709e9b6bf58cab7dd7e3a`) |
| tmle | 2.1.1 |
| SuperLearner | 2.0-29 |

These are also recorded in `simulation_study/package_versions.csv`. `load_sim_packages()` in `config_IAI.R` installs any missing packages; miapack is installed from GitHub (`stmcg/miapack`). To install the exact miapack version used, run `remotes::install_github("stmcg/miapack@06fd66a88e38ab3ae63709e9b6bf58cab7dd7e3a")`.

### How to rerun the simulation study

The full study is computationally intensive: each scenario has 1,000 reps, and the MIA plug-in estimator uses 1,000 bootstrap reps per dataset for its CIs. Each sbatch job uses 16 cores. The number of reps per job and the wall-time limit are set by `reps_per_job()` and `jobtime_per_scen()` in `config_IAI.R`; the limits (3–6 hours for W of dimension 1 and 10 hours for W of dimension 10) include extra time for multiple imputation. In the reported results, Studies 1–2 comprised 3,640 sbatch jobs and Study 3 comprised 928, plus the replacement jobs described under "Jobs that time out." After those runs, `reps_per_job()` was changed to allow at most 50 reps per job for scenarios with multiple imputation and N ≥ 2,000, so the current configuration writes 3,864 and 1,056 jobs, respectively. Because each job's seed depends on its job number (see "Random seeds"), a rerun under the current configuration reproduces the reported results up to Monte Carlo error, not digit for digit. `genSbatch_IAI.R` is specific to SLURM; cluster settings (partition, modules, memory, and wall time) are in `config_IAI.R` and would need to be adapted for other systems.

From `simulation_study/` on the cluster:

1. `bash run_all_IAI.sh submit all` records package versions, then writes and submits the sbatch jobs for both studies. For each study, this writes `results/<study>/scen_params.csv` (the scenario grid) and `results/<study>/sbatch_files/`. Each job writes one file to `results/<study>/long_results/` and its SLURM logs to `results/<study>/logs/`.
2. After all jobs have finished, `bash run_all_IAI.sh stitch all` writes `results/<study>/stitched/stitched.csv` (one row per scenario, rep, and method) and `results/<study>/stitched/agg.csv` (one row per scenario and method; the results reported in the paper). `DATA_DICTIONARY.md` defines their variables. It also lists any jobs that did not write results in `results/<study>/missed_job_nums.csv`; see "Jobs that time out" for how to rerun them, after which step 2 should be repeated.

Each step can also be run for a single study, e.g., `bash run_all_IAI.sh submit study3`.

### Jobs that time out

Each job writes its results file only after all of its reps have finished, so a job that exceeds its wall time writes nothing; it cannot leave a partial file that would later be double-counted. Such jobs can be rerun in two ways, which differ in whether the job keeps its number:

1. **Resubmitting with more time.** `Rscript genSbatch_IAI.R <study> resubmit_missed [time.mult]` resubmits every job that has no results file and is not still queued, with its wall time multiplied by `time.mult` (e.g., 2 to double it; capped at `cluster$max_hours`). The multiplier is passed to `sbatch` on the command line, so the sbatch file itself is unchanged. The job keeps its number, and hence its seed, so it produces exactly the results the original job would have. `Rscript genSbatch_IAI.R <study> check_missed` gives the same report without resubmitting anything.
2. **Splitting into smaller jobs.** If a job still times out, `Rscript split_resubmit_IAI.R <study> <reps per job> <job numbers | missed> [HH:MM:SS] [write]` replaces it with several smaller jobs that run the same scenario and, together, the same number of reps. Each new job is numbered after the largest existing job number, so it has its own seed; reusing the old number would give the new jobs identical random-number streams, and hence identical reps. The script uses the old sbatch file as a template, refuses to split a job that is still queued or already has results, and records each old job number and its replacements in `results/<study>/retired_jobs.csv`. Without `write`, it only prints this plan. Retired jobs are never counted as missing or resubmitted by `check_missed` and `resubmit_missed`, and `stitch_IAI.R` needs no changes, since it combines whatever results files exist.

Because of splitting, the job numbers in a study's results need not be consecutive: a retired job has no results file, and its replacements are numbered above the jobs written by `genSbatch_IAI.R`. For the same reason, once a study has any results, `genSbatch_IAI.R` refuses to regenerate its sbatch files, which would renumber every job (changing its seed) and delete any replacement jobs; to start a study over, first run `bash run_all_IAI.sh clean <study>`.

In the reported results, jobs 1617–1618, 1929–1930, and 1932 of `study12` (scenarios 45 and 51, i.e., DAGs 1B and 2B with N = 5,000 and W of dimension 1) repeatedly exceeded their wall time, including after resubmission with triple the time. Each of these 250-rep jobs was split into five jobs of 50 reps, numbered 3641–3665.

### Random seeds

Each sbatch job uses a seed determined by its study and job number (`job_seed()` in `config_IAI.R`), and doRNG gives each rep within a job an independent random-number stream, so results do not depend on the number of cores. The seed is recorded in each results row (`job.seed`).

### Running one scenario locally

`run_one_scenario_local_IAI.R` runs a few reps of one scenario sequentially, without a cluster, and prints bias and coverage by method. It uses the same data-generating mechanisms and estimators as the full study. Choose the study, scenario, and number of reps in the settings at the top of the script, then run `Rscript run_one_scenario_local_IAI.R` from `simulation_study/` (equivalently, `bash run_all_IAI.sh test`). By default it skips the bootstrap CIs for the MIA plug-in estimator, which dominate run time; set `fast.mode = FALSE` to include them.
