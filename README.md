## CURRENTLY VERBATIM FROM IWN; NEEDS UPDATING.

## Overview
 
This repository contains all code required to reproduce the applied example and simulation studies reported in:

*Mathur MB, Zhang W, Shpitser I (under review). Imputation without nightMARs: Graphical criteria for valid imputation of missing data. Preprint available at [https://osf.io/preprints/osf/zqne9](https://osf.io/preprints/osf/zqne9).*

## How to reproduce the applied examples

For the main-text applied example on sexual identity, the dataset can be accessed after approved ethics application to the [Steering Committee of the Stockholm Public Health Cohort](https://www.ces.regionstockholm.se/projekt-och-uppdrag/halsa-stockholm/SPHC-data). Code to reproduce this example is [here](https://github.com/mayamathur/iwn/tree/main/applied_example_sexual_identity).

For the supplementary applied example on high schoolers, the dataset is [publicly available](https://www.icpsr.umich.edu/web/NADAC/studies/36423) through the Inter-University Consortium for Political and Social Research (ICPSR). Due to their terms of use, the dataset cannot be posted in this repository. Code to reproduce this example is [here](https://github.com/mayamathur/iwn/tree/main/applied_example_supplement). The files use the `renv` package for reproducibility; as noted in the comments of the code files, you can uncomment the calls `renv::snapshot()` if you want to reproduce our software environment. Open the R project `Code/Code.Rproj` and run the scripts as follows:

- `prep_applied_IWN.R` loads the ICPSR data, draws a random sample of 5,000, and preps variables for analysis. The prepped data are written to a separate folder (not publicly available per ICPSR's terms of use.).
- `analyze_applied_IWN.R` conducts all analyses by calling the helper code in `helper_applied_IWN.R`.


## How to re-run the simulation study from scratch

Simulation scripts are parallelized and were run on a SLURM cluster.

The [key scripts](https://github.com/mayamathur/iwn/tree/main/simulation_study) are:

- `helper_IWN.R` and contains helper functions that can be run locally. This is the file to consult if you have questions about the various custom functions called by `doParallel_IWN.R` below. 

- `doParallel_IWN.R` runs a parallelized simulation study. Specifically, it runs `sim.reps` simulation reps on each compute node. 

- `genSbatch_IWN.R` automatically generates the sbatch files based on user-specified simulation parameters (from which `genSbatch_IWN.R` writes a spreadsheet of scenario parameters, `scen_params.csv`, to the cluster environment; this file is called later by `doParallel_IWN.R`). This script is specialized for our cluster and would likely need to be rewritten for other computing systems. 

- `stitch_on_sherlock_IWN.R` takes the output files written by `doParallel_IWN.R`. This was run on the cluster. Writes the data files `stitched.csv` (iterate-level data) and `agg.csv` (scenario-level data) to the Results directory (not committed due to massive size).

- `prep_sims_IWN.R` takes `agg.csv` described above and does variable manipulations to facilitate analysis. 

- `analyze_sims_IWN.R` generates all analyses, results tables, and plots. Each table is printed in LaTeX code that was copied straight into the manuscript. This script calls helper functions in `analyze_sims_helper_IWN.R` and works with the dataset `agg.csv` described above. 

