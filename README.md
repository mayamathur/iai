## Overview
 
This repository contains all code required to reproduce the applied example and simulation studies reported in:

*Mathur MB, Zhang W, Shpitser I (under review). Imputation without nightMARs: Graphical criteria for valid imputation of missing data. Preprint available at [https://osf.io/preprints/osf/zqne9](https://osf.io/preprints/osf/zqne9).*

## How to reproduce the applied examples

The dataset on sexual identity can be accessed after approved ethics application to the [Steering Committee of the Stockholm Public Health Cohort](https://www.ces.regionstockholm.se/projekt-och-uppdrag/halsa-stockholm/SPHC-data). Code to reproduce this example is [here]([https://github.com/mayamathur/iwn/tree/main/applied_example_sexual_identity](https://github.com/willizhang/incomplete-auxiliary-variable-in-imputation/tree/main).

## How to re-run the simulation study from scratch

Simulation scripts are parallelized and were run on a SLURM cluster.

The [key scripts]([https://github.com/mayamathur/iwn/tree/main/simulation_study](https://github.com/mayamathur/iai/tree/main/simulation_study) are:

- `helper_IAI.R` and `helper_IAI_Wblock.R` contain helper functions that can be run locally. This is the file to consult if you have questions about the various custom functions called by `doParallel_IAI.R` below. 

- `doParallel_IAI.R` runs a parallelized simulation study. Specifically, it runs `sim.reps` simulation reps on each compute node. 

- `genSbatch_IAI.R` automatically generates the sbatch files based on user-specified simulation parameters (from which `genSbatch_IAI.R` writes a spreadsheet of scenario parameters, `scen_params.csv`, to the cluster environment; this file is called later by `doParallel_IAI.R`). This script is specialized for our cluster and would likely need to be rewritten for other computing systems. 

- `stitch_on_sherlock_IAI.R` takes the output files written by `doParallel_IAI.R` and aggregates them into the summary metrics presented in the paper. Writes the data files `stitched.csv` (iterate-level data) and `agg.csv` (scenario-level data).

