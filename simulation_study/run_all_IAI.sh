#!/bin/bash
# MASTER SCRIPT FOR THE SIMULATION STUDY
#
# Run from the simulation_study directory. Because cluster jobs run asynchronously,
# the pipeline has two stages per study: submit the jobs, then (once they have all
# finished) stitch the results.
#
#   bash run_all_IAI.sh test              # quick local check (settings in run_one_scenario_local_IAI.R)
#   bash run_all_IAI.sh submit  <study>   # on the cluster: write and submit all sbatch jobs
#   bash run_all_IAI.sh stitch  <study>   # on the cluster, after jobs finish: stitch and aggregate
#
# <study> is study12 (paper's Studies 1-2) or study3 (Study 3), or "all".

set -euo pipefail
cd "$(dirname "$0")"

step=${1:-}
study=${2:-all}
if [ "$study" = "all" ]; then studies="study12 study3"; else studies="$study"; fi

case "$step" in
  test)
    Rscript run_one_scenario_local_IAI.R
    ;;
  submit)
    Rscript record_package_versions_IAI.R
    for s in $studies; do
      Rscript genSbatch_IAI.R "$s" submit
    done
    ;;
  stitch)
    for s in $studies; do
      Rscript stitch_IAI.R "$s"
    done
    ;;
  *)
    echo "Usage: bash run_all_IAI.sh {test | submit <study> | stitch <study>}"
    exit 1
    ;;
esac
