#!/bin/bash
# THROTTLED SUBMISSION OF SBATCH JOBS
#
# Submits a study's sbatch files a batch at a time, keeping at most MAX_QUEUED of your
# jobs in the queue (pending + running). Every SLEEP_MIN minutes it checks the queue
# and tops it up, until every job from FIRST to LAST has been submitted.
#
# Run from /home/groups/manishad/IAI, after writing the sbatch files with
#   Rscript genSbatch_IAI.R <study>        (no "submit")
#
# Usage:
#   bash submit_throttled_IAI.sh <study> [FIRST] [LAST] [MAX_QUEUED] [SLEEP_MIN]
#   defaults: FIRST = 1, LAST = number of sbatch files, MAX_QUEUED = 1000, SLEEP_MIN = 10
#
# Progress is saved to results/<study>/submit_progress.txt, so if this script is
# stopped (or its own job times out), rerunning the same command resumes where it
# left off. Jobs whose results file already exists are skipped, as are jobs retired
# by split_resubmit_IAI.R (listed in results/<study>/retired_jobs.csv), whose
# replacements have their own sbatch files.

set -uo pipefail

STUDY=${1:?Usage: bash submit_throttled_IAI.sh <study> [FIRST] [LAST] [MAX_QUEUED] [SLEEP_MIN]}
SBATCH_DIR="results/$STUDY/sbatch_files"
RESULTS_DIR="results/$STUDY/long_results"
PROGRESS="results/$STUDY/submit_progress.txt"
PARTITION="qsu,owners,normal"

# old job numbers of jobs retired by split_resubmit_IAI.R (column 1, after the header)
RETIRED=" "
if [ -f "results/$STUDY/retired_jobs.csv" ]; then
  RETIRED=" $(tail -n +2 "results/$STUDY/retired_jobs.csv" | cut -d, -f1 | tr -d '"' | sort -u | tr '\n' ' ') "
fi

N_FILES=$(ls "$SBATCH_DIR"/*.sbatch 2>/dev/null | wc -l)
if [ "$N_FILES" -eq 0 ]; then echo "No sbatch files in $SBATCH_DIR"; exit 1; fi

FIRST=${2:-1}
LAST=${3:-$N_FILES}
MAX_QUEUED=${4:-1000}
SLEEP_MIN=${5:-10}

# resume from saved progress if it exists
NEXT=$FIRST
if [ -f "$PROGRESS" ]; then
  SAVED=$(cat "$PROGRESS")
  if [ "$SAVED" -gt "$NEXT" ]; then NEXT=$SAVED; fi
  echo "$(date '+%F %T')  Resuming from job $NEXT (saved in $PROGRESS)"
fi

echo "$(date '+%F %T')  Study $STUDY: submitting jobs $NEXT-$LAST, at most $MAX_QUEUED queued, checking every $SLEEP_MIN min"

while [ "$NEXT" -le "$LAST" ]; do
  
  QUEUED=$(squeue -u "$USER" -h | wc -l)
  ROOM=$(( MAX_QUEUED - QUEUED ))
  SUBMITTED=0
  
  while [ "$ROOM" -gt 0 ] && [ "$NEXT" -le "$LAST" ]; do
    f="$SBATCH_DIR/$NEXT.sbatch"
    if [ -f "$f" ] && [ ! -f "$RESULTS_DIR/long_results_job_${NEXT}_.csv" ] && [[ "$RETIRED" != *" $NEXT "* ]]; then
      if sbatch -p "$PARTITION" "$f" > /dev/null; then
        ROOM=$(( ROOM - 1 )); SUBMITTED=$(( SUBMITTED + 1 ))
      else
        # submission refused (e.g., a per-user limit): stop and retry at the next check
        echo "$(date '+%F %T')  sbatch refused job $NEXT; will retry later"
        break
      fi
    fi
    NEXT=$(( NEXT + 1 ))
    echo "$NEXT" > "$PROGRESS"
  done
  
  echo "$(date '+%F %T')  queued before: $QUEUED; submitted: $SUBMITTED; next job: $NEXT of $LAST"
  
  if [ "$NEXT" -le "$LAST" ]; then sleep $(( SLEEP_MIN * 60 )); fi
done

echo "$(date '+%F %T')  All jobs through $LAST submitted."
rm -f "$PROGRESS"
