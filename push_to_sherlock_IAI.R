



####################### CHECK IN ####################### 
# see the sbatches
cd /home/groups/manishad/IAI/sbatch_files

sbatch -p qsu,owners,normal /home/groups/manishad/IAI/sbatch_files/1.sbatch

# check on my running or pending jobs
squeue -u mmathur -t RUNNING
squeue -u mmathur -t PENDING

# /home/groups/manishad/IAI/sbatch_files/1.sbatch


# see the datasets
vim /home/groups/manishad/IAI/sim_results/long/long_results_job_1_.csv
cd /home/groups/manishad/IAI/sim_results/long
ls -l . | egrep -c '^-'

###### See the Errors #####
vim /home/groups/manishad/IAI/sbatch_files/rm_1.err

# see the scen parameters
nano /home/groups/manishad/IAI/scen_params.csv

# see the stitched results
nano /home/groups/manishad/IAI/sim_results/overall_stitched/sti*
  
  
  
# CODE -> SHERLOCK ----------------------------------

# push all Sherlock code
scp /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/\*Inchoate/2024/2024-10-20\ -\ IAI\ \(Incomplete\ auxiliaries\ in\ imputation\)/Simulation\ study/Code/* mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IAI



# SHERLOCK -> DESKTOP ----------------------------------

scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IAI/stitched_results/stitched.csv ~/Desktop


scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IAI/* ~/Desktop


####################### SHERLOCK -> DESKTOP (DEBUGGING) ####################### 

# move error file to Desktop
scp -r mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IAI/sbatch_files/rm_1250.err ~/Desktop

# move one sbatch file to Desktop
scp -r mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IAI/sbatch_files/2296.sbatch ~/Desktop


####################### RUN SBATCH ####################### 

# run one of them
sbatch -p qsu,normal,owners /home/groups/manishad/IAI/sbatch_files/1.sbatch




####################### RESULTS -> DESKTOP FOR ANALYSIS ####################### 

scp mmathur@login.sherlock.stanford.edu /home/groups/manishad/IAI/results/overall_stitched/stitched.csv ~/Desktop


####################### CLEAN UP ####################### 


  # DELETE ALL LONG RESULTS
  mkdir /home/groups/manishad/IAI/empty_dir
rsync -a --delete /home/groups/manishad/IAI/empty_dir/ /home/groups/manishad/IAI/long_results/
  
  # delete stitched results
  rm /home/groups/manishad/IAI/overall_stitched/*
  rm /home/groups/manishad/IAI/stitched_results/*
  
  # delete "rm" files
  rm /home/users/mmathur/rm_*
  rm /home/groups/manishad/IAI/rmfiles/rm_*
  
  
  # DELETE ALL SBATCHES
  # https://unix.stackexchange.com/questions/37329/efficiently-delete-large-directory-containing-thousands-of-files
  # https://superuser.com/questions/156664/what-are-the-differences-between-the-rsync-delete-options
  mkdir /home/groups/manishad/IAI/empty_dir
rsync -a --delete /home/groups/manishad/IAI/empty_dir/ /home/groups/manishad/IAI/sbatch_files/
  
  
  