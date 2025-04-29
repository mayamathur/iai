
# PRELIMINARIES -----------------------------------------

path = "/home/groups/manishad/IAI"
setwd(path)
source("helper_IAI.R")

allPackages = c("here",
                "crayon",
                "dplyr",
                "foreach",
                "doParallel",
                "data.table",
                "purrr",
                "tidyr",
                "tibble",
                "testthat",
                "Hmisc",
                "stringr")

( packagesNeeded = allPackages[ !( allPackages %in% installed.packages()[,"Package"] ) ] )
if( length(packagesNeeded) > 0 ) install.packages(packagesNeeded)

# load all packages
lapply( allPackages,
        require,
        character.only = TRUE)

#**you need to see all "TRUE" printed by this in order for the package to actually be loaded



# SET SIMULATION PARAMETERS -----------------------------------------


# debugging
# full set with continuous outcome
scen.params = tidyr::expand_grid(
  
  rep.methods = "gold ; IPW-nm",
  
  model = "OLS",  # FOR CONTINUOUS OUTCOME
  
  coef_of_interest = "A",
  N = c(10000),
  
  # MICE parameters
  # as on cluster
  imp_m = 50,  
  imp_maxit = 100,
  mice_method = NA,
  
  dag_name = "14A" )


# # full set with continuous outcome
# scen.params = tidyr::expand_grid(
# 
#   rep.methods = "gold ; CC ; MICE-std ; genloc ; IPW-nm",
#   
#   model = "OLS",  # FOR CONTINUOUS OUTCOME
#   
#   coef_of_interest = "A",
#   N = c(10000),
#   
#   # MICE parameters
#   # as on cluster
#   imp_m = 50,  
#   imp_maxit = 100,
#   mice_method = NA,
#   
#   # full set with continuous outcome
#   dag_name = c("1A", "1B", "2A",
#                "3A", "3B",
#                "3C", "3D", "3E",
#                "4A", "6A", "7A",
#                "7B",
#                "12A", "12B", "12C",
#                "13A", "13B")
#   
# )

# #### full set with binary outcome
# scen.params = tidyr::expand_grid(
#   
#   rep.methods = "gold ; CC ; MICE-std ; genloc ; IPW-nm",
#   
#   model = "logistic", # FOR BINARY OUTCOME
#   
#   coef_of_interest = "A",
#   N = c(10000),
#   
#   # MICE parameters
#   # as on cluster
#   imp_m = 50,  
#   imp_maxit = 100,
#   mice_method = NA,
# 
#   # full set with binary outcome
#   dag_name = c("1A-bin", "1B-bin", "2A-bin",
#                "3A-bin", "3B-bin",
#                "3C-bin", "3D-bin", "3E-bin",
#                "4A-bin", "6A-bin", "7A-bin",
#                "7B-bin",
#                "12A-bin", "12B-bin", "12C-bin",
#                "13A-bin", "13B-bin")
#   
# )

# # remove combos that aren't implemented
# scen.params = scen.params %>% filter( !(dag_name %in% c("1G", "1H", "1F", "1J") &
#                                           coef_of_interest == "(Intercept)") )


# add scen numbers
start.at = 1
scen.params = scen.params %>% add_column( scen = start.at : ( nrow(scen.params) + (start.at - 1) ),
                                          .before = 1 )


( n.scen = nrow(scen.params) )
# look at it
head( as.data.frame(scen.params) )

# write the csv file of params (to Sherlock)
setwd(path)
write.csv( scen.params, "scen_params.csv", row.names = FALSE )


########################### GENERATE SBATCHES ###########################

# load functions for generating sbatch files
source("helper_IAI.R")

# number of sbatches to generate (i.e., iterations within each scenario)
n.reps.per.scen = 500
n.reps.in.doParallel = 10
#n.reps.per.scen = 10
# n.reps.in.doParallel = 1
( n.files = ( n.reps.per.scen / n.reps.in.doParallel ) * n.scen )


path = "/home/groups/manishad/IAI"

scen.name = rep( scen.params$scen, each = ( n.files / n.scen ) )
jobname = paste("job", 1:n.files, sep="_")
outfile = paste("/home/groups/manishad/IAI/rmfiles/rm_", 1:n.files, ".out", sep="")
errorfile = paste("/home/groups/manishad/IAI/rmfiles/rm_", 1:n.files, ".err", sep="")
write_path = paste(path, "/sbatch_files/", 1:n.files, ".sbatch", sep="")
runfile_path = paste(path, "/testRunFile.R", sep="")

sbatch_params <- data.frame(jobname,
                            outfile,
                            errorfile,
                            #jobtime = "01:00:00",  # with IPW-nm
                            jobtime = "00:30:00",  # with only MICE
                            #jobtime = "00:10:00",
                            quality = "normal",
                            node_number = 1,
                            mem_per_node = 64000,
                            mailtype =  "NONE",
                            user_email = "mmathur@stanford.edu",
                            tasks_per_node = 16,
                            cpus_per_task = 1,
                            path_to_r_script = paste(path, "/doParallel_IAI.R", sep=""),
                            args_to_r_script = paste("--args", jobname, scen.name, sep=" "),
                            write_path,
                            stringsAsFactors = F,
                            server_sbatch_path = NA)

generateSbatch(sbatch_params, runfile_path)

n.files

# run just the first one
# sbatch -p qsu,owners,normal /home/groups/manishad/IAI/sbatch_files/1.sbatch

path = "/home/groups/manishad/IAI"
setwd( paste(path, "/sbatch_files", sep="") )
for (i in 1:50) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/IAI/sbatch_files/", i, ".sbatch", sep="") )
}





######## If Running Only Some Jobs To Fill Gaps ########

# run in Sherlock ml load R
path = "/home/groups/manishad/IAI"
setwd(path)
source("helper_IAI.R")

missed.nums = sbatch_not_run( "/home/groups/manishad/IAI/long_results",
                              "/home/groups/manishad/IAI/long_results",
                              .name.prefix = "long_results",
                              .max.sbatch.num = 850 )



setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/IAI/sbatch_files/", i, ".sbatch", sep="") )
}