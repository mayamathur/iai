

path = "/home/groups/manishad/IAI"
setwd(path)
source("stitch_on_sherlock_helper.R")

#stitch()



# TO RUN STITCH() LINE-BY-LINE: ----------------------------------------------

path = "/home/groups/manishad/IAI"
setwd(path)
source("helper_IAI.R")


# PRELIMINARIES ----------------------------------------------

library(data.table)
library(dplyr)
library(testthat)
library(xlsx)
library(xtable)


.results.singles.path = "/home/groups/manishad/IAI/long_results"
.results.stitched.write.path = "/home/groups/manishad/IAI/overall_stitched"
.name.prefix = "long_results_job"
.stitch.file.name="stitched.csv"


# MAKE STITCHED DATA ----------------------------------------------

# get list of all files in folder
all.files = list.files(.results.singles.path, full.names=TRUE)

# we only want the ones whose name includes .name.prefix
keepers = all.files[ grep( .name.prefix, all.files ) ]
length(keepers)

# grab variable names from first file
names = names( read.csv(keepers[1]) )

# temp = lapply(keepers, function(x){
#   dat = read.csv(x, header = TRUE)
#   "dag_name" %in% names(dat) } )
# which(temp == FALSE)

# read in and rbind the keepers
tables <- lapply(keepers, function(x) read.csv(x, header = TRUE, colClasses = c(dag_name = "character")))

# sanity check: do all files have the same names?
# if not, could be because some jobs were killed early so didn't get doParallelTime
#  variable added at the end
#  can be verified by looking at out-file for a job without name "doParallelTime"
allNames = lapply( tables, names )
# # find out which jobs had wrong number of names
# lapply( allNames, function(x) all.equal(x, names ) )
# allNames[[1]][ !allNames[[1]] %in% allNames[[111]] ]

# bind_rows works even if datasets have different names
#  will fill in NAs
s <- do.call(bind_rows, tables)

names(s) = names( read.csv(keepers[1], header= TRUE) )

if( is.na(s[1,1]) ) s = s[-1,]  # delete annoying NA row
# write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )

cat("\n\n nrow(s) =", nrow(s))
cat("\n nuni(s$scen.name) =", nuni(s$scen.name) )

# debugging help:
# if there's only 1 unique scen, you might have set
#  interactive.cluster.run = TRUE in doParallel


# ~ Check for Bad Column Names ---------------------------

# not sure why this is needed - has NA columns at end
names(s)
any(is.na(names(s)))

if ( any(is.na(names(s))) ) {
  NA.names = which( is.na(names(s) ) )
  s = s[ , -NA.names ]
  
}

s = s %>% filter(!is.na(scen.name))

table(s$dag_name)


# Quick Look ----------------------------------------------

# sanity check
table(s$dag_name, s$coef_of_interest)

correct.order = c("gold", "CC", "MICE-std", "Am-std", "genloc", "IPW-custom", "IPW-nm",
                  "af4-np", "af4-sp",
                  "g-form", "custom")
s$method = factor(s$method, levels = correct.order)

# fill in beta (where it's NA) using gold-standard
# s = data.frame( scen.name = c(1,1,1,1,2,2,2,2),
#                 beta = c( rep(NA,4), rep(1,4)),
#                 method = rep( c("gold", "gold", "other", "other"), 2),
#                 bhat = rep( c(2,3,0,1), 2 ) )  # test

beta_emp = s %>% filter(method == "gold") %>%
  group_by(scen.name) %>%
  summarise(beta = meanNA(bhat)) 
as.data.frame(beta_emp)

# same for intercept
int_emp = s %>% filter(method == "gold") %>%
  group_by(scen.name) %>%
  summarise(int = meanNA(inthat)) 
as.data.frame(int_emp)

s2 = s

s2 = s2 %>% rowwise() %>%
  mutate( beta = ifelse( !is.na(beta),
                         beta,
                         beta_emp$beta[ beta_emp$scen.name == scen.name ] ),
          int = int_emp$int[ int_emp$scen.name == scen.name ] )

# sanity check
as.data.frame( s2 %>% group_by(dag_name, coef_of_interest) %>%
                summarise(beta[1]) )
# end of filling in beta and int

# in this case, don't need to group by coef_of_interest because they're all A
t = s2 %>% group_by(dag_name, method) %>%
  summarise( 
    reps = n(),
    PropNA = mean(is.na(bhat)),
    Bhat = meanNA(bhat),
    BhatBias = meanNA(bhat - beta),
    #BhatRelBias = meanNA( (bhat - beta)/beta ),
    #BhatWidth = meanNA(bhat_hi - bhat_lo),
    #BhatRMSE = sqrt( meanNA( (bhat - beta)^2 ) ),
    BhatCover = meanNA( covers(truth = beta,
                               lo = bhat_lo,
                               hi = bhat_hi) ),
    
    IntHat = meanNA(inthat),
    IntBias = meanNA(inthat - int),
    #BhatRelBias = meanNA( (bhat - beta)/beta ),
    #BhatWidth = meanNA(bhat_hi - bhat_lo),
    #BhatRMSE = sqrt( meanNA( (bhat - beta)^2 ) ),
    IntCover = meanNA( covers(truth = int,
                               lo = int_lo,
                               hi = int_hi) ),
    
    sancheck.mean_RB = meanNA(sancheck.mean_RB),
    sancheck.mean_RC = meanNA(sancheck.mean_RC),
    sancheck.prop_complete = meanNA(sancheck.prop_complete),
    ) %>%
  arrange() %>%
  mutate_if(is.numeric, function(x) round(x,2)) 
as.data.frame(t)




# save agg data
path = "/home/groups/manishad/IAI/overall_stitched"
setwd(path)
write.xlsx(as.data.frame(t),
           paste(Sys.Date(), "agg.xlsx") )


# xtable
t2 = t %>% ungroup() %>% select(dag_name, method, Bhat, BhatBias, BhatCover, BhatWidth, BhatRMSE)
print( xtable(t2), include.rownames = FALSE )



# ~ Write stitched.csv ---------------------------

setwd(.results.stitched.write.path)
fwrite(s, .stitch.file.name)

# also make a zipped version
string = paste("zip -m stitched.zip", .stitch.file.name)
system(string)




# MAKE AGG DATA ----------------------------------------------

# path = "/home/groups/manishad/IAI"
# setwd(path)
# source("helper_IAI.R")
# source("analyze_sims_helper_IAI.R")
# 
# # if this says "problem with column OptimConverged", 
# #  you just need to comment out the optim columns in make_agg_data
# #  because you didn't run those methods
# agg = make_agg_data(s)
# 
# setwd(.results.stitched.write.path)
# fwrite(agg, "agg.csv")
# 
# 
# table(agg$method)
# 
# cat("\n\n nrow(agg) =", nrow(agg))
# cat("\n nuni(agg$scen.name) =", nuni(agg$scen.name) )
# 
# # # OPTIONAL: shorter version of agg that's nicer to look at
# # agg.short = agg %>% select(prob.hacked, prob.conf, method, Mhat, MhatBias, MhatCover, MhatWidth,
# #                            #MhatEstFail, MhatCIFail,
# #                            sancheck.dp.k.nonaffirm.unhacked, sancheck.dp.k.nonaffirm.hacked) %>%
# #   filter( !( method %in% c("rtma-adj-pmed", "rtma-adj-pmean") ) ) %>%
# #   mutate_if(is.numeric, function(x) round(x,2))
# #   
# # setwd(.results.stitched.write.path)
# # fwrite(agg.short, "agg_short.csv")
# 
# 
# 
# 
# # look again at failures
# t = agg %>% group_by(method) %>%
#   summarise( mean( is.na(bhat) ) )
# as.data.frame(t)
# 



##### Move to Local #####

# # stitched and agg -> local directory
# scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IAI/overall_stitched/* /Users/mmathur/Dropbox/Personal computer/Independent studies/*Inchoate/2022/IAI/Linked to OSF (IAI)/Simulation study/Data

# scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IAI/overall_stitched/* /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/\*Inchoate/2022/IAI/Linked\ to\ OSF\ \(IAI\)/Simulation\ study/Data




# LOOK FOR MISSED JOBS ----------------------------------------------

path = "/home/groups/manishad/IAI"
setwd(path)
source("helper_IAI.R")
source("analyze_sims_helper_IAI.R")

# look for missed jobs
missed.nums = sbatch_not_run( "/home/groups/manishad/IAI/long_results",
                              "/home/groups/manishad/IAI/long_results",
                              .name.prefix = "long",
                              .max.sbatch.num = 700)

# run any missed jobs
setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/IAI/sbatch_files/", i, ".sbatch", sep="") )
}

