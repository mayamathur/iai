
# IMPORTANT NOTES -----------------------------


# for interactive Sherlock:
# path = "/home/groups/manishad/IAI"; setwd(path); source("doParallel_IAI.R")


# because Sherlock 2.0 restores previous workspace
rm( list = ls() )


# are we running locally?
run.local = FALSE
# run.local = TRUE

# should we set scen params interactively on cluster?
# *if you accidently set this to TRUE and run via sbatches on cluster,
#   they will all run the same scenario! 
interactive.cluster.run = FALSE

# should lots of output be printed for each sim rep?
verbose = TRUE

# ~~ Packages -----------------------------------------------
toLoad = c("crayon",
           "dplyr",
           "foreach",
           "doParallel",
           "data.table",
           "purrr",
           "tidyr",
           "tibble",
           "testthat",
           #"Hmisc",
           "stringr",
           "mice",
           "Amelia",
           "R2jags",  # only needed if running method IPW-nm. If removing, you can also remove "module load openblas/0.3.20" and "module load jags/4.3.1" from genSbatch.
           "geepack", # also only for IPW-nm
           "mix", # only for genloc imputation
           "boot", # for parametric AF4
           "MASS")

if ( run.local == TRUE | interactive.cluster.run == TRUE ) toLoad = c(toLoad, "here")


# SET UP FOR CLUSTER OR LOCAL RUN ------------------------------

# ~~ Cluster Run ----------------------------------------
if (run.local == FALSE ) {
  
  # load command line arguments
  args = commandArgs(trailingOnly = TRUE)
  
  cat("\n\n args received from sbatch file:", args)
  
  jobname = args[1]
  scen = args[2]  # this will be a number
  
  # load packages with informative messages if one can't be installed
  # **Common reason to get the "could not library" error: You did ml load R/XXX using an old version
  any.failed = FALSE
  for (pkg in toLoad) {
    
    cat( paste("\nAbout to try loading package", pkg) )
    
    tryCatch({
      # eval below needed because library() will otherwise be confused
      # https://www.mitchelloharawild.com/blog/loading-r-packages-in-a-loop/
      eval( bquote( library( .(pkg) ) ) )
    }, error = function(err) {
      cat( paste("\n*** COULD NOT LOAD PACKAGE:", pkg) )
      any.failed <<- TRUE
    })
    
  }
  if ( any.failed == TRUE ) stop("Some packages couldn't be loaded. See outfile for details of which ones.")
  
  select = dplyr::select
  
  # helper code
  path = "/home/groups/manishad/IAI"
  setwd(path)
  source("helper_IAI.R")
  
  
  # get scen parameters (made by genSbatch.R)
  setwd(path)
  scen.params = read.csv( "scen_params.csv" )
  p <<- scen.params[ scen.params$scen == scen, ]
  
  cat("\n\nHEAD OF ENTIRE SCEN.PARAMS:\n")
  print(p)
  
  
  
  # ~~****** Cluster sim reps ----------------
  # simulation reps to run within this job
  # **this need to match n.reps.in.doParallel in the genSbatch script
  #sim.reps = 10
  sim.reps = 5
  
  # set the number of cores
  registerDoParallel(cores=16)
  
}



# FOR LOCAL USE  ------------------------------
if ( run.local == TRUE ) {
  
  lapply( toLoad,
          require,
          character.only = TRUE)
  
  select = dplyr::select
  
  # helper fns
  code.dir = here()
  setwd(code.dir)
  source("helper_IAI.R")
  
  # for saving intermediate results
  data.dir = str_replace( string = here(),
                          pattern = "Simulation code",
                          replacement = "Simulation results" )
  
  
  # ~~ ********** Set Sim Params: Local Run -----------------------------
  
  scen.params = tidyr::expand_grid(
    
    #rep.methods = "gold ; CC ; MICE-std ; Am-std ; IPW-custom ; af4", 
    #rep.methods = "gold ; CC ; MICE-std ; IPW-nm ; genloc", 
    #rep.methods = "CC ; MICE-std ; genloc ; IPW-nm",
    rep.methods = "gold ; af4-np ; af4-sp",
    
    model = "OLS", 
    #model = "logistic",  # outcome model
    coef_of_interest = "A",
    N = c(10000),
    
    # MICE parameters
    # as on cluster
    imp_m = 5,  # CURRENTLY SET LOW
    imp_maxit = 5,
    mice_method = NA,  # let MICE use its defaults
    
    # AF4 parameters
    boot_reps_af4 = 100,  # only needed for CIs; if set to 0, won't give CIs
    
    dag_name = "1A"
    # dag_name = c("1A", "1B", "1C",
    #              "2A", "2B",
    #              "3A", "3B" )
    
  )
  
  
  # # remove combos that aren't implemented
  # scen.params = scen.params %>% filter( !(dag_name %in% c("1G", "1H", "1F") &
  #                                           coef_of_interest == "(Intercept)") )
  
  start.at = 1  # scen name to start at
  scen.params$scen = start.at:( nrow(scen.params) + start.at - 1 )
  
  # set the number of local cores
  registerDoParallel(cores=8)
  
  scen = 1
  # data.frame(scen.params %>% filter(scen.name == scen))
  
  # just to avoid errors in doParallel script below
  jobname = "job_1"
  i = 1
}



# RUN SIMULATION ------------------------------


# mimic Sherlock structure
if (run.local == TRUE) ( scens_to_run = scen.params$scen )
if (run.local == FALSE) ( scens_to_run = scen )  # from sbatch

if (run.local == TRUE) sim.reps = 1
#  p = scen.params[ scen.params$scen == scen, names(scen.params) != "scen"]


# BEGIN FOR-LOOP to run multiple scens locally
# if running on cluster, scen will just be length 1
for ( scen in scens_to_run ) {
  
  if ( exists("rs") ) rm(rs)
  if ( exists("rep.res") ) rm(rep.res)
  
  # doParallel handles ONE scen at a time
  # system.time is in seconds
  # ~ ********** Beginning of ForEach Loop -----------------------------
  doParallel.seconds = system.time({
    
    rs = foreach( i = 1:sim.reps, .combine = bind_rows ) %dopar% {
      #for debugging (out file will contain all printed things):
      # for ( i in 1:20 ) {
      
      # only print info for first sim rep for visual clarity
      if ( i == 1 ) cat("\n\n~~~~~~~~~~~~~~~~ BEGIN SIM REP", i, "~~~~~~~~~~~~~~~~")
      
      # results for just this simulation rep
      if ( exists("rep.res") ) suppressWarnings( rm(rep.res) )
      
      # extract simulation params for this scenario (row)
      # exclude the column with the scenario name itself (col) 
      if ( verbose == TRUE ) {
        cat("\n\n scen variable:\n")
        print(scen)
        
        cat("\n\n scen.params again:\n")
        print(scen.params)
      }
      
      p = scen.params[ scen.params$scen == scen, names(scen.params) != "scen"]
      coef_of_interest = p$coef_of_interest
      
      # show beginning of dataset
      #if ( i == 1 & verbose == TRUE) cat("\n\nDIM AND HEAD OF P (SINGLE ROW OF SCEN.PARAMS):\n")
      
      # parse methods string
      ( all.methods = unlist( strsplit( x = p$rep.methods,
                                        split = " ; " ) ) )
      
      
      # ~ Simulate Dataset ------------------------------
      
      
      #if ( i == 1 ) cat("\n\nABOUT TO SIMULATE DATA:\n") print( head(du) )
      
      
      sim_obj = sim_data(.p = p)
      
      du = sim_obj$du
      di = sim_obj$di
      ( form_string = as.character( sim_obj$form_string ) )
      ( gold_form_string = as.character( sim_obj$gold_form_string ) )
      ( beta = as.numeric(sim_obj$beta) )
      ( exclude_from_imp_model = as.character( sim_obj$exclude_from_imp_model ) )
      
      
      #if ( i == 1 ) cat("\n\nHEAD OF DU:\n") print( head(du) )
      
      
      # check number of complete cases
      # sum(complete.cases(du))
      
      
      # coefficient of interest for gold-standard model
      if ( coef_of_interest == "(Intercept)" ){
        coef_of_interest_gold = "(Intercept)"
        
      } else if ( coef_of_interest == "A:C" ){
        coef_of_interest_gold = "A1:C1"
      } else {
        # *this assumes coef_of_interest is always the factual variable
        #  (e.g., A), so need to add "1" to use the variable
        # that's in gold-standard model
        coef_of_interest_gold = paste(coef_of_interest, "1", sep = "")
      }
      
      
      
      # ~ Make Imputed Data ------------------------------
      
      
      # ~~ genloc: JM with general location model ----
      
      if ( "genloc" %in% all.methods & !is.null(di) ) {
        
        message("Starting to impute using genloc")
        
        # ensure all binary vars are factors; otherwise using methods
        #  other than pmm will treat them as continuous
        #di = convert_binary_to_factor(di)
        
        # mix needs: "data matrix containing missing values. The rows of x correspond to observational units, and the columns to variables.
        # Missing values are denoted by NA. The categorical variables must be in the first p columns of x,
        # and they must be coded with consecutive positive integers starting with 1.
        # For example, a binary variable must be coded as 1,2 rather than 0,1."
        # accordingly, recode the binaries and reorder columns.
        temp = recode_binaries(di)
        di2 = temp$df
        n_bin = temp$num_binaries
        
        if (n_bin == 0){
          
          message("Can't use genloc with no binaries")
          imps_genloc = NULL
          
        } else if (n_bin == ncol(di2)) {
          
          message("Can't use genloc with no continuous vars")
          imps_genloc = NULL 
          
        } else {
          
          # this block sometimes throws "improper posterior -- empty cells"
          
          tryCatch({
            # randomize the random seed
            rngseed( runif(min = 1000000, max = 9999999, n=1) )
            
            #2025-05-19: this step is failing for new DAG 1B, even though apparently I was able to run genloc for the previous DAG 4B
            di3 <- prelim.mix(di2, p = n_bin)
            
            thetahat <- em.mix(di3)
            
            m <- p$imp_m  
            imps_genloc <- vector("list", m)
            
            for (i in 1:m) {
              newtheta <- da.mix(di3, thetahat, steps = 100)
              newimp = as.data.frame( imp.mix(s = di3, theta = newtheta, x = di2) )
              
              # revert to original coding
              newimp = reverse_recode_binaries(newimp)
              
              imps_genloc[[i]] <- newimp
            }
            
            # sanity check
            imp1 = imps_genloc[[1]]
            
            if ( any(is.na(imp1)) ) {
              message("MI left NAs in dataset - what a butt")
              imps_genloc = NULL
            } 
            
            
          }, error = function(e) {
            message("error making genloc imputations: ", e$message)
            imps_genloc = NULL
            
            # save the problem dataset
            if (run.local == FALSE) {
              
              #@temp debugging
              setwd("/home/groups/manishad/IAI/rmfiles")
              fwrite( rs, paste( "rm_data", jobname, ".csv", sep="_" ) )
              
            }
            
            
          }
          )
          
        }
        
        message("Done imputing using genloc")
        
        
      } else {
        imps_genloc = NULL
      }
      
      
      
      
      
      
      # ~~ MICE-std ----
      # details of how mice() implements pmm:
      # ?mice.impute.pmm
      if ( "MICE-std" %in% all.methods & !is.null(di) ) {
        
        
        message("Starting to impute using MICE-std")
        
        
        # ensure all binary vars are factors; otherwise using methods
        #  other than pmm will treat them as continuous
        di2 = convert_binary_to_factor(di)
        
        # only pass method arg if it's not NA or NULL
        if ( is.na(p$mice_method) | is.null(p$mice_method) ) {
          imps_mice = mice( di2,
                            maxit = p$imp_maxit,
                            m = p$imp_m,
                            printFlag = FALSE )
        } else {
          imps_mice = mice( di2,
                            maxit = p$imp_maxit,
                            m = p$imp_m,
                            
                            # "A vector of length 4 containing the default imputation methods for 1) numeric data, 2) factor data with 2 levels, 3) factor data with > 2 unordered levels, and 4) factor data with > 2 ordered levels. By default, the method uses pmm, predictive mean matching (numeric data) logreg, logistic regression imputation (binary data, factor with 2 levels) polyreg, polytomous regression imputation for unordered categorical data (factor > 2 levels) polr, proportional odds model for (ordered, > 2 levels)."
                            # default for defaultMethod is: c("pmm", "logreg", "polyreg", "polr")
                            #defaultMethod = c("norm", "logreg", "polyreg", "polr"),
                            
                            method = p$mice_method,
                            printFlag = FALSE )
        }
        
        
        
        
        mice_std_methods = summarize_mice_methods(imps_mice$method)
        
        # sanity check
        imp1 = complete(imps_mice, 1)
        
        if ( any(is.na(imp1)) ) {
          message("MI left NAs in dataset - what a butt")
          imps_mice = NULL
        }
        
        message("Done imputing using MICE-std")
        
        
      } else {
        imps_mice = NULL
      }
      
      
      # ~~ Am-std ----
      if ( "Am-std" %in% all.methods & !is.null(di) ) {
        
        imps_am_std = amelia( as.data.frame(di),
                              m=p$imp_m,
                              p2s = 0 # don't print output
        )
        
        
        imp1 = imps_am_std$imputations$imp1
        
        if ( any(is.na(imp1)) ) {
          message("MI left NAs in dataset - what a butt")
          imps_am_std = NULL
        }
        
        
      } else {
        imps_am_std = NULL
      }
      
      
      
      
      # ~ Initialize Global Vars ------------------------------
      
      # initialize rep.res st run_method_safe and other standalone estimation fns
      #  will correctly recognize it as having 0 rows
      rep.res = data.frame()
      
      
      # ~ Fit Models ------------------------------
      
      
      # ~~ Gold standard: No missing data ----
      if ( "gold" %in% all.methods ) {
        
        
        rep.res = run_method_safe(method.label = c("gold"),
                                  
                                  method.fn = function(x) fit_regression(form_string = gold_form_string,
                                                                         model = p$model,
                                                                         # *this assumes coef_of_interest is always the factual variable
                                                                         #  (e.g., A), so need to add "1" to use the variable
                                                                         # that's in gold-standard model
                                                                         coef_of_interest = coef_of_interest_gold,
                                                                         miss_method = "gold",
                                                                         du = du,
                                                                         imps = NULL),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      # ~~ Complete-case analysis (naive) ----
      if ( "CC" %in% all.methods ) {
        
        message("Entered CC part")
        cat(str(di$B))
        cat(table(di$B, useNA = "ifany"))
        
        
        
        rep.res = run_method_safe(method.label = c("CC"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         # *this assumes coef_of_interest is always the factual variable
                                                                         #  (e.g., A), so need to add "1" to use the variable
                                                                         # that's in gold-standard model
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "CC",
                                                                         du = di,
                                                                         imps = NULL),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      
      
      
      
      # ~~ genloc ----
      if ( "genloc" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("genloc"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "MI",
                                                                         du = NULL,
                                                                         imps = imps_genloc),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      
      
      
      # ~~ MICE-std ----
      if ( "MICE-std" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("MICE-std"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "MI",
                                                                         du = NULL,
                                                                         imps = imps_mice),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      # ~~ Am-std ----
      if ( "Am-std" %in% all.methods & !is.null(imps_am_std) ) {
        rep.res = run_method_safe(method.label = c("Am-std"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "MI",
                                                                         du = NULL,
                                                                         imps = imps_am_std),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      
      
      # ~~ Custom imputation ----
      if ( "custom" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("custom"),
                                  
                                  method.fn = function(x) {
                                    
                                    if ( p$dag_name == "1B-bin" ) {
                                      
                                      # initialize single imputed dataset
                                      imp1 = du %>% select(B, C, A)
                                      
                                      ### Imputation step 1: Impute C by subsetting on RC = 1 (not RC = RB = 1)
                                      prob = mean( du$C[du$RC == 1] )
                                      draws = rbinom( n = nrow(imp1),
                                                      size = 1,
                                                      prob = prob )
                                      
                                      imp1$C[ is.na(imp1$C) ] = draws[ is.na(imp1$C) ]
                                      
                                      # mean(imp1$C); mean(du$C1)  # correct, of course
                                      
                                      
                                      ### Imputation step 2: Impute B using A, C and subsetting to complete cases
                                      imp2 = imp1
                                      imp2$B = du$B
                                      
                                      # complete-case dataset
                                      dc = du[ complete.cases(du), ]
                                      
                                      ( m = lm( B ~ A + C, data = dc ) )
                                      # c.f. truth
                                      lm( B1 ~ A1 + C1, data = du )
                                      # draw imputed values
                                      draws = rnorm( mean = coef(m)[["(Intercept)"]] + coef(m)[["A"]] * imp1$A + coef(m)[["C"]] * imp1$C,
                                                     sd = sigma(m),
                                                     n = nrow(imp1) )
                                      #preds = predict(object = m, newdata = imp2)
                                      imp2$B[ is.na(imp2$B) ] = draws[ is.na(imp2$B) ]
                                      
                                      
                                      # complete-case analysis using the imputed dataset
                                      m_man = fit_regression(form_string = form_string,
                                                             model = p$model,
                                                             coef_of_interest = coef_of_interest,
                                                             miss_method = "CC",
                                                             du = imp2)
                                      m_man
                                      
                                    } else if ( p$dag_name == "1C") {
                                      
                                      # initialize single imputed dataset
                                      imp1 = du %>% select(B, C, A)
                                      
                                      ### Imputation step 1: Impute C using only A and subsetting on RC = 1 (not RC = RB = 1)
                                      ( m = lm( C ~ A, data = du %>% filter(RC == 1) ) )
                                      # c.f. truth
                                      lm( C1 ~ A1, data = du )
                                      # draw imputed values
                                      draws = rnorm( mean = coef(m)[["(Intercept)"]] + coef(m)[["A"]] * imp1$A,
                                                     sd = sigma(m),
                                                     n = nrow(imp1) )
                                      imp1$C[ is.na(imp1$C) ] = draws[ is.na(imp1$C) ]
                                      
                                      
                                      mean(imp1$C); mean(du$C1)  # correct, of course
                                      
                                      
                                      cor(imp1$C, imp1$A)
                                      cor(du$C1, du$A1)
                                      
                                      
                                      ### Imputation step 2: Impute B using A, C and subsetting to complete cases
                                      imp2 = imp1
                                      imp2$B = du$B
                                      
                                      # complete-case dataset
                                      dc = du[ complete.cases(du), ]
                                      
                                      ( m = lm( B ~ A + C, data = dc ) )
                                      # c.f. truth
                                      lm( B1 ~ A1 + C1, data = du )
                                      # draw imputed values
                                      draws = rnorm( mean = coef(m)[["(Intercept)"]] + coef(m)[["A"]] * imp1$A + coef(m)[["C"]] * imp1$C,
                                                     sd = sigma(m),
                                                     n = nrow(imp1) )
                                      #preds = predict(object = m, newdata = imp2)
                                      imp2$B[ is.na(imp2$B) ] = draws[ is.na(imp2$B) ]
                                      
                                      
                                      # complete-case analysis using the imputed dataset
                                      m_man = fit_regression(form_string = form_string,
                                                             model = p$model,
                                                             coef_of_interest = coef_of_interest,
                                                             miss_method = "CC",
                                                             du = imp2)
                                      m_man
                                      
                                    } else { stop("Custom method not implemented for that DAG")
                                    }
                                    
                                  },
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      # ~~ IPW-custom ----
      if ( "IPW-custom" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("IPW-custom"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "IPW-custom",
                                                                         du = du,
                                                                         imps = NULL),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      # ~~ IPW-nm ----
      # Sun & ETT
      if ( "IPW-nm" %in% all.methods ) {
        
        # #DEBUGGING ONLY!!
        # cat("\n\n ~~~~~~~~~~~~~~~~~~~ About to run IPW-nm for sim rep", i)
        # # save the dataset locally
        # setwd("/Users/mmathur/Dropbox/Personal computer/Independent studies/*Inchoate/2024/2024-10-20 - IAI (Incomplete auxiliaries in imputation)/Simulation study/Results/temp debugging")
        # fwrite( du, file = paste("rep", i, ".csv") )
        
        rep.res = run_method_safe(method.label = c("IPW-nm"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "IPW-nm",
                                                                         du = du,
                                                                         imps = NULL),
                                  .rep.res = rep.res )
        
        
        #DEBUGGING ONLY!!
        cat("\n\n  ~~~~~~~~~~~~~~~~~~~ Done running IPW-nm for sim rep", i)
        
        
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      # ~~ G-formula ----
      if ( "g-form" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("g-form"),
                                  
                                  method.fn = function(x) {
                                    
                                    if ( p$dag_name == "1B-bin" ) {
                                      
                                      # p(b(1) | a = 1) = sum_c { p(c | a = 1, RC = 1) * p(b | a = 1, c, RC = RB = 1) }
                                      a = 1
                                      term0 = mean( du$C[ du$RC == 1 ] == 0 ) * mean( du$B[ du$A == a & du$C == 0 & du$RB == 1 & du$RC == 1 ] )
                                      term1 = mean( du$C[ du$RC == 1 ] == 1 ) * mean( du$B[ du$A == a & du$C == 1 & du$RB == 1 & du$RC == 1 ] )
                                      ( ate_term1 = term0 + term1 )
                                      
                                      # c.f. truth
                                      mean(du$B1[du$A1 == a] )
                                      
                                      # p(b(1) | a = 0)
                                      a = 0
                                      term0 = mean( du$C[ du$RC == 1 ] == 0 ) * mean( du$B[ du$A == a & du$C == 0 & du$RB == 1 & du$RC == 1 ] )
                                      term1 = mean( du$C[ du$RC == 1 ] == 1 ) * mean( du$B[ du$A == a & du$C == 1 & du$RB == 1 & du$RC == 1 ] )
                                      ( ate_term0 = term0 + term1 )
                                      
                                      # c.f. truth
                                      mean(du$B1[du$A1 == a] )
                                      
                                      # correct! :D
                                      ( ate = ate_term1 - ate_term0 )
                                      
                                      return( list( stats = data.frame(bhat = ate) ) )
                                      
                                      
                                    } else {
                                      stop("G-form method not implemented for that DAG")
                                    }
                                    
                                  },
                                  .rep.res = rep.res )
      }
      
      # ~~ AF4 - nonparametric ----
      # sum_w { p(b | a, w, c, R = 1) p(w | a, c, RA = RC = RW = 1) }
      
      # current implementation requires everything except outcome to be binary
      # and assumes the aux variable (W) is called D
      if ( "af4-np" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("af4-np"),
                                  
                                  method.fn = function(x) {
                                    
                                    bhat = af4_cate_c(du = du, type = "np", c = 0)
                                      
                                    # no CIs
                                    if ( p$boot_reps_af4 > 0) {
                                      
                                      return( list( stats = data.frame( bhat = bhat) ) )
                                    }
                                      
                                      # bootstrapped CIs
                                      if ( p$boot_reps_af4 > 0) {
                                        boot_stat <- function(data, i) {
                                          db    <- data[i, ]
                                          bhatb = af4_cate_c(du = db, type = "np", c = 0)
                                        }
                                        
                                        bres <- boot(data = du,
                                                     statistic = boot_stat,
                                                     R = p$boot_reps)
                                        
                                        #@TEMP: use percentile method because BCA always throws error: "estimated adjustment 'a' is NA"
                                        # if you change boot type, need to change ci$percent[...] below too
                                        ci <- boot.ci(bres, type = "perc")
                                        
                                        return( list( stats = data.frame( bhat = bhat,
                                                                          bhat_lo = ci$percent[4],
                                                                          bhat_hi = ci$percent[5],
                                                                          bhat_width = ci$percent[5] - ci$percent[4] ) ) )
                                      } 
                          
                                  },
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      
  
      # ~ Add Scen Params and Sanity Checks --------------------------------------
      
      # add in scenario parameters
      # do NOT use rbind here; bind_cols accommodates possibility that some methods' rep.res
      #  have more columns than others
      rep.res = p %>% bind_cols( rep.res )
      
      # these don't come from p because they are from sim_data instead
      rep.res$coef_of_interest = coef_of_interest
      rep.res$beta = beta
      
      rep.res$form_string = form_string
      rep.res$gold_form_string = gold_form_string
      
      # add more info
      rep.res = rep.res %>% add_column( rep.name = i, .before = 1 )
      rep.res = rep.res %>% add_column( scen.name = scen, .before = 1 )
      rep.res = rep.res %>% add_column( job.name = jobname, .before = 1 )
      
      
      
      cat("\ndoParallel flag: Before adding sanity checks to rep.res")
      # could add info about simulated datasets here
      # preface them with "sancheck." to facilitate looking at sanchecks alone
      
      
      # amount of missing data
      # using di_std to avoid having R indicators, etc., in the dataset
      if ( !is.null(di) ) {
        rep.res = rep.res %>% add_column( sancheck.prop_complete = sum( complete.cases(di) ) / nrow(di) )
      }
      
      # missing data in each variable
      rep.res = rep.res %>% add_column( sancheck.mean_RB = mean(du$RB) )
      rep.res = rep.res %>% add_column( sancheck.mean_RC = mean(du$RC) )
      
      # MICE method for each imputation model
      if ( exists("mice_std_methods") ) rep.res = rep.res %>% add_column( sancheck.mice_std_methods = mice_std_methods )
      
      
      
      # if ( !is.null(di_ours) ) {
      #   rep.res = rep.res %>% add_column( sancheck.di_ours.vars = paste( names(di_ours), collapse = " " ) )
      # }
      # 
      # if ( exists("mice_ours_pred_vars_included") ){
      #   if ( !is.null(mice_ours_pred_vars_included) ) {
      #     rep.res = rep.res %>% add_column( sancheck.mice_ours_pred_vars_included = paste( mice_ours_pred_vars_included, collapse = " " ) )
      #   }
      # }
      
      
      cat("\n\n")
      print(rep.res)
      
      rep.res
    }  ### end foreach loop
    
  } )[3]  # end system.time
  
  
  if ( run.local == TRUE ) {
    # # save locally and organize after this scen
    # setwd(data.dir)
    # fwrite( rs,
    #         paste( "rs_scen_", scen, ".csv", sep = "" ) )
    
    # also bind into new file
    if ( scen == scens_to_run[1] ) rs_all_scens = rs
    else rs_all_scens = bind_rows(rs_all_scens, rs)
  }
  
}  # END FOR-LOOP to run multiple scens locally





if ( run.local == TRUE ) {
  
  View(rs_all_scens)
  
  dim(rs_all_scens)
  
  
  # fill in true beta
  beta_emp = rs_all_scens %>% filter(method == "gold") %>%
    group_by(scen.name) %>%
    summarise(beta = meanNA(bhat)) 
  as.data.frame(beta_emp)
  
  rs_all_scens = rs_all_scens %>% rowwise() %>%
    mutate( beta = ifelse( !is.na(beta),
                           beta,
                           beta_emp$beta[ beta_emp$scen.name == scen.name ] ) )
  
  
  t = rs_all_scens %>% group_by(method) %>%
    summarise( 
      reps = n(),
      Bhat = meanNA(bhat),
      BhatBias = meanNA(bhat - beta),
      BhatLo = meanNA(bhat_lo),
      BhatHi = meanNA(bhat_hi),
      BhatRMSE = sqrt( meanNA( (bhat - beta)^2 ) ),
      BhatCover = meanNA( covers(truth = beta,
                                 lo = bhat_lo,
                                 hi = bhat_hi) ),
      EYpred = meanNA(EY_prediction) ) %>%
    arrange() %>%
    mutate_if(is.numeric, function(x) round(x,2)) 
  
  as.data.frame(t)
  
  # setwd(data.dir)
  # fwrite( rs_all_scens,
  #         paste( "stitched_local.csv", sep = "" ) )
}


# ~~ End of ForEach Loop ----------------
rs$rep.seconds = doParallel.seconds/sim.reps
rs$rep.seconds[ rs$method != unique(rs$method)[1] ] = NA


expect_equal( as.numeric( sum(rs$rep.seconds, na.rm = TRUE) ),
              as.numeric(doParallel.seconds) )




# ~ QUICK RESULTS SUMMARY ---------------------------------------------------

if ( run.local == TRUE ) {
  rs %>%
    dplyr::select(method, bhat, bhat_width, bhat_covers) %>%
    
    group_by(method) %>%
    summarise_if(is.numeric, function(x) round( meanNA(x), 2 ) )
  
  any(is.na(rs$bhat))
}


# ~ WRITE LONG RESULTS ------------------------------
if ( run.local == FALSE ) {
  setwd("/home/groups/manishad/IAI/long_results")
  fwrite( rs, paste( "long_results", jobname, ".csv", sep="_" ) )
}
