
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
  sim.reps = 10
  #sim.reps = 1
  
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
    
    #rep.methods = "gold ; CC ; MICE-std ; Am-std ; IPW-custom ; adj-form-4-cate", 
    #rep.methods = "gold ; CC ; IPW-nm ; IPW-custom", 
    rep.methods = "IPW-nm",
    
    model = "OLS", 
    coef_of_interest = "A",
    N = c(5000),
    
    # MICE parameters
    # as on cluster
    imp_m = 5,  # CURRENTLY SET LOW
    imp_maxit = 100,
    mice_method = "pmm",
    
    # # for quicker sims
    # imp_m = 5,
    # imp_maxit = 5,
    # N = c(100),
    
    dag_name = c("12C") )
  
  
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
      #for ( i in 1:4 ) {
      
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
      
      # some methods don't make sense for certain combos of DAG and coef_of_interest
      #@this happens when a variable needed for imputation model is also in the target law
      #  later could deal with this by adding the variable back into dataset after imputation
      if ( (p$dag_name == "1D" & coef_of_interest == "A") |
           (p$dag_name == "1B" & coef_of_interest == "A") ) {
        all.methods = all.methods[ !all.methods %in% c("MICE-ours", "Am-ours") ]
      }
      
      
      # ~ Make Imputed Data ------------------------------
      
      
      # ~~ MICE (standard) ----
      # details of how mice() implements pmm:
      # ?mice.impute.pmm
      if ( "MICE-std" %in% all.methods & !is.null(di) ) {
        
        imps_mice = mice( di,
                          maxit = p$imp_maxit,
                          m = p$imp_m,
                          method = p$mice_method,
                          printFlag = FALSE )
        
        # sanity check
        imp1 = complete(imps_mice, 1)
        
        if ( any(is.na(imp1)) ) {
          message("MI left NAs in dataset - what a butt")
          imps_mice = NULL
        }
        
      } else {
        imps_mice = NULL
      }
      
      
      # ~~ MICE-ours ----
      # MICE by adjusting predictor matrix
      if ( "MICE-ours" %in% all.methods & !is.null(di) ) {
        
        
        if ( !( p$dag_name %in% c("1B-bin", "1B") ) ) {
          stop("MICE-ours not implemented for that DAG")
        }
        
        # modify predictor matrix instead of restricting dataset
        # "A value of 1 specifies that the variable given in the column name is used in the model to impute the variable given in the row name (and 0 specifies that this variable is not used in that model)."
        ini = mice(di, maxit=0)
        pred = ini$predictorMatrix
        
        if ( p$dag_name %in% c("1B-bin", "1B") ) {
          pred["C","B"] = 0  # do NOT use incomplete B to impute auxiliary C
        }
        
        
        imps_mice_ours_pred = mice( di,
                                    predictorMatrix = pred,
                                    maxit = p$imp_maxit,
                                    m = p$imp_m,
                                    method = p$mice_method,
                                    printFlag = FALSE )
        
        # for later sanity checks
        actual_pred = imps_mice_ours_pred$predictorMatrix
        
        # sanity check
        imp1 = complete(imps_mice_ours_pred, 1)
        
        if ( any(is.na(imp1)) ) {
          message("MI left NAs in dataset - what a butt")
          imps_mice_ours_pred = NULL
        }
        
      } else {
        imps_mice_ours_pred = NULL
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
      
      
      
      # ~~ MICE ----
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
      
      
      # ~~ MICE-ours ----
      if ( "MICE-ours" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("MICE-ours"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "MI",
                                                                         du = NULL,
                                                                         imps = imps_mice_ours_pred),
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
        
        rep.res = run_method_safe(method.label = c("IPW-nm"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "IPW-nm",
                                                                         du = du,
                                                                         imps = NULL),
                                  .rep.res = rep.res )
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
      
      
      # ~~ Adj form 1 ----
      
      if ( "adj-form-1" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("adj-form-1"),
                                  
                                  method.fn = function(x) {
                                    
                                    
                                    # p(b(1) | a = 1) = sum_c { p(c | RC = 1) * p(b | a = 1, c, RA = RC = RB = 1) }
                                    a = 1
                                    term0 = mean( du$C[ du$RC == 1 ] == 0 ) *
                                      mean( du$B[ du$A == a & du$C == 0 & du$RA == 1 & du$RB == 1 & du$RC == 1 ] )
                                    
                                    term1 = mean( du$C[ du$RC == 1 ] == 1 ) *
                                      mean( du$B[ du$A == a & du$C == 1 & du$RA == 1 & du$RB == 1 & du$RC == 1 ] )
                                    
                                    ( ate_term1 = term0 + term1 )
                                    
                                    # c.f. truth (IF no conventional confounding)
                                    mean(du$B1[du$A1 == a] )
                                    
                                    # p(b(1) | a = 0)
                                    a = 0
                                    term0 = mean( du$C[ du$RC == 1 ] == 0 ) * mean( du$B[ du$A == a & du$C == 0 & du$RA == 1 & du$RB == 1 & du$RC == 1 ] )
                                    term1 = mean( du$C[ du$RC == 1 ] == 1 ) * mean( du$B[ du$A == a & du$C == 1 & du$RA == 1 & du$RB == 1 & du$RC == 1 ] )
                                    ( ate_term0 = term0 + term1 )
                                    
                                    # c.f. truth (IF no conventional confounding)
                                    mean(du$B1[du$A1 == a] )
                                    
                                    # correct! :D
                                    ( ate = ate_term1 - ate_term0 )
                                    
                                    return( list( stats = data.frame(bhat = ate) ) )
                                    
                                    
                                  },
                                  .rep.res = rep.res )
      }
      
      # ~~ Adj form 2 ----
      # Difference from adj-form-1: This one uses  p(c | a, RC = 1, RA = 1) instead of  p(c | RC = 1)
      
      if ( "adj-form-2" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("adj-form-2"),
                                  
                                  method.fn = function(x) {
                                    
                                    
                                    # p(b(1) | a = 1) = sum_c { p(c | a = 1, RC = 1, RA = 1) * p(b | a = 1, c, RA = RC = RB = 1) }
                                    a = 1
                                    term0 = mean( du$C[ du$A == a & du$RA == 1 & du$RC == 1 ] == 0 ) *
                                      mean( du$B[ du$A == a & du$C == 0 & du$RA == 1 & du$RB == 1 & du$RC == 1 ] )
                                    
                                    term1 = mean( du$C[ du$A == a & du$RA == 1 & du$RC == 1 ] == 1 ) *
                                      mean( du$B[ du$A == a & du$C == 1 & du$RA == 1 & du$RB == 1 & du$RC == 1 ] )
                                    
                                    ( ate_term1 = term0 + term1 )
                                    
                                    # c.f. truth
                                    mean(du$B1[du$A1 == a] )
                                    
                                    # p(b(1) | a = 0)
                                    a = 0
                                    term0 = mean( du$C[ du$A == a & du$RA == 1 & du$RC == 1 ] == 0 ) * mean( du$B[ du$A == a & du$C == 0 & du$RA == 1 & du$RB == 1 & du$RC == 1 ] )
                                    term1 = mean( du$C[ du$A == a & du$RA == 1 & du$RC == 1 ] == 1 ) * mean( du$B[ du$A == a & du$C == 1 & du$RA == 1 & du$RB == 1 & du$RC == 1 ] )
                                    ( ate_term0 = term0 + term1 )
                                    
                                    # c.f. truth
                                    mean(du$B1[du$A1 == a] )
                                    
                                    # correct! :D
                                    ( ate = ate_term1 - ate_term0 )
                                    
                                    return( list( stats = data.frame(bhat = ate) ) )
                                    
                                    
                                  },
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      
      # ~~ Adj form 4 (CATE on C) ----
      # sum_w { p(b | a, w, c, R = 1) p(w | a, c, RA = RC = RW = 1) }
      
      if ( "adj-form-4-cate" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("adj-form-4-cate"),
                                  
                                  method.fn = function(x) {
                                    
                                    
                                    # two restricted datasets
                                    # complete-case dataset
                                    dc = du[ complete.cases(du), ]
                                    # restricted dataset for modeling W
                                    dw = du %>% filter( RA == 1 & RW == 1 & RC == 1 )
                                    
                                    
                                    c = 0  # will fix this level throughout
                                    a = 1
                                    
                                    ### p(b(1) | a = 1, c = c)
                                    term_w0 = mean( dw$W[ dw$A == a & dw$C == c ] == 0 ) *
                                      mean( dc$B[ dc$A == a & dc$C == c & dc$W == 0 ] )
                                    
                                    term_w1 = mean( dw$W[ dw$A == a & dw$C == c ] == 1 ) *
                                      mean( dc$B[ dc$A == a & dc$C == c & dc$W == 1 ] )
                                    
                                    ( ate_term1 = term_w0 + term_w1 )
                                    
                                    # c.f. truth
                                    mean(du$B1[du$A1 == a & du$C1 == c] )
                                    
                                    # compare each sub-term to truth
                                    mean( dc$B[ dc$A == a & dc$C == c & dc$W == 0 ] ); mean( du$B1[ du$A1 == a & du$C1 == c & du$W1 == 0 ] )
                                    mean( dw$W[ dw$A == a & dw$C == c ] == 0 ); mean( du$W1[ du$A1 == a & du$C1 == c ] == 0 )
                                    
                                    ### p(b(1) | a = 0, c = c)
                                    a = 0
                                    term_w0 = mean( dw$W[ dw$A == a & dw$C == c ] == 0 ) *
                                      mean( dc$B[ dc$A == a & dc$C == c & dc$W == 0 ] )
                                    
                                    term_w1 = mean( dw$W[ dw$A == a & dw$C == c ] == 1 ) *
                                      mean( dc$B[ dc$A == a & dc$C == c & dc$W == 1 ] )
                                    
                                    ( ate_term0 = term_w0 + term_w1 )
                                    
                                    # c.f. truth
                                    mean(du$B1[du$A1 == a & du$C1 == c] )
                                    
                                    # correct! :D
                                    ( ate = ate_term1 - ate_term0 )
                                    
                                    
                                    # c.f. gold std 
                                    #lm(B1 ~ A1, data = du %>% filter(C1 == c))
                                    
                                    
                                    return( list( stats = data.frame(bhat = ate) ) )
                                    
                                    
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
  # View(rs_all_scens)
  # dim(rs_all_scens)
  
  
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
