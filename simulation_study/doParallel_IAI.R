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
           "MASS",
           "miapack")

if ( run.local == TRUE | interactive.cluster.run == TRUE ) toLoad = c(toLoad, "here")

# dev version of miapack
#library(remotes)
#remotes::install_github("stmcg/miapack", ref = "ice-implementation")
#library(miapack)


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
  sim.reps = 1
  
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
    rep.methods = "gold ; CC ; mia-pkg-sp", 
    #rep.methods = "CC ; MICE-std ; genloc ; IPW-nm", 
    #rep.methods = "gold ; af4-np ; af4-sp ; IPW-nm",
    #rep.methods = "IPW-nm",
    
    model = "OLS", 
    #model = c( "OLS", "logistic"), 
    coef_of_interest = "A",
    #coef_of_interest = "B",  # ***** for 7D and 7D-bin
    N = c(1000),
    
    W_dim             = 2,
    W_n_cont          = 1,   # 1 continuous, 1 binary
    W_n_cont_complete = 0,   # both incomplete
    W_n_bin_complete  = 0,
    W_n_inter         = 1,   # only 1 possible pair now (needs 2*W_n_inter <= W_dim)
    W_rho             = 0.4,   # latent correlation between the 2 components
    W_cor_type        = "exch",
    W_bin_prob        = 0.5,   # marginal P(W_binary = 1)
    W_miss_rate       = 0.2,     # target marginal P(R_Wj = 0) for incomplete comps
    W_parent_coef     = 1,       # strength of the W parent (X2 or Y, per DAG) -> W
    W_inter_coef      = 1,       # coefficient on the single W1*W2 interaction
    
    
    
    # MICE parameters
    # as on cluster
    #imp_m = 5,  # CURRENTLY SET LOW
    imp_m = 50,
    imp_maxit = 100,
    mice_method = NA,  # let MICE use its defaults
    
    # AF4 parameters
    boot_reps_af4 = 1000,  # only needed for CIs; if set to 0, won't give CIs
    mia_n_mc = 10000, 
    
    dag_name = "1A"
    
    # dag_name = c("5D", "5D-bin",
    #              "6D", "6D-bin",
    #              "7D", "7D-bin"
    #              )  # make sure to pick appropriate outcome model for the DAG
    
  )
  
  
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

if (run.local == TRUE) sim.reps = 10
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
      #for ( i in 1:2 ) {
      
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
        
        
        
        if (n_bin == 0) {
          
          message("No binaries found — adding fake binary column for genloc compatibility")
          di2$z_fake <- sample(1:2, nrow(di2), replace = TRUE)  # coded 1/2 as mix requires
          n_bin <- 1
          # move fake binary to front (mix requires binaries in first p columns)
          di2 <- di2[ , c("z_fake", setdiff(names(di2), "z_fake")) ]
          added_fake <- "binary"
          
        } else if (n_bin == ncol(di2)) {
          
          message("All columns are binary — adding fake continuous column for genloc compatibility")
          di2$z_fake <- rnorm(nrow(di2))
          added_fake <- "continuous"
          
        } else {
          
          added_fake <- "none"
          
        }
        
        
        # this block sometimes throws "improper posterior -- empty cells"
        tryCatch({
          # randomize the random seed
          rngseed( runif(min = 1000000, max = 9999999, n=1) )
          
          # *** generate the imputations
          di3 <- prelim.mix(di2, p = n_bin)
          
          thetahat <- em.mix(di3)
          
          m <- p$imp_m  
          imps_genloc <- vector("list", m)
          
          # NB: this loop used to be indexed by `i`, which is ALSO the foreach
          # sim-rep index. It silently overwrote the rep counter, so every
          # rep.name downstream was recorded as imp_m rather than the rep.
          for (.imp in 1:m) {
            newtheta <- da.mix(di3, thetahat, steps = 100)
            newimp   <- as.data.frame( imp.mix(s = di3, theta = newtheta, x = di2) )
            newimp   <- reverse_recode_binaries(newimp)
            newimp$z_fake <- NULL          # <-- dropped from each imputed dataset before storing
            imps_genloc[[.imp]] <- newimp  # <-- stored without z_fake
          }
          
          # sanity check
          imp1 = imps_genloc[[1]]
          
          if ( any(is.na(imp1)) ) {
            message("MI left NAs in dataset - what a butt")
            imps_genloc = NULL
          } 
          
          
        }, error = function(e) {
          message("error making genloc imputations: ", e$message)
          # superassignment: the previous `imps_genloc = NULL` was local to the
          # handler and never reached the caller
          imps_genloc <<- NULL
          
          # save the problem dataset
          if (run.local == FALSE) {
            
            # for debugging
            setwd("/home/groups/manishad/IAI/rmfiles")
            fwrite( rs, paste( "rm_data", jobname, ".csv", sep="_" ) )
            
          }
        }
        )
        
        
        
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
          imps_mice_raw = mice( di2,
                                maxit = p$imp_maxit,
                                m = p$imp_m,
                                printFlag = FALSE )
          
          
          # reorganize the complicated mids object into list of imputed datasets
          # needed for fit_regression to find the imputed datasets
          imps_mice = lapply(seq_len(imps_mice_raw$m), function(i) complete(imps_mice_raw, i))
          
          # recode any 0/1 factors as numeric for regression joy
          imps_mice <- lapply(imps_mice, function(.d) 
            .d %>% mutate(across(where(is.factor), ~ as.numeric(as.character(.)))))
          
        } else {
          imps_mice_raw = mice( di2,
                                maxit = p$imp_maxit,
                                m = p$imp_m,
                                
                                # "A vector of length 4 containing the default imputation methods for 1) numeric data, 2) factor data with 2 levels, 3) factor data with > 2 unordered levels, and 4) factor data with > 2 ordered levels. By default, the method uses pmm, predictive mean matching (numeric data) logreg, logistic regression imputation (binary data, factor with 2 levels) polyreg, polytomous regression imputation for unordered categorical data (factor > 2 levels) polr, proportional odds model for (ordered, > 2 levels)."
                                # default for defaultMethod is: c("pmm", "logreg", "polyreg", "polr")
                                #defaultMethod = c("norm", "logreg", "polyreg", "polr"),
                                
                                method = p$mice_method,
                                printFlag = FALSE )
          
          # reorganize the complicated mids object into list of imputed datasets
          # needed for fit_regression to find the imputed datasets
          imps_mice = lapply(seq_len(imps_mice_raw$m), function(i) complete(imps_mice_raw, i))
          
          # recode any 0/1 factors as numeric for regression joy
          imps_mice <- lapply(imps_mice, function(.d) 
            .d %>% mutate(across(where(is.factor), ~ as.numeric(as.character(.)))))
        }
        
        
        
        # was imps_mice$method, but imps_mice is a plain list of completed
        # datasets -- the mids object is imps_mice_raw
        mice_std_methods = summarize_mice_methods(imps_mice_raw$method)
        
        # sanity check
        imp1 = imps_mice[[1]]
        
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
        
        if (run.local == TRUE) srr(rep.res)
      }
      
      
      
      
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
        
        if (run.local == TRUE) srr(rep.res)
      }
      
      
      
      
      
      
      
      
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
        
        if (run.local == TRUE) srr(rep.res)
      }
      
      
      
      
      
      
      
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
        
        if (run.local == TRUE) srr(rep.res)
      }
      
      
      
      
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
        
        if (run.local == TRUE) srr(rep.res)
      }
      
      
      
      
      
      
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
        
        if (run.local == TRUE) srr(rep.res)
        
      }
      
      
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
        
        if (run.local == TRUE) srr(rep.res)
      }
      
      
      
      
      # ~~ IPW-nm ----
      # Sun & ETT
      if ( "IPW-nm" %in% all.methods ) {
        
        # #FOR DEBUGGING ONLY!!
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
        
        
        cat("\n\n  ~~~~~~~~~~~~~~~~~~~ Done running IPW-nm for sim rep", i)
        
        if (run.local == TRUE) srr(rep.res)
        
      }
      
      
      
      
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
                                    
                                    ests = af4_cate_c(du = du, type = "np", c = 0)
                                    
                                    # no CIs
                                    if ( p$boot_reps_af4 == 0) {
                                      
                                      return( list( stats = data.frame( bhat = ests[1],
                                                                        inthat = ests[2] ) ) )
                                    }
                                    
                                    # bootstrapped CIs
                                    if ( p$boot_reps_af4 > 0) {
                                      boot_stat <- function(data, i) {
                                        db    <- data[i, ]
                                        
                                        # in order: CATE, \hat E[B | a=1, c=0]
                                        estsb = af4_cate_c(du = db, type = "np", c = 0)
                                      }
                                      
                                      bres <- boot(data = du,
                                                   statistic = boot_stat,
                                                   R = p$boot_reps_af4)
                                      
                                      #@TEMP: using percentile method because BCA always throws error: "estimated adjustment 'a' is NA"
                                      # if you change boot type, need to change ci$percent[...] below too
                                      cis = get_boot_CIs(boot.res = bres, type = "perc", n.ests = 2)
                                      
                                      
                                      return( list( stats = data.frame( bhat = ests[1],
                                                                        bhat_lo = cis[[1]][1],
                                                                        bhat_hi = cis[[1]][2],
                                                                        bhat_width = cis[[1]][2] - cis[[1]][1],
                                                                        
                                                                        inthat = ests[2],
                                                                        int_lo = cis[[2]][1],
                                                                        int_hi = cis[[2]][2],
                                                                        int_width = cis[[2]][2] - cis[[2]][1]
                                                                        
                                      ) ) )
                                    } 
                                    
                                  },
                                  .rep.res = rep.res )
        
        if (run.local == TRUE) srr(rep.res)
      }
      
      
      
      
      # ~~ AF4 - semiparametric ----
      # sum_w { p(b | a, w, c, R = 1) p(w | a, c, RA = RC = RW = 1) }
      
      # current implementation requires everything except outcome to be binary
      # and assumes the aux variable (W) is called D
      if ( "af4-sp" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("af4-sp"),
                                  
                                  method.fn = function(x) {
                                    
                                    ests = af4_cate_c(du = du, type = "sp", c = 0)
                                    
                                    # no CIs
                                    if ( p$boot_reps_af4 == 0) {
                                      
                                      return( list( stats = data.frame( bhat = ests[1],
                                                                        inthat = ests[2] ) ) )
                                    }
                                    
                                    # bootstrapped CIs
                                    if ( p$boot_reps_af4 > 0) {
                                      boot_stat <- function(data, i) {
                                        db    <- data[i, ]
                                        bhatb = af4_cate_c(du = db, type = "sp", c = 0)
                                      }
                                      
                                      bres <- boot(data = du,
                                                   statistic = boot_stat,
                                                   R = p$boot_reps_af4)
                                      
                                      #@TEMP: using percentile method because BCA always throws error: "estimated adjustment 'a' is NA"
                                      # if you change boot type, need to change ci$percent[...] below too
                                      cis = get_boot_CIs(boot.res = bres, type = "perc", n.ests = 2)
                                      
                                      return( list( stats = data.frame( bhat = ests[1],
                                                                        bhat_lo = cis[[1]][1],
                                                                        bhat_hi = cis[[1]][2],
                                                                        bhat_width = cis[[1]][2] - cis[[1]][1],
                                                                        
                                                                        inthat = ests[2],
                                                                        int_lo = cis[[2]][1],
                                                                        int_hi = cis[[2]][2],
                                                                        int_width = cis[[2]][2] - cis[[2]][1] )
                                                    
                                      ) ) 
                                    } 
                                    
                                  },
                                  .rep.res = rep.res )
        
        if (run.local == TRUE) srr(rep.res)
      }
      
      
      
      # rep.res = data.frame()
      
      # ~~ MIA package; semiparametric ----
      if ( "mia-pkg-sp" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("mia-pkg-sp"),
                                  
                                  method.fn = function(x) {
                                    
                                    # parse the gold model into outcome + predictors
                                    fo        = as.formula(form_string)   # e.g. B ~ A * C
                                    outcome   = all.vars(fo)[1]           # LHS, e.g. "B"
                                    exposure  = coef_of_interest          # e.g. "A"
                                    rhs_vars  = all.vars(fo)[-1]          # main-effect vars on RHS
                                    covars    = setdiff(rhs_vars, exposure)  # analysis covars X \ exposure
                                    
                                    Wobs   = w_names(p)$obs               # W component columns
                                    X_names = c(exposure, covars)         # predictor set for mia()
                                    
                                    # miapack::mia REQUIRES the outcome column to be named "Y"
                                    # (see ?mia / dat.sim). Rename on a local copy only; predictors
                                    # keep their real names and are referenced via X_names.
                                    di_mia = di
                                    names(di_mia)[names(di_mia) == outcome] = "Y"
                                    
                                    # rebuild the outcome-model formula with LHS "Y", same RHS
                                    # terms as the gold model (keeps interactions) plus W
                                    rhs_terms = labels(terms(fo))         # e.g. "A","C","A:C"
                                    Y_form = as.formula(
                                      paste("Y ~", paste(c(rhs_terms, Wobs), collapse = " + ")) )
                                    
                                    # contrast: exposure 1 vs 0, all covars held at reference level 0
                                    ref = rep(0, length(covars))
                                    Xv1 = c(1, ref)
                                    Xv0 = c(0, ref)
                                    
                                    # types, aligned to how sim_data builds the columns 
                                    Y_type = if ( p$model == "logistic" ) "binary" else "continuous"
                                    W_type = ifelse( seq_along(Wobs) <= p$W_n_cont, "normal", "binary" )
                                    
                                    # one W-model per component (sequential factorization):
                                    #      w_j ~ exposure + covars + earlier components
                                    W_forms = lapply( seq_along(Wobs), function(j) {
                                      preds = c(X_names, Wobs[seq_len(j - 1)])
                                      as.formula( paste(Wobs[j], "~", paste(preds, collapse = " + ")) )
                                    })
                                    
                                    mia_args = list(
                                      data          = di_mia,
                                      X_names       = X_names,
                                      X_values_1    = Xv1,
                                      X_values_2    = Xv0,
                                      contrast_type = "difference",
                                      Y_model       = Y_form,
                                      Y_type        = Y_type,
                                      W_model       = W_forms,
                                      W_type        = W_type,
                                      n_mc          = p$mia_n_mc
                                    )
                                    
                                    fit = do.call(miapack::mia, mia_args)
                                    
                                    # point estimate = the exposure 1 vs 0 contrast
                                    # inthat = reference-level mean E0 (exposure 0, covars 0),
                                    # i.e. fit$mean_est_2 -- the "(Intercept)"-analogue level.
                                    if ( p$boot_reps_af4 == 0 ) {
                                      return( list( stats = data.frame(
                                        bhat   = fit$contrast_est,
                                        inthat = fit$mean_est_2
                                      ) ) )
                                    }
                                    
                                    # bootstrap CI via miapack::get_CI 
                                    # get_CI takes the FITTED mia object (not the modeling args)
                                    # and wraps boot / boot.ci. type = "bca" is the mia default.
                                    ci_obj = miapack::get_CI(
                                      mia_res = fit,
                                      n_boot  = p$boot_reps_af4,
                                      type    = "bca",   
                                      conf    = 0.95
                                    )
                                    
                                    bhat_ci_row = ci_obj$ci_contrast[[4]]
                                    bhat_ci_lo  = bhat_ci_row[ length(bhat_ci_row) - 1 ]
                                    bhat_ci_hi  = bhat_ci_row[ length(bhat_ci_row) ]
                                    
                                    inthat_ci_row = ci_obj$ci_2[[4]]  # ci_2 because inthat = mean_est_2 (all predictors 0)
                                    inthat_ci_lo  = inthat_ci_row[ length(inthat_ci_row) - 1 ]
                                    inthat_ci_hi  = inthat_ci_row[ length(inthat_ci_row) ]
                                    
                                    return( list( stats = data.frame(
                                      bhat       = fit$contrast_est,
                                      bhat_lo    = bhat_ci_lo,
                                      bhat_hi    = bhat_ci_hi,
                                      bhat_width = bhat_ci_hi - bhat_ci_lo,
                                      
                                      inthat    = fit$mean_est_2,
                                      int_lo    = inthat_ci_lo,
                                      int_hi    = inthat_ci_hi,
                                      int_width = inthat_ci_hi - inthat_ci_lo
                                    ) ) )
                                  },
                                  .rep.res = rep.res )
        
        if (run.local == TRUE) srr(rep.res)
      }
      
      
      
      # ~~ MIA-ICE (miapack, iterative conditional expectation) --------------
      # miapack::mia_ice (ice-implementation branch): plug-in estimator of
      #   mu_MIA(x) = E[ E[Y | X=x, W, M=1] | X=x, R_W=R_X=1 ].
      # Differs from the Monte Carlo mia(): no W-density model and no n_mc.
      # Instead it fits the outcome model, predicts g_hat = Ehat[Y|x,W,M=1] at
      # the target x, then regresses g_hat on X via `outer_model` (LHS must be
      # g_hat; RHS may reference ONLY the X predictors). Saturating outer_model
      # in X reduces mu_MIA(x) to the sample mean of g_hat within each X cell.
      #
      # Everything DAG-specific is parsed from sim_obj$form_string +
      # coef_of_interest + w_names(p); nothing is hard-coded.
      if ( "mia-pkg-ice" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("mia-pkg-ice"),
                                  
                                  method.fn = function(x) {
                                    
                                    # ---- parse the gold model into outcome + predictors ----
                                    fo        = as.formula(form_string)   # e.g. B ~ A * C
                                    outcome   = all.vars(fo)[1]           # LHS, e.g. "B"
                                    exposure  = coef_of_interest          # e.g. "A"
                                    rhs_vars  = all.vars(fo)[-1]          # main-effect vars on RHS
                                    covars    = setdiff(rhs_vars, exposure)  # analysis covars X \ exposure
                                    
                                    Wobs   = w_names(p)$obs               # W component columns
                                    X_names = c(exposure, covars)         # predictor set for mia_ice()
                                    
                                    # mia_ice REQUIRES the outcome column to be named "Y" and the
                                    # Y_model LHS to be "Y". Rename on a local copy only.
                                    di_mia = di
                                    names(di_mia)[names(di_mia) == outcome] = "Y"
                                    
                                    # outcome model: LHS "Y", same RHS terms as the gold model
                                    # (keeps interactions) plus the W components.
                                    rhs_terms = labels(terms(fo))         # e.g. "A","C","A:C"
                                    Y_form = as.formula(
                                      paste("Y ~", paste(c(rhs_terms, Wobs), collapse = " + ")) )
                                    
                                    # outer model: g_hat ~ (X predictors only), saturated.
                                    # RHS may reference ONLY X_names (mia_ice errors otherwise), so
                                    # it is built from the X's, NOT from rhs_terms and NOT with W.
                                    #   1 covar  -> g_hat ~ A * C   (saturated in binary A, C)
                                    #   0 covars -> g_hat ~ A
                                    outer_rhs  = paste(X_names, collapse = " * ")
                                    outer_form = as.formula( paste("g_hat ~", outer_rhs) )
                                    
                                    # contrast: exposure 1 vs 0, covars held at reference level 0
                                    ref = rep(0, length(covars))
                                    Xv1 = c(1, ref)
                                    Xv0 = c(0, ref)
                                    
                                    Y_type = if ( p$model == "logistic" ) "binary" else "continuous"
                                    
                                    fit = miapack::mia_ice(
                                      data          = di_mia,
                                      X_names       = X_names,
                                      X_values_1    = Xv1,
                                      X_values_2    = Xv0,
                                      contrast_type = "difference",
                                      Y_model       = Y_form,
                                      Y_type        = Y_type,
                                      outer_model   = outer_form
                                    )
                                    
                                    # point estimate = the exposure 1 vs 0 contrast
                                    # inthat = reference-level mean E0 (exposure 0, covars 0),
                                    # i.e. fit$mean_est_2 -- the "(Intercept)"-analogue level.
                                    if ( p$boot_reps_af4 == 0 ) {
                                      return( list( stats = data.frame(
                                        bhat   = fit$contrast_est,
                                        inthat = fit$mean_est_2
                                      ) ) )
                                    }
                                    
                                    # bootstrap CI via miapack::get_CI 
                                    # get_CI takes the FITTED mia object (not the modeling args)
                                    # and wraps boot / boot.ci. type = "bca" is the mia default.
                                    ci_obj = miapack::get_CI(
                                      mia_res = fit,
                                      n_boot  = p$boot_reps_af4,
                                      type    = "bca",   
                                      conf    = 0.95
                                    )
                                    
                                    bhat_ci_row = ci_obj$ci_contrast[[4]]
                                    bhat_ci_lo  = bhat_ci_row[ length(bhat_ci_row) - 1 ]
                                    bhat_ci_hi  = bhat_ci_row[ length(bhat_ci_row) ]
                                    
                                    inthat_ci_row = ci_obj$ci_2[[4]]  # ci_2 because inthat = mean_est_2 (all predictors 0)
                                    inthat_ci_lo  = inthat_ci_row[ length(inthat_ci_row) - 1 ]
                                    inthat_ci_hi  = inthat_ci_row[ length(inthat_ci_row) ]
                                    
                                    return( list( stats = data.frame(
                                      bhat       = fit$contrast_est,
                                      bhat_lo    = bhat_ci_lo,
                                      bhat_hi    = bhat_ci_hi,
                                      bhat_width = bhat_ci_hi - bhat_ci_lo,
                                      
                                      inthat    = fit$mean_est_2,
                                      int_lo    = inthat_ci_lo,
                                      int_hi    = inthat_ci_hi,
                                      int_width = inthat_ci_hi - inthat_ci_lo
                                    ) ) )
                                    
                                    
                                  },
                                  .rep.res = rep.res )
        
        if (run.local == TRUE) srr(rep.res)
      }
      
      
      
      # return this rep's results as the last expression of the foreach body
      rep.res
      
    }  # END foreach %dopar% body
  })  # END system.time({
  
  # stitch this scen's foreach results into the running results object
  if ( exists("rs_all_scens") ) {
    rs_all_scens = bind_rows(rs_all_scens, rs)
  } else {
    rs_all_scens = rs
  }
  
}  # END FOR-LOOP to run multiple scens locally





if ( run.local == TRUE ) {
  
  #View(rs_all_scens)
  
  dim(rs_all_scens)
  
  # look for NA CIs
  rs_all_scens %>% filter(method == "mia-pkg-ice") %>%
    summarise(mean_NA = meanNA(bhat_lo))
  
  mean( is.na(rs_all_scens$bhat_lo[ rs_all_scens$method == "mia-pkg-ice"]) )
  
  
  # fill in true beta
  beta_emp = rs_all_scens %>% filter(method == "gold") %>%
    group_by(scen.name) %>%
    summarise(beta = meanNA(bhat)) 
  as.data.frame(beta_emp)
  
  rs_all_scens = rs_all_scens %>% rowwise() %>%
    mutate( beta = ifelse( !is.na(beta),
                           beta,
                           beta_emp$beta[ beta_emp$scen.name == scen.name ] ) )
  
  
  t = rs_all_scens %>% group_by(dag_name, method) %>%
    summarise( 
      reps = n(),
      Bhat = meanNA(bhat),
      BhatBias = meanNA(bhat - beta),
      BhatLo = meanNA(bhat_lo),
      BhatHi = meanNA(bhat_hi),
      BhatRMSE = sqrt( meanNA( (bhat - beta)^2 ) ),
      BhatCover = meanNA( covers(truth = beta,
                                 lo = bhat_lo,
                                 hi = bhat_hi) ) ) %>%
    # EYpred = meanNA(EY_prediction) ) %>%
    arrange() %>%
    mutate_if(is.numeric, function(x) round(x,2)) 
  
  as.data.frame(t)
  
  # # with E[B |A,C]
  # t = rs_all_scens %>% group_by(method) %>%
  #   summarise( 
  #     reps = n(),
  #     Bhat = meanNA(bhat),
  #     BhatBias = meanNA(bhat - beta),
  #     BhatLo = meanNA(bhat_lo),
  #     BhatHi = meanNA(bhat_hi),
  #     BhatRMSE = sqrt( meanNA( (bhat - beta)^2 ) ),
  #     BhatCover = meanNA( covers(truth = beta,
  #                                lo = bhat_lo,
  #                                hi = bhat_hi) ),
  #     Ehat_B_a1c0 = meanNA(Ehat_B_a1c0),
  #     E_B_a1c0 = meanNA(E_B_a1c0)) %>%
  #   arrange() %>%
  #   mutate_if(is.numeric, function(x) round(x,2)) 
  # 
  # as.data.frame(t)
  
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
  cat("\n *** FLAG 1: About to write long results")
  setwd("/home/groups/manishad/IAI/long_results")
  fwrite( rs, paste( "long_results", jobname, ".csv", sep="_" ) )
  cat("\n *** FLAG 3: Done writing long results")
}