
expit = function(p) exp(p) / (1 + exp(p))
logit = function(p) p / (1-p)





# DATA-GENERATION FNS ---------------------

sim_data = function(.p) {
  
  if (.p$model != "OLS" ) stop("Only handles model OLS for now")
  
  # ~ DAG 1A -----------------------------
  # conceptually replicates Dowd's Figure 2, upper left panel
  # R_Y <- A -> B(1); R_Y <- C(1) -> B(1)
  # R_Y -> R_C so that if Y(1) is missing, then C(1) is definitely observed
  
  if ( .p$dag_name == "1A" ) {
    
    du = data.frame( C1 = rnorm( n = .p$N ),  
                     A1 = rnorm( n = .p$N ) )  # only including this because neither imputation method runs with 1 variable
    
    coef1 = 2
    coef2 = 1.6
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = coef1*A1 + coef2*C1 ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(3*A1 + 3*C1) ),
              
              RC = rbinom(n = 1, size = 1, prob = 0.5) )
    
    # monotone missingness: conditionally overwrite RZ
    du$RC[ du$RB == 0 ] = 1 
    # missmap(du %>% select(A, B, C))
    
    du = du %>% rowwise() %>%
      mutate( A = A1,
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "1A"
  
  
  
  
  
  # ~ DAG 1A-bin -----------------------------
  # same graph as 1A, but binary A and C 
  
  if ( .p$dag_name == "1A-bin" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),  
                     A1 = rbinom( n = .p$N, 
                                  size = 1, 
                                  prob = 0.5 ) )  
    
    coef1 = 2
    coef2 = 1.6
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = coef1*A1 + coef2*C1),
              
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.4 + 0.2*A1 + 0.3*C1 ),
              
              # previous version (before 2024-10-26):
              # RB = rbinom( n = 1,
              #              size = 1,
              #              prob = expit(-3 + 3*A1 + 3*C1) ),
              
              RC = rbinom(n = 1, size = 1, prob = 0.5) )
    
    # monotone missingness: conditionally overwrite RC
    du$RC[ du$RB == 0 ] = 1 
    # missmap(du %>% select(A, B, C))
    
    du = du %>% rowwise() %>%
      mutate( A = A1,
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "1A-bin"
  
  
  
  
  
  # ~ DAG 1B -----------------------------
  # same as above, but no R_Y -> R_C edge (same graph as Dowd, though different law)
  
  if ( .p$dag_name == "1B" ) {
    
    du = data.frame( C1 = rnorm( n = .p$N ),  
                     A1 = rnorm( n = .p$N ) )  # only including this because neither imputation method runs with 1 variable
    
    coef1 = 2
    coef2 = 1.6
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = coef1*A1 + coef2*C1 ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(3*A1 + 3*C1) ),
              
              RC = rbinom(n = 1, size = 1, prob = 0.75) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = A1,
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "1B"
  
  
  
  # ~ DAG 1B-bin -----------------------------
  # same graph as 1B, but binary A and C 
  
  if ( .p$dag_name == "1B-bin" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),  
                     A1 = rbinom( n = .p$N, 
                                  size = 1, 
                                  prob = 0.5 ) )  
    
    coef1 = 2
    coef2 = 1.6
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = coef1*A1 + coef2*C1),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(-3 + 3*A1 + 3*C1) ),
              
              RC = rbinom(n = 1, size = 1, prob = 0.5) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = A1,
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "1A-bin"
  
  
  
  
  # ~ DAG 1C -----------------------------
  # exactly same distribution as Dowd
  
  if ( .p$dag_name == "1C" ) {
    
    # for cluster if running R version 4.3.2:
    #install_version("MASS", version = "7.3-60.0.1", repos = "http://cran.us.r-project.org")
    
    
    du = as.data.frame( mvrnorm(n = .p$N,
                                mu = c(-3, 6, 2),  # X, Y, Z per Dowd pg 7
                                Sigma = matrix( c(1, 0.6, 0,
                                                  0.6, 1, 0.7,
                                                  0, 0.7, 1),
                                                byrow = TRUE,
                                                nrow = 3) ) )
    names(du) = c("A1", "B1", "C1")
    
    
    # replicating their code rather than what the preprint says (typos)
    du$RB = 0
    du$RB[ pnorm( du$A1, mean = -3, sd = 1 ) > sqrt(0.5) ] = 1
    du$RB[ pnorm( du$C1, mean = 2, sd = 1 ) > sqrt(0.5) ] = 1
    mean(du$RB)  # should be 0.50
    
    # C is MCAR
    # In missing auxiliary mechanism 1, Z was set to missing if a random draw from the uniform distribution
    # bounded by 0 and 1 was less than μ, where μ was varied between 0 and 0.9 in increments of 0.1.
    U = runif( n = nrow(du), min = 0, max = 1 )
    du$RC = (U < 0.5)
    mean(du$RC)
    
    du = du %>% rowwise() %>%
      mutate( A = A1,
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    colMeans(du)
    #cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = NA
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "1A-bin"
  
  
  
  
  
  
  
  
  # ~ DAG 2A -----------------------------
  # Ilya's DAG with self-censoring confounder: R_C <- C(1) -> A -> Y; C(1) -> Y
  
  if ( .p$dag_name == "2A" ) {
    
    du = data.frame( C1 = rnorm( n = .p$N ) )  
    
    coef1 = 2
    coef2 = 1.6
    
    du = du %>% rowwise() %>%
      mutate( A1 = rnorm( n = 1,
                          mean = coef2*C1 ),
              
              B1 = rnorm( n = 1,
                          mean = coef1*A1 + coef2*C1 ),
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(3*C1) ) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = A1,
              B = B1,
              C = ifelse(RC == 1, C1, NA) )
    
    # colMeans(du)
    # cor(du %>% select(A1, B1, C1, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A + C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 + C1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "2A"
  
  
  # ~ DAG 1B -----------------------------
  # same as above, but no R_Y -> R_C edge (same graph as Dowd, though different law)
  
  if ( .p$dag_name == "1B" ) {
    
    du = data.frame( C1 = rnorm( n = .p$N ),  
                     A1 = rnorm( n = .p$N ) )  # only including this because neither imputation method runs with 1 variable
    
    coef1 = 2
    coef2 = 1.6
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = coef1*A1 + coef2*C1 ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(3*A1 + 3*C1) ),
              
              RC = rbinom(n = 1, size = 1, prob = 0.75) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = A1,
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "1B"
  
  
  
  # ~ DAG 3B-bin -----------------------------
  # same as above, but has C1 -> Y1 edge
  
  if ( .p$dag_name == "3B-bin" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    coef2 = 1.6
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              # add edge from C1 -> B1
              B1 = rnorm( n = 1,
                          mean = coef1*A1 + coef1*C1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A + C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 + C1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "3B-bin"
  
  
  # ~ DAG 3B-bin-mono -------------------------------------------------
  # monotone version of 3B-bin *and* has A*C interaction on Y
  
  if ( .p$dag_name == "3B-bin-mono" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    coef2 = 1.6
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              # add edge from C1 -> B1
              B1 = rnorm( n = 1,
                          # here, the A1*C1 interaction is new compared to 3B-bin
                          mean = coef1*A1 + coef1*C1 + A1*C1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ) )
    
    
    # monotone missingness: conditionally overwrite indicator
    du$RA[ du$RC == 0 ] = 0
    du$RB[ du$RA == 0 ] = 0
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A + C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 + C1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "3B-bin-mono"
  
  
  # ~ DAG 4A -----------------------------
  
  # for adjustment formula 4, CATE version
  # C1 -> A1 -> W1 -> B1
  # C1 -> Y1, C1 -> RC
  # W1 -> RY
  
  
  if ( .p$dag_name == "4A" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coefWB = 2
    coefAW = 3
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              W1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + coefAW*A1) ),
              
              B1 = rnorm( n = 1,
                          mean = coefWB*W1 + 2.6*C1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RW = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*W1) ) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              W = ifelse(RW == 1, W1, NA) )
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, W1, RA, RB, RC, RW) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(A, B, C, W)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A + C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 + C"
      
      beta = NA
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "4A"
  
  
  
  # ~ DAG 4A - Steve's version -----------------------------
  
  # C1 -> A1 -> W1 -> B1
  # C1 -> B1, NO EDGE C1 -> RC
  # W1 -> RY
  # D1: a standalone complete variable included so that Amelia can run
  
  
  if ( .p$dag_name == "4A-Steve" ) {
    
    du = data.frame( C1 = rnorm( n = .p$N ),
                     D1 = rnorm( n = .p$N ) ) 

    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = 1 / (1 + exp(-( -log(1/.5-1) + log(2) * C1))) ),
              
              W1 = rnorm( n = 1,
                          mean = A1 ),
              
              B1 = rnorm( n = 1,
                          mean = C1 + A1 + W1),
              
              # following Steve's code, these are the complements of my R's 
              RA_comp = rbinom( n = 1,
                           size = 1,
                           prob = 0.3 ),
              
              RW_comp = rbinom( n = 1,
                           size = 1,
                           prob = 0.3 ),
              
              RC_comp = rbinom( n = 1,
                           size = 1,
                           # note: C is not self-censoring as in 4A
                           prob = 0.3 ),
              
              RB_comp = rbinom( n = 1,
                           size = 1,
                           prob = 1 / (1 + exp(-( -log(1/.3-1) + log(2) * W1))) ),
              
              RA = (RA_comp == 0),
              RW = (RW_comp == 0),
              RC = (RC_comp == 0),
              RB = (RB_comp == 0) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              W = ifelse(RW == 1, W1, NA),
              D = D1 )
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, W1, D1, RA, RB, RC, RW) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(A, B, C, W, D)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A + C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 + C"
      
      beta = NA
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "4A-Steve"
  
  
  
  
  # ~ DAG 5A -----------------------------
  
  # for adjustment formula 4, CATE version
  # will break, but correct imputation is possible
  # RA <- W (complete) -> Y1 and A1 -> Y1
  # RY and C are isolated
  
  if ( .p$dag_name == "5A" ) {
    
    du = data.frame( A1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),

                     W1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    coefAB = 2
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = coefAB*A1 + 1.6*W1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*W1) ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RC = 1, 
              RW = 1 )
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              W = ifelse(RW == 1, W1, NA) )
    
    
    # colMeans(du)
    # cor(du %>% select(A1, B1, C1, W1, RA, RB, RC, RW) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(A, B, C, W)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = coefAB
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "5A"
  
  
  
  
  
  # ~ DAG 5B -----------------------------
  
  # same DAG as 5A, but with A*W interaction
  if ( .p$dag_name == "5B" ) {
    
    du = data.frame( A1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     W1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    coefAB = 2
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = coefAB*A1 + 1.6*W1 + 2*A1*W1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*W1) ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RC = 1, 
              RW = 1 )
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              W = ifelse(RW == 1, W1, NA) )
    
    
    # colMeans(du)
    # cor(du %>% select(A1, B1, C1, W1, RA, RB, RC, RW) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(A, B, C, W)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = NA
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "5B"
  
  
  
  # ~ DAG 6A -----------------------------
  
  # C1 -> RC -> RB (monotone)
  # A -> Y1
  # B1 <- W -> RB
  # intuitively, I expect MAR methods to work even though it's MNAR
  
  if ( .p$dag_name == "6A" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ), 
                     W1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ), 
                     A1 = rbinom( n = .p$N, 
                                  size = 1, 
                                  prob = 0.5 ) )  
    
    coef1 = 2
    coef2 = 1.6
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          # need EMM for unadjusted CCA to be biased
                          mean = coef1*A1 + coef2*W1 + 1*A1*W1 ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.4 + 0.4*W1 ),
              
              RC = rbinom(n = 1,
                          size = 1,
                          prob = expit(0 + 3*C1) ),
              
              RW = 1,
              RA = 1)
    
    # monotone missingness: conditionally overwrite indicator
    du$RB[ du$RC == 0 ] = 0
    # missmap(du %>% select(A, B, C))
    
    du = du %>% rowwise() %>%
      mutate( A = A1,
              W = W1,
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, W1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A, W)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A + C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 + C1"
      
      beta = NA
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "6A"
  
  
  
  
  
  # ~ DAG 6B -----------------------------
  
  # same as 6A, but no W
  
  if ( .p$dag_name == "6B" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ), 

                     A1 = rbinom( n = .p$N, 
                                  size = 1, 
                                  prob = 0.5 ) )  
    
    coef1 = 2
    coef2 = 1.6
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = coef1*A1 ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RC = rbinom(n = 1,
                          size = 1,
                          prob = expit(0 + 3*C1) ),
              
              RA = 1)
    
    # monotone missingness: conditionally overwrite indicator
    du$RB[ du$RC == 0 ] = 0
    # missmap(du %>% select(A, B, C))
    
    du = du %>% rowwise() %>%
      mutate( A = A1,
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A + C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 + C1"
      
      beta = NA
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "6B"
  
  
  
  # ~ DAG 1-AY ----
  # taken from CCvMI and modified to use binary vars
  # also no confounder Q for simplicity
  # also monotone missingness for ease of implementing IPMW, but this shouldn't matter
  if ( .p$dag_name %in% c( "1-AY" ) ) {
    
    du = data.frame( W1 = rbinom( n = .p$N,
                                 size = 1,
                                 prob = 0.5),
                     
                     A1 = rbinom( n = .p$N,
                                  size = 1,
                                  prob = 0.5) )
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                         mean = 3 * W1 + A1 ),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*W1 + A1) ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*W1 + A1) ),
              
              RW = 1,
              
              R = RA * RB)
    

    # monotone missingness: conditionally overwrite indicator
    du$RB[ du$RA == 0 ] = 0
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              W = W1)
    
    colMeans(du) 
    cor(du %>% select(A1, B1, W1, RB) )
    # missmap(du %>% select(A, B, W))
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, W, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = 1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }
  
  
  
  
  
  
  # ~ Finish generating data ----------------
  
  # marginal prevalences
  colMeans(du, na.rm = TRUE)
  
  # can't use llist here in case some list elements are NULL (e.g., di_ours)
  # return( llist(du,
  #               di_std,
  #               di_ours,
  #               exclude_from_imp_model,
  #               form_string,
  #               gold_form_string,
  #               coef_of_interest,
  #               beta) )
  
  return( list(du = du,
               di = di,
               exclude_from_imp_model = exclude_from_imp_model,
               form_string = form_string,
               gold_form_string = gold_form_string,
               beta = beta) )
  
  
  
}

# for testing:
if (FALSE){
  .p = data.frame(
    model = "OLS",
    #coef_of_interest = "A:C",  # "(Intercept)" or "A"
    coef_of_interest = "A",
    N = c(1000),
    
    dag_name = "1J"
  )
  
  sim_data(.p)
}




# ANALYSIS METHOD FNS ---------------------

# miss_method: CC, MI, gold, IPW
# model: logistic, OLS
fit_regression = function(form_string,
                          model,
                          coef_of_interest,
                          miss_method,
                          ps_string = NA,
                          du,
                          imps) {
  
  
  # # test only
  # form_string = CC_adj_form_string
  # model = "OLS"
  # miss_method = "CC"
  
  
  #if ( miss_method %in% c("CC", "IPW") ) dat = dm
  if ( miss_method == "MI" ) dat = imps
  if ( miss_method %in% c("gold", "CC", "IPW") ) dat = du
  
  # ~ CC and gold std  ---------------------
  if ( miss_method %in% c("CC", "gold") ) {
    
    if ( model == "OLS" ) {
      #if ( nuni(dat$Y) <= 2 ) stop("You have a binary outcome but are fitting OLS with model-based SEs; need to allow robust SEs")
      
      mod = lm( eval( parse(text = form_string) ),
                data = dat )
      
    }
    
    if ( model == "logistic" ) {
      mod = glm( eval( parse(text = form_string) ),
                 data = dat,
                 family = binomial(link = "logit") )
    }
    
    bhats = coef(mod)
    CI = as.numeric( confint(mod)[coef_of_interest,] )
    
    return( list( stats = data.frame( bhat = as.numeric( bhats[coef_of_interest] ),
                                      bhat_lo = CI[1],
                                      bhat_hi = CI[2],
                                      bhat_width = CI[2] - CI[1] ) ) )
  }
  
  # ~ MI  ---------------------
  if ( miss_method == "MI" ) {
    
    
    if ( model == "OLS" ) {
      #if ( nuni( complete(imps,1)$Y) <= 2 ) stop("You have a binary outcome but are fitting OLS with model-based SEs; need to allow robust SEs")
      
      # works for both MICE and Amelia
      mod = with(imps,
                 lm( eval( parse(text = form_string) ) ) )
    }
    
    
    if ( model == "logistic" ) {
      mod = with(imps,
                 glm( eval( parse(text = form_string) ),
                      family = binomial(link = "logit") ) )
    }
    
    #browser()
    mod_pool = mice::pool(mod)
    summ = summary(mod_pool, conf.int = TRUE)
    
    bhat_lo = summ$`2.5 %`[ summ$term == coef_of_interest ]
    bhat_hi = summ$`97.5 %`[ summ$term == coef_of_interest ]
    
    return( list( stats = data.frame( bhat = mod_pool$pooled$estimate[ mod_pool$pooled$term == coef_of_interest ],
                                      bhat_lo = bhat_lo,
                                      bhat_hi = bhat_hi,
                                      bhat_width = bhat_hi - bhat_lo ) ) )
  }
  
  
  
  
  # ~ IPW  ---------------------
  
  if ( miss_method == "IPW" ) {
    
    #@THIS IMPLEMENTATION IS QUICK AND DIRTY. FOR EXAMPLE:
    #  - Inference ignores the fact that weights are estimated.
    #  - I trimmed the weights in an ad hoc way.
    #  - I didn't standardize the weights. 
    #  - I didn't customize the PS model predictors to the DAG; just regressed on 
    #      all non-missing vars.
    
    if ( is.na(ps_string) ) stop("Need to provide ps_string")
    
    
    if ( model == "OLS" ) {
      
      if ( nuni(dat$Y) <= 2 ) stop("You have a binary outcome but are fitting OLS with model-based SEs; need to allow robust SEs")
      
      # could replace with patternwise approach, as in Sun & ETT
      # i.e., for pattern where (A,Y) are missing, regress 1{being in that pattern} on C alone
      # for pattern where A alone is missing, regress 1{being in that pattern} on C,Y
      
      mod_PS = glm( eval( parse(text = ps_string) ),
                    data = dat,
                    family = binomial(link = "logit") )
      
      
      dat$pR = predict(mod_PS, type = "response")
      # # trim extreme PS probabilities
      # dat$p_miss = pmin( dat$p_miss, 0.90 )
      # dat$p_miss = pmax( dat$p_miss, 0.10 )
      
      # ### TEMP: USE TRUE P(R=1) (FOR DAG)
      # if ( dat$dag_name == "I" ) dat$pR = expit(dat$C1)
      # if ( dat$dag_name == "III" ) dat$pR = expit(2*dat$C1)
      # ### END TEMP
      
      
      # estimated IP weights
      # note that the latter weights, for incomplete cases, don't actually get used because 
      #  only CCs (wrt the analysis variables) are included in regression below
      dat$wt = dat$R*( 1/dat$pR ) + (1-dat$R)*( 1/(1-dat$pR) )
      
      # # sanity checks
      # summary(dat$wt)
      # # should be close to 2:
      # sum(dat$wt)/nrow(dat)
      
      
      # PS-weighted outcome model
      mod_wls = lm( eval( parse(text = form_string) ),
                    data = dat,
                    weights = wt)
      
      #browser()
      
      # sanity check: WLS should always be consistent for this (DAG V)
      if (FALSE) {
        summary(dat$pR)
        
        lm( Y1 ~ A1,
            data = dat)
        
        lm( Y ~ A,
            data = dat)
      }
      
      
      # sanity check: reproduce WLS using simpler expectations
      if (FALSE) {
        
        # temporarily dichotomize A
        dat2 = dat
        dat2$A = dat2$A > 0
        dat2$A1 = dat2$A1 > 0
        
        # make weighted Y = [ 1/(P(R=1 | C)) ] * Y
        dat2$Yw = (1/dat2$pR) * dat2$Y
        # **critical step:
        dat2$Yw[ is.na(dat2$Y) ] = 0  #@@IS THIS RIGHT??
        any(is.na(dat2$Yw))
        
        dcc = dat2 %>% filter(R==1)
        any(is.na(dcc$Yw))
        
        # 0. gold std with dichotomized A
        lm(Y1 ~ A1, data = dat2)
        
        # 1. 
        ( my_beta = mean( dcc$Yw[dcc$A == 1] ) - mean( dcc$Yw[dcc$A == 0] ) )
        # 2. (agrees with 1)
        lm( Yw ~ A, dat = dcc)  # if A binary, agrees with my_beta above
        
        # **1b. using dat instead of dcc
        # **agrees with gold std, and 3-4, if you set dat2$Yw[ is.na(dat2$Y) ] = 0 
        ( my_beta = meanNA( dat2$Yw[dat2$A == 1] ) - meanNA( dat2$Yw[dat2$A == 0] ) )
        # 2b. agrees with 1b
        lm( Yw ~ A, dat = dat2)
        
        # 3. refit mod_wls with dichotomized A
        # also, this is close to gold std
        lm( eval( parse(text = form_string) ),
            data = dat,
            weights = wt)
        # 4. (agrees with 3, but not 1 & 2)
        # and doesn't agree with 3 for DAG III
        lm( Y ~ A, dat = dcc, weights = wt)  
        
        # 5. different from everything else
        lm( (1 / sum(1 / dcc$pR)) * Yw ~ A, dat = dcc)
        # end sanity check
        
      }
      
      
      
      
      # to get robust SEs:
      mod_hc0 = my_ols_hc0(coefName = "A",
                           ols = mod_wls)
      
      
      
    }
    
    if ( model == "logistic" ) {
      stop("Logistic case not implemented")
    }
    
    return( list( stats = data.frame( bhat = mod_hc0$est,
                                      bhat_lo = mod_hc0$lo,
                                      bhat_hi = mod_hc0$hi,
                                      bhat_width = mod_hc0$hi - mod_hc0$lo ) ) ) 
  }
  
  
  # ~ IPW-custom  ---------------------
  
  # customized for each DAG
  if ( miss_method == "IPW-custom" ) {
    
    if ( p$dag_name == "6A" ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RC == 0 & du$RB == 0 ] = 3
      dat$M[ du$RC == 1 & du$RB == 0 ] = 2
      dat$M[ du$RC == 1 & du$RB == 1 ] = 1

      # complete cases for analysis model that includes C
      dc = dat %>% filter(!is.na(B) & !is.na(C))
      
      # probability of R=3 pattern (RC = RY = 0)
      ( m_R3 = glm( I(M == 3) ~ A + W, data = dat ) )
      
      
      # conditional probability of R=2 pattern (RC = 1, RY = 0)
      ( m_R2 = glm( I(M == 2) ~ A + W + C, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R3) * (1 - phat_R2)
      
      
      # Marginal p(R=1)
      mnum = mean(dat$M == 1)
      
      dc$wt = mnum / phat_R1
      
      
      # PS-weighted outcome model
      mod_wls = lm( eval( parse(text = form_string) ),
                    data = dc,
                    weights = wt)
      # to get robust SEs:
      mod_hc0 = my_ols_hc0(coefName = "A",
                           ols = mod_wls)
      
      # # from Ross code
      # # exactly equivalent estimates and CIs :)
      # ( mod = geeglm( eval(parse(text = form_string)), data = dc,
      #                 weights = wt, id=1:nrow(dc), corstr="independence") )
      # # from Ross code
      # rd = coef(mod)[[2]]
      # lcl = tidy(mod, conf.int=T, exp = F)[[2,"conf.low"]]
      # ucl = tidy(mod, conf.int=T, exp = F)[[2,"conf.high"]]
    
      
    }  else if ( p$dag_name == "1-AY" ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RA == 0 & du$RB == 0 ] = 3
      dat$M[ du$RA == 1 & du$RB == 0 ] = 2
      dat$M[ du$RA == 1 & du$RB == 1 ] = 1
      
      # complete cases for analysis model 
      dc = dat %>% filter(!is.na(B) & !is.na(A))
      
      # probability of R=3 pattern (RA = RB = 0)
      # this model will be wrong because also depends on A
      ( m_R3 = glm( I(M == 3) ~ W, data = dat ) )
      
      
      # conditional probability of R=2 pattern (RA = 1, RB = 0)
      # this model is right
      ( m_R2 = glm( I(M == 2) ~ A + W, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R3) * (1 - phat_R2)
      
      
      # Marginal p(R=1)
      mnum = mean(dat$M == 1)
      
      dc$wt = mnum / phat_R1
      
      
      # PS-weighted outcome model
      ( mod_wls = lm( eval( parse(text = form_string) ),
                    data = dc,
                    weights = wt) )
      # to get robust SEs:
      mod_hc0 = my_ols_hc0(coefName = "A",
                           ols = mod_wls)
      
      # sanity check: check equivalence with my iPad simplification
      if (FALSE) {
        
        ### Need to have PS estimate for every row, not just CCs (for later expressions)
        # refit models so they work with cfactual vars (makes no difference except in calling predict)
        ( m_R3_b = glm( I(M == 3) ~ W1, data = dat ) )
        ( m_R2_b = glm( I(M == 2) ~ A1 + W1, data = dat %>% filter(M <= 2) ) )
        phat_R3_b = predict(newdata = dat, object = m_R3_b, type = "response")
        phat_R2_b = predict(newdata = dat, object = m_R2_b, type = "response")
        dat$phat_R1_b = (1 - phat_R3_b) * (1 - phat_R2_b)
        
        
        ### Estimate E[Y(a)]
        a = 1
        
        dat$phat_R1 = NA; dat$phat_R1[ dat$M == 1 ] = phat_R1
    
        #### Eq. (1)
        dat$wt = 0  # for the incomplete cases
        dat$wt[ dat$M == 1 ] = 1 / dat$phat_R1[ dat$M == 1 ]
        # Eq (1) on Overleaf: E[Y(a)]
        ( EY1 = sum( dat$wt * dat$B1 * (dat$A1 == 1) ) /  sum( dat$wt * (dat$A1 == 1) ) )
        ( EY0 = sum( dat$wt * dat$B1 * (dat$A1 == 0) ) /  sum( dat$wt * (dat$A1 == 0) ) )
        EY1 - EY0
        # yes!!! agrees with mod_wls :D (both are wrong)
        
        ### Eq. (9)
        # first need to get correct model for p(R=1 | B1, A1, C1)
        mod_R = glm( I(M == 1) ~ B1 + A1 + W1, data = dat )
        
        ### with completely wrong weights: DOESN'T MATCH
        wt_fake = sample(dc$wt)
        ( mod_wls = lm( eval( parse(text = form_string) ),
                        data = dc,
                        weights = wt_fake) )
        
      }
      
    } else if ( p$dag_name == "1-AY" ) {
        
        dat = du
        
        # make pattern indicator, M
        dat$M = NA
        dat$M[ du$RA == 0 & du$RB == 0 ] = 3
        dat$M[ du$RA == 1 & du$RB == 0 ] = 2
        dat$M[ du$RA == 1 & du$RB == 1 ] = 1
        
        # complete cases for analysis model 
        dc = dat %>% filter(!is.na(B) & !is.na(A))
        
        # probability of R=3 pattern (RA = RB = 0)
        # this model will be wrong because also depends on A
        ( m_R3 = glm( I(M == 3) ~ W, data = dat ) )
        
        
        # conditional probability of R=2 pattern (RA = 1, RB = 0)
        # this model is right
        ( m_R2 = glm( I(M == 2) ~ A + W, data = dat %>% filter(M <= 2) ) )
        
        # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
        #  be analyzed)
        phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
        phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
        phat_R1 = (1 - phat_R3) * (1 - phat_R2)
        
        
        # Marginal p(R=1)
        mnum = mean(dat$M == 1)
        
        dc$wt = mnum / phat_R1
        
        
        # PS-weighted outcome model
        ( mod_wls = lm( eval( parse(text = form_string) ),
                        data = dc,
                        weights = wt) )
        # to get robust SEs:
        mod_hc0 = my_ols_hc0(coefName = "A",
                             ols = mod_wls)
      
      
    } else if ( p$dag_name == "3B-bin-mono" ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RC == 0 & du$RA == 0 & du$RB == 0 ] = 4
      dat$M[ du$RC == 1 & du$RA == 0 & du$RB == 0 ] = 3
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 0 ] = 2
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 1 ] = 1
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(A) & !is.na(C) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R4 = glm( I(M == 4) ~ 1, data = dat ) )
      ( m_R3 = glm( I(M == 3) ~ C, data = dat %>% filter(M <= 3) ) )
      ( m_R2 = glm( I(M == 2) ~ C + A, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R4 = predict(newdata = dc, object = m_R4, type = "response")
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R4) * (1 - phat_R3) * (1 - phat_R2)
      
      
      # Marginal p(R=1)
      mnum = mean(dat$M == 1)
      
      dc$wt = mnum / phat_R1
      
      
      # PS-weighted outcome model
      ( mod_wls = lm( eval( parse(text = form_string) ),
                      data = dc,
                      weights = wt) )
      # to get robust SEs:
      mod_hc0 = my_ols_hc0(coefName = "A",
                           ols = mod_wls)
      
      
    } else {
      stop("IPW-custom not implemented for that DAG")
    }

    
    return( list( stats = data.frame( bhat = mod_hc0$est,
                                      bhat_lo = mod_hc0$lo,
                                      bhat_hi = mod_hc0$hi,
                                      bhat_width = mod_hc0$hi - mod_hc0$lo ) ) ) 
  }
  
}


# MODEL-FITTING HELPERS ---------------------

# ~~ Wrapper Fn to Safely Run a Method -------

# See note at the beginning of this script
#  this fn automatically runs the method within a tryCatch loop, 
#  records any error messages, and writes a results row to global var rep.res whether 
#  or not the estimation method threw an error

# Important: this fn works if method.fn() returns multiple rows
# BUT in that case, it assumes that the CIs are shared for all rows of that method

# expects global vars: all.errors, rep.res
# directly edits res via superassignment
run_method_safe = function( method.label,
                            method.fn,
                            .rep.res ) {
  
  cat( paste("\n run_method_safe flag 1: about to try running method", method.label) )
  
  
  tryCatch({
    
    method.output = method.fn()
    new.rows = method.output$stats
    
    if ( !exists("new.rows") ) {
      cat("\n\n**** Object new.rows didn't exist for method", method.label)
      cat("\nHere is method.output:\n")
      print(method.output)
    }
    
    cat( paste("\n run_method_safe flag 2: done calling method.fn() for", method.label) )
    
    error = NA
    
  }, error = function(err) {
    # needs to be superassignment because inside the "error" fn
    error <<- err$message
    
    # only need one variable in the blank dataframe since bind_rows below
    #  will fill in the rest
    new.rows <<- data.frame( method = method.label )
    
  })
  
  new.rows = new.rows %>% add_column( method = method.label, .before = 1 )
  new.rows$overall.error = error
  
  
  if ( nrow(.rep.res) == 0 ) .rep.res = new.rows else .rep.res = bind_rows(.rep.res, new.rows)
  return(.rep.res) 
  
}



# coefName: which coefficient to report
# ols: the OLS model from lm()
my_ols_hc0 = function( coefName, ols ){
  
  ( bhat.ols = coef(ols)[coefName] )
  
  # heteroskedasticity-consistent robust SEs:
  (se.hc0 = sqrt( sandwich::vcovHC( ols, type="HC0")[coefName, coefName] ) )
  
  tcrit = qt(.975, df = ols$df.residual)
  t = as.numeric( abs(bhat.ols / se.hc0) )
  
  return( data.frame(
    est = bhat.ols,
    se = se.hc0,
    lo = bhat.ols - tcrit * se.hc0,
    hi = bhat.ols + tcrit * se.hc0,
    pval =  2 * ( 1 - pt(t, df = ols$df.residual ) ) ) )
}

# correlation of proxies in du only
# i.e., not variables with "R" or "1"
proxy_cor = function(.du) {
  temp = .du %>%
    select( -contains("R"), -contains("1") ) %>% select(sort(names(.)))
  round( cor(temp, use = "pairwise.complete.obs" ), 3 )
}

# correlation of counterfactuals in du only
cfact_cor = function(.du) {
  
  temp = .du %>%
    select( contains("1") ) %>% select(sort(names(.)))
  round( cor(temp), 3 )
}

# average correlation across imputations
# works for both Amelia and mice
imps_cor = function(.imps){
  
  if ( class(.imps) == "amelia" ) {
    m = length(.imps$imputations)
    
    cors = lapply( X = 1:m,
                   FUN = function(.m) cor( imps_am_std$imputations[[.m]] ) )
  }
  
  if ( class(.imps) == "mids" ) {
    m = .imps$m
    
    cors = lapply( X = 1:m,
                   FUN = function(.m) cor(complete(.imps, .m) ) )
    
  }
  
  ( mean_cor_imps = Reduce("+", cors) / length(cors) )
  round(mean_cor_imps, 3)
}


# SMALL GENERIC HELPERS ---------------------

# quickly look at results when running doParallel locally
srr = function(.rep.res) {
  
  
  cat("\n")
  print( .rep.res %>%
           mutate_if(is.numeric, function(x) round(x,2)) )
  cat("\n")
  
}



# check CI coverage
covers = function( truth, lo, hi ) {
  return( as.numeric( (lo <= truth) & (hi >= truth) ) )
}

# get names of dataframe containing a string
namesWith = function(pattern, dat){
  names(dat)[ grepl(pattern = pattern, x = names(dat) ) ]
}


# quick length(unique)
nuni = function(x) {
  length(unique(x))
}

# (re-)install package AND its dependencies
# useful for stupid rstan issues in which rstan itself it UTD but not its dependencies
# https://stackoverflow.com/questions/21010705/update-a-specific-r-package-and-its-dependencies
instPkgPlusDeps <- function(pkg, install = FALSE,
                            which = c("Depends", "Imports", "LinkingTo"),
                            inc.pkg = TRUE) {
  stopifnot(require("tools")) ## load tools
  ap <- available.packages() ## takes a minute on first use
  ## get dependencies for pkg recursively through all dependencies
  deps <- package_dependencies(pkg, db = ap, which = which, recursive = TRUE)
  ## the next line can generate warnings; I think these are harmless
  ## returns the Priority field. `NA` indicates not Base or Recommended
  pri <- sapply(deps[[1]], packageDescription, fields = "Priority")
  ## filter out Base & Recommended pkgs - we want the `NA` entries
  deps <- deps[[1]][is.na(pri)]
  ## install pkg too?
  if (inc.pkg) {
    deps = c(pkg, deps)
  }
  ## are we installing?
  if (install) {
    install.packages(deps)
  }
  deps ## return dependencies
}

# example
# instPkgPlusDeps("fields")


# CLUSTER FNS ---------------------------------------------------------------

# DO NOT CHANGE THE INDENTATION IN THE BELOW OR ELSE SLURM 
#  WILL SILENTLY IGNORE THE BATCH COMMANDS DUE TO EXTRA WHITESPACE!!
# DO NOT CHANGE THE INDENTATION IN THE BELOW OR ELSE SLURM 
#  WILL SILENTLY IGNORE THE BATCH COMMANDS DUE TO EXTRA WHITESPACE!!
sbatch_skeleton <- function() {
  return(
    "#!/bin/bash
#################
#set a job name  
#SBATCH --job-name=JOBNAME
#################  
#a file for job output, you can check job progress
#SBATCH --output=OUTFILE
#################
# a file for errors from the job
#SBATCH --error=ERRORFILE
#################
#time you think you need; default is one hour
#SBATCH --time=JOBTIME
#################
#quality of service; think of it as job priority
#SBATCH --qos=QUALITY
#################
#submit to both owners and normal partition
#SBATCH -p normal,owners
#################
#number of nodes you are requesting
#SBATCH --nodes=NODENUMBER
#################
#memory per node; default is 4000 MB
#SBATCH --mem=MEMPERNODE
#you could use --mem-per-cpu; they mean what we are calling cores
#################
#get emailed about job BEGIN, END, and FAIL
#SBATCH --mail-type=MAILTYPE
#################
#who to send email to; please change to your email
#SBATCH  --mail-user=USER_EMAIL
#################
#task to run per node; each node has 16 cores
#SBATCH --ntasks=TASKS_PER_NODE
#################
#SBATCH --cpus-per-task=CPUS_PER_TASK
#now run normal batch commands

ml load v8
ml load R/4.3.2
R -f PATH_TO_R_SCRIPT ARGS_TO_R_SCRIPT")
}



generateSbatch <- function(sbatch_params,
                           runfile_path = NA,
                           run_now = F) {
  
  #sbatch_params is a data frame with the following columns
  #jobname: string, specifies name associated with job in SLURM queue
  #outfile: string, specifies the name of the output file generated by job
  #errorfile: string, specifies the name of the error file generated by job
  #jobtime: string in hh:mm:ss format, max (maybe soft) is 48:00:00 
  #specifies the amoung of time job resources should be allocated
  #jobs still running after this amount of time will be aborted
  #quality: kind of like priority, normal works
  #node_number, integer: the number of nodes (computers w/16 cpus each) to allocate 
  #mem_per_node, integer: RAM, in MB, to allocate to each node
  #mailtype, string: ALL, BEGIN, END, FAIL: what types of events should you be notified about via email
  #user_email string: email address: email address to send notifications
  #tasks_per_node: integer, number of tasks, you should probably use 1
  #cpus_per_task: integer, 1-16, number of cpus to use, corresponds to number of available cores per task
  #path_to_r_script: path to r script on sherlock
  #args_to_r_script: arguments to pass to r script on command line
  #write_path: where to write the sbatch file
  #server_sbatch_path: where sbatch files will be stored on sherlock
  #runfile_path is a string containing a path at which to write an R script that can be used to run
  #the batch files generated by this function. 
  #if NA, no runfile will be written
  #run_now is a boolean specifying whether batch files should be run as they are generated
  
  sbatches <- list()
  if (!is.na(runfile_path)) {
    outfile_lines <- c(paste0("# Generated on ",  Sys.time()))
  }
  for (sbatch in 1:nrow(sbatch_params) ) {
    gen_batch <- sbatch_skeleton()
    #set job name
    if (is.null(sbatch_params$jobname[sbatch])) { 
      gen_batch <- gsub("JOBNAME", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("JOBNAME", sbatch_params$jobname[sbatch], gen_batch) 
    }
    #set outfile name
    if (is.null(sbatch_params$outfile[sbatch])) { 
      gen_batch <- gsub("OUTFILE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("OUTFILE", sbatch_params$outfile[sbatch], gen_batch) 
    }
    #set errorfile name
    if (is.null(sbatch_params$errorfile[sbatch])) { 
      gen_batch <- gsub("ERRORFILE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("ERRORFILE", sbatch_params$errorfile[sbatch], gen_batch) 
    }
    #set jobtime
    if (is.null(sbatch_params$jobtime[sbatch])) { 
      gen_batch <- gsub("JOBTIME", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("JOBTIME", sbatch_params$jobtime[sbatch], gen_batch) 
    }
    #set quality
    if (is.null(sbatch_params$quality[sbatch])) { 
      gen_batch <- gsub("QUALITY", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("QUALITY", sbatch_params$quality[sbatch], gen_batch) 
    }
    #set number of nodes
    if (is.null(sbatch_params$node_number[sbatch])) { 
      gen_batch <- gsub("NODENUMBER", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("NODENUMBER", sbatch_params$node_number[sbatch], gen_batch) 
    }
    #set memory per node
    if (is.null(sbatch_params$mem_per_node[sbatch])) { 
      gen_batch <- gsub("MEMPERNODE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("MEMPERNODE", sbatch_params$mem_per_node[sbatch], gen_batch) 
    }
    #set requested mail message types
    if (is.null(sbatch_params$mailtype[sbatch])) { 
      gen_batch <- gsub("MAILTYPE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("MAILTYPE", sbatch_params$mailtype[sbatch], gen_batch) 
    }
    #set email at which to receive messages
    if (is.null(sbatch_params$user_email[sbatch])) { 
      gen_batch <- gsub("USER_EMAIL", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("USER_EMAIL", sbatch_params$user_email[sbatch], gen_batch) 
    }
    #set tasks per node
    if (is.null(sbatch_params$tasks_per_node[sbatch])) { 
      gen_batch <- gsub("TASKS_PER_NODE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("TASKS_PER_NODE", sbatch_params$tasks_per_node[sbatch], gen_batch) 
    }
    #set cpus per task
    if (is.null(sbatch_params$cpus_per_task[sbatch])) { 
      gen_batch <- gsub("CPUS_PER_TASK", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("CPUS_PER_TASK", sbatch_params$cpus_per_task[sbatch], gen_batch) 
    }
    #set path to r script
    if (is.null(sbatch_params$path_to_r_script[sbatch])) { 
      gen_batch <- gsub("PATH_TO_R_SCRIPT", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("PATH_TO_R_SCRIPT", sbatch_params$path_to_r_script[sbatch], gen_batch) 
    }
    #set args to r script
    if (is.null(sbatch_params$args_to_r_script[sbatch])) { 
      gen_batch <- gsub("ARGS_TO_R_SCRIPT", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("ARGS_TO_R_SCRIPT", sbatch_params$args_to_r_script[sbatch], gen_batch) 
    }
    
    #write batch file
    if (is.null(sbatch_params$write_path[sbatch])) { 
      cat(gen_batch, file = paste0("~/sbatch_generated_at_", gsub(" |:|-", "_", Sys.time()) ), append = F)
    } else { 
      cat(gen_batch, file = sbatch_params$write_path[sbatch], append = F)
    }
    
    if (!is.na(sbatch_params$server_sbatch_path[sbatch])) {
      outfile_lines <- c(outfile_lines, paste0("system(\"sbatch ", sbatch_params$server_sbatch_path[sbatch], "\")"))
    } 
    sbatches[[sbatch]] <- gen_batch
  }
  if (!is.na(runfile_path)) {
    cat(paste0(outfile_lines, collapse = "\n"), file = runfile_path)
  }
  if(run_now) { system(paste0("R -f ", runfile_path)) } 
  
  return(sbatches)
}


# looks at results files to identify sbatches that didn't write a file
# .max.sbatch.num: If not passed, defaults to largest number in actually run jobs.

sbatch_not_run = function(.results.singles.path,
                          .results.write.path,
                          .name.prefix,
                          .max.sbatch.num = NA ) {
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # extract job numbers
  sbatch.nums = as.numeric( unlist( lapply( strsplit( keepers, split = "_"), FUN = function(x) x[5] ) ) )
  
  # check for missed jobs before the max one
  if ( is.na(.max.sbatch.num) ) .max.sbatch.num = max(sbatch.nums)
  all.nums = 1 : .max.sbatch.num
  missed.nums = all.nums[ !all.nums %in% sbatch.nums ]
  
  # give info
  print( paste("The max job number is: ", max(sbatch.nums) ) )
  print( paste( "Number of jobs that weren't run: ",
                ifelse( length(missed.nums) > 0, length(missed.nums), "none" ) ) )
  
  if( length(missed.nums) > 0 ) {
    setwd(.results.write.path)
    write.csv(missed.nums, "missed_job_nums.csv")
  }
  
  return(missed.nums)
  
}

# STITCH RESULTS FILES -------------------------------------

# given a folder path for results and a common beginning of file name of results files
#   written by separate workers in parallel, stitch results files together into a
#   single csv.

stitch_files = function(.results.singles.path, .results.stitched.write.path=.results.singles.path,
                        .name.prefix, .stitch.file.name="stitched_model_fit_results.csv") {
  
  # .results.singles.path = "/home/groups/manishad/MRM/sim_results/long"
  # .results.stitched.write.path = "/home/groups/manishad/MRM/sim_results/overall_stitched"
  # .name.prefix = "long_results"
  # .stitch.file.name="stitched.csv"
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # grab variable names from first file
  names = names( read.csv(keepers[1] )[-1] )
  
  # read in and rbind the keepers
  tables <- lapply( keepers, function(x) read.csv(x, header= TRUE) )
  s <- do.call(rbind, tables)
  
  names(s) = names( read.csv(keepers[1], header= TRUE) )
  
  if( is.na(s[1,1]) ) s = s[-1,]  # delete annoying NA row
  write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )
  return(s)
}


# quickly look at results from job #1

res1 = function() {
  setwd("/home/groups/manishad/SAPH/long_results")
  rep.res = fread("long_results_job_1_.csv")
  srr()
  
  cat("\nErrors by method:" )
  print( rep.res %>% group_by(method) %>%
           summarise(prop.error = mean( overall.error != "" ) ) )
  
  #table(rep.res$overall.error)
  
  cat("\n\nDim:", dim(rep.res))
  cat("\n\nReps completed:", nrow(rep.res)/nuni(rep.res$method))
}



make_agg_data = function(s) {
  
  correct.order = c("gold", "CC", "Am-std", "Am-ours", "MICE-std", "MICE-ours", "MICE-ours-pred")
  s$method = factor(s$method, levels = correct.order)
  
  # fill in beta (where it's NA) using gold-standard
  beta_emp = s %>% filter(method == "gold") %>%
    group_by(scen.name) %>%
    summarise(beta = meanNA(bhat)) 
  as.data.frame(beta_emp)
  
  s2 = s
  
  s2 = s2 %>% rowwise() %>%
    mutate( beta = ifelse( !is.na(beta),
                           beta,
                           beta_emp$beta[ beta_emp$scen.name == scen.name ] ) )
  
  # sanity check
  as.data.frame( s2 %>% group_by(dag_name, coef_of_interest) %>%
                   summarise(beta[1]) )
  # end of filling in beta
  
  
  aggo = s2 %>% group_by(dag_name, coef_of_interest, method) %>%
    summarise( 
      reps = n(),
      Bhat = meanNA(bhat),
      BhatBias = meanNA(bhat - beta),
      BhatLo = meanNA(bhat_lo),
      BhatHi = meanNA(bhat_hi),
      BhatFail = mean(is.na(bhat)),
      BhatRMSE = sqrt( meanNA( (bhat - beta)^2 ) ),
      BhatCover = meanNA( covers(truth = beta,
                                 lo = bhat_lo,
                                 hi = bhat_hi) ) ) %>%
    arrange() %>%
    mutate_if(is.numeric, function(x) round(x,2)) 
  
  return(aggo)
  
}


wrangle_agg_data = function(.aggo) {
  
  agg = .aggo
  
  # recode variables
  agg$coef_of_interest_pretty = agg$coef_of_interest
  agg$coef_of_interest_pretty[ agg$dag_name %in% c("1B", "1D") & agg$coef_of_interest == "(Intercept)"] = "E[A]"
  agg$coef_of_interest_pretty[ agg$dag_name %in% c("1Fb") & agg$coef_of_interest == "(Intercept)"] = "E[B]" 
  agg$coef_of_interest_pretty[ agg$coef_of_interest == "A"] = "E[B | A]" 
  # check it
  agg %>% group_by(dag_name, coef_of_interest) %>% 
    summarise(unique(coef_of_interest_pretty))
  
  agg$dag_name_pretty = agg$dag_name
  agg$dag_name_pretty[ agg$dag_name == "1B" ] = "DAG (a)"
  agg$dag_name_pretty[ agg$dag_name == "1D" ] = "DAG (b)"
  agg$dag_name_pretty[ agg$dag_name == "1Fb" ] = "DAG (c)"
  agg$dag_name_pretty[ agg$dag_name == "1J" ] = "DAG (d)"
  
  agg$method_pretty = as.character(agg$method)
  agg$method_pretty[ agg$method == "gold" ] = "Benchmark"
  agg$method_pretty[ agg$method == "CC" ] = "Complete-case"
  agg$method_pretty[ agg$method == "Am-std" ] = "Amelia (standard)"
  agg$method_pretty[ agg$method == "Am-ours" ] = "Amelia (m-backdoor)"
  agg$method_pretty[ agg$method == "MICE-std" ] = "MICE (standard)"
  agg$method_pretty[ agg$method == "MICE-ours" ] = "MICE (m-backdoor)"
  
  return(agg)
}


# INPUT/OUTPUT FNS ----------------------------------------------


# one or both dirs can be NA
my_ggsave = function(name,
                     .plot = last_plot(),
                     .width,
                     .height,
                     .results.dir = results.dir,
                     .overleaf.dir = overleaf.dir) {
  
  dirs = c(.results.dir, .overleaf.dir)
  dirIsNA = sapply(dirs, is.na)
  validDirs = dirs[ !dirIsNA ]
  
  
  for ( dir in validDirs ) {
    setwd(dir)
    ggsave( name,
            plot = .plot,
            width = .width,
            height = .height,
            device = "pdf" )
  }
}

# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
# expects "study" to be a global var
update_result_csv = function( name,
                              .section = NA,
                              .results.dir = results.dir,
                              .overleaf.dir = overleaf.dir.stats,
                              value = NA,
                              print = FALSE ) {
  
  # if either is NULL, it just won't be included in this vector
  dirs = c(.results.dir, .overleaf.dir)
  
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(.section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  for (.dir in dirs) {
    
    setwd(.dir)
    
    if ( "stats_for_paper.csv" %in% list.files() ) {
      res <<- read.csv( "stats_for_paper.csv",
                        stringsAsFactors = FALSE,
                        colClasses = rep("character", 3 ) )
      
      # if this entry is already in the results file, overwrite the
      #  old one
      if ( all(name %in% res$name) ) res[ res$name %in% name, ] <<- new.rows
      else res <<- rbind(res, new.rows)
    }
    
    if ( ! "stats_for_paper.csv" %in% list.files() ) {
      res <<- new.rows
    }
    
    write.csv( res, 
               "stats_for_paper.csv",
               row.names = FALSE,
               quote = FALSE )
    
  }  # end "for (.dir in dirs)"
  
  
  if ( print == TRUE ) {
    View(res)
  }
  
}



# stands for "wipe results"
wr = function(){
  setwd(results.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
  setwd(overleaf.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
}

# stands for "view results"
vr = function(){
  setwd(results.dir)
  View( read.csv("stats_for_paper.csv") )
}


# GENERIC SMALL HELPERS ----------------------------------------------

# quick mean with NAs removed
meanNA = function(x){
  mean(x, na.rm = TRUE)
}

# quick median with NAs removed
medNA = function(x){
  median(x, na.rm = TRUE)
}

# quick length(unique) equivalent
uni = function(x){
  length(unique(x))
}

expit = function(x) 1 / (1+exp(-x))

logit = function(x) log(x/(1-x))


# ~ Handling strings -----------------

# return strings containing anything in pattern vector
stringsWith = function(pattern, x){
  # make regex expression 
  patterns = paste(pattern, collapse="|")
  x[ grepl(pattern = patterns, x = x)]
}
# stringsWith( pattern = c("dog", "cat"),
#  x = c("dogcat", "horse", "cat", "lion") )


# return indices of strings containing anything in pattern vector
whichStrings = function(pattern, x){
  patterns = paste(pattern, collapse="|")
  grepl(pattern = pattern, x = x)
}

names_with = function(.dat, .pattern) {
  names(.dat)[ grepl(pattern = .pattern, x = names(.dat) ) ]
}



# ~ Calculate simple stats -----------------

quick_ci = function( est, var ) {
  c( est - qnorm(.975) * sqrt(var),
     est + qnorm(.975) * sqrt(var) )
}

quick_pval = function( est, var ) {
  2 * ( 1 - pnorm( abs( est / sqrt(var) ) ) )
}


# ~ Formatting stats as strings -----------------

# round while keeping trailing zeroes
my_round = function(x, digits) {
  formatC( round( x, digits ), format='f', digits=digits )
}

format_CI = function( lo, hi, digits ) {
  paste( "[", my_round( lo, digits ), ", ", my_round( hi, digits ), "]", sep="" )
}

# round down to nearest integer
format_sval = function( sval, digits ) {
  if ( as.character(sval) == "--" ) return("Already NS")
  else if ( as.character(sval) == "Not possible" ) return("Not possible")
  else return( as.character( round(sval, digits) ) )
  #else return( floor(sval) )
}

format_pval = function(p) {
  if (p >= 0.01) return( my_round( p, 2 ) )
  if (p < 0.01 & p > 10^-5 ) return( formatC( p, format = "e", digits = 0 ) )
  if ( p < 10^-5 ) return("< 1e-05")
}





