
expit = function(p) exp(p) / (1 + exp(p))
logit = function(p) p / (1-p)





# DATA-GENERATION FNS ---------------------

sim_data = function(.p) {
  
  #if (.p$model != "OLS" ) stop("Only handles model OLS for now")
  
  # ~ DAG 1A -----------------------------
  
  if ( .p$dag_name == "1A" ) {
    
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
                          mean = coef1*A1 + coef2*C1 + A1*C1),
              
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.3 + 0.4*A1 ),
              
              RC = rbinom(n = 1, size = 1, prob = 0.5) )
    
    # monotone missingness: conditionally overwrite RC
    du$RC[ du$RB == 0 ] = 0 
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
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
    # NEW!!
    # add any needed interactions to imputation dataset
    #di = add_interactions_from_formula(df = di, formula_str = form_string)
    
  }  # end of .p$dag_name == "1A"
  
  
  
  # ~ DAG 1B -----------------------------
  
  if ( .p$dag_name == "1B" ) {
    
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
                          mean = coef1*A1 + coef2*C1 + A1*C1),
              
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.3 + 0.4*A1 ),
              
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
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "1B"
  
  
  
  
  
  # ~ DAG 2A -----------------------------
  
  #@ has normal vars
  
  # 2025-04-21: edited to have only B1 binary
  
  if ( .p$dag_name == "2A" ) {
    
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
                          mean = coef1*A1 + coef1*C1 + A1*C1),
              
              RA = 1,
              
              RB = 1,
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
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
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "2A"
  
  
  
  
  # ~ DAG 3A -----------------------------
  
  if ( .p$dag_name == "3A" ) {
    
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
                          mean = coef1*A1),
              
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
    
  }  # end of .p$dag_name == "3A"
  
  
  
  # ~ DAG 3B -----------------------------
  # same as above, but has C1 -> Y1 edge
  
  if ( .p$dag_name == "3B" ) {
    
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
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "3B"
  
  
  # ~ DAG 3C -------------------------------------------------
  
  if ( .p$dag_name == "3C" ) {
    
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
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
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
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "3C"
  
  
  # ~ DAG 3D -----------------------------
  if ( .p$dag_name == "3D" ) {
    
    # designed for using Ross' rjags code, so A needs to be complete
    # has A*C interaction
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
                          mean = coef1*A1 + coef1*C1 + coef1*A1*C1),
              
              RA = 1,
              
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
    cor(du %>% select( A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "3D"
  
  
  # ~ DAG 3E -----------------------------
  if ( .p$dag_name == "3E" ) {
    
    # designed for using Ross' rjags code, so A needs to be complete
    # has A*C interaction
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
                          mean = coef1*A1 + coef1*C1 + coef1*A1*C1),
              
              RA = 1,
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ) )
    
    
    
    # monotonicity
    du$RB[ du$RC == 0] = 0
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    
    colMeans(du)
    cor(du %>% select( A1, B1, C1, RB, RC) )
    
    
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
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "3E"
  
  
  
  
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
                          mean = coefWB*W1 + 2.6*C1 + W1*C1),
              
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
    
    
    # make dataset for imputation (exclude W because it's latent)
    di = du %>% select(A, B, C)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = NA
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL
    }
    
  }  # end of .p$dag_name == "4A"
  
  
  
  
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
    
    
    # make dataset for imputation (exclude W: treat as latent)
    di = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = NA
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "6A"
  
  
  
  
  
  # ~ DAG 7A -----------------------------
  
  if ( .p$dag_name == "7A" ) {
    
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
                          mean = coef1*A1 + coef1*C1 + A1*C1 ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(0 + 3*A1) ),
              
              RA = rbinom(n = 1,
                          size = 1,
                          prob = expit(0 + 3*A1) ),
              
              RC = 1 )
    
    # monotone missingness RA -> RB
    du$RB[ du$RA == 0 ] = 0
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    # missmap(du %>% select(A, B, C))
    
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
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = NA
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "7A"
  
  
  # ~ DAG 7B -----------------------------
  
  # same as 7A, but with "direction" of monotone missingness reversed (i.e., RY -> RA here)
  
  if ( .p$dag_name == "7B" ) {
    
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
                          mean = coef1*A1 + coef1*C1 + A1*C1 ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(0 + 3*A1) ),
              
              RA = rbinom(n = 1,
                          size = 1,
                          prob = expit(0 + 3*A1) ),
              
              RC = 1 )
    
    # monotone missingness RA -> RB
    du$RA[ du$RB == 0 ] = 0
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    # missmap(du %>% select(A, B, C))
    
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
      form_string = "B ~ A * C"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1"
      
      beta = NA
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "7B"
  
  
  # ~ DAG 12A -----------------------------
  
  if ( .p$dag_name == "12A" ) {
    
    du = data.frame( X1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ), # complete variable included only to please Amelia
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     # called C^1_2 in IAI log; this is also in conditioning set
                     D1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = 0.5),
              #prob = expit(-1 + 3*C1) ),
              
              # add edge from C1 -> B1
              B1 = rnorm( n = 1,
                          mean = 2*A1 + 2*D1 + A1*D1),
              
              RX = 1,
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = 0.75 ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.75 ),
              
              RD = RB,
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(0 + 3*D1) ) )
    
    
    # monotone missingness: conditionally overwrite indicator
    du$RB[ du$RA == 0 ] = 0
    du$RD = du$RB
    du$RC[ du$RD == 0 ] = 0
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              D = ifelse(RD == 1, D1, NA),
              X = X1)
    
    #missmap( du %>% select(A, B, C, D) )
    colMeans(du)
    cor(du %>% select(A1, B1, C1, D1, RA, RB, RC, RD) )
    #cor(du$B1, du$RC)  # this is the key path that must be strong enough to elicit bias
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(A, B, C, D, X)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A * C * D"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1 * D1"
      
      beta = 2
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "12A"
  
  
  
  
  
  # ~ DAG 12B -----------------------------
  
  # like 12A, but nonmonotone
  
  if ( .p$dag_name == "12B" ) {
    
    du = data.frame( X1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ), # complete variable included only to please Amelia
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     # called C^1_2 in IAI log; this is also in conditioning set
                     D1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = 0.5),
              #prob = expit(-1 + 3*C1) ),
              
              # add edge from C1 -> B1
              B1 = rnorm( n = 1,
                          mean = 2*A1 + 2*D1 + A1*D1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RD = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(0 + 3*D1) ),
              
              RX = 1 )
    
    
    # # monotone missingness: conditionally overwrite indicator
    # du$RB[ du$RA == 0 ] = 0
    # du$RD = du$RB
    # du$RC[ du$RD == 0 ] = 0
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              D = ifelse(RD == 1, D1, NA),
              X = X1)
    
    #missmap( du %>% select(A, B, C, D) )
    colMeans(du)
    cor(du %>% select(A1, B1, C1, D1, RA, RB, RC, RD) )
    #cor(du$B1, du$RC)  # this is the key path that must be strong enough to elicit bias
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(A, B, C, D, X)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A * C * D"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1 * D1"
      
      beta = 2
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "12B"
  
  
  
  
  
  # ~ DAG 12C -----------------------------
  
  # like 12A, but partially nonmonotone
  
  if ( .p$dag_name == "12C" ) {
    
    du = data.frame( X1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ), # complete variable included only to please Amelia
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     # called C^1_2 in IAI log; this is also in conditioning set
                     D1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = 0.5),
              #prob = expit(-1 + 3*C1) ),
              
              # add edge from C1 -> B1
              B1 = rnorm( n = 1,
                          mean = 2*A1 + 2*D1 + A1*D1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RD = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(0 + 3*D1) ),
              
              RX = 1 )
    
    
    # partially monotone missingness: conditionally overwrite indicator
    du$RD[ du$RB == 1 ] = 1
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              D = ifelse(RD == 1, D1, NA),
              X = X1)
    
    #missmap( du %>% select(A, B, C, D) )
    colMeans(du)
    cor(du %>% select(A1, B1, C1, D1, RA, RB, RC, RD) )
    #cor(du$B1, du$RC)  # this is the key path that must be strong enough to elicit bias
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(A, B, C, D, X)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A * C * D"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1 * C1 * D1"
      
      beta = 2
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "12C"
  
  
  
  
  # ~ DAG 13A -----------------------------
  
  if ( .p$dag_name == "13A" ) {
    
    # C1 is unrelated to everything; only here to avoid having all-NA rows
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     A1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = 2*A1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*A1) ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.75 ),
              
              RC = 1)
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    
    # monotone missingness: conditionally overwrite indicator
    du$RA[ du$RB == 0 ] = 0
    
    colMeans(du)
    cor(du %>% select(A1, B1, RA, RB) )
    
    
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
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = 2
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "13A"
  
  
  
  
  
  
  
  
  # ~ DAG 13B -----------------------------
  
  if ( .p$dag_name == "13B" ) {
    
    # C1 is unrelated to everything; only here to avoid having all-NA rows
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     A1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = 2*A1),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*A1) ),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = 0.75 ),
              
              RC = 1)
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA) )
    
    
    # monotone missingness: conditionally overwrite indicator
    du$RA[ du$RB == 0 ] = 0
    
    colMeans(du)
    cor(du %>% select(A1, B1, RA, RB) )
    
    
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
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = 2
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "13B"
  
  
  
  
  
  
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
  
  
  cat("\n ***** fit_regression flag1: form_string")
  cat(form_string)
  
  
  # # test only
  # form_string = CC_adj_form_string
  # model = "OLS"
  # miss_method = "CC"
  
  # prediction for conditional mean E[Y | A=1, ...] for some choice of covariate values
  # only used for certain DAGs and used to check if these means could be wrong even though CATE is correct
  EY_prediction = NA
  
  if ( miss_method == "MI" ) dat = imps
  if ( miss_method %in% c("gold", "CC", "IPW", "IPW-nm") ) dat = du
  
  # ~ CC and gold std  ---------------------
  if ( miss_method %in% c("CC", "gold") ) {
    
    if ( model == "OLS" ) {
      mod = lm( eval( parse(text = form_string) ),
                data = dat )
      
    }
    
    if ( model == "logistic" ) {
      mod = glm( eval( parse(text = form_string) ),
                 data = dat,
                 family = binomial(link = "logit") )
    }
    
    if ( model == "log" ) {
      mod = glm( eval( parse(text = form_string) ),
                 data = dat,
                 family = binomial(link = "log") )
    }
    
    
    bhats = coef(mod)
    
    
    # robust SEs in case we're fitting OLS with binary outcome
    mod_hc0 = my_ols_hc0(coefName = coef_of_interest,
                         ols = mod)
    
    # previous: model-based CIs
    #CI = as.numeric( confint(mod)[coef_of_interest,] )
    
    
    # just for debugging
    if ( p$dag_name %in% c("9A", "9A-bin", "9B", "9B-bin") & miss_method == "gold" ) {
      EY_prediction = as.numeric( predict(object = mod, newdata = data.frame(A1 = 1,
                                                                             C1 = 1,
                                                                             D1 = 1) ) )
    }
    # just for debugging
    if ( p$dag_name %in% c("9A", "9A-bin", "9B", "9B-bin") & miss_method == "CC" ) {
      EY_prediction = as.numeric( predict(object = mod, newdata = data.frame(A = 1,
                                                                             C = 1,
                                                                             D = 1) ) )
    }
    
    
    return( list( stats = data.frame( bhat = as.numeric( bhats[coef_of_interest] ),
                                      
                                      bhat_lo = mod_hc0$lo,
                                      bhat_hi = mod_hc0$hi,
                                      bhat_width = mod_hc0$hi - mod_hc0$lo,
                                      
                                      EY_prediction = EY_prediction ) ) )
  }
  
  # ~ IPW-nm  ---------------------
  
  if ( miss_method == "IPW-nm" ) {
    
    ### Make wtd data by running generalized Sun fns
    # Ensure ID exists
    if (!"id" %in% names(dat)) {
      dat$id <- 1:nrow(dat)
    }
    
    # Identify variables to be used in the analysis
    analysis_vars <- all.vars( as.formula(form_string) )
    
    # Add pattern indicators
    data_with_patterns <- create_pattern_indicators(dat, analysis_vars)
    
    # # Print pattern distribution
    # message("Distribution of missingness patterns:")
    # print(table(data_with_patterns$M))
    
    # Run Bayesian model for missingness patterns
    message("Running Bayesian model for missingness patterns...")
    jags_results <- run_missingness_model(data_with_patterns, analysis_vars)
    message("Done running Bayesian model for missingness patterns.")
    
    weighted_data <- as.data.frame( calculate_ipmw_weights2(jags_results, data_with_patterns) )
    
    message("IPMW weight summary:")
    print(summary(weighted_data$ipmw))
    ### end of generalized Sun fns
    
    
    if ( model == "OLS" ) {
      # PS-weighted outcome model
      ( mod_wls = lm( eval( parse(text = form_string) ),
                      data = weighted_data,
                      weights = ipmw) )
      
    }
    
    if ( model == "logistic" ) {
      ( mod_wls = glm( eval( parse(text = form_string) ),
                       data = weighted_data,
                       weights = ipmw,
                       family = binomial(link = "logit") ) )
    }
    
    if ( model == "log" ) {
      ( mod_wls = glm( eval( parse(text = form_string) ),
                       data = weighted_data,
                       weights = ipmw,
                       family = binomial(link = "log") ) )
    }
    
    # to get robust SEs:
    mod_hc0 = my_ols_hc0(coefName = "A",
                         ols = mod_wls)
    
    
    return( list( stats = data.frame( bhat = mod_hc0$est,
                                      bhat_lo = mod_hc0$lo,
                                      bhat_hi = mod_hc0$hi,
                                      bhat_width = mod_hc0$hi - mod_hc0$lo ) ) ) 
    
  }
  
  # ~ MI  ---------------------
  if ( miss_method == "MI" ) {
  
    
    if ( model == "OLS" ) {

      
      # need to use different strategies for fitting model to each dataset depending on whether it's 
      #  from MICE/Amelia or genloc
      if ( class(imps) %in% c("mids", "amelia") ) {
        
        # works for both MICE and Amelia
        mod = with(imps,
                   glm( eval( parse(text = form_string) ) ) )
        

      } else {
        # for a plain list
        mod = lapply(imps, function(d) lm( as.formula(form_string), data = d ) )
      }
      
      

    }
    
    
    if ( model == "logistic" ) {
      
      
      if ( class(imps) %in% c("mids", "amelia") ) {
        
        # works for both MICE and Amelia
        mod = with(imps,
                   glm( eval( parse(text = form_string) ),
                        family = binomial(link = "logit") ) )
        
        
      } else {
        # for a plain list
        mod = lapply( imps, function(d) glm( as.formula(form_string),
                                            data = d,
                                            family = binomial(link = "logit") ) )
      }
      
      

    }
    
    
    if ( model == "log" ) {

      
      if ( class(imps) %in% c("mids", "amelia") ) {
        
        # works for both MICE and Amelia
        mod = with(imps,
                   glm( eval( parse(text = form_string) ),
                        family = binomial(link = "log") ) )
        
        
      } else {
        # for a plain list
        mod = lapply( imps, function(d) glm( as.formula(form_string),
                                             data = d,
                                             family = binomial(link = "log") ) )
      }
      
      
    }
    
    
    mod_pool = mice::pool(mod)
    summ = summary(mod_pool, conf.int = TRUE)
    
    # deal with the following irritating issue:
    #  for MICE, we turn all binaries into factors to avoid imputing them as continuous
    #  but that means the coefs in resulting lm model will have names like "A1" instead of "A"
    #  this fn updates coef_of_interest to match
    coef_of_interest_recoded = match_coef_names_to_mice(coef_of_interest = coef_of_interest,
                                                        pooled_terms = as.character(mod_pool$pooled$term))
    
    bhat_lo = summ$`2.5 %`[ summ$term == coef_of_interest_recoded ]
    bhat_hi = summ$`97.5 %`[ summ$term == coef_of_interest_recoded ]
    
    # also return the summarized string
    if( class(imps) == "mids" ) imp_methods = summarize_mice_methods(imps$method) else imp_methods = NA
    
    return( list( stats = data.frame( bhat = mod_pool$pooled$estimate[ mod_pool$pooled$term == coef_of_interest_recoded ],
                                      bhat_lo = bhat_lo,
                                      bhat_hi = bhat_hi,
                                      bhat_width = bhat_hi - bhat_lo,
                                      imp_methods = imp_methods) ) )
  }
  
  
  # ~ IPW-custom  ---------------------
  
  # Note: this ignores the model argument passed to fit_regression
  
  # customized for each DAG
  if ( miss_method == "IPW-custom" ) {
    
    stop("This code isn't UTD! DAG names may have changed, and there are lots of unused DAGs in here.")
    
    
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
      ( m_R2 = glm( I(M == 2) ~ C * A, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R4 = predict(newdata = dc, object = m_R4, type = "response")
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R4) * (1 - phat_R3) * (1 - phat_R2)
      
      
    } else if ( p$dag_name == "3C-bin-mono" ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RC == 0 & du$RA == 0 & du$RB == 0 ] = 4
      dat$M[ du$RC == 0 & du$RA == 0 & du$RB == 1 ] = 3
      dat$M[ du$RC == 0 & du$RA == 1 & du$RB == 1 ] = 2
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 1 ] = 1
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(A) & !is.na(C) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R4 = glm( I(M == 4) ~ 1, data = dat ) )
      ( m_R3 = glm( I(M == 3) ~ B, data = dat %>% filter(M <= 3) ) )
      ( m_R2 = glm( I(M == 2) ~ B * A, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R4 = predict(newdata = dc, object = m_R4, type = "response")
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R4) * (1 - phat_R3) * (1 - phat_R2)
      
      
    } else if ( p$dag_name %in% c("7A", "7A-bin") ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RC == 1 & du$RA == 0 & du$RB == 0 ] = 3
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 0 ] = 2
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 1 ] = 1
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(A) & !is.na(C) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R3 = glm( I(M == 3) ~ C, data = dat %>% filter(M <= 3) ) )
      ( m_R2 = glm( I(M == 2) ~ C * A, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R3) * (1 - phat_R2)
      
    } else if ( p$dag_name %in% c("7C-bin" ) ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RC == 1 & du$RA == 0 & du$RB == 0 ] = 3
      dat$M[ du$RC == 1 & du$RA == 0 & du$RB == 1 ] = 2
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 1 ] = 1
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(A) & !is.na(C) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R3 = glm( I(M == 3) ~ C, data = dat %>% filter(M <= 3) ) )
      ( m_R2 = glm( I(M == 2) ~ C * B, data = dat %>% filter(M <= 2) ) )
      # c.f. truth
      # glm( I(M == 3) ~ C1 * A1 * B1, data = dat %>% filter(M <= 3) )
      # glm( I(M == 2) ~ C1 * A1 * B1, data = dat %>% filter(M <= 2) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R3) * (1 - phat_R2)
      
    } else if ( p$dag_name %in% c("7B", "7B-bin" ) ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RC == 1 & du$RA == 0 & du$RB == 0 ] = 3
      dat$M[ du$RC == 1 & du$RA == 0 & du$RB == 1 ] = 2
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 1 ] = 1
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(A) & !is.na(C) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R3 = glm( I(M == 3) ~ C, data = dat %>% filter(M <= 3) ) )
      ( m_R2 = glm( I(M == 2) ~ C * B, data = dat %>% filter(M <= 2) ) )
      # c.f. truth
      # glm( I(M == 3) ~ C1 * A1 * B1, data = dat %>% filter(M <= 3) )
      # glm( I(M == 2) ~ C1 * A1 * B1, data = dat %>% filter(M <= 2) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R3) * (1 - phat_R2)
      
      
    } else if ( p$dag_name == "8A" ) {
      
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RC == 0 & du$RW == 0 & du$RB == 0 ] = 4
      dat$M[ du$RC == 1 & du$RW == 0 & du$RB == 0 ] = 3
      dat$M[ du$RC == 1 & du$RW == 1 & du$RB == 0 ] = 2
      dat$M[ du$RC == 1 & du$RW == 1 & du$RB == 1 ] = 1
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(W) & !is.na(C) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R4 = glm( I(M == 4) ~ A, data = dat ) )
      ( m_R3 = glm( I(M == 3) ~ A * C, data = dat %>% filter(M <= 3) ) )
      ( m_R2 = glm( I(M == 2) ~ A * C * W, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R4 = predict(newdata = dc, object = m_R4, type = "response")
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R4) * (1 - phat_R3) * (1 - phat_R2)
      
    } else if ( p$dag_name == "8B" ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RB == 0 & du$RW == 0 & du$RC == 0 ] = 4
      dat$M[ du$RB == 1 & du$RW == 0 & du$RC == 0 ] = 3
      dat$M[ du$RB == 1 & du$RW == 1 & du$RC == 0 ] = 2
      dat$M[ du$RB == 1 & du$RW == 1 & du$RC == 1 ] = 1
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(W) & !is.na(C) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R4 = glm( I(M == 4) ~ A, data = dat ) )
      ( m_R3 = glm( I(M == 3) ~ A * B, data = dat %>% filter(M <= 3) ) )
      ( m_R2 = glm( I(M == 2) ~ A * B * W, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R4 = predict(newdata = dc, object = m_R4, type = "response")
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R4) * (1 - phat_R3) * (1 - phat_R2)
      
    } else if ( p$dag_name %in% c("9A", "9A-bin", "10A" ) ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RC == 0 & du$RA == 0 & du$RB == 0 & du$RD == 0 ] = 5
      dat$M[ du$RC == 1 & du$RA == 0 & du$RB == 0 & du$RD == 0 ] = 4
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 0 & du$RD == 0 ] = 3
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 1 & du$RD == 0 ] = 2
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 1 & du$RD == 1 ] = 1
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(A) & !is.na(C) & !is.na(D) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R5 = glm( I(M == 5) ~ 1, data = dat ) )
      ( m_R4 = glm( I(M == 4) ~ C, data = dat %>% filter(M <= 4) ) )
      ( m_R3 = glm( I(M == 3) ~ C * A, data = dat %>% filter(M <= 3) ) )
      # THIS ONE COULD BE MISSPECIFIED FOR 9A BECAUSE B IS CONTINUOUS:
      ( m_R2 = glm( I(M == 2) ~ C * A * B, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R5 = predict(newdata = dc, object = m_R5, type = "response")
      phat_R4 = predict(newdata = dc, object = m_R4, type = "response")
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R5) * (1 - phat_R4) * (1 - phat_R3) * (1 - phat_R2)
      
      
    } else if ( p$dag_name %in% c("9B", "9B-bin", "10B") ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RC == 0 & du$RA == 0 & du$RB == 0 & du$RD == 0 ] = 5
      dat$M[ du$RC == 1 & du$RA == 0 & du$RB == 0 & du$RD == 0 ] = 4
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 0 & du$RD == 0 ] = 3
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 0 & du$RD == 1 ] = 2
      dat$M[ du$RC == 1 & du$RA == 1 & du$RB == 1 & du$RD == 1 ] = 1
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(A) & !is.na(C) & !is.na(D) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R5 = glm( I(M == 5) ~ 1, data = dat ) )
      ( m_R4 = glm( I(M == 4) ~ C, data = dat %>% filter(M <= 4) ) )
      ( m_R3 = glm( I(M == 3) ~ C * A, data = dat %>% filter(M <= 3) ) )
      ( m_R2 = glm( I(M == 2) ~ C * A * D, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R5 = predict(newdata = dc, object = m_R5, type = "response")
      phat_R4 = predict(newdata = dc, object = m_R4, type = "response")
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R5) * (1 - phat_R4) * (1 - phat_R3) * (1 - phat_R2)
      
    } # ~ DAG 3D-bin -----------------------------
    if ( .p$dag_name == "3D-bin" ) {
      
      # designed for using Ross' rjags code, so A needs to be complete
      # has A*C interaction
      du = data.frame( C1 = rbinom( n = .p$N,
                                    size = 1, 
                                    prob = 0.5 ) ) 
      
      
      coef1 = 2
      coef2 = 1.6
      
      
      # 2025-04-11c - Same coef strengths as original, but add A*C interaction
      du = du %>% rowwise() %>%
        mutate( A1 = rbinom( n = 1,
                             size = 1,
                             prob = expit(-1 + 3*C1) ),
                
                # add edge from C1 -> B1
                B1 = rnorm( n = 1,
                            mean = coef1*A1 + coef1*C1 + coef1*A1*C1),
                
                RA = 1,
                
                # RA = rbinom( n = 1,
                #              size = 1,
                #              prob = expit(-1 + 3*C1) ),
                
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
      cor(du %>% select( A1, B1, C1, RB, RC) )
      
      
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
        form_string = "B ~ A * C"
        
        # gold-standard model uses underlying variables
        gold_form_string = "B1 ~ A1 * C1"
        
        beta = coef1
        
        # custom predictor matrix for MICE-ours-pred
        exclude_from_imp_model = NULL # B is in target law
      }
      
    }  # end of .p$dag_name == "3D-bin"
    
    
    # ~ DAG 3D-bin-mono -----------------------------
    if ( .p$dag_name == "3D-bin-mono" ) {
      
      # designed for using Ross' rjags code, so A needs to be complete
      # has A*C interaction
      du = data.frame( C1 = rbinom( n = .p$N,
                                    size = 1, 
                                    prob = 0.5 ) ) 
      
      
      coef1 = 2
      coef2 = 1.6
      
      # 2025-04-11c: Same coefs, but A*C interaction
      du = du %>% rowwise() %>%
        mutate( A1 = rbinom( n = 1,
                             size = 1,
                             prob = expit(-1 + 3*C1) ),
                
                # add edge from C1 -> B1
                B1 = rnorm( n = 1,
                            mean = coef1*A1 + coef1*C1 + coef1*A1*C1),
                
                RA = 1,
                
                # RA = rbinom( n = 1,
                #              size = 1,
                #              prob = expit(-1 + 3*C1) ),
                
                RB = rbinom( n = 1,
                             size = 1,
                             prob = expit(-1 + 3*C1) ),
                
                RC = rbinom( n = 1,
                             size = 1,
                             prob = expit(-1 + 3*C1) ) )
      
      # # 2025-04-11b - Same but stronger coefs
      # du = du %>% rowwise() %>%
      #   mutate( A1 = rbinom( n = 1,
      #                        size = 1,
      #                        prob = expit(-1 + 3*C1) ),
      #           
      #           # add edge from C1 -> B1
      #           B1 = rnorm( n = 1,
      #                       mean = coef1*A1 + 3*C1 + 3*A1*C1),
      #           
      #           RA = 1,
      #           
      #           # RA = rbinom( n = 1,
      #           #              size = 1,
      #           #              prob = expit(-1 + 3*C1) ),
      #           
      #           RB = rbinom( n = 1,
      #                        size = 1,
      #                        prob = expit(-2 + 4*C1) ),
      #           
      #           RC = rbinom( n = 1,
      #                        size = 1,
      #                        prob = expit(-1 + 3*C1) ) )
      
      # # original version: with C1 -> A edge
      # du = du %>% rowwise() %>%
      #   mutate( A1 = rbinom( n = 1,
      #                        size = 1,
      #                        prob = expit(-1 + 3*C1) ),
      # 
      #           # add edge from C1 -> B1
      #           B1 = rnorm( n = 1,
      #                       mean = coef1*A1 + coef1*C1),
      # 
      #           RA = 1,
      # 
      #           # RA = rbinom( n = 1,
      #           #              size = 1,
      #           #              prob = expit(-1 + 3*C1) ),
      # 
      #           RB = rbinom( n = 1,
      #                        size = 1,
      #                        prob = expit(-1 + 3*C1) ),
      # 
      #           RC = rbinom( n = 1,
      #                        size = 1,
      #                        prob = expit(-1 + 3*C1) ) )
      
      # monotonicity
      du$RB[ du$RC == 0] = 0
      
      
      du = du %>% rowwise() %>%
        mutate( A = ifelse(RA == 1, A1, NA),
                B = ifelse(RB == 1, B1, NA),
                C = ifelse(RC == 1, C1, NA) )
      
      
      colMeans(du)
      cor(du %>% select( A1, B1, C1, RB, RC) )
      
      
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
        form_string = "B ~ A * C"
        
        # gold-standard model uses underlying variables
        gold_form_string = "B1 ~ A1 * C1"
        
        beta = coef1
        
        # custom predictor matrix for MICE-ours-pred
        exclude_from_imp_model = NULL # B is in target law
      }
      
    }  # end of .p$dag_name == "3D-bin"
    else if ( p$dag_name %in% c("11A" ) ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RC == 0 & du$RB == 0 ] = 3
      dat$M[ du$RC == 0 & du$RB == 1 ] = 2
      dat$M[ du$RC == 1 & du$RB == 1 ] = 1
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(A) & !is.na(C) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R3 = glm( I(M == 3) ~ 1, data = dat ) )
      ( m_R2 = glm( I(M == 2) ~ B, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R3) * (1 - phat_R2)
      
      
    } else if ( p$dag_name %in% c("12A" ) ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RA == 0 & du$RB == 0 & du$RD == 0 & du$RC == 0 ] = 4
      dat$M[ du$RA == 1 & du$RB == 0 & du$RD == 0 & du$RC == 0 ] = 3
      dat$M[ du$RA == 1 & du$RB == 1 & du$RD == 1 & du$RC == 0 ] = 2
      dat$M[ du$RA == 1 & du$RB == 1 & du$RD == 1 & du$RC == 1 ] = 1
      
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(A) & !is.na(C) & !is.na(D) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R4 = glm( I(M == 4) ~ 1, data = dat ) )
      ( m_R3 = glm( I(M == 3) ~ A, data = dat %>% filter(M <= 3) ) )
      ( m_R2 = glm( I(M == 2) ~ A * B * D, data = dat %>% filter(M <= 2) ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R4 = predict(newdata = dc, object = m_R4, type = "response")
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R4) * (1 - phat_R3) * (1 - phat_R2)
      
      
    } else if ( p$dag_name %in% c("13A", "13B" ) ) {
      
      dat = du
      
      # make pattern indicator, M
      dat$M = NA
      dat$M[ du$RA == 0 & du$RB == 0 ] = 3
      dat$M[ du$RA == 0 & du$RB == 1 ] = 2
      dat$M[ du$RA == 1 & du$RB == 1 ] = 1
      
      if ( any(is.na(dat$M)) ) stop("Something is wrong with pattern coding")
      
      
      # complete cases for analysis model 
      dc = dat %>% filter( !is.na(B) & !is.na(A) & !is.na(C) )
      
      # probability of each pattern under faulty MAR assumption
      ( m_R3 = glm( I(M == 3) ~ 1, data = dat, family = binomial(link = "logit") ) )
      ( m_R2 = glm( I(M == 2) ~ B, data = dat %>% filter(M <= 2), family = binomial(link = "logit") ) )
      
      # probability of R=1 (only need to predict this for complete cases, since they're the only ones to 
      #  be analyzed)
      phat_R3 = predict(newdata = dc, object = m_R3, type = "response")
      phat_R2 = predict(newdata = dc, object = m_R2, type = "response")
      phat_R1 = (1 - phat_R3) * (1 - phat_R2)
      
    } else {
      stop("IPW-custom not implemented for that DAG")
    }
    
    
    ### Fit PS-weighted outcome model
    # marginal p(R=1)
    mnum = mean(dat$M == 1)
    
    dc$wt = mnum / phat_R1
    
    if ( model == "OLS" ) {
      # PS-weighted outcome model
      ( mod_wls = lm( eval( parse(text = form_string) ),
                      data = dc,
                      weights = wt) )
      
    }
    
    if ( model == "logistic" ) {
      ( mod_wls = glm( eval( parse(text = form_string) ),
                       data = dc,
                       weights = wt,
                       family = binomial(link = "logit") ) )
    }
    
    if ( model == "log" ) {
      ( mod_wls = glm( eval( parse(text = form_string) ),
                       data = dc,
                       weights = wt,
                       family = binomial(link = "log") ) )
    }
    
    # to get robust SEs:
    mod_hc0 = my_ols_hc0(coefName = "A",
                         ols = mod_wls)
    
    ### Experiment
    if ( p$dag_name %in% c("9A", "9A-bin", "9B", "9B-bin") ) {
      EY_prediction = as.numeric( predict(object = mod_wls, newdata = data.frame(A = 1,
                                                                                 C = 1,
                                                                                 D = 1) ) )
    }
    
    return( list( stats = data.frame( bhat = mod_hc0$est,
                                      bhat_lo = mod_hc0$lo,
                                      bhat_hi = mod_hc0$hi,
                                      bhat_width = mod_hc0$hi - mod_hc0$lo,
                                      EY_prediction = EY_prediction ) ) ) 
  }
  
}


# IPMW-NM (AKA SUN'S IPW) -------------------------------------------------

# versions from 2025-04-15

#' Scale a variable ignoring missing values
#' @param data Dataframe
#' @param x Column name to scale
#' @return Scaled vector
scale_rmna <- function(data, x) {
  mean <- mean(data[[x]], na.rm = TRUE)
  sd <- sd(data[[x]], na.rm = TRUE)
  (data[[x]] - mean) / sd
}

#' Scale all variables in a dataset
#' @param data Dataframe with variables to scale
#' @param vars Vector of variable names to scale
#' @return Dataframe with scaled variables
scale_dataset <- function(data, vars) {
  result <- data
  for (v in vars) {
    result[[v]] <- scale_rmna(data, v)
  }
  return(result)
}

#' Create pattern indicators
#' @param data Dataframe
#' @param vars Variables to check for missingness
#' @return Dataframe with pattern indicators
create_pattern_indicators <- function(data, vars) {
  
  # Step 1: Create a subset of the data containing only the specified variables
  subset_data <- data[, vars, drop = FALSE]
  
  # Step 2: Create a binary matrix where TRUE means the value is missing (NA)
  is_missing <- is.na(subset_data)
  
  # Step 3: Convert each row to a unique pattern identifier
  # - First convert each row to a string representation (e.g., "TFFT")
  # - This creates a factor with one level per unique missingness pattern
  pattern_strings <- apply(is_missing, 1, function(row) paste(as.integer(row), collapse = ""))
  
  # Step 4: Convert the factor to numeric values starting from 1
  # - Pattern 1 means no missing values (all FALSE)
  # - Other patterns get assigned 2, 3, 4, etc.
  
  # First create a factor with the pattern strings
  pattern_factor <- factor(pattern_strings)
  
  # Get the "no missingness" pattern (all zeros)
  no_missing_pattern <- paste(rep(0, length(vars)), collapse = "")
  
  # Reorder the levels so the "no missingness" pattern is first
  if(no_missing_pattern %in% levels(pattern_factor)) {
    new_levels <- c(no_missing_pattern, 
                    levels(pattern_factor)[levels(pattern_factor) != no_missing_pattern])
    pattern_factor <- factor(pattern_factor, levels = new_levels)
  }
  
  # Convert to numeric
  pattern_numeric <- as.numeric(pattern_factor)
  
  # Step 5: Add the pattern indicator to the original dataset
  data$M <- pattern_numeric
  
  # Return the modified dataset
  return(data)
}

#' Prepare data for JAGS
#' @param data Dataframe with M pattern indicator
#' @param vars Variables in model
#' @return List formatted for JAGS
prepare_jags_data <- function(data, vars) {
  # Sort so complete cases (M=1) are first
  sorted <- data %>% arrange(M)
  
  # Create list for JAGS
  dat <- list(R = sorted$M)
  
  # Create matrix of variables, replacing NA with 0 (will be ignored in model)
  dat$L <- as.matrix(sorted[, vars])
  #dat$L[is.na(dat$L)] <- 0  # Replace NA with 0 instead of -9999
  #MM: changed back
  dat$L[is.na(dat$L)] <- -9999
  
  dat$N <- nrow(sorted)
  dat$f <- rep(1, dat$N)
  dat$Nc <- sum(sorted$M == 1)
  dat$onesc <- rep(1, dat$Nc)
  dat$c <- 10^-8
  
  return(dat)
}

#' Generate random initial values
#' @param pattern_counts Vector with counts of variables observed in each pattern
#' @return Random initial values for JAGS
generate_initial_values <- function(pattern_counts) {
  ints <- runif(length(pattern_counts), min = -4, max = -1)
  lim <- 0.06
  
  result <- c()
  for (i in 1:length(pattern_counts)) {
    result <- c(result, ints[i], runif(pattern_counts[i], min = -lim, max = lim))
  }
  
  return(result)
}



#' Create JAGS model for missingness patterns
#' @param num_patterns Number of patterns
#' @param vars_per_pattern List with variables available for each pattern
#' @return JAGS model as text
create_jags_model <- function(num_patterns, vars_per_pattern) {
  # Build JAGS model text with better handling of missing values
  model_text <- "
  model {
    for(i in 1:N) {
      f[i] ~ dbern(pmiss[i,R[i]]) # f = 1 for all obs 
      "
  
  # Add pattern probability equations
  param_idx <- 1
  for (p in 2:num_patterns) {
    model_text <- paste0(model_text, "\n      logit(pmiss[i, ", p, "]) <- g[", param_idx, "]")
    param_idx <- param_idx + 1
    
    # Only include variables if they're observed in this pattern
    if (length(vars_per_pattern[[p]]) > 0) {
      # Add coefficients for observed variables
      for (v_idx in vars_per_pattern[[p]]) {
        # Add a check for missing values (though we've replaced them with 0)
        model_text <- paste0(model_text, " + g[", param_idx, "]*L[i,", v_idx, "]")
        param_idx <- param_idx + 1
      }
    }
    # If no variables are observed, we've already added the intercept only
  }
  
  # Add calculation for pattern 1 probability
  model_text <- paste0( model_text, "
      
      # Calculate pattern 1 probability
      pmiss[i,1] <- 1" )
  
  # Subtract all other pattern probabilities
  for (p in 2:num_patterns) {
    model_text <- paste0(model_text, " - pmiss[i,", p, "]")
  }
  
  model_text <- paste0(model_text, "
    }
    
    # constraint
    for (j in 1:Nc) {
      onesc[j] ~ dbern(C[j]) 
      C[j] <- step(pmiss[j,1]-c)
    }
    
    # priors
    for(k in 1:", param_idx - 1, ") {
      g[k] ~ dnorm(0,1/100) # diffuse prior
    }
  }")
  
  return(model_text)
}



#' Run Bayesian estimation for missingness model 
#' @param data Dataset with pattern indicators
#' @param vars Variables for model
#' @return JAGS fit object
run_missingness_model <- function(data, vars) {
  
  vars = sort(vars)
  
  if (!"M" %in% names(data)) {
    working_data <- create_pattern_indicators(data, vars)
  } else {
    working_data = data
  }
  
  # # Make a copy to avoid modifying the original - save for future use in pkg
  # working_data <- data
  
  # Ensure all variables exist, adding dummy columns if needed
  for (v in vars) {
    if (!v %in% names(working_data)) {
      working_data[[v]] <- NA
    }
  }
  
  # Scale variables
  scaled_data <- scale_dataset(working_data, vars)
  
  # Get number of patterns
  num_patterns <- length(unique(scaled_data$M))
  message(paste("Number of patterns:", num_patterns))
  
  # Determine which variables are available for each pattern
  vars_per_pattern <- list()
  for (p in 1:num_patterns) {
    pattern_data <- scaled_data[scaled_data$M == p, ]
    
    # Check which variables have non-missing values for this pattern
    if (nrow(pattern_data) > 0) {
      observed_vars <- sapply(vars, function(v) {
        !all(is.na(pattern_data[[v]]))
      })

      vars_per_pattern[[p]] <- sort(which(observed_vars))
    } else {
      vars_per_pattern[[p]] <- integer(0)
    }
    
    message(paste("Pattern", p, "has", length(vars_per_pattern[[p]]), 
                  "observed variables:", 
                  ifelse(length(vars_per_pattern[[p]]) > 0, 
                         paste(vars_per_pattern[[p]], collapse=", "), 
                         "none (intercept-only model)")))
  }
  
  
  jags_data <- prepare_jags_data(scaled_data, vars)
  
  # Count parameters per pattern (intercept + coefficients for observed variables)
  var_counts <- sapply(2:num_patterns, function(p) {
    # Each pattern has at least an intercept
    1 + length(vars_per_pattern[[p]])
  })
  
  # Calculate total parameters
  total_params <- sum(var_counts)
  message(paste("Total parameters:", total_params))
  
  # Create initial values for 3 chains
  # generalized initialization fn
  # generates one SET of coefficients for each chain
  initialvals2 <- function(numcoeff, int_min = -5, int_max = -3) {
    # numcoeff: a numeric vector where each element represents the number of slope coefficients for that group.
    n_groups <- length(numcoeff)              # Determine the number of groups/patterns.
    
    #intercepts <- runif(n_groups, min = -4, max = -1)  # Generate one intercept per group.
    # *** If jags throws "invalid parent values", try making the intercepts more negative
    #  (maybe need to make them more negative as the number of patterns increases, to keep the complete-case probability from getting too low?)
    intercepts <- runif(n_groups, min = int_min, max = int_max)  # Initialize an empty vector for storing the initial values.
    lim <- 0.06                               # Limit for the slope coefficients.
    
    init_values <- c()  # Initialize an empty vector for storing the initial values.
    
    # Loop over each group and append its intercept and its slope coefficients.
    for (i in seq_along(numcoeff)) {
      group_vals <- c(intercepts[i], runif(numcoeff[i], min = -lim, max = lim))
      init_values <- c(init_values, group_vals)
    }
    
    return(init_values)
  }
  # calculate the number of slope coeffs for each pattern M>1
  nCoeff = unlist( lapply(vars_per_pattern[2: length(vars_per_pattern)], length) )
  # init has one set of start vals per chain
  nchains = 3 
  init <- lapply(1:nchains, function(i) list(g = initialvals2(nCoeff)))
  
  
  # Create JAGS model
  jags_model_text <- create_jags_model(num_patterns, vars_per_pattern)
  
  # Print model for debugging
  message("JAGS model:")
  cat(jags_model_text)
  
  # Create a temporary file for the JAGS model
  model_file <- tempfile()
  writeLines(jags_model_text, model_file)
  
  # Run JAGS with try-catch to provide better error messages
  jags_fit <- tryCatch({
    R2jags::jags(
      data = jags_data,
      inits = init,
      n.chains = 3,
      parameters.to.save = 'g',
      n.iter = 1000,
      n.burnin = 500,
      n.thin = 1,
      model.file = model_file
    )
  }, error = function(e) {
    message("JAGS error: ", e$message)
    message("Trying a simpler approach...")
    
    # If error occurs, try a simpler model with stronger priors
    simpler_model <- "
    model {
      for(i in 1:N) {
        f[i] ~ dbern(pmiss[i,R[i]])
        
        # Fixed probability for each pattern
        for (p in 2:16) {
          pmiss[i,p] <- theta[p-1]
        }
        
        # Complete cases probability
        pmiss[i,1] <- max(0.01, 1 - sum(theta[]))
      }
      
      # Constraint
      for (j in 1:Nc) {
        onesc[j] ~ dbern(C[j])
        C[j] <- step(pmiss[j,1]-c)
      }
      
      # Dirichlet prior on pattern probabilities
      theta[1:15] ~ ddirch(alpha[])
    }
    "
    writeLines(simpler_model, model_file)
    
    # Create alpha parameter for Dirichlet (all 1's for uniform)
    jags_data$alpha <- rep(1, num_patterns-1)
    
    # New init values
    init_simple <- list(
      list(theta = rep(1/(num_patterns*2), num_patterns-1)),
      list(theta = rep(1/(num_patterns*2), num_patterns-1)),
      list(theta = rep(1/(num_patterns*2), num_patterns-1))
    )
    
    # Try the simpler model
    tryCatch({
      R2jags::jags(
        data = jags_data,
        inits = init_simple,
        n.chains = 3,
        parameters.to.save = 'theta',
        n.iter = 1000,
        n.burnin = 500, 
        n.thin = 1,
        model.file = model_file
      )
    }, error = function(e2) {
      message("Second attempt also failed: ", e2$message)
      return(NULL)
    })
  })
  
  # Clean up the temporary file
  file.remove(model_file)
  
  if (is.null(jags_fit)) {
    stop("JAGS modeling failed after multiple attempts")
  }
  
  jags_results = list(
    fit = jags_fit,
    scaled_data = scaled_data,
    vars_per_pattern = vars_per_pattern,
    model_text = jags_model_text
  )
  #browser()
  
  return(list(
    fit = jags_fit,
    scaled_data = scaled_data,
    vars_per_pattern = vars_per_pattern,
    model_text = jags_model_text
  ))
}



# 2025-04-17 - Resolves negative p1's by calculating p1 for each draw, then averaging those

#' @param jags_results Results from run_missingness_model
#' @param data Original dataset
#' @param use_posterior_draws If TRUE, draws parameters from the posterior samples themselves. If FALSE, just uses posterior medians.
#' @return Dataset with IPMW weights
calculate_ipmw_weights2 <- function(jags_results, data, use_posterior_draws = TRUE) {
  
  #browser()
  
  # ### TEMP DEBUGGING
  # if ( FALSE ) {
  #   load("data")
  #   load("jags_results")
  #   use_posterior_draws = TRUE
  # }
  # ###
  
  
  # Extract parameter estimates (medians)
  g_estimates <- jags_results$fit$BUGSoutput$median$g
  
  # g_draws has one row per draw and one column per parameter to be estimated
  # NOTE for generalization: this breaks if there is only 1 parameter to be estimate, bc sims.matrix has a "deviance" col and a "g" col
  if (use_posterior_draws) {
    g_draws <- jags_results$fit$BUGSoutput$sims.matrix
    keepers <- grep("^g", colnames(g_draws))
    g_draws <- g_draws[, keepers]
    
    num_draws <- nrow(g_draws)
    
    # catch the case where there's only 1 parameter; in that case g_draws is a vector
    if ( length(keepers) == 1 ) g_draws = matrix(g_draws, ncol=1)
  } else {
    stop("use_posterior_draws = FALSE is not implemented")
  }
  
  
  # Get complete cases from scaled data
  cc_scaled <- jags_results$scaled_data %>%
    filter(M == 1)
  n_cc = nrow(cc_scaled)
  
  # Get variables used in model
  # MM: this only works if data, as passed to this fn, *only* contains M variables and the vars in PS model
  vars <- names(cc_scaled)[names(cc_scaled) %in% names(data) &
                             !names(cc_scaled) %in% c("id", "M")]
  
  
  # Get number of patterns
  num_patterns <- length(unique(jags_results$scaled_data$M))
  vars_per_pattern <- jags_results$vars_per_pattern
  
  # Print variable information
  #message("Variables in model:")
  #print(vars) # not true - this is all vars in cc_scaled
  message("Parameter estimates:")
  print(g_estimates)
  

  # --- 1) initialize list to store each pattern’s draws-by-CC matrix ---
  prob_mats <- vector("list", num_patterns - 1)   # **NEW** length = P
  param_idx <- 1
  
  for (p in 2:num_patterns) {
    message("Pattern ", p, " vars: ", paste( names(vars_per_pattern[[p]]), collapse = ", "))
    message("Starting param_idx: ", param_idx)
    
    # Initialize linear predictor matrix
    # Repeat intercept into a matrix with dimensions: [num_draws × num_obs]
    # note: this matrix will look blank if you print it, but that's only how R displays it. View() works.
    # lp_mat has one row per draw and one col per *observation* in the CC dataset
    intercept <- as.numeric( g_draws[, param_idx] ); param_idx <- param_idx + 1
    lp_mat <- matrix(intercept, nrow = num_draws, ncol = n_cc)  # broadcasts intercept over columns
    
    # Add covariate terms
    if (length(vars_per_pattern[[p]]) > 0) {
      
      for ( v_name in names( vars_per_pattern[[p]] ) ) {

        message("Starting v_name = ", v_name)
        
        # recall that g_draws has one row per draw and one column per parameter to be estimated
        coef <- g_draws[, param_idx]
        param_idx <- param_idx + 1
        
        X <- matrix(rep(cc_scaled[[v_name]], each = num_draws), nrow = num_draws)
        lp_mat <- lp_mat + coef * X
      }
      
    }
    
    # now have lp_mat: linear predictor for being in pattern p, with entries representing every draw (rows) and every complete case a column
    # turn in into prob_mat (plogis)
    prob_mat = plogis(lp_mat)
    
    # store prob_mat in prob_mats
    # this is a list where entry p is a matrix of probs for being in pattern p-1
    # each prob_mat has: 1 row for each draw; 1 col for each complete case
    prob_mats[[p-1]] = prob_mat
    
  }
  
  
  # each column is the colMeans of one prob_mat (dim n_cc x num_patterns)
  pattern_probs = sapply(prob_mats, colMeans)
    
  # 2) now sum across the columns of that matrix
  p_incomplete <- rowSums(pattern_probs)
  
  p1 <- 1 - ( as.numeric(p_incomplete) )
  if ( any( p1 < 0 ) ) stop("Oh no! Some p1's are negative!")

  cc_scaled$p1 = p1 
  
  # Calculate IPMW
  mnum <- mean(data$M == 1, na.rm = TRUE)
  cc_scaled$ipmw <- mnum / cc_scaled$p1
  
  # Trim extreme weights
  if (any(cc_scaled$ipmw > 10)) {
    message("Some weights are large. Trimming at 99th percentile.")
    cc_scaled$ipmw <- pmin(cc_scaled$ipmw, quantile(cc_scaled$ipmw, 0.99))
  }
  
  # Print weight summary
  message("IPMW weight summary:")
  print(summary(cc_scaled$ipmw))
  
  # Merge with original data
  if ("id" %in% names(data)) {
    cc_result <- cc_scaled %>%
      select(id, ipmw, p1) %>%
      right_join(data %>% filter(M == 1), by = "id")
  } else {
    cc_data <- data %>% filter(M == 1)
    cc_data$ipmw <- cc_scaled$ipmw
    cc_data$p1 <- cc_scaled$p1
    cc_result <- cc_data
  }
  
  return(cc_result)
}


# 2025-04-17 - version that can give negative wts for series 3# because calculates p1 *after* taking means across draws, rather than for each draw#' Calculate IPMW weights using JAGS results
#' @param jags_results Results from run_missingness_model
#' @param data Original dataset
#' @param use_posterior_draws If TRUE, draws parameters from the posterior samples themselves. If FALSE, just uses posterior medians.
#' @return Dataset with IPMW weights
calculate_ipmw_weights <- function(jags_results, data, use_posterior_draws = TRUE) {
  
  # Extract parameter estimates (medians)
  g_estimates <- jags_results$fit$BUGSoutput$median$g
  
  # g_draws has one row per draw and one column per parameter to be estimated
  # NOTE for generalization: this breaks if there is only 1 parameter to be estimate, bc sims.matrix has a "deviance" col and a "g" col
  if (use_posterior_draws) {
    g_draws <- jags_results$fit$BUGSoutput$sims.matrix
    keepers = grep("^g", colnames(g_draws))
    g_draws <- g_draws[, keepers]
    
    # catch the case where there's only 1 parameter; in that case g_draws is a vector
    if ( length(keepers) == 1 ) g_draws = matrix(g_draws, ncol=1)
  }
  
  
  # Get complete cases from scaled data
  cc_scaled <- jags_results$scaled_data %>%
    filter(M == 1)
  
  # Get variables used in model
  # MM: this only works if data, as passed to this fn, *only* contains M variables and the vars in PS model
  vars <- names(cc_scaled)[names(cc_scaled) %in% names(data) &
                             !names(cc_scaled) %in% c("id", "M", paste0("M", 1:16))]
  
  
  # Get number of patterns
  num_patterns <- length(unique(jags_results$scaled_data$M))
  vars_per_pattern <- jags_results$vars_per_pattern
  
  # Print variable information
  message("Variables in model:")
  print(vars)
  message("Parameter estimates:")
  print(g_estimates)
  
  # Initialize pattern probabilities
  pattern_probs <- matrix(0, nrow = nrow(cc_scaled), ncol = num_patterns - 1)
  
  # For each non-complete pattern, calculate probability
  # Parameter index counter
  param_idx <- 1
  
  #browser()
  for (p in 2:num_patterns) {
    message(paste("Pattern", p, "variable indices:", paste(vars_per_pattern[[p]], collapse = ", ")))
    
    # Compute linear predictor
    if (use_posterior_draws) {
      num_draws <- nrow(g_draws)
      num_obs <- nrow(cc_scaled)
      
      # Get intercept
      intercept <- as.numeric(g_draws[, param_idx])
      param_idx <- param_idx + 1
      
      # Initialize linear predictor matrix
      # Repeat intercept into a matrix with dimensions: [num_draws × num_obs]
      # note: this matrix will look blank if you print it, but that's only how R displays it. View() works.
      # lp_mat has one row per draw and one col per *observation* in the CC dataset
      lp_mat <- matrix(intercept, nrow = num_draws, ncol = num_obs)  # broadcasts intercept over columns
      
      # Add covariate terms
      if (length(vars_per_pattern[[p]]) > 0) {
        for (v_idx in vars_per_pattern[[p]]) {
          var_name <- vars[v_idx]
          # recall that g_draws has one row per draw and one column per parameter to be estimated
          coef <- g_draws[, param_idx]
          param_idx <- param_idx + 1
          
          X <- matrix(rep(cc_scaled[[var_name]], each = num_draws), nrow = num_draws)
          lp_mat <- lp_mat + coef * X
        }
      }
      
      # Convert to probabilities and take mean over posterior draws
      prob_mat <- plogis(lp_mat)
      pattern_probs[, p - 1] <- colMeans(prob_mat)
      
      # Also store mean plogis in the data frame for reference
      cc_scaled[[paste0("p", p)]] <- colMeans(prob_mat)
      
      message(paste("Pattern", p, "probability summary (draw-averaged):"))
      print(summary(cc_scaled[[paste0("p", p)]]))
      
    }
    
    
    #@@ haven't tested this yet
    if (!use_posterior_draws) {
      # Use median point estimates
      intercept <- g_estimates[param_idx]
      param_idx <- param_idx + 1
      
      lp <- rep(intercept, nrow(cc_scaled))
      
      if (length(vars_per_pattern[[p]]) > 0) {
        for (v_idx in vars_per_pattern[[p]]) {
          var_name <- vars[v_idx]
          coef <- g_estimates[param_idx]
          param_idx <- param_idx + 1
          
          lp <- lp + coef * cc_scaled[[var_name]]
        }
      }
      
      prob_vec <- plogis(lp)
      pattern_probs[, p - 1] <- prob_vec
      cc_scaled[[paste0("p", p)]] <- prob_vec
      
      message(paste("Pattern", p, "probability summary (from medians):"))
      print(summary(prob_vec))
    }
  }
  
  
  # # Calculate p1 (complete case probability)
  # # If all pattern probabilities are close to 1, something's wrong
  # # Check if this is the case
  # if (all(colMeans(pattern_probs) > 0.9)) {
  #   message("WARNING: All pattern probabilities are very high - likely a calculation error.")
  #   message("Falling back to empirical frequencies.")
  # 
  #   # Use empirical frequencies instead
  #   emp_freqs <- prop.table(table(jags_results$scaled_data$M))
  #   for (p in 2:num_patterns) {
  #     cc_scaled[[paste0("p", p)]] <- emp_freqs[as.character(p)]
  #   }
  # 
  #   # Calculate p1
  #   cc_scaled$p1 <- emp_freqs["1"]
  # } else {
  #   # Calculate p1 as 1 minus sum of other probabilities
  #   # THIS MIGHT BE THE ISSUE?
  #   cc_scaled$p1 <- 1 - rowSums(pattern_probs)
  # }
  
  # ### TEMP DEBUGGING - look at rows with negative p1
  # #browser()
  # summary(cc_scaled$p1)
  # x = cc_scaled %>% filter(p1 < 0)
  # ###
  
  
  # Calculate IPMW
  mnum <- mean(data$M == 1, na.rm = TRUE)
  cc_scaled$ipmw <- mnum / cc_scaled$p1
  
  # Trim extreme weights
  if (any(cc_scaled$ipmw > 10)) {
    message("Some weights are large. Trimming at 99th percentile.")
    cc_scaled$ipmw <- pmin(cc_scaled$ipmw, quantile(cc_scaled$ipmw, 0.99))
  }
  
  # Print weight summary
  message("IPMW weight summary:")
  print(summary(cc_scaled$ipmw))
  
  # Merge with original data
  if ("id" %in% names(data)) {
    cc_result <- cc_scaled %>%
      select(id, ipmw, p1) %>%
      right_join(data %>% filter(M == 1), by = "id")
  } else {
    cc_data <- data %>% filter(M == 1)
    cc_data$ipmw <- cc_scaled$ipmw
    cc_data$p1 <- cc_scaled$p1
    cc_result <- cc_data
  }
  
  return(cc_result)
}



#' Fit outcome model using IPMW weights
#' @param weighted_data Data with IPMW weights
#' @param outcome Outcome variable name
#' @param exposure Exposure variable name
#' @param adjustment Variables to adjust for
#' @return Model output and results
fit_outcome_model <- function(weighted_data, outcome, exposure, adjustment = NULL) {
  # Create outcome model formula with adjustment variables and interactions
  if (!is.null(adjustment) && length(adjustment) > 0) {
    # Create all possible interactions
    all_vars <- c(exposure, adjustment)
    interaction_terms <- c()
    
    for (i in 2:length(all_vars)) {
      combos <- combn(all_vars, i, simplify = FALSE)
      for (combo in combos) {
        interaction_terms <- c(interaction_terms, paste(combo, collapse = ":"))
      }
    }
    
    # Combine main effects and interactions
    formula_terms <- c(all_vars, interaction_terms)
    outcome_formula <- as.formula(paste(outcome, "~", paste(formula_terms, collapse = " + ")))
  } else {
    # No adjustment - just exposure
    outcome_formula <- as.formula(paste(outcome, "~", exposure))
  }
  
  # Fit linear model with ipmw weights
  model <- lm(outcome_formula, data = weighted_data, weights = weighted_data$ipmw)
  
  # Get robust standard errors (HC0)
  robust_se <- sqrt(diag(sandwich::vcovHC(model, type = "HC0")))
  
  # Extract coefficient for exposure
  effect_estimate <- coef(model)[[exposure]]
  effect_se <- robust_se[names(coef(model)) == exposure]
  
  # Create results
  results <- tibble(
    estimate = effect_estimate,
    se = effect_se,
    lcl = effect_estimate - 1.96 * effect_se,
    ucl = effect_estimate + 1.96 * effect_se
  )
  
  return(list(
    model = model,
    results = results,
    robust_se = robust_se
  ))
}



#' Main function to implement IPMW analysis
#' @param data Dataset
#' @param outcome Outcome variable
#' @param exposure Exposure variable
#' @param adjustment Adjustment variables
#' @return Full analysis results
# ipmw_analysis <- function(data, outcome, exposure, adjustment = NULL) {
#   
#   # Ensure ID exists
#   if (!"id" %in% names(data)) {
#     data$id <- 1:nrow(data)
#   }
#   
#   # Identify variables to be used in the analysis
#   analysis_vars <- unique(c(exposure, outcome, adjustment))
#   
#   # Add pattern indicators
#   data_with_patterns <- create_pattern_indicators(data, analysis_vars)
#   
#   # Print pattern distribution
#   message("Distribution of missingness patterns:")
#   print(table(data_with_patterns$M))
#   
#   # Run Bayesian model for missingness patterns
#   message("Running Bayesian model for missingness patterns...")
#   jags_results <- run_missingness_model(data_with_patterns, analysis_vars)
#   
#   # Calculate IPMW weights
#   message("Calculating IPMW weights...")
#   weighted_data <- calculate_ipmw_weights(jags_results, data_with_patterns)
#   
#   message("IPMW weight summary:")
#   print(summary(weighted_data$ipmw))
#   
#   # Fit outcome model
#   message("Fitting outcome model...")
#   model_results <- fit_outcome_model(weighted_data, outcome, exposure, adjustment)
# 
#   message("IPMW analysis results:")
#   print(model_results$results)
#   
#   # Return complete results
#   return(list(
#     #ipmw_results = model_results$results,
#     model = model_results$model,
#     weighted_data = weighted_data,
#     jags_fit = jags_results$fit,
#     missingness_patterns = table(data_with_patterns$M)
#   ))
# }

# Example usage:
# results <- ipmw_analysis(
#   data = my_data,
#   outcome = "D",
#   exposure = "A",
#   adjustment = c("B", "C")
# )




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


# IMPUTATION HELPERS ---------------------------------------------------------------


add_interactions_from_formula <- function(df, formula_str) {
  # Convert the formula string to a formula object
  fml <- as.formula(formula_str)
  
  # Get model matrix to find which terms are involved
  mm <- model.matrix(fml, data = df)
  
  # Remove the intercept column if present
  mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  
  # Loop over interaction columns and add to df
  for (term in colnames(mm)) {
    if (grepl(":", term)) {
      # Interaction term
      vars <- strsplit(term, ":")[[1]]
      newname <- paste(vars, collapse = "")
      df[[newname]] <- df[[vars[1]]]
      for (j in 2:length(vars)) {
        df[[newname]] <- df[[newname]] * df[[vars[j]]]
      }
    } else if (!(term %in% names(df))) {
      # If a main effect was encoded (e.g., factor variable expansion), just ignore
      warning(sprintf("Term %s not added because it's not directly available in df", term))
    }
  }
  
  return(df)
}



convert_binary_to_factor <- function(df, vars = NULL, exclude = NULL) {
  # If vars not specified, consider all columns
  if (is.null(vars)) {
    vars <- names(df)
  }
  
  if (!is.null(exclude)) {
    vars <- setdiff(vars, exclude)
  }
  
  for (v in vars) {
    x <- df[[v]]
    if (is.numeric(x) && length(unique(x[!is.na(x)])) == 2) {
      df[[v]] <- factor(x)
    }
  }
  
  return(df)
}


convert_binary_to_factor <- function(df) {
  is_binary <- function(x) is.numeric(x) && length(unique(x[!is.na(x)])) == 2
  df[] <- lapply(df, function(x) if (is_binary(x)) as.factor(x) else x)
  return(df)
}

convert_binary_factor_to_numeric <- function(df) {
  is_binary_factor <- function(x) is.factor(x) && length(levels(x)) == 2
  df[] <- lapply(df, function(x) if (is_binary_factor(x)) as.numeric(x) - 1 else x)
  return(df)
}

# make a nice single string to report the methods used for each imp model
summarize_mice_methods <- function(method_vector) {
  method_vector <- method_vector[method_vector != ""]
  paste(paste0(names(method_vector), ": ", method_vector), collapse = "; ")
}

match_coef_names_to_mice <- function(coef_of_interest, pooled_terms) {
  coef_of_interest <- as.character(coef_of_interest)
  pooled_terms <- as.character(pooled_terms)  # <- fixes your error
  
  # Extract all variable names used in pooled terms (excluding intercept)
  vars_in_pooled <- unique(unlist(strsplit(pooled_terms[pooled_terms != "(Intercept)"], "[:*]")))
  vars_with_1 <- vars_in_pooled[grepl("1$", vars_in_pooled)]
  vars_clean   <- sub("1$", "", vars_with_1)
  
  replacements <- setNames(vars_with_1, vars_clean)  # e.g., "A" -> "A1"
  
  # Replace clean names with mice-style names (e.g., A → A1)
  out <- coef_of_interest
  for (v in names(replacements)) {
    pattern <- paste0("\\b", v, "\\b")
    out <- gsub(pattern, replacements[[v]], out)
  }
  
  return(out)
}
# # tests
# match_coef_names_to_mice("B", c("A1", "B1", "C1:D1"))
# match_coef_names_to_mice("B", c("A", "B", "C:D"))



# for pleasing mix pkg
recode_binaries <- function(df) {
  # Identify binary variables (containing only 0 and 1, excluding NA)
  is_binary <- sapply(df, function(col) all(na.omit(col) %in% c(0, 1)) && length(unique(na.omit(col))) == 2)
  
  # Number of binary variables
  num_binaries <- sum(is_binary)
  
  # Recode 0/1 as 1/2
  df[is_binary] <- lapply(df[is_binary], function(col) col + 1)
  
  # Rearrange columns: binaries first, then the rest
  df_reordered <- cbind(df[is_binary], df[!is_binary])
  
  # Return list with recoded dataframe and number of binary variables
  list(
    df = df_reordered,
    num_binaries = num_binaries
  )
}

reverse_recode_binaries <- function(df) {
  # Identify binary variables recoded as 1/2
  is_binary <- sapply(df, function(col) all(na.omit(col) %in% c(1, 2)) && length(unique(na.omit(col))) == 2)
  
  # Reverse recode from 1/2 back to 0/1
  df[is_binary] <- lapply(df[is_binary], function(col) col - 1)
  
  # Return dataframe with reversed recoding
  df
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
module load openblas/0.3.20
module load jags/4.3.1
ml load R/4.3.2
R -f PATH_TO_R_SCRIPT ARGS_TO_R_SCRIPT

##VISUAL_ALERT##")
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
      
      
      ### START NEW ADDITION
      # add visual alert if jobname is "job_1"
      if (sbatch_params$jobname[sbatch] == "job_1") {
        alert_block <- paste(
          'echo "========================================================="',
          'echo "🚨🚨🚨 JOB_1 COMPLETE — CHECK YOUR OUTPUT! 🚨🚨🚨"',
          'echo "========================================================="',
          'echo -e "\\a\\a\\a"',
          sep = "\n"
        )
      } else {
        alert_block <- ""
      }
      gen_batch <- gsub("##VISUAL_ALERT##", alert_block, gen_batch, fixed = TRUE)
      ### END NEW ADDITION
      
      
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



# original version: no visual alerts
# generateSbatch <- function(sbatch_params,
#                            runfile_path = NA,
#                            run_now = F) {
#   
#   #sbatch_params is a data frame with the following columns
#   #jobname: string, specifies name associated with job in SLURM queue
#   #outfile: string, specifies the name of the output file generated by job
#   #errorfile: string, specifies the name of the error file generated by job
#   #jobtime: string in hh:mm:ss format, max (maybe soft) is 48:00:00 
#   #specifies the amoung of time job resources should be allocated
#   #jobs still running after this amount of time will be aborted
#   #quality: kind of like priority, normal works
#   #node_number, integer: the number of nodes (computers w/16 cpus each) to allocate 
#   #mem_per_node, integer: RAM, in MB, to allocate to each node
#   #mailtype, string: ALL, BEGIN, END, FAIL: what types of events should you be notified about via email
#   #user_email string: email address: email address to send notifications
#   #tasks_per_node: integer, number of tasks, you should probably use 1
#   #cpus_per_task: integer, 1-16, number of cpus to use, corresponds to number of available cores per task
#   #path_to_r_script: path to r script on sherlock
#   #args_to_r_script: arguments to pass to r script on command line
#   #write_path: where to write the sbatch file
#   #server_sbatch_path: where sbatch files will be stored on sherlock
#   #runfile_path is a string containing a path at which to write an R script that can be used to run
#   #the batch files generated by this function. 
#   #if NA, no runfile will be written
#   #run_now is a boolean specifying whether batch files should be run as they are generated
#   
#   sbatches <- list()
#   if (!is.na(runfile_path)) {
#     outfile_lines <- c(paste0("# Generated on ",  Sys.time()))
#   }
#   for (sbatch in 1:nrow(sbatch_params) ) {
#     gen_batch <- sbatch_skeleton()
#     #set job name
#     if (is.null(sbatch_params$jobname[sbatch])) { 
#       gen_batch <- gsub("JOBNAME", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("JOBNAME", sbatch_params$jobname[sbatch], gen_batch) 
#     }
#     #set outfile name
#     if (is.null(sbatch_params$outfile[sbatch])) { 
#       gen_batch <- gsub("OUTFILE", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("OUTFILE", sbatch_params$outfile[sbatch], gen_batch) 
#     }
#     #set errorfile name
#     if (is.null(sbatch_params$errorfile[sbatch])) { 
#       gen_batch <- gsub("ERRORFILE", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("ERRORFILE", sbatch_params$errorfile[sbatch], gen_batch) 
#     }
#     #set jobtime
#     if (is.null(sbatch_params$jobtime[sbatch])) { 
#       gen_batch <- gsub("JOBTIME", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("JOBTIME", sbatch_params$jobtime[sbatch], gen_batch) 
#     }
#     #set quality
#     if (is.null(sbatch_params$quality[sbatch])) { 
#       gen_batch <- gsub("QUALITY", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("QUALITY", sbatch_params$quality[sbatch], gen_batch) 
#     }
#     #set number of nodes
#     if (is.null(sbatch_params$node_number[sbatch])) { 
#       gen_batch <- gsub("NODENUMBER", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("NODENUMBER", sbatch_params$node_number[sbatch], gen_batch) 
#     }
#     #set memory per node
#     if (is.null(sbatch_params$mem_per_node[sbatch])) { 
#       gen_batch <- gsub("MEMPERNODE", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("MEMPERNODE", sbatch_params$mem_per_node[sbatch], gen_batch) 
#     }
#     #set requested mail message types
#     if (is.null(sbatch_params$mailtype[sbatch])) { 
#       gen_batch <- gsub("MAILTYPE", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("MAILTYPE", sbatch_params$mailtype[sbatch], gen_batch) 
#     }
#     #set email at which to receive messages
#     if (is.null(sbatch_params$user_email[sbatch])) { 
#       gen_batch <- gsub("USER_EMAIL", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("USER_EMAIL", sbatch_params$user_email[sbatch], gen_batch) 
#     }
#     #set tasks per node
#     if (is.null(sbatch_params$tasks_per_node[sbatch])) { 
#       gen_batch <- gsub("TASKS_PER_NODE", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("TASKS_PER_NODE", sbatch_params$tasks_per_node[sbatch], gen_batch) 
#     }
#     #set cpus per task
#     if (is.null(sbatch_params$cpus_per_task[sbatch])) { 
#       gen_batch <- gsub("CPUS_PER_TASK", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("CPUS_PER_TASK", sbatch_params$cpus_per_task[sbatch], gen_batch) 
#     }
#     #set path to r script
#     if (is.null(sbatch_params$path_to_r_script[sbatch])) { 
#       gen_batch <- gsub("PATH_TO_R_SCRIPT", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("PATH_TO_R_SCRIPT", sbatch_params$path_to_r_script[sbatch], gen_batch) 
#     }
#     #set args to r script
#     if (is.null(sbatch_params$args_to_r_script[sbatch])) { 
#       gen_batch <- gsub("ARGS_TO_R_SCRIPT", "unnamed", gen_batch) 
#     } else { 
#       gen_batch <- gsub("ARGS_TO_R_SCRIPT", sbatch_params$args_to_r_script[sbatch], gen_batch) 
#     }
#     
#     #write batch file
#     if (is.null(sbatch_params$write_path[sbatch])) { 
#       cat(gen_batch, file = paste0("~/sbatch_generated_at_", gsub(" |:|-", "_", Sys.time()) ), append = F)
#     } else { 
#       cat(gen_batch, file = sbatch_params$write_path[sbatch], append = F)
#     }
#     
#     if (!is.na(sbatch_params$server_sbatch_path[sbatch])) {
#       outfile_lines <- c(outfile_lines, paste0("system(\"sbatch ", sbatch_params$server_sbatch_path[sbatch], "\")"))
#     } 
#     sbatches[[sbatch]] <- gen_batch
#   }
#   if (!is.na(runfile_path)) {
#     cat(paste0(outfile_lines, collapse = "\n"), file = runfile_path)
#   }
#   if(run_now) { system(paste0("R -f ", runfile_path)) } 
#   
#   return(sbatches)
# }


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
  
  correct.order = c("gold", "CC", "Am-std", "Am-ours", "MICE-std", "MICE-ours", "MICE-ours-pred", "IPW-nm")
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
      PropNA = mean(is.na(Bhat)),
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





