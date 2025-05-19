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
    
    
  }  # end of .p$dag_name == "1A"
  
  
  
  
  
  # ~ DAG 1A-bin -----------------------------
  
  if ( .p$dag_name == "1A-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),  
                     A1 = rbinom( n = .p$N, 
                                  size = 1, 
                                  prob = 0.5 ) )  
    
    coef1 = 2
    coef2 = 1
    
    du = du %>% rowwise() %>%
      mutate( B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + coef2*C1 + A1*C1) ),
              
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "1A-bin"
  
  
  
  
  
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
  
  
  
  
  
  
  # ~ DAG 1B-bin -----------------------------
  
  if ( .p$dag_name == "1B-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),  
                     A1 = rbinom( n = .p$N, 
                                  size = 1, 
                                  prob = 0.5 ) )  
    
    coef1 = 2
    coef2 = 1
    
    du = du %>% rowwise() %>%
      mutate( B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + coef2*C1 + A1*C1) ),
              
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = 0.3 + 0.4*A1 ),
              
              RC = rbinom(n = 1, size = 1, prob = 0.5) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = A1,
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "1B-bin"
  
  
  
  
  
  
  
  # ~ DAG 2A -----------------------------
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
  
  
  
  
  
  
  # ~ DAG 2A-bin -----------------------------
  
  if ( .p$dag_name == "2A-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    coef2 = 1
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + coef2*C1 + A1*C1) ),
              
              
              RA = 1,
              
              RB = 1,
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    
    # colMeans(du)
    # cor(du %>% select(A1, B1, C1, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "2A-bin"
  
  
  
  
  
  
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
  
  
  
  
  
  # ~ DAG 3A-bin -----------------------------
  
  if ( .p$dag_name == "3A-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    coef2 = 1
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -1 + coef1*A1 ) ),
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "3A-bin"
  
  
  
  
  
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
  
  
  
  # ~ DAG 3B-bin -----------------------------
  # same as above, but has C1 -> Y1 edge
  
  if ( .p$dag_name == "3B-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    coef2 = 1
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + coef2*C1 + A1*C1) ),
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "3B-bin"
  
  
  
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
  
  
  
  # ~ DAG 3C-bin -------------------------------------------------
  
  if ( .p$dag_name == "3C-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    coef2 = 1
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + coef2*C1 + A1*C1) ),
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "3C-bin"
  
  
  
  
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
  
  
  
  # ~ DAG 3D-bin -----------------------------
  if ( .p$dag_name == "3D-bin" ) {
    
    # designed for using Ross' rjags code, so A needs to be complete
    # has A*C interaction
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    coef2 = 1
    
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + coef2*C1 + A1*C1) ),
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    
    colMeans(du)
    cor(du %>% select( A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A, X)
    
    
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
  
  
  
  
  
  # ~ DAG 3E-bin -----------------------------
  if ( .p$dag_name == "3E-bin" ) {
    
    # designed for using Ross' rjags code, so A needs to be complete
    # has A*C interaction
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    coef2 = 1
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + coef2*C1 + A1*C1) ),
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    
    colMeans(du)
    cor(du %>% select( A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "3E-bin"
  
  
  
  
  
  
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
  
  
  
  
  
  
  
  
  # ~ DAG 4A-bin -----------------------------
  
  # for adjustment formula 4, CATE version
  # C1 -> A1 -> W1 -> B1
  # C1 -> Y1, C1 -> RC
  # W1 -> RY
  
  
  if ( .p$dag_name == "4A-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
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
              
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coefWB*W1 + 2*C1 + W1*C1) ),
              
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
              W = ifelse(RW == 1, W1, NA),
              X = X1)
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, W1, RA, RB, RC, RW) )
    
    
    # make dataset for imputation (exclude W because it's latent)
    di = du %>% select(A, B, C, X)
    
    
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
    
  }  # end of .p$dag_name == "4A-bin"
  
  
  
  
  
  
  
  # ~ DAG 4B -----------------------------
  
  # same as 4A, but mediator (now called D) is observed, so available to use in IPMW and MI
  # however, it's nonmonotone
  
  if ( .p$dag_name == "4B" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coefDB = 2
    coefAD = 3
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              D1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + coefAD*A1) ),
              
              B1 = rnorm( n = 1,
                          mean = coefDB*D1 + 2.6*C1 + D1*C1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RD = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*D1) ),
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*D1) ) )
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              D = ifelse(RD == 1, D1, NA) )
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, D1, RA, RB, RC, RD) )
    
    
    # make dataset for imputation
    di = du %>% select(A, B, C, D)
    
    
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
    
  }  # end of .p$dag_name == "4B"
  
  
  # ~ DAG 4C -----------------------------
  
  # like 4B, but partially monotone
  
  if ( .p$dag_name == "4C" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coefDB = 2
    coefAD = 3
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              D1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + coefAD*A1) ),
              
              B1 = rnorm( n = 1,
                          mean = coefDB*D1 + 2.6*C1 + D1*C1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RD = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*D1) ),
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*D1) ) )
    
    # partial monotonicity: B is the most missing
    du$RB[ du$RA == 0 | du$RC == 0 | du$RD == 0 ] = 0 
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              D = ifelse(RD == 1, D1, NA) )
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, D1, RA, RB, RC, RD) )
    
    
    # make dataset for imputation
    di = du %>% select(A, B, C, D)
    
    
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
    
  }  # end of .p$dag_name == "4C"
  
  
  
  
  
  
  
  
  
  
  # ~ DAG 4D -----------------------------
  
  # like 4C, but both D and B are most missing vars
  
  if ( .p$dag_name == "4D" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coefDB = 2
    coefAD = 3
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              D1 = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + coefAD*A1) ),
              
              B1 = rnorm( n = 1,
                          mean = coefDB*D1 + 2.6*C1 + D1*C1),
              
              RA = rbinom( n = 1,
                           size = 1,
                           prob = 0.5 ),
              
              RD = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*D1) ),
              
              RC = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*C1) ),
              
              RB = rbinom( n = 1,
                           size = 1,
                           prob = expit(-1 + 3*D1) ) )
    
    # partial monotonicity: B is the most missing
    du$RB[ du$RA == 0 | du$RC == 0 | du$RD == 0 ] = 0 
    du$RD[ du$RB == 0 ] = 0  # only change from previous one
    
    
    du = du %>% rowwise() %>%
      mutate( A = ifelse(RA == 1, A1, NA),
              B = ifelse(RB == 1, B1, NA),
              C = ifelse(RC == 1, C1, NA),
              D = ifelse(RD == 1, D1, NA) )
    
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, D1, RA, RB, RC, RD) )
    
    
    # make dataset for imputation
    di = du %>% select(A, B, C, D)
    
    
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
    
  }  # end of .p$dag_name == "4D"
  
  
  
  
  
  
  
  
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
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "6A"
  
  
  
  
  
  
  # ~ DAG 6A-bin -----------------------------
  
  # C1 -> RC -> RB (monotone)
  # A -> Y1
  # B1 <- W -> RB
  # intuitively, I expect MAR methods to work even though it's MNAR
  
  if ( .p$dag_name == "6A-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ), 
                     W1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ), 
                     A1 = rbinom( n = .p$N, 
                                  size = 1, 
                                  prob = 0.5 ) )  
    
    coef1 = 2
    coef2 = 1
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          # need EMM for unadjusted CCA to be biased
                          mean = coef1*A1 + coef2*W1 + 1*A1*W1 ),
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + coef2*W1 + A1*W1) ),
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, W1, RB, RC) )
    
    
    # make dataset for imputation (exclude W: treat as latent)
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "6A-bin"
  
  
  
  
  
  
  
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
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "7A"
  
  
  
  # ~ DAG 7A-bin -----------------------------
  
  if ( .p$dag_name == "7A-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ), 
                     
                     A1 = rbinom( n = .p$N, 
                                  size = 1, 
                                  prob = 0.5 ) )  
    
    coef1 = 2
    coef2 = 1
    
    du = du %>% rowwise() %>%
      mutate( B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + coef2*C1 + A1*C1) ),
              
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    # missmap(du %>% select(A, B, C))
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "7A-bin"
  
  
  
  
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
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "7B"
  
  
  
  # ~ DAG 7B-bin -----------------------------
  
  # same as 7A, but with "direction" of monotone missingness reversed (i.e., RY -> RA here)
  
  if ( .p$dag_name == "7B-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ), 
                     
                     A1 = rbinom( n = .p$N, 
                                  size = 1, 
                                  prob = 0.5 ) )  
    
    coef1 = 2
    coef2 = 1
    
    du = du %>% rowwise() %>%
      mutate( B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + coef2*C1 + A1*C1) ),
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    # missmap(du %>% select(A, B, C))
    
    colMeans(du)
    cor(du %>% select(A1, B1, C1, RB, RC) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "7B-bin"
  
  
  
  # ~ DAG 12A -----------------------------
  
  if ( .p$dag_name == "12A" ) {
    
    du = data.frame( X1 = rnorm( n = .p$N ), # complete variable included only to please Amelia
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     # called C^1_2 in IAI log; this is also in conditioning set
                     D1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = 0.5),
              #prob = expit(-1 + 3*C1) ),
              
              # add edge from C1 -> B1
              B1 = rnorm( n = 1,
                          mean = coef1*A1 + 2*D1 + A1*D1),
              
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
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "12A"
  
  
  
  
  
  
  # ~ DAG 12A-bin -----------------------------
  
  if ( .p$dag_name == "12A-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     # called C^1_2 in IAI log; this is also in conditioning set
                     D1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    coef1 = 2
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = 0.5),
              #prob = expit(-1 + 3*C1) ),
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + 2*D1 + A1*D1) ),
              
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
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "12A-bin"
  
  
  
  
  
  
  
  # ~ DAG 12B -----------------------------
  
  # like 12A, but nonmonotone
  
  if ( .p$dag_name == "12B" ) {
    
    du = data.frame( X1 = rnorm( n = .p$N ), # complete variable included only to please Amelia
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     # called C^1_2 in IAI log; this is also in conditioning set
                     D1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    coef1 = 2
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = 0.5),
              #prob = expit(-1 + 3*C1) ),
              
              # add edge from C1 -> B1
              B1 = rnorm( n = 1,
                          mean = coef1*A1 + 2*D1 + A1*D1),
              
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
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "12B"
  
  
  
  
  
  
  # ~ DAG 12B-bin -----------------------------
  
  # like 12A, but nonmonotone
  
  if ( .p$dag_name == "12B-bin" ) {
    
    du = data.frame( X1 = rnorm( n = .p$N ), 
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     # called C^1_2 in IAI log; this is also in conditioning set
                     D1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = 0.5),
              #prob = expit(-1 + 3*C1) ),
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + 2*D1 + A1*D1) ),
              
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
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "12B-bin"
  
  
  
  
  
  
  
  # ~ DAG 12C -----------------------------
  
  # like 12A, but partially nonmonotone
  
  if ( .p$dag_name == "12C" ) {
    
    du = data.frame( X1 = rnorm( n = .p$N ), # complete variable included only to please Amelia
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     # called C^1_2 in IAI log; this is also in conditioning set
                     D1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = 0.5),
              #prob = expit(-1 + 3*C1) ),
              
              # add edge from C1 -> B1
              B1 = rnorm( n = 1,
                          mean = coef1*A1 + 2*D1 + A1*D1),
              
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
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "12C"
  
  
  
  
  
  
  # ~ DAG 12C-bin -----------------------------
  
  # like 12A, but partially nonmonotone
  
  if ( .p$dag_name == "12C-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     # called C^1_2 in IAI log; this is also in conditioning set
                     D1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    
    
    du = du %>% rowwise() %>%
      mutate( A1 = rbinom( n = 1,
                           size = 1,
                           prob = 0.5),
              #prob = expit(-1 + 3*C1) ),
              
              
              B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -2 + coef1*A1 + 2*D1 + A1*D1) ),
              
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
    
  }  # end of .p$dag_name == "12C-bin"
  
  
  
  
  
  
  # ~ DAG 13A -----------------------------
  
  if ( .p$dag_name == "13A" ) {
    
    # C1 is unrelated to everything; only here to avoid having all-NA rows
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     A1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    coef1 = 2
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = coef1*A1),
              
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
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "13A"
  
  
  
  
  
  
  
  
  
  # ~ DAG 13A-bin -----------------------------
  
  if ( .p$dag_name == "13A-bin" ) {
    
    # C1 is unrelated to everything; only here to avoid having all-NA rows
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     A1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    
    coef1 = 2
    
    du = du %>% rowwise() %>%
      mutate( B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -1 + coef1*A1 ) ),
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    
    # monotone missingness: conditionally overwrite indicator
    du$RA[ du$RB == 0 ] = 0
    
    colMeans(du)
    cor(du %>% select(A1, B1, RA, RB) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(B, C, A, X)
    
    
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
    
    coef1 = 2
    
    du = du %>% rowwise() %>%
      mutate( B1 = rnorm( n = 1,
                          mean = coef1*A1),
              
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
      
      beta = coef1
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "13B"
  
  
  
  
  
  
  
  # ~ DAG 13B-bin -----------------------------
  
  if ( .p$dag_name == "13B-bin" ) {
    
    # X1 (isolated node) is only there to allow genloc to run (needs at least 1 continuous var)
    du = data.frame( X1 = rnorm( n = .p$N ),
                     C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ),
                     
                     A1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) ) 
    
    coef1 = 2
    
    du = du %>% rowwise() %>%
      mutate( B1 = rbinom( n = 1,
                           size = 1,
                           prob = plogis( -1 + coef1*A1) ),
              
              
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
              C = ifelse(RC == 1, C1, NA),
              X = X1)
    
    
    # monotone missingness: conditionally overwrite indicator
    du$RA[ du$RB == 0 ] = 0
    
    colMeans(du)
    cor(du %>% select(A1, B1, RA, RB) )
    
    
    # make dataset for imputation (standard way: all measured variables)
    #di = du[ !( is.na(du$A) & is.na(du$B) & is.na(du$C) ), ]  # remove any rows that are all NA
    di = du %>% select(B, C, A, X)
    
    
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
    
  }  # end of .p$dag_name == "13B-bin"
  
  
  
  # ~ DAG 14A-debug  -------------------------------------------------
  
  # simplify until rjags works
  if ( .p$dag_name == "14A-debug" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) )
    
    
    # --- Define coefficients upfront ---
    intercept_A1 = -0.8
    coef_A1_C1 = 1.5
    
    intercept_D1 = -1
    coef_D1_A1 = 1.5
    coef_D1_C1 = 1
    
    coef_DB = 1.5   # NEW: D1 effect on B1
    coef_AB = 2   # NEW: A1 effect on B1
    coef_ACB = 1.0  # NEW: A1*C1 interaction
    
    coef_B1_C1 = 2.6
    
    intercept_missing = -1
    coef_missing = 1
    
    # --- STEP 1: Simulate A1 based only on C1 ---
    du = du %>%
      rowwise() %>%
      mutate( lp_A1 = intercept_A1 + coef_A1_C1*C1,
              A1 = rbinom( n = 1, size = 1, prob = expit(lp_A1) ) )
    
    
    # --- STEP 2: Now D1 depends on A1 and C1 ---
    du = du %>%
      rowwise() %>%
      mutate( lp_D1 = intercept_D1 + coef_D1_A1*A1 + coef_D1_C1*C1,
              D1 = rbinom( n = 1, size = 1, prob = expit(lp_D1) ) )
    
    
    # --- STEP 4: Generate B1 ---
    du = du %>%
      rowwise() %>%
      mutate(
        B1 = rnorm( n = 1,
                    mean = coef_DB*D1 + coef_AB*A1 + coef_B1_C1*C1 + coef_ACB*A1*C1,
                    sd = 1 )
      )
    
    # --- STEP 5: Generate linear predictors for missingness ---
    du = du %>%
      mutate(
        lp_RA = intercept_missing + coef_missing*(A1 + C1 + D1),
        lp_RC = intercept_missing + coef_missing*(A1 + C1 + D1),
        lp_RD = intercept_missing + coef_missing*(A1 + C1 + D1),
        lp_RB = (intercept_missing + 1) + coef_missing*(A1 + C1 + D1)
      )
    
    # --- STEP 6: Simulate missingness indicators based on LPs ---
    du = du %>%
      mutate(
        RA = rbinom( n = 1, size = 1, prob = expit(lp_RA) ),
        RC = rbinom( n = 1, size = 1, prob = expit(lp_RC) ),
        RD = rbinom( n = 1, size = 1, prob = expit(lp_RD) ),
        RB = rbinom( n = 1, size = 1, prob = expit(lp_RB) )
      )
    
    # --- STEP 7: Force RB = 0 if any other vars are 0 ---
    du = du %>%
      mutate( RB = ifelse(RA == 0 | RC == 0 | RD == 0, 0, RB) )
    
    # --- STEP 8: Summarize LP-to-probability ranges ---
    
    # sanity check
    
    if (FALSE) {
      linear_predictor_summary = tibble(
        Variable = c("A1", "D1", "RA", "RC", "RD", "RB"),
        Min_LP = c(min(du$lp_A1),
                   min(du$lp_D1),
                   min(du$lp_RA),
                   min(du$lp_RC),
                   min(du$lp_RD),
                   min(du$lp_RB)),
        Max_LP = c(max(du$lp_A1),
                   max(du$lp_D1),
                   max(du$lp_RA),
                   max(du$lp_RC),
                   max(du$lp_RD),
                   max(du$lp_RB))
      ) %>%
        mutate(
          Min_Prob = expit(Min_LP),
          Max_Prob = expit(Max_LP)
        )
      
      print(linear_predictor_summary)
      
      
    }  # end sanity check
    
    # --- STEP 9: Remove linear predictor variables from du ---
    du = du %>% select(-starts_with("lp_"))
    
    # --- STEP 10: Create observed variables ---
    du = du %>% rowwise() %>%
      mutate(
        A = ifelse(RA == 1, A1, NA),
        B = ifelse(RB == 1, B1, NA),
        C = ifelse(RC == 1, C1, NA),
        D = ifelse(RD == 1, D1, NA)
      )
    
    
    # sanity checks
    if ( FALSE ) {
      colMeans(du)
      cor(du %>% select(A1, B1, C1, D1, RA, RB, RC, RD))
    }
    
    # dataset for imputation
    di = du %>% select(A, B, C, D)
    
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
    
  }  # end of .p$dag_name == "14A-debug"
  
  
  
  # ~ DAG 14A  -------------------------------------------------
  
  if ( .p$dag_name == "14A" ) {
    
    du = data.frame( C1 = rbinom( n = .p$N,
                                  size = 1, 
                                  prob = 0.5 ) )
    
    
    # --- Define coefficients upfront ---
    intercept_A1 = -0.8
    coef_A1_C1 = 1.5
    
    intercept_D1 = -1
    coef_D1_A1 = 1.5
    coef_D1_C1 = 1
    
    intercept_E1 = -1.3
    coef_E1_D1 = 1.5
    coef_E1_A1 = 1
    coef_E1_C1 = 1
    
    coef_EB = 1.5
    coef_DB = 1.5   # NEW: D1 effect on B1
    coef_AB = 2   # NEW: A1 effect on B1
    coef_ACB = 1.0  # NEW: A1*C1 interaction
    
    coef_B1_C1 = 2.6
    
    intercept_missing = -2
    coef_missing = 1
    
    # --- STEP 1: Simulate A1 based only on C1 ---
    du = du %>%
      rowwise() %>%
      mutate( lp_A1 = intercept_A1 + coef_A1_C1*C1,
              A1 = rbinom( n = 1, size = 1, prob = expit(lp_A1) ) )
    
    
    # --- STEP 2: Now D1 depends on A1 and C1 ---
    du = du %>%
      rowwise() %>%
      mutate( lp_D1 = intercept_D1 + coef_D1_A1*A1 + coef_D1_C1*C1,
              D1 = rbinom( n = 1, size = 1, prob = expit(lp_D1) ) )
    
    # --- STEP 3: Now E1 depends on D1, A1, and C1 ---
    du = du %>%
      rowwise() %>%
      mutate(
        lp_E1 = intercept_E1 + coef_E1_D1*D1 + coef_E1_A1*A1 + coef_E1_C1*C1,
        E1 = rbinom( n = 1, size = 1, prob = expit(lp_E1) )
      )
    
    # --- STEP 4: Generate B1 based on D1, E1, A1, C1 and A1*C1 interaction ---
    du = du %>%
      rowwise() %>%
      mutate(
        B1 = rnorm( n = 1,
                    mean = coef_EB*E1 + coef_DB*D1 + coef_AB*A1 + coef_B1_C1*C1 + coef_ACB*A1*C1,
                    sd = 1 )
      )
    
    # --- STEP 5: Generate linear predictors for missingness ---
    du = du %>%
      mutate(
        lp_RA = intercept_missing + coef_missing*(A1 + C1 + D1 + E1),
        lp_RC = intercept_missing + coef_missing*(A1 + C1 + D1 + E1),
        lp_RD = intercept_missing + coef_missing*(A1 + C1 + D1 + E1),
        lp_RE = intercept_missing + coef_missing*(A1 + C1 + D1 + E1),
        lp_RB = (intercept_missing + 0.5) + coef_missing*(A1 + C1 + D1 + E1)
      )
    
    # --- STEP 6: Simulate missingness indicators based on LPs ---
    du = du %>%
      mutate(
        RA = rbinom( n = 1, size = 1, prob = expit(lp_RA) ),
        RC = rbinom( n = 1, size = 1, prob = expit(lp_RC) ),
        RD = rbinom( n = 1, size = 1, prob = expit(lp_RD) ),
        RE = rbinom( n = 1, size = 1, prob = expit(lp_RE) ),
        RB = rbinom( n = 1, size = 1, prob = expit(lp_RB) )
      )
    
    # --- STEP 7: Force RB = 0 if any other vars are 0 ---
    du = du %>%
      mutate( RB = ifelse(RA == 0 | RC == 0 | RD == 0 | RE == 0, 0, RB) )
    
    # --- STEP 8: Summarize LP-to-probability ranges ---
    
    # sanity check
    
    if (FALSE) {
      linear_predictor_summary = tibble(
        Variable = c("A1", "D1", "E1", "RA", "RC", "RD", "RE", "RB"),
        Min_LP = c(min(du$lp_A1),
                   min(du$lp_D1),
                   min(du$lp_E1),
                   min(du$lp_RA),
                   min(du$lp_RC),
                   min(du$lp_RD),
                   min(du$lp_RE),
                   min(du$lp_RB)),
        Max_LP = c(max(du$lp_A1),
                   max(du$lp_D1),
                   max(du$lp_E1),
                   max(du$lp_RA),
                   max(du$lp_RC),
                   max(du$lp_RD),
                   max(du$lp_RE),
                   max(du$lp_RB))
      ) %>%
        mutate(
          Min_Prob = expit(Min_LP),
          Max_Prob = expit(Max_LP)
        )
      
      print(linear_predictor_summary)
      
      
    }  # end sanity check
    
    # --- STEP 9: Remove linear predictor variables from du ---
    du = du %>% select(-starts_with("lp_"))
    
    # --- STEP 10: Create observed variables ---
    du = du %>% rowwise() %>%
      mutate(
        A = ifelse(RA == 1, A1, NA),
        B = ifelse(RB == 1, B1, NA),
        C = ifelse(RC == 1, C1, NA),
        D = ifelse(RD == 1, D1, NA),
        E = ifelse(RE == 1, E1, NA)
      )
    
    
    # sanity checks
    if ( FALSE ) {
      colMeans(du)
      cor(du %>% select(A1, B1, C1, D1, E1, RA, RB, RC, RD, RE))
    }
    
    # dataset for imputation
    di = du %>% select(A, B, C, D, E)
    
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
    
  }  # end of .p$dag_name == "14A"
  
  
  
  
  
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