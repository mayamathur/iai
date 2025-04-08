

sim_obj = sim_data(.p = data.frame(N=1000,
                                   coef_of_interest = "A",
                                   dag_name = "12B" ) )


du = sim_obj$du


# PACKAGE  -------------------------------------------------

library(NMMIPW)

nmm_fit(data = du, O = cbind(du$RA, du$RB, du$RC), formula = B ~ A * C, func = lm)


debug(nmm_fit)
undebug(nmm_fit)


# their example
n = 100
X = rnorm(n, 0, 1)
Y = rnorm(n, 1 * X, 1)
O1 = rbinom(n, 1, 1/(1 + exp(-1 - 0.5 * X)))
O2 = rbinom(n, 1, 1/(1 + exp(+0.5 + 1 * Y)))
O = cbind(O1, O2)
df <- data.frame(Y = Y, X = X)
fit <- nmm_fit(data = df, O = O, formula = Y ~ X, funct = lm)
summary(fit)



# CLAUDE  -------------------------------------------------

# problem was that optimizers aren't finding the min -- not sure why

###########################################
#
# Flexible IPMW Implementation for Custom Datasets
# Based on Sun & Tchetgen Tchetgen (2018)
#
##########################################

library(tidyverse)
library(geepack)
library(mice)
library(Hmisc)

###########################################
#
# Flexible IPMW Implementation for Custom Datasets
# Based on Sun & Tchetgen Tchetgen (2018)
# With direct adjustment and interaction terms
#
##########################################

library(tidyverse)
library(geepack)
library(mice)
library(Hmisc)
library(Formula)




###########################################
#
# Flexible IPMW Implementation for Custom Datasets
# Based on Sun & Tchetgen Tchetgen (2018)
#
##########################################


### TEMP ONLY
data = du
outcome = "B"
exposure = "A"
adjustment = c("C", "D")
###

###########################################
#
# Flexible IPMW Implementation for Custom Datasets
# Based on Sun & Tchetgen Tchetgen (2018)
# With direct adjustment and interaction terms
#
##########################################

###########################################
#
# Flexible IPMW Implementation for Custom Datasets
# Based on Sun & Tchetgen Tchetgen (2018)
# With direct adjustment and interaction terms
#
##########################################

###########################################
#
# Flexible IPMW Implementation for Custom Datasets
# Based on Sun & Tchetgen Tchetgen (2018)
# With direct adjustment and conditional effects
#
##########################################

library(tidyverse)
library(mice)
library(Hmisc)


#bm: something is wrong with the nlm optimizer. it's heavily dependent on which init values I provide.

# Function to implement IPMW for any dataset with variables A1, B1, C1, D1 (or subset)
# Requires specifying:
# - data: your dataset
# - outcome: the outcome variable name (e.g., "D1")
# - exposure: the exposure variable name (e.g., "A1")
# - adjustment: vector of variable names to adjust for in outcome model (e.g., c("B1", "C1"))
ipmw_analysis <- function(data, outcome, exposure, adjustment = NULL, init_value = 0) {
  
  # Create a copy of the original data to work with
  original_data <- data
  
  # Identify variables to be used in the analysis
  analysis_vars <- unique(c(exposure, outcome, adjustment))
  
  # Extract only needed variables
  working_data <- original_data %>%
    select(all_of(analysis_vars))
  
  working_data$id = 1:nrow(working_data)
  
  # Determine missingness patterns
  message("Analyzing missingness patterns...")
  md_pattern <- mice::md.pattern(working_data[, analysis_vars])
  print(md_pattern)
  
  # Identify complete cases and create pattern indicator
  working_data <- working_data %>%
    mutate(M = 0)
  
  # Create indicators for each pattern
  num_patterns <- nrow(md_pattern) - 1  # Subtract 1 for the summary row
  
  # Loop through patterns and assign pattern numbers
  # Pattern 1 is complete cases
  for(p in 1:num_patterns) {
    pattern_mask <- rep(TRUE, nrow(working_data))
    
    # For each variable, check if it should be missing in this pattern
    for(v in 1:length(analysis_vars)) {
      var_name <- analysis_vars[v]
      # If md_pattern shows 0 for this variable in this pattern, it should be missing
      if(md_pattern[p, v] == 0) {
        pattern_mask <- pattern_mask & is.na(working_data[[var_name]])
      } else {
        pattern_mask <- pattern_mask & !is.na(working_data[[var_name]])
      }
    }
    
    # Assign pattern number
    working_data$M[pattern_mask] <- p
  }
  
  # Create indicators for each pattern (M1, M2, etc.)
  for(p in 1:num_patterns) {
    working_data[[paste0("M", p)]] <- ifelse(working_data$M == p, 1, 0)
  }
  
  # Display pattern distribution
  pattern_freq <- table(working_data$M)
  message("Distribution of missingness patterns:")
  print(pattern_freq)
  

  # Create outcome model formula with adjustment variables and interaction terms
  if(!is.null(adjustment) && length(adjustment) > 0) {
    # Create main effects terms
    main_effects <- paste(c(exposure, adjustment), collapse = " + ")
    
    # Create interaction terms for adjustment variables
    if(length(adjustment) > 1) {
      # Get all pairwise and higher-order interactions among adjustment variables
      adjustment_interactions <- c()
      for(i in 2:length(adjustment)) {
        interact_terms <- combn(adjustment, i, simplify = FALSE)
        for(terms in interact_terms) {
          adjustment_interactions <- c(adjustment_interactions, paste(terms, collapse = ":"))
        }
      }
      
      # Add interaction with exposure
      exposure_interactions <- paste(exposure, adjustment, sep = ":")
      
      # Combine all terms
      all_terms <- c(main_effects, adjustment_interactions, exposure_interactions)
      outcome_formula <- as.formula(paste(outcome, "~", paste(all_terms, collapse = " + ")))
    } else {
      # Just one adjustment variable, so only interaction with exposure
      outcome_formula <- as.formula(paste(outcome, "~", exposure, "+", adjustment, "+", 
                                          exposure, ":", adjustment))
    }
  } else {
    # No adjustment variables
    outcome_formula <- as.formula(paste(outcome, "~", exposure))
  }
  
  
  # ~ UMLE for IPMW if there are at least 2 patterns
  if(num_patterns > 1) {
    message("Implementing UMLE for IPMW...")
    
    # Build the negative log-likelihood function based on observed patterns
    umleLogL <- function(g, data) {
      # Initialize a matrix to hold pattern probabilities (one column per non-complete pattern)
      pattern_probs <- matrix(NA, nrow = nrow(data), ncol = num_patterns - 1)
      
      # Parameter index counter
      param_idx <- 1
      
      # For each non-complete pattern, build a probability model
      for(p in 2:num_patterns) {
        # Identify which variables are observed in this pattern
        observed_in_pattern <- colnames(md_pattern)[md_pattern[p, ] == 1]
        observed_in_pattern <- observed_in_pattern[observed_in_pattern %in% analysis_vars]
        
        # Create formula terms for this pattern's model
        formula_terms <- c()
        for(var in observed_in_pattern) {
          # Add term if it's observed in this pattern
          if(var %in% names(data)) {
            formula_terms <- c(formula_terms, paste0("g[", param_idx, "]*", var))
            param_idx <- param_idx + 1
          }
        }
        
        # Create the linear predictor with intercept
        lp <- paste0("g[", param_idx, "]", 
                     ifelse(length(formula_terms) > 0, paste0(" + ", paste(formula_terms, collapse = " + ")), ""))
        param_idx <- param_idx + 1
        
        # Calculate probability using logistic function
        pattern_probs[, p-1] <- with(data, plogis(eval(parse(text = lp))))
      }
      
      # Calculate sum of probabilities
      sump <- rowSums(pattern_probs, na.rm = TRUE)
      # Ensure sump is less than 1 to avoid negative probabilities for complete cases
      sump <- pmin(sump, 0.999)
      
      
      # Log-likelihood for each observation
      ilogL <- rep(NA, nrow(data))
      
      # Complete cases
      ilogL[data$M == 1] <- log(1 - sump[data$M == 1])
      
      # Non-complete cases
      for(p in 2:num_patterns) {
        ilogL[data$M == p] <- log(pattern_probs[data$M == p, p-1])
      }
      
      # Return negative log-likelihood
      return(-1 * sum(ilogL, na.rm = TRUE))
    }
    
    # Estimate number of parameters needed
    total_params <- 0
    for(p in 2:num_patterns) {
      observed_in_pattern <- colnames(md_pattern)[md_pattern[p, ] == 1]
      observed_in_pattern <- observed_in_pattern[observed_in_pattern %in% analysis_vars]
      total_params <- total_params + length(observed_in_pattern) + 1  # +1 for intercept
    }
    
    # Initial values: all zeros
    #init <- rep(0, total_params)
    init <- rep(init_value, total_params)
    
    # *** Optimize to find UMLE estimates
    umlefit <- tryCatch({
      nlm(umleLogL, init, data = working_data)
    }, error = function(e) {
      message("Error in UMLE optimization: ", e$message)
      return(NULL)
    })
    
    # c.f. neglogL for initial values:
    #init = c(-2, 0, -1, 0, -1)
    umleLogL(init, working_data)
    
    if(!is.null(umlefit)) {
      gumle <- umlefit$estimate
      
      # Calculate probability of each pattern for complete cases
      cc_umle <- working_data %>%
        filter(M == 1)
      
      # Calculate pattern probabilities
      param_idx <- 1
      pattern_probs <- matrix(NA, nrow = nrow(cc_umle), ncol = num_patterns - 1)
      
      for(p in 2:num_patterns) {
        observed_in_pattern <- colnames(md_pattern)[md_pattern[p, ] == 1]
        observed_in_pattern <- observed_in_pattern[observed_in_pattern %in% analysis_vars]
        
        formula_terms <- c()
        for(var in observed_in_pattern) {
          if(var %in% names(cc_umle)) {
            formula_terms <- c(formula_terms, paste0("gumle[", param_idx, "]*", var))
            param_idx <- param_idx + 1
          }
        }
        
        lp <- paste0("gumle[", param_idx, "]", 
                     ifelse(length(formula_terms) > 0, paste0(" + ", paste(formula_terms, collapse = " + ")), ""))
        param_idx <- param_idx + 1
        
        pattern_probs[, p-1] <- with(cc_umle, plogis(eval(parse(text = lp))))
      }
      
      # Calculate probability of being a complete case
      cc_umle$p1 <- 1 - rowSums(pattern_probs)
      # Ensure probabilities are positive
      cc_umle$p1 <- pmax(cc_umle$p1, 0.001)
      
      # Obtain marginal probability of complete cases
      mnum <- mean(working_data$M1)
      
      # Create IPMW weights
      cc_umle <- cc_umle %>%
        mutate(ipmw = mnum / p1)
      
      message("IPMW summary:")
      print(summary(cc_umle$ipmw))
      
      # ***Fit weighted outcome model using geeglm
      uoutmod <- tryCatch({
        geeglm(outcome_formula, 
               data = cc_umle, 
               weights = cc_umle$ipmw, 
               id = cc_umle$id, 
               corstr = "independence")
      }, error = function(e) {
        message("Error in UMLE outcome model: ", e$message)
        return(NULL)
      })
      
      # ***added
      # Calculate simple treatment effect and return results
      # Calculate treatment effect and return results
      if(!is.null(uoutmod)) {
        # Extract the coefficient for the exposure variable
        treatment_effect <- coef(uoutmod)[[exposure]]
        se <- coef(summary(uoutmod))[exposure, 2]
        
        results_umle <- tibble(
          estimate = treatment_effect,
          se = se,
          lcl = treatment_effect - 1.96 * se,
          ucl = treatment_effect + 1.96 * se
        )
        
        message("UMLE-IPMW analysis results:")
        print(results_umle)
        
        # Return results
        return(list(
          cc_results = if(exists("results_cc")) results_cc else NULL,
          ipmw_results = results_umle,
          ipmw_model = uoutmod,
          cc_model = cc_out,
          weighted_data = cc_umle,
          missingness_patterns = table(working_data$M),
          umle_parameters = gumle
        ))
      }
      
  }  # end if(!is.null(umlefit))
        
} # end if(num_patterns > 1)
  

  
}
  
res = ipmw_analysis(
   data = du,
   outcome = "B",
   exposure = "A",
   adjustment = c("C", "D") )

res$ipmw_results


##### sanity checks:

# compare to existing results for manually implemented IPMW for monotone MAR (should agree)
sim_obj = sim_data(.p = data.frame(N=10000,
                                   coef_of_interest = "A",
                                   dag_name = "6A" ) )


du = sim_obj$du

# with init values of 0
# res = ipmw_analysis(
#   data = du,
#   outcome = "B",
#   exposure = "A",
#   adjustment = c("C") )
# 
# res$ipmw_results
# res$umle_parameters

res = ipmw_analysis(
  data = du,
  outcome = "B",
  exposure = "A",
  adjustment = c("C"),
  init_value = -2)

res$ipmw_results
res$umle_parameters

# with these init values, it comes up with more reasonable umle_parameters, but still overestimates the CATE. hmmmm.

