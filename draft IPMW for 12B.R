

sim_obj = sim_data(.p = data.frame(N=1000,
                                   coef_of_interest = "A",
                                   dag_name = "12B" ) )


du = sim_obj$du


# CLAUDE  -------------------------------------------------



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



# Function to implement IPMW for any dataset with variables A1, B1, C1, D1 (or subset)
# Requires specifying:
# - data: your dataset
# - outcome: the outcome variable name (e.g., "D1")
# - exposure: the exposure variable name (e.g., "A1")
# - adjustment: vector of variable names to adjust for in outcome model (e.g., c("B1", "C1"))
ipmw_analysis <- function(data, outcome, exposure, adjustment = NULL) {
  
  # Create a copy of the original data to work with
  original_data <- data
  
  # Identify variables to be used in the analysis
  analysis_vars <- unique(c(exposure, outcome, adjustment))
  
  # Extract only needed variables
  working_data <- original_data %>%
    select(all_of(analysis_vars))
  
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
    init <- rep(0, total_params)
    
    # Optimize to find UMLE estimates
    umlefit <- tryCatch({
      nlm(umleLogL, init, data = working_data)
    }, error = function(e) {
      message("Error in UMLE optimization: ", e$message)
      return(NULL)
    })
    
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
      
      #**** Fit weighted outcome model with adjustment variables and interactions
      uoutmod <- tryCatch({
        lm(outcome_formula, data = cc_umle, weights = cc_umle$ipmw)
      }, error = function(e) {
        message("Error in UMLE outcome model: ", e$message)
        return(NULL)
      })
      
      # ***added
      # Calculate simple treatment effect and return results
      if(!is.null(uoutmod)) {
        # Just extract the coefficient for the exposure variable
        treatment_effect <- coef(uoutmod)[[exposure]]
        se <- summary(uoutmod)$coefficients[exposure, 2]
        
        results_umle <- tibble(
          estimate = treatment_effect,
          se = se,
          lcl = treatment_effect - 1.96 * se,
          ucl = treatment_effect + 1.96 * se
        )
        
        message("UMLE-IPMW analysis results:")
        print(results_umle)
        
        # Return results and model objects
        return(list(
          #cc_results = if(exists("results_cc")) results_cc else NULL,
          ipmw_results = results_umle,
          ipmw_model = uoutmod,
          #cc_model = cc_out,
          weighted_data = cc_umle,
          missingness_patterns = table(working_data$M),
          umle_parameters = gumle
        ))
      }
      
  }  # end if(!is.null(umlefit))
        
} # end if(num_patterns > 1)
  

  
}
  
  
# Example usage:
# results <- ipmw_analysis(
#   data = my_data,
#   outcome = "D1",
#   exposure = "A1",
#   adjustment = c("B1", "C1")
# )           
                          

res = ipmw_analysis(
   data = du,
   outcome = "B",
   exposure = "A",
   adjustment = c("C", "D") )

res$ipmw_results


# CHATGPT  -------------------------------------------------

data = du

# add pattern indicator
# Generate missingness pattern variable M (1 = complete case)
mat <- as.matrix(!is.na(data[, vars]))
pattern_strings <- apply(mat, 1, paste0, collapse = "")
unique_patterns <- unique(pattern_strings)
mapping <- match(pattern_strings, unique_patterns)

complete_pattern <- paste0(rep("1", length(vars)), collapse = "")
complete_id <- which(unique_patterns == complete_pattern)
if (length(complete_id) > 0 && complete_id != 1) {
  mapping[mapping == 1] <- -1
  mapping[mapping == complete_id] <- 1
  mapping[mapping == -1] <- complete_id
}
data$M <- mapping

# Relabel to ensure complete cases are M = 1
pattern_matrix <- unique(apply(!is.na(data[, vars]), 1, function(r) paste0(as.integer(r), collapse = "")))
complete_pattern <- paste0(rep(1, length(vars)), collapse = "")
complete_id <- match(complete_pattern, pattern_matrix)
M_map <- seq_along(pattern_matrix)
M_map[complete_id] <- 1
if (complete_id != 1) M_map[1] <- complete_id
data$M <- M_map[data$M]


umleLogL <- function(g, data) {
  vars <- c("A", "B", "C", "D")

  Mvals <- sort(unique(data$M))
  Mvals <- Mvals[Mvals != 1]
  num_patterns <- length(Mvals)
  
  ilogL <- rep(NA, nrow(data))
  sump_vec <- rep(0, nrow(data))
  offset <- 0
  
  for (j in seq_along(Mvals)) {
    m <- Mvals[j]
    rows_m <- which(data$M == m)
    obs_vars <- names(which(colMeans(!is.na(data[rows_m, vars])) == 1))
    
    if (length(obs_vars) == 0) {
      # Intercept-only model
      eta <- rep(g[offset + 1], length(rows_m))
      offset <- offset + 1
    } else {
      rhs <- paste(obs_vars, collapse = "*")
      fml <- as.formula(paste("~", rhs))
      subdata <- data[rows_m, obs_vars, drop = FALSE]
      subdata[is.na(subdata)] <- 0  # safe fallback
      X <- model.matrix(fml, data = subdata)
      k <- ncol(X)
      coefs <- g[(offset + 1):(offset + k)]
      offset <- offset + k
      eta <- X %*% matrix(coefs, ncol = 1)
    }
    
    probs <- plogis(eta)
    sump_vec[rows_m] <- sump_vec[rows_m] + probs
    ilogL[rows_m] <- log(probs)
  }
  
  # Clamp sump to ensure valid log(1 - sump)
  sump_vec <- pmin(sump_vec, 1 - 1e-8)
  ilogL[data$M == 1] <- log(1 - sump_vec[data$M == 1])
  
  # Return negative log-likelihood
  return(-sum(ilogL, na.rm = TRUE))
}


# Minimize negative log-likelihood
init <- c(0,0,0,0) #starting values
umlefit <- nlm(umleLogL, init, data=data) # need to use data instead of du bc needs to have M added
( gumle <- umlefit$est ) #umle gamma estimates


# Obtain marginal pr R=1
( mnum <- mean(withmiss$R1) )

# Obtain probability of complete case
cc_umle <- withmiss %>%
  filter(R==1) %>%
  mutate(p2 = plogis(gumle[1] + gumle[2]*z + gumle[3]*x             ),
         p3 = plogis(gumle[4]              + gumle[5]*x             ),
         p4 = plogis(gumle[6]              + gumle[7]*x + gumle[8]*y),
         p1 = 1 - p2 - p3 - p4,
         ipmw = mnum/p1)

summary(cc_umle$ipmw)
sum(cc_umle$ipmw)





# PART 2: in progress


# Compute marginal probability of complete case
mnum <- mean(data$M == 1)

# Get all non-complete missingness patterns
Mvals <- sort(unique(data$M))
Mvals <- Mvals[Mvals != 1]

# Restrict to complete cases
cc_rows <- which(data$M == 1)
cc_data <- data[cc_rows, ]
n_cc <- nrow(cc_data)

# Initialize matrix of pattern probabilities
pattern_probs <- matrix(0, nrow = n_cc, ncol = length(Mvals))
offset <- 0

for (j in seq_along(Mvals)) {
  m <- Mvals[j]
  obs_vars <- names(which(colMeans(!is.na(data[data$M == m, vars])) == 1))
  
  if (length(obs_vars) == 0) {
    eta <- rep(gumle[offset + 1], n_cc)
    offset <- offset + 1
  } else {
    rhs <- paste(obs_vars, collapse = "*")
    fml <- as.formula(paste("~", rhs))
    subdata <- cc_data[, obs_vars, drop = FALSE]
    subdata[is.na(subdata)] <- 0
    X <- model.matrix(fml, data = subdata)
    k <- ncol(X)
    coefs <- gumle[(offset + 1):(offset + k)]
    offset <- offset + k
    eta <- X %*% matrix(coefs, ncol = 1)
  }
  
  pattern_probs[, j] <- plogis(eta)
}

# Compute p1 and IPMW for complete cases only
sump <- rowSums(pattern_probs)
sump <- pmin(sump, 1 - 1e-8)
p1 <- 1 - sump
ipmw <- mnum / p1

# Attach IPMW to complete-case data frame
cc_umle = cc_data
cc_umle$ipmw = ipmw
ipmw = ipmw

summary(cc_umle$ipmw)
sum(cc_umle$ipmw)

#bm: issue now is that every entry of sump and hence ipmw is NA! 
# you got this!! 




# NOT UPDATED YET
# Fit weighted propensity score model
upsmodel <- glm(x ~ z, data=cc_umle, family='binomial', weights = cc_umle$ipmw)

# Create iptw and combine weights
uwithweight <- cc_umle %>%
  mutate(pscore = predict(upsmodel, ., type="response"),
         iptw = x/pscore + (1-x)/(1-pscore),
         ipw = ipmw*iptw)


# Fit weighted linear-binomial outcome model
uoutmod <- geeglm(y ~ x, family=binomial("identity"), data=uwithweight, 
                  weights=uwithweight$ipw, id=id, corstr="independence")

# Results
results_umle <- tibble(rd = coef(uoutmod)[[2]],
                       se = coef(summary(uoutmod))[[2,2]],
                       lcl = tidy(uoutmod, conf.int=T, exp = F)[[2,"conf.low"]],
                       ucl = tidy(uoutmod, conf.int=T, exp = F)[[2,"conf.high"]])
results_umle