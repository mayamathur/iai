# =====================================================================
# Compares  (1) legacy AF4-sp   (2) miapack::mia [SP]   (3) miapack::mia_ice [ICE]

# Run from the directory containing your CURRENT helper + Wblock helper.
# Requires: miapack (branch ice-implementation), dplyr, the current sim_data().
# =====================================================================

suppressMessages({
  library(dplyr)
  library(miapack)   # devtools::install_github("stmcg/miapack", ref = "ice-implementation")
})

# ---- source the CURRENT code that defines sim_data() -----------------
# Edit these paths to point at your current helper files.
source("helper_IAI.R")          # defines sim_data(), expit(), etc.
# helper_IAI_Wblock.R is sourced inside helper_IAI.R; for |W|=1 it is never exercised,
# but it must exist so the source() call at the top of helper_IAI.R succeeds.



# ---- (1) legacy AF4-sp, inlined so the reprex is self-contained -------
# Reads a column named D (the observed auxiliary) + RA/RC/RD gating.
# We alias the current data's W01 -> D and RW01 -> RD before calling it.
af4_sp <- function(du) {
  
  
  if (p$W_dim == 1) {
    dc <- du[complete.cases(du), ]
    dw <- du %>% filter(RA == 1 & RD == 1 & RC == 1)
    
    m_B_ac <- function(a, c) {
      fit_B <- lm(B ~ A * C * D, data = dc)
      fit_D <- glm(D ~ A * C, data = dw, family = binomial)
      
      # summing over the 2 possible values of W
      sum(sapply(0:1, function(d) {
        # p(W=1 | a, c, RA = RC = RW = 1)
        p_d1 <- predict(fit_D, newdata = data.frame(A = a, C = c), type = "response")
        # turn it into p(W=0|...) if needed (d=0 case)
        p_d  <- if (d == 1) p_d1 else 1 - p_d1
        # E[Y | a, c, d, RY = RA = RC = RW = 1]
        mu   <- predict(fit_B, newdata = data.frame(A = a, C = c, D = d))
        # summand of MIA functional
        p_d * mu
      }))
      
    }
    cate <- m_B_ac(1, 0) - m_B_ac(0, 0)
    int  <- m_B_ac(0, 0)
    return( c(cate = cate, int = int) )
  }

  
  else {
    # not implemented for W_dim > 1 cases
    return(  c(cate = NA, int = NA) )
  }
}

# build the data frame miapack expects ----------------------------
# miapack wants columns: Y, X1, X2, W  (+ it derives R_X, R_W, R_Y from NAs).
# Map paper/code names -> miapack names:  B->Y, A->X2, C->X1, W01->W.
# (X2 is the coef of interest / "A"; X1 is C.)
to_mia_df <- function(du) {
  
  if (p$W_dim == 1 ) {
    data.frame(
      Y  = du$B,      # outcome, NA where RB==0
      X1 = du$C,      
      X2 = du$A,
      W  = du$W01     # observed auxiliary, NA where RW01==0
    )
  } else if (p$W_dim == 10) {
    du2 = du %>% rename(Y = B) %>%
      select(A, Y, C, matches("^W[0-9]+$"))
  }

}

# ---- (2) SP and (3) ICE via miapack ----------------------------------



mia_sp_est <- function(du) {
  d <- to_mia_df(du)
  

  if ( p$W_dim == 1 ) {
    # the mia call below hard-codes what we're passing in doParallel:
    # > X_names
    # [1] "A" "C"
    # > Xv1
    # [1] 1 0
    # > Xv0
    # [1] 0 0
    # > Y_form
    # Y ~ (A + C + A:C) * (W01)
    # > W_forms
    # [[1]]
    # W01 ~ A + C
    # <environment: 0x751d5f3f0>
    #   
    #   > W_type
    # [1] "normal"
    # > outer_form
    # g_hat ~ A * C
    
    mia_res <- mia(data = d,
                   X_names = c("X1", "X2"), # X2 is the exposure
                   X_values_1 = c(0,1),
                   X_values_2 = c(0,0),
                   Y_model = Y ~ W * X1 * X2, W_model = W ~ X1 + X2,
                   Y_type = "continuous", W_type = "binary", n_mc = 20000)
    
    return( c(cate = mia_res$contrast_est, int = mia_res$mean_est_2) )
    
    
  } else if ( p$W_dim == 10 ) {
    

    # beginning of stuff that's verbatim from doParallel
    #  (either hard-coded or using the same code to produce objects) 
    X_names = c("A", "C")
    
    # # as in doParallel
    # Y_form = as.formula("Y ~ (A + C + A:C) * (W01 + W02 + W03 + W04 + W05 + W06 + W07 + 
    #                                 W08 + W09 + W10)")
    # remove three-way A*C*W interactions, which result in overparameterization and aren't needed given how Y is generated anyway
    #***NEED TO MAKE THIS CHANGE IN DOPARALLEL
    Y_form = as.formula("Y ~ (A + C) * (W01 + W02 + W03 + W04 + W05 + W06 + W07 + 
                                    W08 + W09 + W10) + A:C")
    
    W_type = ifelse( seq_along(Wobs) <= p$W_n_cont, "normal", "binary" )
    
    # one W-model per component (sequential factorization):
    #      w_j ~ exposure + covars + earlier components
    W_forms = lapply( seq_along(Wobs), function(j) {
      preds = c(X_names, Wobs[seq_len(j - 1)])
      as.formula( paste(Wobs[j], "~", paste(preds, collapse = " + ")) )
    })
    
  
    mia_res <- mia(data = d,
                   X_names = X_names,
                   X_values_1 = c(1,0),  # opposite order from W_dim=10 case since here A is listed first in X_names
                   X_values_2 = c(0,0),
                   Y_model = Y_form,
                   W_model = W_forms,
                   Y_type = "continuous",
                   W_type = W_type,
                   n_mc = 20000)
    
    return( c(cate = mia_res$contrast_est, int = mia_res$mean_est_1) )
    
  }

}




mia_ice_est <- function(du) {
  
  if ( p$W_dim == 1 ) {
    d <- to_mia_df(du)
    mia_res <- mia_ice(data = d, X_names = c("X1", "X2"), X_values_1 = c(0,1),
                       X_values_2 = c(0,0),
                       Y_model = Y ~ W * X1 * X2,
                       outer_model = g_hat ~ X1 * X2,
                       Y_type = "continuous")
    
    return( c(cate = mia_res$contrast_est, int = mia_res$mean_est_2) )
    
    
  } else if ( p$W_dim == 10 ) {

    # beginning of stuff that's verbatim from doParallel
    #  (either hard-coded or using the same code to produce objects) 
    X_names = c("A", "C")

    # # as in doParallel
    # Y_form = as.formula("Y ~ (A + C + A:C) * (W01 + W02 + W03 + W04 + W05 + W06 + W07 + 
    #                                 W08 + W09 + W10)")
    # remove three-way A*C*W interactions, which result in overparameterization and aren't needed given how Y is generated anyway
    Y_form = as.formula("Y ~ (A + C) * (W01 + W02 + W03 + W04 + W05 + W06 + W07 + 
                                    W08 + W09 + W10) + A:C")
    
    W_type = ifelse( seq_along(Wobs) <= p$W_n_cont, "normal", "binary" )
    
    # one W-model per component (sequential factorization):
    #      w_j ~ exposure + covars + earlier components
    W_forms = lapply( seq_along(Wobs), function(j) {
      preds = c(X_names, Wobs[seq_len(j - 1)])
      as.formula( paste(Wobs[j], "~", paste(preds, collapse = " + ")) )
    })
    
    mia_res <- mia_ice(data = d,
                   X_names = X_names,
                   X_values_1 = c(1,0),  # opposite order from W_dim=10 case since here A is listed first in X_names
                   X_values_2 = c(0,0),
                   Y_model = Y_form,
                   outer_model = g_hat ~ A * C,
                   Y_type = "continuous")
    
    return( c(cate = mia_res$contrast_est, int = mia_res$mean_est_1) )
  }
  

}

# gold std
gold_mod = function(du){

    mod = lm(B1 ~ A1 * C1, data = du)
    return( list(gold_cate = coef(mod)[["A1"]],
                 gold_int = coef(mod)[["(Intercept)"]] ) )

}



# ---- RUN SIMPLE SIMULATION ---------------------------------------------------

# # W_dim = 1 scens
# p <- data.frame(    dag_name         = "2A",
#                     N                = 1000,
#                     model            = "OLS",
#                     coef_of_interest = "A",
#                     W_dim            = 1,      # <= 1 triggers the LEGACY scalar-W branch
#                     imp_m = 50, imp_maxit = 100, mice_method = NA,
#                     boot_reps_af4 = 0, mia_n_mc = 10000)

# W_dim = 10 scens
p <- data.frame(    dag_name         = "2A",
                    N                = 1000,
                    model            = "OLS",
                    coef_of_interest = "A",
                    W_dim            = 10,      # <= 1 triggers the LEGACY scalar-W branch
                    imp_m = 50, imp_maxit = 100, mice_method = NA,
                    boot_reps_af4 = 0, mia_n_mc = 10000,
                    
                    # W-block of parameters
                    W_n_cont          = 5,   # 5 continuous, 5 binary when W_dim = 10
                    
                    # W^+ / W^- split: complete vs incomplete components, type-balanced.
                    # Set W_n_cont_complete = W_n_bin_complete = 0 for an all-incomplete arm.
                    W_n_cont_complete = 3,
                    W_n_bin_complete  = 2,
                    
                    W_rho             = 0.4,  # LATENT-scale correlation
                    W_cor_type        = "exch",                        # "exch" or "ar1"
                    W_bin_prob        = 0.5,                            # marginal P(W_binary = 1)
                    
                    # target marginal P(R_Wj = 0) for incomplete components. Legacy value is
                    # 0.4252 (what expit(-1 + 3*D1) implies); fine at W_dim = 1 but leaves ~3.6%
                    # complete cases at W_dim = 10, so the high-dim arms use 0.10.
                    W_miss_rate       = 0.10,
                    
                    # required by the W-block generator (their absence caused the crash):
                    W_parent_coef     = 1,     # strength of the W parent (X2 or Y, per DAG) -> W
                    W_n_inter         = 3,     # # of W_j*W_k interaction terms in S_R (needs 2*W_n_inter <= W_dim)
                    W_inter_coef      = 1 )  # coefficient on each interaction term)


# # test
# sim_obj = sim_data(p)
# du = sim_obj$du




if(exists("mia_res")) rm(mia_res)
if(exists("res")) rm(res)
set.seed(1)
R <- 500
res <- lapply(1:R, function(r) {
  sd <- sim_data(p)
  du <- sd$du

  if ( p$W_dim == 1 ){
    # alias for legacy AF4:
    du$D  <- du$W01
    du$RD <- du$RW01
  }

  g <- gold_mod(du)
  a  <- af4_sp(du)
  s  <- mia_sp_est(du)
  i  <- mia_ice_est(du)

  data.frame(rep = r,
             gold_cate = g["gold_cate"], gold_int = g["gold_int"],
             af4_cate = a["cate"], af4_int = a["int"],
             sp_cate  = s["cate"], sp_int  = s["int"],
             ice_cate = i["cate"], ice_int = i["int"])
})
res <- bind_rows(res)

colMeans(res)



# # ---- report bias -----------------------------------------------------
# bias <- function(x, truth) mean(x - truth, na.rm = TRUE)
# cat("\n================ BIAS (est - truth) ================\n")
# cat(sprintf("beta_X2 (CATE@C=0), truth=%.4f\n", truth_cate))
# cat(sprintf("   AF4-sp : % .4f\n", bias(res$af4_cate, truth_cate)))
# cat(sprintf("   MIA-sp : % .4f\n", bias(res$sp_cate,  truth_cate)))
# cat(sprintf("   MIA-ice: % .4f\n", bias(res$ice_cate, truth_cate)))
# cat(sprintf("\nbeta_0 (E[B|A=0,C=0]), truth=%.4f\n", truth_int))
# cat(sprintf("   AF4-sp : % .4f\n", bias(res$af4_int, truth_int)))
# cat(sprintf("   MIA-sp : %  .4f\n", bias(res$sp_int,  truth_int)))
# cat(sprintf("   MIA-ice: % .4f\n", bias(res$ice_int, truth_int)))
# 
# # ---- one-rep deep dive: dump fitted models so divergence is visible --
# cat("\n================ SINGLE-REP MODEL DUMP ================\n")
# set.seed(42)
# sd <- sim_data(make_p(N = 5000)); du <- sd$du; du$D <- du$W01; du$RD <- du$RW01
# dc <- du[complete.cases(du), ]
# dw <- du %>% filter(RA==1 & RD==1 & RC==1)
# cat("\n-- legacy AF4 subsets --\n")
# cat(sprintf("dc (Y-model, complete.cases) n = %d\n", nrow(dc)))
# cat(sprintf("dw (W-model, RA&RD&RC)       n = %d\n", nrow(dw)))
# d <- to_mia_df(du)
# R_X <- complete.cases(d[, c("X1","X2")]); R_W <- complete.cases(d[, "W", drop=FALSE]); R_Y <- !is.na(d$Y)
# cat("\n-- miapack subsets --\n")
# cat(sprintf("data_fit_Y (R_X & R_W & R_Y) n = %d\n", sum(R_X & R_W & R_Y)))
# cat(sprintf("data_fit_W (R_X & R_W)       n = %d\n", sum(R_X & R_W)))
# cat("\nIf dc n  != data_fit_Y n, or dw n != data_fit_W n, the fit subsets differ -> that's the bug.\n")
# cat("Legacy Y-model coefs (B ~ A*C*D):\n");  print(round(coef(lm(B ~ A*C*D, data = dc)), 4))
# cat("\nmiapack Y-model coefs (Y ~ W*X1*X2 on data_fit_Y):\n")
# print(round(coef(lm(Y ~ W*X1*X2, data = d[R_X & R_W & R_Y, ])), 4))