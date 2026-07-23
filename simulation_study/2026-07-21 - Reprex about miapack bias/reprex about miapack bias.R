# =====================================================================
# reprex: DGM (a) / code DAG 1A, |W| = 1
# Compares  (1) legacy AF4-sp   (2) miapack::mia [SP]   (3) miapack::mia_ice [ICE]
# on the SAME simulated data, over many reps, to reproduce the ~ -0.08 bias
# in SP/ICE that should not be there (AF4-sp expected ~ 0).
#
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


# ---- parameter object for sim_data (|W|=1 arm) -----------------------
p <- data.frame(    dag_name         = "2A",
                    N                = 1000,
                    model            = "OLS",
                    coef_of_interest = "A",
                    W_dim            = 1,      # <= 1 triggers the LEGACY scalar-W branch
                    imp_m = 50, imp_maxit = 100, mice_method = NA,
                    boot_reps_af4 = 0, mia_n_mc = 10000)


# test
sim_data(p)

# ---- (1) legacy AF4-sp, inlined so the reprex is self-contained -------
# Reads a column named D (the observed auxiliary) + RA/RC/RD gating.
# We alias the current data's W01 -> D and RW01 -> RD before calling it.
af4_sp <- function(du) {
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
  c(cate = cate, int = int)
}

# ---- build the data frame miapack expects ----------------------------
# miapack wants columns: Y, X1, X2, W  (+ it derives R_X, R_W, R_Y from NAs).
# Map paper/code names -> miapack names:  B->Y, A->X2, C->X1, W01->W.
# (X2 is the coef of interest / "A"; X1 is C.)
to_mia_df <- function(du) {
  data.frame(
    Y  = du$B,      # outcome, NA where RB==0
    X1 = du$C,      
    X2 = du$A,
    W  = du$W01     # observed auxiliary, NA where RW01==0
  )
}

# ---- (2) SP and (3) ICE via miapack ----------------------------------

# reproduce what we're passing in doParallel:
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


mia_sp_est <- function(du) {
  d <- to_mia_df(du)
  # mu(X2=0) and mu(X2=1) at ... note: X1 (=C) must also be fixed to 0 to match CATE@C=0.
  # miapack marginalizes over W but NOT over other X's, so we fix X1=0 in both.
  mia_res <- mia(data = d, X_names = c("X1", "X2"), X_values_1 = c(0,1),
            X_values_2 = c(0,0),
            Y_model = Y ~ W * X1 * X2, W_model = W ~ X1 + X2,
            Y_type = "continuous", W_type = "binary", n_mc = 20000)

  c(cate = mia_res$contrast_est, int = mia_res$mean_est_2)  
}

mia_ice_est <- function(du) {
  d <- to_mia_df(du)
  mia_res <- mia_ice(data = d, X_names = c("X1", "X2"), X_values_1 = c(0,1),
                 X_values_2 = c(0,0),
                 Y_model = Y ~ W * X1 * X2, outer_model = g_hat ~ X1 * X2,
                 Y_type = "continuous")
  
  c(cate = mia_res$contrast_est, int = mia_res$mean_est_2) 
}

# gold std
gold_mod = function(du){
  mod = lm(B1 ~ A1 * C1, data = du)
  return( list(gold_cate = coef(mod)[["A1"]],
               gold_int = coef(mod)[["(Intercept)"]] ) )
}



# ---- run many reps ---------------------------------------------------



if(exists("res")) rm(res)
set.seed(1)
R <- 500
res <- lapply(1:R, function(r) {
  sd <- sim_data(p)
  du <- sd$du
  # alias for legacy AF4:
  du$D  <- du$W01
  du$RD <- du$RW01
  g <- tryCatch(gold_mod(du),     error = function(e) c(cate=NA, int=NA))
  a  <- tryCatch(af4_sp(du),     error = function(e) c(cate=NA, int=NA))
  s  <- tryCatch(mia_sp_est(du), error = function(e) c(cate=NA, int=NA))
  i  <- tryCatch(mia_ice_est(du),error = function(e) c(cate=NA, int=NA))
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
# cat(sprintf("   MIA-sp : % .4f\n", bias(res$sp_int,  truth_int)))
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