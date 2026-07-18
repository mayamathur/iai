
# ============================================================================
# HIGH-DIMENSIONAL W BLOCK
# ============================================================================
# Generates the auxiliary block W = (W_1, ..., W_d) with:
#   - mixed types: first .p$W_n_cont components continuous, the rest binary
#   - correlation across ALL components (incl. cont-bin) via a Gaussian copula
#   - optionally a parent (X_2 -> W in DAG 1A)
#   - per-component missingness of one of two types:
#       W_R_type = "self" : W_j -> R_Wj, intercept solved to hit .p$W_miss_rate
#                           (the 5-* / 6-* DAGs)
#       W_R_type = "mcar" : R_Wj ~ Bern(1 - .p$W_miss_rate), no parents
#                           (DAG 1A, where legacy RD ~ Bern(0.5))
#
# Component naming (W_dim >= 1):
#   truth:      W01_true, ..., W10_true
#   observed:   W01,      ..., W10       (NA where R = 0)
#   indicators: RW01,     ..., RW10      (identically 1 for complete comps)
#
# Backward compatibility:
#   .p$W_dim == 0  -> no W block at all (the 5-MAR-* / 6-MAR-* arms)
#   .p$W_dim == 1  -> additionally aliases D1 / D / RD
#   In DAG 1A, W_dim <= 1 uses the ORIGINAL legacy code path untouched; the
#   block below is only engaged for W_dim > 1.
# ----------------------------------------------------------------------------


# ~ Defaults ------------------------------------------------------------------

w_default_W_params = function() {
  list(
    W_dim             = 10,     # number of auxiliary components (0 = none, 1 = legacy D)
    W_n_cont          = 5,      # how many are continuous; remaining W_dim - W_n_cont are binary
    W_n_cont_complete = 3,      # how many continuous components are COMPLETE (in W^+)
    W_n_bin_complete  = 2,      # how many binary components are COMPLETE (in W^+)
    W_rho             = 0.4,    # latent correlation parameter
    W_cor_type        = "exch", # "exch" (exchangeable) or "ar1"
    W_bin_prob        = 0.5,    # marginal P(W_j = 1) for binary components
    W_miss_rate       = 0.10,   # target marginal P(R_Wj = 0) for incomplete components
    W_R_type          = "self", # "self" (W_j -> R_Wj) or "mcar" (R_Wj has no parents)
    W_R_slope_cont    = 1,      # coef of W_j in logit P(R_Wj = 1), continuous comps
    W_R_slope_bin     = 3,      # coef of W_j in logit P(R_Wj = 1), binary comps
    W_parent_coef     = 1,      # coef of the parent in the latent mean (0 = no parent)
    W_n_inter         = 3,      # number of W_j x W_k interaction terms in the R_Y index
    W_inter_coef      = 1       # coefficient on each interaction term
  )
}


# ~ Names and index bookkeeping -----------------------------------------------

w_names = function(.p) {

  d = .p$W_dim
  if ( is.null(d) || is.na(d) || d == 0 ) {
    return( list( true = character(0), obs = character(0), R = character(0),
                  cont = integer(0), bin = integer(0) ) )
  }

  idx    = sprintf("%02d", 1:d)
  n_cont = .p$W_n_cont

  list( true = paste0("W", idx, "_true"),
        obs  = paste0("W", idx),
        R    = paste0("RW", idx),
        cont = if (n_cont > 0) 1:n_cont else integer(0),
        bin  = if (n_cont < d) (n_cont + 1):d else integer(0) )
}


# TRUE = component is complete (in W^+); FALSE = incomplete (in W^-).
w_complete_flag = function(.p) {

  nm = w_names(.p)
  d  = .p$W_dim
  if (d == 0) return(logical(0))

  if ( .p$W_n_cont_complete > length(nm$cont) ) stop("W_n_cont_complete exceeds W_n_cont")
  if ( .p$W_n_bin_complete  > length(nm$bin)  ) stop("W_n_bin_complete exceeds the number of binary components")

  flag = rep(FALSE, d)
  if ( .p$W_n_cont_complete > 0 ) flag[ nm$cont[ seq_len(.p$W_n_cont_complete) ] ] = TRUE
  if ( .p$W_n_bin_complete  > 0 ) flag[ nm$bin[  seq_len(.p$W_n_bin_complete)  ] ] = TRUE
  flag
}


# Which components interact in the R_Y index. Deliberately simple and
# deterministic: adjacent disjoint pairs (1,2), (3,4), (5,6), ...
# With W_n_cont = 5, the pair (5,6) is a cont x bin interaction.
w_inter_pairs = function(.p) {

  n = .p$W_n_inter
  if ( is.null(n) || is.na(n) || n == 0 || .p$W_dim < 2 ) return( matrix(nrow = 0, ncol = 2) )
  if ( 2*n > .p$W_dim ) stop("W_n_inter too large: need 2 * W_n_inter <= W_dim")

  cbind( seq(1, by = 2, length.out = n),
         seq(2, by = 2, length.out = n) )
}


# ~ Latent correlation --------------------------------------------------------

w_latent_Sigma = function(.p) {

  d = .p$W_dim

  if ( .p$W_cor_type == "exch" ) {
    S = matrix(.p$W_rho, nrow = d, ncol = d)
    diag(S) = 1

  } else if ( .p$W_cor_type == "ar1" ) {
    S = .p$W_rho ^ abs( outer(1:d, 1:d, "-") )

  } else {
    stop("Unrecognized W_cor_type: use 'exch' or 'ar1'")
  }

  if ( min( eigen(S, symmetric = TRUE, only.values = TRUE)$values ) <= 0 ) {
    stop("W latent correlation matrix is not positive definite for this W_rho / W_dim")
  }

  S
}


# ~ Calibrating the R_W models ------------------------------------------------

# Solve the intercept a such that P(R_Wj = 1) = target_obs, given slope b.
#   Continuous: W_j ~ N(0,1)          => P(R=1) = E[ expit(a + b*W) ]
#   Binary:     W_j ~ Bern(bin_prob)  => P(R=1) = (1-pi)*expit(a) + pi*expit(a + b)
# Only used when W_R_type == "self".
w_solve_R_intercept = function(b, target_obs, type, bin_prob = NULL) {

  if ( target_obs <= 0 | target_obs >= 1 ) stop("target_obs must be strictly between 0 and 1")

  if ( type == "cont" ) {
    pr = function(a) integrate( function(z) plogis(a + b*z) * dnorm(z),
                                lower = -Inf, upper = Inf )$value
  } else if ( type == "bin" ) {
    pr = function(a) (1 - bin_prob) * plogis(a) + bin_prob * plogis(a + b)
  } else {
    stop("type must be 'cont' or 'bin'")
  }

  uniroot( function(a) pr(a) - target_obs, interval = c(-30, 30), tol = 1e-10 )$root
}


# ~ Generator -----------------------------------------------------------------

# parent: numeric vector of length .p$N (e.g. A1 in DAG 1A), or NULL for the
#         5-* / 6-* DAGs where W is a source node.
# thresh: fixed binarization thresholds on the latent scale. Supply the values
#         returned by the calibration pilot so that they are DGM constants
#         rather than functions of the realized sample. NULL => solve from the
#         sample (used only inside the pilot itself).
gen_W_block = function(.p, parent = NULL, thresh = NULL) {

  d = .p$W_dim
  if ( is.null(d) || is.na(d) || d == 0 ) return(NULL)

  nm   = w_names(.p)
  comp = w_complete_flag(.p)

  # ~~ latent draws (vectorized; no rowwise) ----
  # Z = W_parent_coef * parent + E,  E ~ MVN(0, R)
  # The latent is deliberately NOT standardized: the W -> Y and W -> R_Y indices
  # are calibrated downstream, so their scale absorbs Var(Z) anyway.
  E = MASS::mvrnorm( n = .p$N, mu = rep(0, d), Sigma = w_latent_Sigma(.p) )
  if (d == 1) E = matrix(E, ncol = 1)

  Z = E
  if ( !is.null(parent) ) {
    if ( length(parent) != .p$N ) stop("parent must have length .p$N")
    Z = Z + .p$W_parent_coef * parent
  }

  # ~~ mixed-type marginals via the copula ----
  Wt = matrix(NA_real_, nrow = .p$N, ncol = d)
  if ( length(nm$cont) > 0 ) Wt[, nm$cont] = Z[, nm$cont, drop = FALSE]

  if ( length(nm$bin) > 0 ) {
    if ( is.null(thresh) ) {
      # solve so that P(W_j = 1) = W_bin_prob marginally, averaging over the
      # parent; done once in the pilot and then held fixed
      thresh = apply( Z[, nm$bin, drop = FALSE], 2,
                      function(z) quantile(z, probs = 1 - .p$W_bin_prob, names = FALSE) )
    }
    if ( length(thresh) != length(nm$bin) ) stop("thresh must have one entry per binary component")
    Wt[, nm$bin] = 1 * sweep( Z[, nm$bin, drop = FALSE], 2, thresh, ">" )
  }
  colnames(Wt) = nm$true

  # ~~ missingness ----
  R = matrix(1L, nrow = .p$N, ncol = d)
  colnames(R) = nm$R
  R_int = rep(NA_real_, d)

  if ( .p$W_R_type == "self" ) {
    # W_j -> R_Wj. R_j depends on W_j itself (not the latent Z_j): making it a
    # function of Z_j would instead induce W_j <- Z_j -> R_j, a different DAG.
    target_obs = 1 - .p$W_miss_rate
    for ( j in which(!comp) ) {
      is_cont  = j %in% nm$cont
      b        = if (is_cont) .p$W_R_slope_cont else .p$W_R_slope_bin
      R_int[j] = w_solve_R_intercept( b          = b,
                                      target_obs = target_obs,
                                      type       = if (is_cont) "cont" else "bin",
                                      bin_prob   = .p$W_bin_prob )
      R[, j] = rbinom( n = .p$N, size = 1, prob = plogis( R_int[j] + b * Wt[, j] ) )
    }

  } else if ( .p$W_R_type == "mcar" ) {
    # R_Wj has no parents (DAG 1A: legacy RD ~ Bern(0.5))
    for ( j in which(!comp) ) R[, j] = rbinom( n = .p$N, size = 1, prob = 1 - .p$W_miss_rate )

  } else {
    stop("Unrecognized W_R_type: use 'self' or 'mcar'")
  }

  # ~~ punch out the observed versions ----
  Wo = Wt
  Wo[ R == 0 ] = NA
  colnames(Wo) = nm$obs

  out = cbind( as.data.frame(Wt), as.data.frame(Wo), as.data.frame(R) )

  if ( d == 1 ) {
    out$D1 = out[[ nm$true[1] ]]
    out$D  = out[[ nm$obs[1]  ]]
    out$RD = out[[ nm$R[1]    ]]
  }

  attr(out, "W_complete_flag") = comp
  attr(out, "W_R_intercept")   = R_int
  attr(out, "W_thresh")        = thresh
  out
}


# ~ W indices -----------------------------------------------------------------

# The raw (uncalibrated) index that W enters its children through.
#   with_inter = FALSE -> plain sum of components        (the W -> Y index)
#   with_inter = TRUE  -> sum + W_n_inter pairwise terms (the W -> R_Y index)
# Equal weights keep this simple and make W_dim == 1 collapse to D1 exactly.
w_index_raw = function(W, .p, with_inter) {

  nm = w_names(.p)
  M  = as.matrix( W[, nm$true, drop = FALSE] )

  raw = rowSums(M)

  if ( with_inter ) {
    prs = w_inter_pairs(.p)
    if ( nrow(prs) > 0 ) {
      for ( k in 1:nrow(prs) ) raw = raw + .p$W_inter_coef * M[, prs[k,1]] * M[, prs[k,2]]
    }
  }

  raw
}


# ~ DAG 1A calibration --------------------------------------------------------
#
# Legacy 1A:
#   C1 ~ Bern(0.5); A1 ~ Bern(expit(-1 + 3*C1)); D1 ~ Bern(expit(-1 + 3*A1))
#   B1 ~ N( coefDB*D1 + 2.6*C1 + D1*C1 + coefAB*A1 + A1*C1 )
#   RD ~ Bern(0.5);  RB ~ Bern(expit(-1 + 3*D1))
#
# The W-contribution to Y is D1*(coefDB + C1). We replace D1 by an index
#   S_Y = aY + bY * raw_Y
# and solve (aY, bY) so that S_Y*(coefDB + C1) has the SAME MEAN AND VARIANCE as
# D1*(coefDB + C1). For any bY the mean equation pins aY, so this reduces to a
# 1-D root find on the variance.
#
# For R_Y we replace D1 by S_R = aR + bR * raw_R (raw_R carries the W_j x W_k
# interactions), scaled to D1's mean and sd, then solve the R_Y intercept so
# that marginal P(R_Y = 1) matches legacy.
#
# All constants are computed ONCE from a fixed-seed pilot and cached, so they
# are DGM constants and the units stay iid. The caller's RNG stream is restored.

.w_cal_cache = new.env(parent = emptyenv())

w_calibrate_1A = function(.p, coefDB = 2, n_pilot = 2e5, seed = 20260717) {

  key = paste("1A", .p$W_dim, .p$W_n_cont, .p$W_rho, .p$W_cor_type, .p$W_bin_prob,
              .p$W_parent_coef, .p$W_n_inter, .p$W_inter_coef, coefDB, sep = "|")
  if ( !is.null(.w_cal_cache[[key]]) ) return( .w_cal_cache[[key]] )

  has_seed = exists(".Random.seed", envir = .GlobalEnv)
  if (has_seed) old_seed = get(".Random.seed", envir = .GlobalEnv)
  set.seed(seed)

  pp   = .p
  pp$N = n_pilot

  C1 = rbinom( n_pilot, 1, 0.5 )
  A1 = rbinom( n_pilot, 1, plogis(-1 + 3*C1) )
  D1 = rbinom( n_pilot, 1, plogis(-1 + 3*A1) )   # the legacy scalar W

  Wp = gen_W_block(pp, parent = A1, thresh = NULL)
  W_thresh = attr(Wp, "W_thresh")

  raw_Y = w_index_raw(Wp, pp, with_inter = FALSE)
  raw_R = w_index_raw(Wp, pp, with_inter = TRUE)

  # ~~ Y index: match the mean and variance of the W-contribution to Y ----
  mult     = coefDB + C1
  tgt_mean = mean( D1 * mult )
  tgt_var  = var(  D1 * mult )

  Emult  = mean(mult)
  Ermult = mean(raw_Y * mult)
  a_of_b = function(b) ( tgt_mean - b * Ermult ) / Emult
  varfun = function(b) var( (a_of_b(b) + b * raw_Y) * mult ) - tgt_var

  bY = uniroot(varfun, interval = c(1e-8, 5), extendInt = "upX", tol = 1e-10)$root
  aY = a_of_b(bY)

  # ~~ R_Y index: scale to D1's mean/sd, then match marginal P(R_Y = 1) ----
  bR  = sd(D1) / sd(raw_R)
  aR  = mean(D1) - bR * mean(raw_R)
  S_R = aR + bR * raw_R

  tgt_RY = mean( plogis(-1 + 3*D1) )
  RY_int = uniroot( function(a0) mean( plogis(a0 + 3*S_R) ) - tgt_RY,
                    interval = c(-30, 30), tol = 1e-10 )$root

  out = list( aY = aY, bY = bY, aR = aR, bR = bR, RY_int = RY_int,
              W_thresh = W_thresh,
              # diagnostics, carried through to the sanchecks
              chk_Y_mean_legacy = tgt_mean,
              chk_Y_var_legacy  = tgt_var,
              chk_Y_mean_new    = mean( (aY + bY*raw_Y) * mult ),
              chk_Y_var_new     = var(  (aY + bY*raw_Y) * mult ),
              chk_RY_legacy     = tgt_RY,
              chk_RY_new        = mean( plogis(RY_int + 3*S_R) ) )

  if (has_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  .w_cal_cache[[key]] = out
  out
}


# ~ Sanity-check summary ------------------------------------------------------

w_sanchecks = function(W, .p, cal = NULL) {

  if ( is.null(W) ) return( data.frame( sancheck.W_dim = 0 ) )

  nm   = w_names(.p)
  comp = attr(W, "W_complete_flag")

  Rmat = as.matrix( W[, nm$R, drop = FALSE] )
  Wt   = as.matrix( W[, nm$true, drop = FALSE] )

  cors    = suppressWarnings( cor(Wt) )
  offdiag = if (ncol(Wt) > 1) cors[ upper.tri(cors) ] else NA

  out = data.frame(
    sancheck.W_dim            = .p$W_dim,
    sancheck.W_n_incomplete   = sum(!comp),
    sancheck.W_miss_rate_mean = ifelse( any(!comp), mean( 1 - colMeans(Rmat)[!comp] ), 0 ),
    sancheck.W_allobs_prop    = mean( rowSums(Rmat) == .p$W_dim ),
    sancheck.W_cor_mean       = mean(offdiag),
    sancheck.W_cor_min        = min(offdiag),
    sancheck.W_cor_max        = max(offdiag) )

  if ( !is.null(cal) ) {
    out$sancheck.W_Y_mean_ratio = cal$chk_Y_mean_new / cal$chk_Y_mean_legacy
    out$sancheck.W_Y_var_ratio  = cal$chk_Y_var_new  / cal$chk_Y_var_legacy
    out$sancheck.W_RY_ratio     = cal$chk_RY_new     / cal$chk_RY_legacy
  }

  out
}
