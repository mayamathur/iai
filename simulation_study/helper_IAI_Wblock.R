
# ============================================================================
# HIGH-DIMENSIONAL W BLOCK
# ============================================================================
# Generates the auxiliary block W = (W_1, ..., W_d) with:
#   - mixed types: first .p$W_n_cont components continuous, the rest binary
#   - correlation across ALL components (incl. cont-bin) via a Gaussian copula
#   - the same edge structure as the legacy single auxiliary D:
#       each W_j -> R_{W_j}, and nothing else.
#     There are NO cross-component edges and NO edges into (A, B), so no
#     interactions are possible or generated (see W is disconnected from A, B).
#   - per-component missingness calibrated to a TARGET marginal rate
#     (.p$W_miss_rate) by solving each R-model intercept, rather than
#     inheriting the legacy coefficients.
#
# Component naming (W_dim >= 1):
#   truth:      W01_true, ..., W10_true
#   observed:   W01,      ..., W10       (NA where R = 0)
#   indicators: RW01,     ..., RW10      (identically 1 for complete comps)
#
# Backward compatibility:
#   .p$W_dim == 0  -> no W block at all (the 5-MAR-* / 6-MAR-* arms)
#   .p$W_dim == 1  -> additionally aliases D1 / D / RD, so every legacy code
#                     path that references D keeps working untouched. With
#                     W_n_cont = 0, W_bin_prob = 0.5, W_R_slope_bin = 3 and
#                     W_miss_rate = 0.4252, the solved intercept is -1 and the
#                     block reproduces the legacy `RD ~ expit(-1 + 3*D1)` DGM
#                     in distribution.
#
# Required .p columns (see w_default_W_params() for defaults):
#   W_dim, W_n_cont, W_n_cont_complete, W_n_bin_complete,
#   W_rho, W_cor_type, W_bin_prob, W_miss_rate,
#   W_R_slope_cont, W_R_slope_bin
# ----------------------------------------------------------------------------


# ~ Defaults ------------------------------------------------------------------

# convenience: the W-block columns to append to scen.params
w_default_W_params = function() {
  list(
    W_dim             = 10,     # number of auxiliary components (0 = none, 1 = legacy D)
    W_n_cont          = 5,      # how many are continuous; the remaining W_dim - W_n_cont are binary
    W_n_cont_complete = 3,      # how many of the continuous components are COMPLETE (in W^+)
    W_n_bin_complete  = 2,      # how many of the binary components are COMPLETE (in W^+)
    W_rho             = 0.4,    # latent correlation parameter
    W_cor_type        = "exch", # "exch" (exchangeable) or "ar1"
    W_bin_prob        = 0.5,    # marginal P(W_j = 1) for binary components
    W_miss_rate       = 0.10,   # TARGET marginal P(R_{W_j} = 0) for incomplete components
    W_R_slope_cont    = 1,      # coef of W_j in logit P(R_{W_j} = 1), continuous comps
    W_R_slope_bin     = 3       # coef of W_j in logit P(R_{W_j} = 1), binary comps
  )
}


# ~ Names and index bookkeeping -----------------------------------------------

# Everything downstream should go through these accessors rather than
# hard-coding "D" / "RD" / c("A","B","C","D").
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


# TRUE  = component is complete (in W^+); FALSE = incomplete (in W^-).
# Completeness is assigned within type, so the W^+ / W^- split is explicit and
# type-balanced rather than an accident of component ordering.
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

  # exchangeable is only PD for rho > -1/(d-1); catch it here rather than in mvrnorm
  if ( min( eigen(S, symmetric = TRUE, only.values = TRUE)$values ) <= 0 ) {
    stop("W latent correlation matrix is not positive definite for this W_rho / W_dim")
  }

  S
}


# ~ Calibrating the R-models --------------------------------------------------

# Solve for the intercept a such that P(R_{W_j} = 1) = target_obs exactly,
# given the slope b and the marginal distribution of W_j.
#
# Continuous: W_j ~ N(0,1)          => P(R=1) = E[ expit(a + b*W) ]
# Binary:     W_j ~ Bern(bin_prob)  => P(R=1) = (1-pi)*expit(a) + pi*expit(a + b)
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

  uniroot( function(a) pr(a) - target_obs,
           interval = c(-30, 30),
           tol = 1e-10 )$root
}


# ~ Generator -----------------------------------------------------------------

# Returns a data.frame with N rows and, for each component, its truth, its
# observed (NA-punched) version, and its missingness indicator.
gen_W_block = function(.p) {

  d = .p$W_dim
  if ( is.null(d) || is.na(d) || d == 0 ) return(NULL)

  nm   = w_names(.p)
  comp = w_complete_flag(.p)

  # ~~ latent draws (vectorized; no rowwise) ----
  Z = MASS::mvrnorm( n = .p$N, mu = rep(0, d), Sigma = w_latent_Sigma(.p) )
  if (d == 1) Z = matrix(Z, ncol = 1)

  # ~~ mixed-type marginals via the copula ----
  Wt = matrix(NA_real_, nrow = .p$N, ncol = d)
  if ( length(nm$cont) > 0 ) {
    Wt[, nm$cont] = Z[, nm$cont, drop = FALSE]
  }
  if ( length(nm$bin) > 0 ) {
    # threshold so that P(W_j = 1) = W_bin_prob exactly, in expectation
    thresh = qnorm(1 - .p$W_bin_prob)
    Wt[, nm$bin] = 1 * ( Z[, nm$bin, drop = FALSE] > thresh )
  }
  colnames(Wt) = nm$true

  # ~~ missingness: W_j -> R_{W_j} only ----
  # NOTE: R_j depends on W_j itself (not on the latent Z_j), so the edge is
  # genuinely W_j -> R_{W_j}. Making R_j a function of Z_j would instead induce
  # W_j <- Z_j -> R_j, i.e. an unmeasured common cause, which is a different DAG.
  R = matrix(1L, nrow = .p$N, ncol = d)
  colnames(R) = nm$R

  target_obs = 1 - .p$W_miss_rate
  R_int = rep(NA_real_, d)

  for ( j in which(!comp) ) {
    is_cont  = j %in% nm$cont
    b        = if (is_cont) .p$W_R_slope_cont else .p$W_R_slope_bin
    R_int[j] = w_solve_R_intercept( b          = b,
                                    target_obs = target_obs,
                                    type       = if (is_cont) "cont" else "bin",
                                    bin_prob   = .p$W_bin_prob )

    R[, j] = rbinom( n = .p$N, size = 1, prob = plogis( R_int[j] + b * Wt[, j] ) )
  }

  # ~~ punch out the observed versions ----
  Wo = Wt
  Wo[ R == 0 ] = NA
  colnames(Wo) = nm$obs

  out = cbind( as.data.frame(Wt), as.data.frame(Wo), as.data.frame(R) )

  # ~~ legacy aliases so W_dim == 1 leaves existing code paths untouched ----
  if ( d == 1 ) {
    out$D1 = out[[ nm$true[1] ]]
    out$D  = out[[ nm$obs[1]  ]]
    out$RD = out[[ nm$R[1]    ]]
  }

  attr(out, "W_complete_flag") = comp
  attr(out, "W_R_intercept")   = R_int
  out
}


# ~ Sanity-check summary ------------------------------------------------------

# Returns a one-row data.frame of sancheck.* columns to bind onto rep.res.
# The complete-case fraction is the number that decides whether the
# complete-case-based estimators (af4 / MIA / uCCF) have anything to work with.
w_sanchecks = function(W, .p, du = NULL) {

  if ( is.null(W) ) return( data.frame( sancheck.W_dim = 0 ) )

  nm   = w_names(.p)
  comp = attr(W, "W_complete_flag")

  Rmat = as.matrix( W[, nm$R, drop = FALSE] )
  Wt   = as.matrix( W[, nm$true, drop = FALSE] )

  # observed pairwise correlation among the true W components
  cors = cor(Wt)
  offdiag = cors[ upper.tri(cors) ]

  data.frame(
    sancheck.W_dim              = .p$W_dim,
    sancheck.W_n_incomplete     = sum(!comp),
    sancheck.W_miss_rate_mean   = mean( 1 - colMeans(Rmat)[!comp] ),
    sancheck.W_miss_rate_max    = max( c(0, 1 - colMeans(Rmat)[!comp]) ),
    sancheck.W_allobs_prop      = mean( rowSums(Rmat) == .p$W_dim ),
    sancheck.W_cor_mean         = mean(offdiag),
    sancheck.W_cor_min          = min(offdiag),
    sancheck.W_cor_max          = max(offdiag)
  )
}
