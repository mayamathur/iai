# SIMULATE AND ANALYZE ONE DATASET ---------------------------------------------------
#
# sim_one_rep() generates one dataset under scenario p and applies every estimation
# method listed in p$rep.methods. It is called once per simulation rep by both
# doParallel_IAI.R (cluster runs) and run_one_scenario_local_IAI.R (local runs).
#
# Arguments:
#   p:       one row of the scenario grid (see make_scen_params() in config_IAI.R),
#            without the `scen` column
#   verbose: print each method's results as they are computed?
#
# Returns a data frame with one row per method, containing the estimate of the
# contrast of interest (bhat) and the reference-level mean (inthat), their 95% CIs,
# any error message (overall.error), and the scenario parameters.
#
# Estimation methods:
#   gold        benchmark: analysis model fit to the full data, before missingness
#   CC          complete-case analysis
#   IPW-nm      inverse-probability weighting under a no-self-censoring model
#   mia-pkg-ice MIA plug-in estimator (iterative conditional expectation), via miapack
#   mia-tmle    MIA targeted maximum likelihood estimator, via tmle

sim_one_rep = function(p, verbose = TRUE) {
  
  coef_of_interest = p$coef_of_interest
  
  # parse methods string
  all.methods = unlist( strsplit( x = p$rep.methods, split = " ; " ) )
  
  
  # ~ Simulate Dataset ------------------------------
  sim_obj = sim_data(.p = p)
  
  du = sim_obj$du
  di = sim_obj$di
  form_string      = as.character(sim_obj$form_string)
  gold_form_string = as.character(sim_obj$gold_form_string)
  beta             = as.numeric(sim_obj$beta)
  

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
    
    if (verbose) srr(rep.res)
  }


# ~~ Complete-case analysis (naive) ----
  if ( "CC" %in% all.methods ) {


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
    
    if (verbose) srr(rep.res)
  }


# ~~ IPW-nm ----
  # Sun et al.'s IPW under a no-self-censoring model; see helper_IAI.R
  if ( "IPW-nm" %in% all.methods ) {
    
    rep.res = run_method_safe(method.label = c("IPW-nm"),
                              
                              method.fn = function(x) fit_regression(form_string = form_string,
                                                                     model = p$model,
                                                                     coef_of_interest = coef_of_interest,
                                                                     miss_method = "IPW-nm",
                                                                     du = du,
                                                                     imps = NULL),
                              .rep.res = rep.res )


if (verbose) srr(rep.res)
    
  }


# ~~ MIA-ICE (iterative conditional expectation, using miapack) --------------
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

                                
                                # parse the gold model into outcome + predictors
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
                                
                                
                                # rebuild the outcome-model formula with LHS "Y", same RHS
                                # terms as the gold model (keeps interactions), FULLY CROSSED
                                # with the W block. 
                                # ***Note: It's very important to include all C-W interactions since some DAGs, e.g., 1A, include
                                #  those interactions! HOWEVER, the code below does NOT include W-W interactions, which matches
                                #  all DAGs' DGMs as of 2026-07-22.
                                rhs_terms = labels(terms(fo))         # e.g. "A","C","A:C"
                                
                                # X-part: the gold model's RHS as a single grouped term,
                                # e.g. "(A + C + A:C)". Falls back to intercept-only if the
                                # gold model has no RHS terms.
                                x_part = if (length(rhs_terms) > 0)
                                  paste0("(", paste(rhs_terms, collapse = " + "), ")") else "1"
                                
                                # W-part: all observed W components as a single grouped term,
                                # e.g. "(W01)" for |W|=1 or "(W1 + W2 + ... )" for a W block.
                                w_part = paste0("(", paste(Wobs, collapse = " + "), ")")
                                
                                # Full cross: Y ~ (X terms) * (W terms). The "*" expands to all
                                # main effects plus every X:W interaction (and X:X, W:W within
                                # each group as already implied by rhs_terms / additive W).
                                Y_form = as.formula(paste0("Y ~ ", x_part, " * ", w_part))
                                # end of building the outcome-model formula

                                
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
                                if ( p$boot_reps_mia_ice == 0 ) {
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
                                  n_boot  = p$boot_reps_mia_ice,
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
    
    if (verbose) srr(rep.res)
  }


# ~~ MIA-tmle -------------------------------------------------
  ## mu_MIA(x) = E[ E(Y | X = x, W, S, r_Y = 1) | X = x, S ],  S = {r_X = r_W = 1}
  ##
  ## Restrict to S, subset to the X = x stratum, call tmle() with A = NULL and
  ## Delta = r_Y. tmle's EY1 averages the targeted outcome regression over the
  ## empirical covariate distribution of whatever subset it is handed, which on
  ## the X = x subset is exactly p_hat(w | X = x, S) -- the outer measure the MIA
  ## functional integrates against. No reweighting needed.
  ##
  ## bhat   = mu_MIA(exposure = 1, covars = 0) - mu_MIA(exposure = 0, covars = 0)
  ## inthat = mu_MIA(exposure = 0, covars = 0)   <- reference level, matching the
  ##          mean_est_2 / ci_2 convention used for the miapack methods.

  
  if ( "mia-tmle" %in% all.methods ) {
  rep.res = run_method_safe(
    method.label = c("mia-tmle"),
    method.fn = function(x) {
      
      # TMLE estimation of mu_MIA(x) at values xv of the X-variables
      # Do not try to move this fn to helper_IAI.R! It depends on locally scoped vars.
      mia_tmle_pt_est = function(xv) {
        
        keep = Reduce(`&`, lapply(seq_along(X_names),
                                  function(k) dS[[ X_names[k] ]] == xv[k]))
        idx = which(keep)
        
        # X is constant inside the stratum, so the adjustment set is W alone.
        # Note this sidesteps the X-by-W interaction trap that bites the
        # additive Y_model specification: within a stratum every X:W term is
        # collinear with the corresponding W main effect, so an additive-in-W
        # model here is the exact analogue of the fully crossed
        # Y ~ (A + C + A:C) * (W...) model used by mia-pkg-sp.
        cc_mean = function() {
          yo = Yv[idx][ rY[idx] == 1 ]
          list( psi = mean(yo), var = var(yo) / length(yo) )
        }
        
        Wd = dS[idx, Wobs, drop = FALSE]
        
        # drop no-variation columns BEFORE expansion: a single-level factor
        # makes model.matrix() error on contrasts
        Wd = Wd[, vapply(Wd, function(z) length(unique(z)) > 1L, logical(1)),
                drop = FALSE]
        if ( ncol(Wd) == 0L ) return( cc_mean() )
        
        # Expand factors to numeric dummies HERE, so that tmle's internal
        #   W <- model.matrix(tempY ~ -1 + ., data = data.frame(tempY, W))
        # (tmle.R ~line 1720) is a no-op on the column names. Passing a factor
        # straight through lets tmle rename W01 -> W010/W011, after which a
        # hand-built Qform referencing "W01" dies with
        #   Error in eval(predvars, data, env) : object 'W01' not found
        Wd = stats::model.matrix( ~ ., data = Wd )[, -1, drop = FALSE]
        
        # a dummy can still be constant within a stratum
        Wd = Wd[, apply(Wd, 2, function(z) length(unique(z)) > 1L), drop = FALSE]
        if ( ncol(Wd) == 0L ) return( cc_mean() )
        
        # generic syntactic names: formula-safe, and guaranteed to survive
        # tmle's model.matrix pass unchanged since Wd is now numeric
        colnames(Wd) = paste0("W", seq_len(ncol(Wd)))
        
        # missingness model
        # Fitted HERE rather than handed to tmle via g.Deltaform. tmle's internal
        # call is
        #   estimateG(d = data.frame(Delta, Z=1, A, W[, retainW.Delta]), ...)
        # (tmle.R ~line 1898) and that subset carries no drop = FALSE, so when
        # exactly one W column is retained it collapses to a vector and
        # data.frame() names the column off the deparsed expression instead of
        # "W1" -- after which a g.Deltaform referencing W1 dies with
        #   Error in eval(predvars, data, env) : object 'W1' not found
        # Supplying pDelta1 skips that path entirely. With Z = NULL it must be
        # n x 2, [P(Delta=1|A=0,W), P(Delta=1|A=1,W)]; A is constant here so the
        # two columns are identical. tmle still applies its own truncation
        # downstream at line 1901, so nothing is lost by precomputing.
        pD = if ( all(rY[idx] == 1) ) {
          matrix(1, nrow = length(idx), ncol = 2)
        } else {
          gfit   = stats::glm( rY[idx] ~ ., data = as.data.frame(Wd),
                               family = stats::binomial() )
          pi_hat = stats::predict(gfit, type = "response")
          cbind(pi_hat, pi_hat)
        }  # end of mu_at fn
        
        # outcome model
        # Q is safe to pass as a formula: its data frame is
        # data.frame(Y, Z, A, W, Delta) with the FULL W, so it never hits the
        # drop-to-vector path above. collapse = " " is load-bearing -- deparse()
        # returns a character VECTOR once the formula exceeds width.cutoff = 60
        # chars, which "Y ~ A + W1 + ... + W10" does, and tmle would then pass
        # that multi-element vector to formula() (deprecated).
        Q_form = paste( deparse( reformulate(c("A", colnames(Wd)), response = "Y") ),
                        collapse = " " )
        
        fit = tmle::tmle(
          Y            = Yv[idx],
          A            = NULL,           # no treatment => EY1 is the target
          W            = Wd,
          Delta        = rY[idx],
          pDelta1      = pD,             # missingness model supplied directly
          family       = fam,
          Qform        = Q_form,
          prescreenW.g = FALSE,          # keep the whole W block in the g model
          verbose      = FALSE,
          #cvQinit = FALSE  # ****2026-07-27 - added to prevent tmle from crashing
          
          # 2026-07-28 - NOTE: The next 2 arguments patch a bug in tmle(): 
          #  throws "subscript out of bounds" whenever cvQinit = TRUE and the input has fewer rows than V.Q,
          #  because estimateQ builds its fold list via split(sample(1:n.id), rep(1:V, length = n.id)). 
          # That returns only n.id folds when n.id < V — then indexes id.split[[v]] over the full 1:V.
          cvQinit = TRUE,
          V.Q = max(2, min(10, floor(length(idx)/5)))
        )
        
        # var.psi is the plug-in EIF variance, var(IC)/n, where
        #   IC = rY/pi(x,W) * {Y - b(x,W)} + b(x,W) - psi
        # already back-transformed to the original Y scale.
        list( psi = fit$estimates$EY1$psi,
              var = fit$estimates$EY1$var.psi )
      }
      
      # parse the gold model into outcome + predictors, same as the mia-pkg-sp
      # block so the two methods are guaranteed to target the same contrast
      fo       = as.formula(form_string)      # e.g. B ~ A * C
      outcome  = all.vars(fo)[1]              # LHS, e.g. "B"
      exposure = coef_of_interest             # e.g. "A"
      rhs_vars = all.vars(fo)[-1]             # main-effect vars on RHS
      covars   = setdiff(rhs_vars, exposure)  # analysis covars X \ exposure
      
      Wobs    = w_names(p)$obs                # W component columns
      X_names = c(exposure, covars)           # the conditioning set X
      
      # restrict to S = {r_X = r_W = 1}
      S_rows = stats::complete.cases( di[, c(X_names, Wobs), drop = FALSE] )
      dS     = di[S_rows, , drop = FALSE]
      
      rY = as.integer( !is.na(dS[[outcome]]) )
      
      # tmle() will not tolerate NA in Y even on Delta == 0 rows: Qbounds
      # defaults to range(Y), which would go NA and poison the fit. The
      # placeholder is arbitrary -- those rows enter only via the missingness
      # model -- but it must be in range.
      Yv = dS[[outcome]]
      Yv[rY == 0] = 0
      
      fam = if ( p$model == "logistic" ) "binomial" else "gaussian"
      
      # check that X is discrete, as it must be for this implementation
      n_lev = vapply(X_names, function(v) length(unique(dS[[v]])), integer(1))
      if ( any(n_lev > 10) ) {
        stop("mia-tmle requires discrete X; ",
             paste0(X_names[n_lev > 10], " has ", n_lev[n_lev > 10], " levels",
                    collapse = "; "),
             ". Use mia() / mia_ice() for continuous X.")
      }

      
      # contrast: exposure 1 vs 0, covars held at reference 0
      ref = rep(0, length(covars))
      m1  = mia_tmle_pt_est( c(1, ref) )
      m0  = mia_tmle_pt_est( c(0, ref) )
      
      bhat   = m1$psi - m0$psi
      inthat = m0$psi
      
      # The two strata are disjoint sets of rows and their nuisance models are
      # fit separately, so the estimates are independent and the variances add
      # with no covariance term.
      v_b = m1$var + m0$var
      v_i = m0$var
      
      if ( p$calculate_tmle_CIs == FALSE ) {
        return( list( stats = data.frame(
          bhat   = bhat,
          inthat = inthat
        ) ) )
      }
      
      # influence-curve based CIs
      z = stats::qnorm(0.975)
      
      return( list( stats = data.frame(
        bhat       = bhat,
        bhat_lo    = bhat - z * sqrt(v_b),
        bhat_hi    = bhat + z * sqrt(v_b),
        bhat_width = 2 * z * sqrt(v_b),
        
        inthat    = inthat,
        int_lo    = inthat - z * sqrt(v_i),
        int_hi    = inthat + z * sqrt(v_i),
        int_width = 2 * z * sqrt(v_i)
      ) ) )
    },
    .rep.res = rep.res )
  
  if (verbose) srr(rep.res)
  }  # end mia-tmle

  # ~ Add Scen Params and Sanity Checks --------------------------------------
  
  # add in scenario parameters
  # do NOT use rbind here; bind_cols accommodates possibility that some methods' rep.res
  #  have more columns than others
  rep.res = p %>% bind_cols( rep.res )
  
  # these don't come from p because they are from sim_data instead
  rep.res$coef_of_interest = coef_of_interest
  rep.res$beta = beta
  
  rep.res$form_string = form_string
  rep.res$gold_form_string = gold_form_string
  
  rep.res
}
