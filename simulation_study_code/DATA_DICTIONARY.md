# Data dictionary: simulation results

`stitch_IAI.R` writes two files for each study to `results/<study>/stitched/`:

- `agg.csv`: one row per scenario and estimation method; contains the results reported in the paper.
- `stitched.csv`: one row per scenario, simulation rep, and estimation method.

Both are comma-separated text with a header row. Missing values are blank (`agg.csv`) or `NA`.

## Scenario parameters (both files)

These columns are constant within a scenario and are defined in `make_scen_params()` in `config_IAI.R`.

| Column | Type | Description |
|---|---|---|
| `scen.name` | integer | Scenario number within the study (restarts at 1 in each study) |
| `study` | character | `study12` (paper's Studies 1–2) or `study3` (Study 3) |
| `dag_name` | character | Data-generating DAG; see the mapping to the paper's DAGs at the top of `helper_IAI.R` |
| `N` | integer | Sample size of each simulated dataset |
| `W_dim` | integer | Dimension of the auxiliary block W (1 or 10) |
| `rep.methods` | character | Estimation methods run in the scenario, separated by `;` |
| `model` | character | Analysis model (`OLS`: linear regression) |
| `coef_of_interest` | character | Variable whose contrast is estimated (`A`, i.e., X2 in the paper) |
| `boot_reps_mia_ice` | integer | Bootstrap reps used for the `mia-pkg-ice` CIs |
| `calculate_tmle_CIs` | logical | Whether CIs were computed for `mia-tmle` |
| `imp_m` | integer | Number of imputations for `MICE-std` and `Am-std` |
| `imp_maxit` | integer | Number of iterations for `MICE-std` |
| `mice_method` | character | Imputation method passed to mice; blank or `NA` means mice's defaults |
| `W_n_cont` | integer | Number of continuous components of W (the rest are binary) |
| `W_n_cont_complete`, `W_n_bin_complete` | integer | Numbers of always-observed continuous and binary components of W |
| `W_rho`, `W_cor_type` | numeric, character | Latent-scale correlation among components of W, and its structure (`exch`: exchangeable) |
| `W_bin_prob` | numeric | Marginal probability that a binary component of W equals 1 |
| `W_miss_rate` | numeric | Marginal probability that an incomplete component of W is missing |
| `W_parent_coef` | numeric | Coefficient of W's parent on W |
| `W_n_inter`, `W_inter_coef` | integer, numeric | Number and coefficient of interactions among components of W in the missingness model |
| `form_string`, `gold_form_string` | character | Analysis model formula for the observed and full data |
| `beta` | numeric | True value of the contrast of interest; if not available analytically, the mean benchmark (`gold`) estimate across reps |
| `int` | numeric | True reference-level mean, taken as the mean benchmark (`gold`) estimate across reps (`agg.csv` only) |

## Estimation methods (`method`)

| Value | Description |
|---|---|
| `gold` | Benchmark: analysis model fit to the full data, before missingness |
| `CC` | Complete-case analysis |
| `MICE-std` | Multiple imputation by chained equations (mice), pooled with Rubin's rules |
| `Am-std` | Multiple imputation under a joint normal model (Amelia), pooled with Rubin's rules; not in the default scenario grids |
| `IPW-nm` | Inverse-probability weighting under a no-self-censoring missingness model |
| `mia-pkg-ice` | MIA plug-in estimator (iterative conditional expectation), via miapack |
| `mia-tmle` | MIA targeted maximum likelihood estimator |

## Performance metrics (`agg.csv`)

Metrics are computed across the reps of a scenario, omitting reps in which a method failed, and are rounded to two decimal places. "Contrast" is the contrast of interest (the estimand for `bhat`); "reference-level mean" is the conditional mean at the reference level of the predictors (the estimand for `inthat`).

| Column | Description |
|---|---|
| `reps` | Number of reps |
| `PropNA` | Proportion of reps in which the method returned no estimate |
| `Bhat` | Mean estimate of the contrast |
| `BhatBias` | Bias of the contrast estimate (mean of `bhat` minus `beta`) |
| `BhatRMSE` | Root mean squared error of the contrast estimate |
| `BhatWidth` | Mean width of the 95% CI for the contrast |
| `BhatCover` | Proportion of 95% CIs for the contrast that contain `beta` |
| `IntHat`, `IntBias`, `IntRMSE`, `IntWidth`, `IntCover` | The same metrics for the reference-level mean, relative to `int` |
| `overall.error` | Error message; included only when constant within every scenario, in which case blank means the method never failed |

## Rep-level results (`stitched.csv`)

In addition to the scenario parameters and `method`, each row contains:

| Column | Description |
|---|---|
| `job.name`, `job.seed` | Cluster job that produced the row, and that job's random seed |
| `rep.name` | Rep number within the job |
| `bhat`, `bhat_lo`, `bhat_hi`, `bhat_width` | Estimate of the contrast, with 95% CI limits and width |
| `inthat`, `int_lo`, `int_hi`, `int_width` | Estimate of the reference-level mean, with 95% CI limits and width |
| `overall.error` | Error message if the method failed in this rep; otherwise `NA` |
| `doParallel.seconds` | Elapsed time of the job, in seconds |
