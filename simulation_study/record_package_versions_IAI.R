# RECORD SOFTWARE VERSIONS ---------------------------------------------------------
#
# Writes package_versions.csv (R version and the version of every package used by
# the simulation scripts) and session_info.txt. Run on the computing environment
# used for the simulations, from the simulation_study directory:
#   Rscript record_package_versions_IAI.R

pkgs = c("dplyr", "tidyr", "tibble", "data.table", "foreach", "doParallel", "doRNG",
         "MASS", "R2jags", "rjags", "boot", "miapack", "tmle", "SuperLearner")

versions = data.frame(
  package = c("R", pkgs),
  version = c( paste(R.version$major, R.version$minor, sep = "."),
               sapply( pkgs, function(p) tryCatch( as.character( packageVersion(p) ),
                                                    error = function(e) NA_character_ ) ) ) )

# for packages installed from GitHub, record the commit
versions$github_sha = c( NA, sapply( pkgs, function(p) {
  desc = tryCatch( packageDescription(p), error = function(e) NULL )
  if ( is.null(desc) || is.null(desc$RemoteSha) ) NA_character_ else desc$RemoteSha
} ) )

write.csv(versions, "package_versions.csv", row.names = FALSE)
print(versions)

# JAGS itself is not an R package; record its version too
jags.version = tryCatch( as.character( rjags::jags.version() ), error = function(e) NA )
writeLines( c( capture.output( sessionInfo() ), "", paste("JAGS version:", jags.version) ),
            "session_info.txt" )
