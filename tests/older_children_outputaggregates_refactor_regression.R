test_env <- new.env(parent = globalenv())
source("R/5_24/outputaggregates.5_24.R", local = test_env)

required_helpers <- c(
  "ResolveOutputDirs5_24",
  "CountryCacheFiles5_24",
  "LoadCountryInputs5_24",
  "LoadTrajectoryArrays5_24",
  "BuildOlderChildrenContext5_24",
  "RunCountryResults5_24",
  "RunWorldResults5_24",
  "BuildRegionalSpecs5_24",
  "RunAllRegionalResults5_24",
  "RunRegionalResults5_24",
  "RegionalBundleExists5_24",
  "SaveRegionalBundle5_24",
  "GetRegionalResults5_24",
  "CalculateRegionalDeaths5_24",
  "CombineAndOutputRegionalResults5_24",
  "WorldCacheFiles5_24",
  "RegionalCombinedCacheFiles5_24"
)

missing_helpers <- required_helpers[
  !vapply(required_helpers, exists, logical(1), envir = test_env, inherits = FALSE)
]

if (length(missing_helpers) > 0) {
  stop(
    sprintf("Missing 5_24 helpers: %s", paste(missing_helpers, collapse = ", ")),
    call. = FALSE
  )
}

if (exists("GetRegionalResults", envir = test_env, inherits = FALSE)) {
  stop(
    "Did not expect generic GetRegionalResults to remain defined in outputaggregates.5_24.R",
    call. = FALSE
  )
}

if (exists("CalculateRegionalDeaths", envir = test_env, inherits = FALSE)) {
  stop(
    "Did not expect generic CalculateRegionalDeaths to remain defined in outputaggregates.5_24.R",
    call. = FALSE
  )
}

files_world <- test_env$WorldCacheFiles5_24()
if ("global.RoDs.ui.rda" %in% files_world) {
  stop(
    "WorldCacheFiles5_24 should not require global.RoDs.ui.rda",
    call. = FALSE
  )
}

bundle.dir <- file.path(tempdir(), "regional-bundle-regression")
dir.create(bundle.dir, recursive = TRUE, showWarnings = FALSE)
unlink(file.path(bundle.dir, "*.rda"))

if (test_env$RegionalBundleExists5_24(bundle.dir, 1L)) {
  stop("RegionalBundleExists5_24 should be FALSE when no bundle files exist", call. = FALSE)
}

bundle <- list(
  q0.rt = matrix(1, nrow = 1, ncol = 1),
  q1to4.rt = matrix(2, nrow = 1, ncol = 1),
  q5.rt = matrix(3, nrow = 1, ncol = 1),
  death0.all.rt = matrix(4, nrow = 1, ncol = 1),
  death1to4.all.rt = matrix(5, nrow = 1, ncol = 1),
  deathu5.all.rt = matrix(6, nrow = 1, ncol = 1)
)
test_env$SaveRegionalBundle5_24(bundle = bundle, output.dir.samples.region = bundle.dir, j = 1L)

if (!test_env$RegionalBundleExists5_24(bundle.dir, 1L)) {
  stop("RegionalBundleExists5_24 should be TRUE after SaveRegionalBundle5_24 writes all files", call. = FALSE)
}

cat("Older-children helper naming and cache contract regression checks passed.\n")
