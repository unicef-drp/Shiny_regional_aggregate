test_env <- new.env(parent = globalenv())
source("R/outputaggregates-BWC.R", local = test_env)

if (!exists("CombineAndOutputRegionalResultsBWC", envir = test_env, inherits = FALSE)) {
  stop(
    "Expected under-five helper CombineAndOutputRegionalResultsBWC to exist after sourcing outputaggregates-BWC.R",
    call. = FALSE
  )
}

if (exists("CombineAndOutputRegionalResults", envir = test_env, inherits = FALSE)) {
  stop(
    "Did not expect generic CombineAndOutputRegionalResults to remain defined in outputaggregates-BWC.R",
    call. = FALSE
  )
}

cat("Under-five combiner uses the BWC-specific helper name.\n")
