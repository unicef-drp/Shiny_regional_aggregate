test_env <- new.env(parent = globalenv())
source("R/outputaggregates-BWC.R", local = test_env)

cases <- list(
  list(
    name = "reuse replacement skips ordinary generation",
    args = list(
      country.combined.exists = FALSE,
      country.trajectories.exist = FALSE,
      ordinary.selected.active = FALSE,
      replace.rates.reg = "M49Region",
      reuse.replacement.country = TRUE
    ),
    expected = FALSE
  ),
  list(
    name = "ordinary generation still runs without replacement reuse",
    args = list(
      country.combined.exists = FALSE,
      country.trajectories.exist = FALSE,
      ordinary.selected.active = FALSE,
      replace.rates.reg = "M49Region",
      reuse.replacement.country = FALSE
    ),
    expected = TRUE
  ),
  list(
    name = "selected ordinary reruns still happen without replacement mode",
    args = list(
      country.combined.exists = TRUE,
      country.trajectories.exist = TRUE,
      ordinary.selected.active = TRUE,
      replace.rates.reg = NULL,
      reuse.replacement.country = FALSE
    ),
    expected = TRUE
  )
)

for (case in cases) {
  actual <- do.call(test_env$ShouldGenerateOrdinaryCountryResultsBWC, case$args)
  if (!identical(actual, case$expected)) {
    stop(
      sprintf(
        "%s failed: expected %s but got %s",
        case$name,
        deparse(case$expected),
        deparse(actual)
      ),
      call. = FALSE
    )
  }
}

cat("Replacement-country reuse skip regression checks passed.\n")
