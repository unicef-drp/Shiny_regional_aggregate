test_env <- new.env(parent = globalenv())
source("R/helper_funcs_for_app.R", local = test_env)

if (!exists("RecodePlotIndicators", envir = test_env, inherits = FALSE)) {
  stop("RecodePlotIndicators helper is missing", call. = FALSE)
}

indicator_names <- c(
  "U5MR median",
  "IMR median",
  "NMR median",
  "Mortality rate age 5-24"
)

if (!is.null(levels(indicator_names))) {
  stop("Regression setup should use character indicators without factor levels", call. = FALSE)
}

actual <- test_env$RecodePlotIndicators(indicator_names, indicator.order = indicator_names)
expected <- c(
  "Under-five Mortality Rate",
  "Infant Mortality Rate",
  "Neonatal Mortality Rate",
  "Mortality rate age 5-24"
)

if (!identical(as.character(actual), expected)) {
  stop(
    sprintf(
      "Indicator recode mismatch. Expected %s but got %s",
      paste(expected, collapse = ", "),
      paste(as.character(actual), collapse = ", ")
    ),
    call. = FALSE
  )
}

if (!identical(levels(actual), expected)) {
  stop("RecodePlotIndicators should preserve the requested indicator order as factor levels", call. = FALSE)
}

cat("Plot indicator recode regression checks passed.\n")
