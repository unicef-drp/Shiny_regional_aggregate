test_env <- new.env(parent = globalenv())
source("R/5_24/outputaggregates.5_24.R", local = test_env)

tmp_root <- tempfile("getregionalresults-")
dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(tmp_root, "samples"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(tmp_root, "samples_combined"), recursive = TRUE, showWarnings = FALSE)

deathu5.ctj <- array(0, dim = c(1, 1, 1))
save(deathu5.ctj, file = file.path(tmp_root, "samples_combined", "deathu5.ctj.rda"))

test_env$CalculateRegionalDeaths5_24 <- function(...) {
  invisible(NULL)
}

run_case <- function(case_name, replace_arg, expected_value, include_replace_arg) {
  captured <- new.env(parent = emptyenv())
  capture_combine <- function(..., replace.rates.reg = NULL) {
    captured$replace.rates.reg <- replace.rates.reg
    invisible(NULL)
  }
  test_env$CombineAndOutputRegionalResultsBWC <- capture_combine
  test_env$CombineAndOutputRegionalResults5_24 <- capture_combine

  call_args <- list(
    output.dir = tmp_root,
    output.dir.samples = file.path(tmp_root, "samples"),
    output.dir.samplescombined = file.path(tmp_root, "samples_combined"),
    regions = data.frame(AdhocCountries = "Low income", stringsAsFactors = FALSE),
    regiontypes = "Low income",
    filename = "AdhocCountries",
    run.on.server = FALSE,
    percentiles = 0.5,
    ndigits = 10
  )

  if (isTRUE(include_replace_arg)) {
    call_args$replace.rates.reg <- replace_arg
  }

  do.call(test_env$GetRegionalResults5_24, call_args)

  if (!identical(captured$replace.rates.reg, expected_value)) {
    stop(
      sprintf(
        "%s failed: expected replace.rates.reg to be %s but got %s",
        case_name,
        deparse(expected_value),
        deparse(captured$replace.rates.reg)
      ),
      call. = FALSE
    )
  }
}

run_collision_case <- function() {
  captured <- new.env(parent = emptyenv())

  test_env$CombineAndOutputRegionalResultsBWC <- function(...) {
    stop("foreign combine invoked", call. = FALSE)
  }
  test_env$CombineAndOutputRegionalResults5_24 <- function(..., replace.rates.reg = NULL) {
    captured$replace.rates.reg <- replace.rates.reg
    invisible(NULL)
  }

  do.call(
    test_env$GetRegionalResults5_24,
    list(
      output.dir = tmp_root,
      output.dir.samples = file.path(tmp_root, "samples"),
      output.dir.samplescombined = file.path(tmp_root, "samples_combined"),
      regions = data.frame(AdhocCountries = "Low income", stringsAsFactors = FALSE),
      regiontypes = "Low income",
      filename = "AdhocCountries",
      run.on.server = FALSE,
      percentiles = 0.5,
      ndigits = 10
    )
  )

  if (!is.null(captured$replace.rates.reg)) {
    stop(
      sprintf(
        "combine collision isolation failed: expected NULL but got %s",
        deparse(captured$replace.rates.reg)
      ),
      call. = FALSE
    )
  }
}

run_case(
  case_name = "default NULL forwarding",
  replace_arg = NULL,
  expected_value = NULL,
  include_replace_arg = FALSE
)

run_case(
  case_name = "explicit forwarding",
  replace_arg = "M49Region",
  expected_value = "M49Region",
  include_replace_arg = TRUE
)

run_collision_case()

cat("All GetRegionalResults5_24 replace.rates.reg regression checks passed.\n")
