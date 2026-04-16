test_env <- new.env(parent = globalenv())
source("R/helper funcs for app.R", local = test_env)

if (!exists("ResolvePlotColors", envir = test_env, inherits = FALSE)) {
  stop("ResolvePlotColors helper is missing", call. = FALSE)
}

colors_for_six_regions_plus_world <- test_env$ResolvePlotColors(7L)

if (!identical(length(colors_for_six_regions_plus_world), 7L)) {
  stop("ResolvePlotColors(7) should return exactly 7 colors", call. = FALSE)
}

if (anyNA(colors_for_six_regions_plus_world)) {
  stop("ResolvePlotColors(7) should not return NA colors", call. = FALSE)
}

if (any(!nzchar(colors_for_six_regions_plus_world))) {
  stop("ResolvePlotColors(7) should not return blank colors", call. = FALSE)
}

cat("Plot palette regression checks passed.\n")
