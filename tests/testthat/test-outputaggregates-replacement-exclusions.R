testthat::test_that("replacement country generation leaves excluded ISO rows as NA", {
  get_outputaggregates_object <- function(name) {
    if (exists(name, inherits = TRUE)) {
      return(get(name, inherits = TRUE))
    }
    get(name, envir = asNamespace("shinyregionalaggregate"), inherits = FALSE)
  }
  calc_replacement <- get_outputaggregates_object("CalculateCountryDeathsBWC.replacemissingrates")
  build_context <- get_outputaggregates_object("BuildCountryCalculationContextBWC")
  m49_regions <- get_outputaggregates_object("M49RegionAll")

  tmp <- tempfile("replacement-exclusion-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  samples_dir <- file.path(tmp, "samples")
  combined_dir <- file.path(tmp, "samples_combined")
  dir.create(samples_dir, recursive = TRUE)
  dir.create(combined_dir, recursive = TRUE)

  iso.c <- c("AFG", "LIE")
  nyears <- 7L
  est.years <- seq(2020.5, length.out = nyears, by = 1)

  u5mr.ctj <- array(
    c(rep(50, nyears), rep(NA_real_, nyears)),
    dim = c(length(iso.c), nyears, 1L),
    dimnames = list(iso.c, floor(est.years), NULL)
  )
  imr.ctj <- array(
    c(rep(30, nyears), rep(NA_real_, nyears)),
    dim = c(length(iso.c), nyears, 1L),
    dimnames = list(iso.c, floor(est.years), NULL)
  )

  replacement_rates <- array(
    40,
    dim = c(length(m49_regions), nyears, 1L),
    dimnames = list(m49_regions, floor(est.years), NULL)
  )
  u5mr.rtj <- replacement_rates
  imr.rtj <- replacement_rates
  save(u5mr.rtj, file = file.path(combined_dir, "M49Region_u5mr.rtj.rda"))
  save(imr.rtj, file = file.path(combined_dir, "M49Region_imr.rtj.rda"))

  calc_replacement(
    j = 1L,
    u5mr.ctj = u5mr.ctj,
    imr.ctj = imr.ctj,
    nmr.ctj = NULL,
    a0.c = c(0.1, 0.1),
    a1to4.c = c(0.4, 0.4),
    pop0.orig.ct = matrix(1000, nrow = length(iso.c), ncol = nyears),
    pop1to4.orig.ct = matrix(4000, nrow = length(iso.c), ncol = nyears),
    livebirths.ct = matrix(1000, nrow = length(iso.c), ncol = nyears),
    iso.c = iso.c,
    est.years = est.years,
    year1 = 2020.5,
    year2 = 2021.5,
    year4 = 2026.5,
    year.target = 2026.5,
    factor.target = 1 / 3,
    ndigits = 1,
    output.dir = samples_dir,
    output.dir.samplescombined = combined_dir,
    replace.rates.reg = "M49Region",
    replace.rates.cat = c("Southern Asia", "Western Europe"),
    country.ctx = build_context(
      nyears = nyears,
      years.start = 2020L,
      nn.exists = FALSE
    )
  )

  load(file.path(samples_dir, "deathu5.ct_1_M49Region-replace.rda"))
  load(file.path(samples_dir, "death0.ct_1_M49Region-replace.rda"))

  lie_idx <- match("LIE", iso.c)
  testthat::expect_true(any(!is.na(deathu5.ct[-lie_idx, ])))
  testthat::expect_true(all(is.na(deathu5.ct[lie_idx, ])))
  testthat::expect_true(all(is.na(death0.ct[lie_idx, ])))
})

testthat::test_that("replacement country generation clears excluded ISO rows loaded from cache", {
  get_outputaggregates_object <- function(name) {
    if (exists(name, inherits = TRUE)) {
      return(get(name, inherits = TRUE))
    }
    get(name, envir = asNamespace("shinyregionalaggregate"), inherits = FALSE)
  }
  calc_replacement <- get_outputaggregates_object("CalculateCountryDeathsBWC.replacemissingrates")
  build_context <- get_outputaggregates_object("BuildCountryCalculationContextBWC")
  m49_regions <- get_outputaggregates_object("M49RegionAll")

  tmp <- tempfile("replacement-exclusion-cache-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  samples_dir <- file.path(tmp, "samples")
  combined_dir <- file.path(tmp, "samples_combined")
  dir.create(samples_dir, recursive = TRUE)
  dir.create(combined_dir, recursive = TRUE)

  iso.c <- c("AFG", "LIE")
  lie_idx <- match("LIE", iso.c)
  nyears <- 7L
  est.years <- seq(2020.5, length.out = nyears, by = 1)

  u5mr.ctj <- array(
    c(rep(50, nyears), rep(NA_real_, nyears)),
    dim = c(length(iso.c), nyears, 1L),
    dimnames = list(iso.c, floor(est.years), NULL)
  )
  imr.ctj <- array(
    c(rep(30, nyears), rep(NA_real_, nyears)),
    dim = c(length(iso.c), nyears, 1L),
    dimnames = list(iso.c, floor(est.years), NULL)
  )

  replacement_rates <- array(
    40,
    dim = c(length(m49_regions), nyears, 1L),
    dimnames = list(m49_regions, floor(est.years), NULL)
  )
  u5mr.rtj <- replacement_rates
  imr.rtj <- replacement_rates
  save(u5mr.rtj, file = file.path(combined_dir, "M49Region_u5mr.rtj.rda"))
  save(imr.rtj, file = file.path(combined_dir, "M49Region_imr.rtj.rda"))

  death0.ctj <- death1to4.ctj <- deathu5.ctj <- array(
    999,
    dim = c(length(iso.c), nyears, 1L)
  )
  dx.array.ctj <- lx.array.ctj <- array(
    999,
    dim = c(3L, nyears * 52L, length(iso.c), 1L)
  )
  save(death0.ctj, file = file.path(combined_dir, "death0.ctj.M49Region-replace.rda"))
  save(death1to4.ctj, file = file.path(combined_dir, "death1to4.ctj.M49Region-replace.rda"))
  save(deathu5.ctj, file = file.path(combined_dir, "deathu5.ctj.M49Region-replace.rda"))
  save(dx.array.ctj, file = file.path(combined_dir, "dx.array.ctj_M49Region-replace.rda"))
  save(lx.array.ctj, file = file.path(combined_dir, "lx.array.ctj_M49Region-replace.rda"))

  calc_replacement(
    j = 1L,
    u5mr.ctj = u5mr.ctj,
    imr.ctj = imr.ctj,
    nmr.ctj = NULL,
    a0.c = c(0.1, 0.1),
    a1to4.c = c(0.4, 0.4),
    pop0.orig.ct = matrix(1000, nrow = length(iso.c), ncol = nyears),
    pop1to4.orig.ct = matrix(4000, nrow = length(iso.c), ncol = nyears),
    livebirths.ct = matrix(1000, nrow = length(iso.c), ncol = nyears),
    iso.c = iso.c,
    est.years = est.years,
    year1 = 2020.5,
    year2 = 2021.5,
    year4 = 2026.5,
    year.target = 2026.5,
    factor.target = 1 / 3,
    ndigits = 1,
    output.dir = samples_dir,
    output.dir.samplescombined = combined_dir,
    replace.rates.reg = "M49Region",
    replace.rates.cat = c("Southern Asia", "Western Europe"),
    country.ctx = build_context(
      nyears = nyears,
      years.start = 2020L,
      nn.exists = FALSE
    ),
    selected.country.idx = lie_idx
  )

  load(file.path(samples_dir, "deathu5.ct_1_M49Region-replace.rda"))
  load(file.path(samples_dir, "death0.ct_1_M49Region-replace.rda"))
  load(file.path(samples_dir, "dx.array.ct_1_M49Region-replace.rda"))

  testthat::expect_true(all(is.na(deathu5.ct[lie_idx, ])))
  testthat::expect_true(all(is.na(death0.ct[lie_idx, ])))
  testthat::expect_true(all(is.na(dx.array.by.c[, , lie_idx])))
})
