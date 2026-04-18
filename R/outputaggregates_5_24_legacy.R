#----------------------------------------------------------------------
# outputaggregates.R
# Jin Rou New, 2012-2013
#----------------------------------------------------------------------

WorldCacheFiles5_24 <- function() {
  c("res.world.rda", "u5mr.wtj.rda", "imr.wtj.rda", "cmr.wtj.rda",
    "deathu5.all.wtj.rda", "death0.all.wtj.rda", "death1to4.all.wtj.rda",
    "pop0.wt.rda", "pop1to4.wt.rda",
    "pop0.orig.wt.rda", "pop1to4.orig.wt.rda", "popu5.orig.wt.rda",
    "coverage0.wt.rda", "coverageu5.wt.rda")
}

GetPhaseElapsed5_24 <- function(start.time) {
  proc.time()[["elapsed"]] - start.time
}

ResolveOutputDirs5_24 <- function(output.dir) {
  if (is.null(output.dir)) {
    output.dir <- "output_numberofdeaths"
  }

  output.dirs <- list(
    output.dir = output.dir,
    output.dir.samples = file.path(output.dir, "samples"),
    output.dir.samplescombined = file.path(output.dir, "samples_combined")
  )

  dir.create(output.dirs$output.dir, showWarnings = FALSE)
  dir.create(output.dirs$output.dir.samples, showWarnings = FALSE)
  dir.create(output.dirs$output.dir.samplescombined, showWarnings = FALSE)
  output.dirs
}

CountryCacheFiles5_24 <- function() {
  c("cmr.ctj.rda", "death0.ctj.rda", "death1to4.ctj.rda", "deathu5.ctj.rda",
    "ARR.year1.year6.cj.rda", "ARR.year1.year2.cj.rda",
    "ARR.year3.year4.cj.rda", "ARR.year5.year6.cj.rda",
    "decline.year1.year6.cj.rda", "decline.year1.year2.cj.rda",
    "decline.year3.year4.cj.rda", "decline.year5.year6.cj.rda")
}

RegionalCombinedCacheFiles5_24 <- function(filename) {
  c(paste0(filename, "_u5mr.rtj.rda"),
    paste0(filename, "_imr.rtj.rda"),
    paste0(filename, "_cmr.rtj.rda"),
    paste0(filename, "_deathu5.all.rtj.rda"),
    paste0(filename, "_death0.all.rtj.rda"),
    paste0(filename, "_death1to4.all.rtj.rda"))
}

LoadCountryInputs5_24 <- function(
    country.info.file = NULL,
    population.file = NULL,
    data.a0.file = NULL
) {
  if (is.null(country.info.file)) {
    country.info.file <- file.path("input", "country.info.CME.csv")
  }
  if (is.null(population.file)) {
    population.file <- file.path("input", "country.info.CME.csv")
  }
  if (is.null(data.a0.file)) {
    data.a0.file <- file.path("input", "a0.csv")
  }

  country.info <- read.csv(
    file = country.info.file,
    header = TRUE,
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )
  country.info <- country.info[, !grepl("pop", colnames(country.info))]

  data.pop <- read.csv(
    file = population.file,
    header = TRUE,
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )
  data.a0 <- read.csv(
    file = data.a0.file,
    header = TRUE,
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )

  data.pop <- dplyr::inner_join(data.frame(ISO3Code = country.info$ISO3Code), data.pop)
  data.a0 <- dplyr::inner_join(data.frame(iso = country.info$ISO3Code), data.a0)

  if (sum(is.na(data.a0$a0)) > 0) {
    cat(paste0(
      "Note that a0 is NA for ",
      paste(data.a0$iso[is.na(data.a0$a0)], collapse = ", "),
      ".\n"
    ))
  }

  list(
    country.info = country.info,
    data.pop = data.pop,
    data.a0 = data.a0
  )
}

LoadTrajectoryArrays5_24 <- function(
    runname.U5MR = NULL,
    runname.IMR = NULL,
    results.U5MR.file = NULL,
    results.IMR.file = NULL,
    filename.U5MR = NULL,
    filename.IMR = NULL,
    country.info,
    output.dirs,
    est.years,
    test = FALSE
) {
  if (!is.null(runname.U5MR) & !is.null(runname.IMR)) {
    cat("Reading in results from output/runname.U5MR and output/runname.IMR.\n")
  } else if (!is.null(results.U5MR.file) & !is.null(results.IMR.file)) {
    cat("Reading in results from results.U5MR.file and results.IMR.file.\n")
  } else {
    stop("Either runname.U5MR and runname.IMR or results.U5MR.file and results.IMR.file must be specified.")
  }

  if (is.null(filename.U5MR)) {
    filename.U5MR <- "u5mrfinal.ctj.rda"
  }
  if (is.null(filename.IMR)) {
    filename.IMR <- "imrfinal.ctj.rda"
  }

  if (!is.null(results.U5MR.file) & !is.null(results.IMR.file)) {
    SummariseResults(
      results.file = results.U5MR.file,
      output.dir = file.path(output.dirs$output.dir, "U5MR"),
      filename.output = gsub(".rda", "", filename.U5MR)
    )
    SummariseResults(
      results.file = results.IMR.file,
      output.dir = file.path(output.dirs$output.dir, "IMR"),
      filename.output = gsub(".rda", "", filename.IMR)
    )

    u5mr.obj.name <- load(file = file.path(output.dirs$output.dir, "U5MR", filename.U5MR))
    u5mrfinal.ctj <- get(u5mr.obj.name)
    load(file = file.path(output.dirs$output.dir, "U5MR", "iso.c.rda"))
    load(file = file.path(output.dirs$output.dir, "U5MR", "year.t.rda"))
    isoU5MR.c <- iso.c
    yearU5MR.t <- year.t

    imr.obj.name <- load(file = file.path(output.dirs$output.dir, "IMR", filename.IMR))
    imrfinal.ctj <- get(imr.obj.name)
    load(file = file.path(output.dirs$output.dir, "IMR", "iso.c.rda"))
    load(file = file.path(output.dirs$output.dir, "IMR", "year.t.rda"))
    isoIMR.c <- iso.c
    yearIMR.t <- year.t
  } else {
    u5mr.obj.name <- load(file = file.path("output", runname.U5MR, filename.U5MR))
    u5mrfinal.ctj <- get(u5mr.obj.name)
    load(file = file.path("output", runname.U5MR, "iso.c.rda"))
    load(file = file.path("output", runname.U5MR, "year.t.rda"))
    isoU5MR.c <- iso.c
    yearU5MR.t <- year.t

    imr.obj.name <- load(file = file.path("output", runname.IMR, filename.IMR))
    imrfinal.ctj <- get(imr.obj.name)
    load(file = file.path("output", runname.IMR, "iso.c.rda"))
    load(file = file.path("output", runname.IMR, "year.t.rda"))
    isoIMR.c <- iso.c
    yearIMR.t <- year.t
  }

  if (test) {
    u5mrfinal.ctj <- u5mrfinal.ctj[, , 1:10]
    imrfinal.ctj <- imrfinal.ctj[, , 1:10]
  }

  iso.c <- country.info$ISO3Code
  C <- length(iso.c)
  nyears <- length(est.years)
  nsim <- dim(u5mrfinal.ctj)[3]
  cache.files <- file.path(output.dirs$output.dir.samplescombined, c("u5mr.ctj.rda", "imr.ctj.rda"))

  if (sum(!file.exists(cache.files)) > 0) {
    u5mr.ctj <- imr.ctj <- array(NA, c(C, nyears, nsim))
    u5mr.ctj[match(isoU5MR.c, iso.c), is.element(est.years, yearU5MR.t), ] <-
      u5mrfinal.ctj[, is.element(yearU5MR.t, est.years), ]
    imr.ctj[match(isoIMR.c, iso.c), is.element(est.years, yearIMR.t), ] <-
      imrfinal.ctj[, is.element(yearIMR.t, est.years), ]

    u5mr.ctj[u5mr.ctj > 1000] <- 1000
    imr.ctj[imr.ctj > 1000] <- 1000

    q1to4.ctj <- 1 - (1 - u5mr.ctj/1000)/(1 - imr.ctj/1000)
    imr.ctj[!is.na(imr.ctj) & imr.ctj > u5mr.ctj] <-
      u5mr.ctj[!is.na(imr.ctj) & imr.ctj > u5mr.ctj]

    arr.ind.select <- which(is.na(q1to4.ctj), arr.ind = TRUE)
    u5mr.ctj[arr.ind.select] <- NA
    imr.ctj[arr.ind.select] <- NA

    select.NA.c <- apply(u5mr.ctj[, , 1], 1, function(x) sum(!is.na(x)) == 0)
    cat(paste0(
      "Note that U5MR estimates are NA for all years of estimation for ",
      paste(iso.c[select.NA.c], collapse = ", "),
      ".\n"
    ))
    select.NA.c <- apply(imr.ctj[, , 1], 1, function(x) sum(!is.na(x)) == 0)
    cat(paste0(
      "Note that IMR estimates are NA for all years of estimation for ",
      paste(iso.c[select.NA.c], collapse = ", "),
      ".\n"
    ))

    if (!dir.exists(output.dirs$output.dir.samplescombined)) {
      dir.create(output.dirs$output.dir.samplescombined)
    }
    save(u5mr.ctj, file = file.path(output.dirs$output.dir.samplescombined, "u5mr.ctj.rda"))
    save(imr.ctj, file = file.path(output.dirs$output.dir.samplescombined, "imr.ctj.rda"))
    cat(paste0("Processed trajectories saved to ", output.dirs$output.dir.samplescombined, "\n"))
  } else {
    load(file = file.path(output.dirs$output.dir.samplescombined, "u5mr.ctj.rda"))
    load(file = file.path(output.dirs$output.dir.samplescombined, "imr.ctj.rda"))
    cat(paste0("Processed trajectories loaded from ", output.dirs$output.dir.samplescombined, "\n"))
  }

  list(
    u5mr.ctj = u5mr.ctj,
    imr.ctj = imr.ctj
  )
}

BuildOlderChildrenContext5_24 <- function(
    country.info,
    data.pop,
    data.a0,
    u5mr.ctj,
    imr.ctj,
    est.years,
    output.dirs,
    year1,
    year2,
    year3,
    year4,
    year5,
    year6,
    year.target,
    factor.target
) {
  est.years.floor <- floor(est.years)
  a0.c <- rep(0.5, nrow(country.info))
  a1to4.c <- rep(0.5, nrow(country.info))

  list(
    country.info = country.info,
    data.pop = data.pop,
    data.a0 = data.a0,
    u5mr.ctj = u5mr.ctj,
    imr.ctj = imr.ctj,
    est.years = est.years,
    est.years.floor = est.years.floor,
    iso.c = country.info$ISO3Code,
    nyears = length(est.years),
    nsim = dim(u5mr.ctj)[3],
    a0.c = a0.c,
    a1to4.c = a1to4.c,
    pop0.orig.ct = data.pop[, is.element(colnames(data.pop), paste0("pop0", est.years.floor))],
    pop1to4.orig.ct = data.pop[, is.element(colnames(data.pop), paste0("pop1to4", est.years.floor))],
    year1 = year1,
    year2 = year2,
    year3 = year3,
    year4 = year4,
    year5 = year5,
    year6 = year6,
    year.target = year.target,
    factor.target = factor.target,
    output.dir = output.dirs$output.dir,
    output.dir.samples = output.dirs$output.dir.samples,
    output.dir.samplescombined = output.dirs$output.dir.samplescombined
  )
}

RunCountryResults5_24 <- function(ctx, percentiles, ndigits, run.on.server) {
  phase.start <- proc.time()[["elapsed"]]
  files.country <- CountryCacheFiles5_24()

  if (sum(!file.exists(file.path(ctx$output.dir.samplescombined, files.country))) > 0) {
    cat("Generating country results...\n")
    if (run.on.server) {
      registerDoMC()
      foreach (j = seq_len(ctx$nsim)) %dopar% {
        CalculateCountryDeaths(
          j = j,
          u5mr.ctj = ctx$u5mr.ctj,
          imr.ctj = ctx$imr.ctj,
          a0.c = ctx$a0.c,
          a1to4.c = ctx$a1to4.c,
          pop0.orig.ct = ctx$pop0.orig.ct,
          pop1to4.orig.ct = ctx$pop1to4.orig.ct,
          iso.c = ctx$iso.c,
          est.years = ctx$est.years,
          year1 = ctx$year1,
          year2 = ctx$year2,
          year3 = ctx$year3,
          year4 = ctx$year4,
          year5 = ctx$year5,
          year6 = ctx$year6,
          year.target = ctx$year.target,
          factor.target = ctx$factor.target,
          ndigits = ndigits,
          output.dir = ctx$output.dir.samples
        )
        cat(paste0(
          "Output generated for trajectory ",
          j,
          " out of ",
          ctx$nsim,
          ifelse(ctx$nsim > 1, " trajectories.\n", " trajectory.\n")
        ))
      }
    } else {
      for (j in seq_len(ctx$nsim)) {
        CalculateCountryDeaths(
          j = j,
          u5mr.ctj = ctx$u5mr.ctj,
          imr.ctj = ctx$imr.ctj,
          a0.c = ctx$a0.c,
          a1to4.c = ctx$a1to4.c,
          pop0.orig.ct = ctx$pop0.orig.ct,
          pop1to4.orig.ct = ctx$pop1to4.orig.ct,
          iso.c = ctx$iso.c,
          est.years = ctx$est.years,
          year1 = ctx$year1,
          year2 = ctx$year2,
          year3 = ctx$year3,
          year4 = ctx$year4,
          year5 = ctx$year5,
          year6 = ctx$year6,
          year.target = ctx$year.target,
          factor.target = ctx$factor.target,
          ndigits = ndigits,
          output.dir = ctx$output.dir.samples
        )
        cat(paste0(
          "Output generated for trajectory ",
          j,
          " out of ",
          ctx$nsim,
          ifelse(ctx$nsim > 1, " trajectories.\n", " trajectory.\n")
        ))
      }
    }

    cat("Combining and outputting country results...\n")
    CombineAndOutputCountryResults(
      u5mr.ctj = ctx$u5mr.ctj,
      imr.ctj = ctx$imr.ctj,
      country.info = ctx$country.info,
      percentiles = percentiles,
      ndigits = ndigits,
      output.dir = ctx$output.dir,
      output.dir.samples = ctx$output.dir.samples,
      output.dir.samplescombined = ctx$output.dir.samplescombined
    )
  } else {
    sapply(files.country, LoadFile, output.dir = ctx$output.dir.samplescombined, envir = environment())
    cat(paste("Country results loaded from ", ctx$output.dir.samplescombined, "\n"))
  }

  GetPhaseElapsed5_24(phase.start)
}

DeleteWorldOutputs5_24 <- function(output.dir.samplescombined) {
  unlink(file.path(output.dir.samplescombined, WorldCacheFiles5_24()))
}

RunWorldResults5_24 <- function(ctx, percentiles, ndigits) {
  phase.start <- proc.time()[["elapsed"]]
  files.world <- WorldCacheFiles5_24()

  if (sum(!file.exists(file.path(ctx$output.dir.samplescombined, files.world))) > 0) {
    cat("Generating world results...\n")
    CalculateWorldDeaths(
      output.dir.samplescombined = ctx$output.dir.samplescombined,
      output.dir = ctx$output.dir,
      percentiles = percentiles,
      ndigits = ndigits
    )
    cat("Output generated for world.\n")
  } else {
    sapply(files.world, LoadFile, output.dir = ctx$output.dir.samplescombined, envir = environment())
    cat(paste("World results loaded from ", ctx$output.dir.samplescombined, "\n"))
  }

  GetPhaseElapsed5_24(phase.start)
}

BuildAdhocRegionalSpec5_24 <- function(country.info) {
  matching_cols <- colnames(country.info)[
    grepl("AdhocCountries", colnames(country.info), ignore.case = TRUE)
  ]
  if (length(matching_cols) < 1) {
    return(NULL)
  }

  hierarchical_cols <- matching_cols[
    grepl("^AdhocCountries[0-9]*$", matching_cols, ignore.case = TRUE)
  ]
  singlelevel_cols <- matching_cols[
    grepl("^AdhocCountries_", matching_cols, ignore.case = TRUE)
  ]

  if (length(hierarchical_cols) > 0) {
    all_region_names <- unique(unlist(lapply(hierarchical_cols, function(col) {
      unique(country.info[country.info[, col] != "", col])
    })))
    all_region_names <- all_region_names[all_region_names != ""]

    return(list(
      selection_key = "Adhoc",
      filename = "AdhocCountries",
      regiontypes = all_region_names,
      regions = country.info[, hierarchical_cols, drop = FALSE],
      announce = paste0(
        "Processing ",
        length(all_region_names),
        " hierarchical regions: ",
        paste(all_region_names, collapse = ", ")
      )
    ))
  }

  if (length(singlelevel_cols) > 0) {
    all_region_names <- unique(unlist(lapply(singlelevel_cols, function(col) {
      unique(country.info[country.info[, col] != "", col])
    })))
    all_region_names <- all_region_names[all_region_names != ""]

    return(list(
      selection_key = "Adhoc",
      filename = "AdhocCountries",
      regiontypes = all_region_names,
      regions = country.info[, singlelevel_cols, drop = FALSE],
      announce = paste0(
        "Processing ",
        length(all_region_names),
        " single-level regions: ",
        paste(all_region_names, collapse = ", ")
      )
    ))
  }

  list(
    selection_key = "Adhoc",
    filename = "AdhocCountries",
    regiontypes = "Adhoc",
    regions = country.info[, matching_cols, drop = FALSE],
    announce = NULL
  )
}

BuildStandardRegionalSpecs5_24 <- function(country.info, regiontypes.select) {
  specs <- list()

  add_spec <- function(selection_key, regiontypes, pattern, filename) {
    if (!is.element(selection_key, regiontypes.select)) {
      return(invisible(NULL))
    }
    selected.cols <- grepl(pattern, colnames(country.info))
    if (!any(selected.cols)) {
      return(invisible(NULL))
    }
    specs[[length(specs) + 1L]] <<- list(
      selection_key = selection_key,
      filename = filename,
      regiontypes = regiontypes,
      regions = country.info[, selected.cols, drop = FALSE],
      announce = NULL
    )
    invisible(NULL)
  }

  add_spec("EAPRO", EAPRORegionAll, "EAPRORegion", "EAPRORegion")
  add_spec("SPhumanitarian", SPhumanitarianAll, "SPhumanitarian2022", "SPhumanitarianRegion")
  add_spec("SPhighburden", SPhighburdenAll, "SPhighburden", "SPhighburdenRegion")
  add_spec("UNICEFProg", UNICEFProgRegionAll, "UNICEFProg", "UNICEFProgRegion")
  add_spec("SDGSimple", SDGSimpleRegionAll, "SDGSimple", "SDGSimpleRegion")
  add_spec("UNICEFReport", UNICEFReportRegionAll, "UNICEFReport", "UNICEFReportRegion")
  add_spec("MDG", MDGRegionAll, "MDG", "MDGRegion")
  add_spec("WHO", WHORegionAll, "WHO", "WHORegion")
  add_spec("WB", WBRegionAll, "WB", "WBRegion")
  add_spec("UNPD", UNPDRegionAll, "UNPD", "UNPDRegion")
  add_spec("OIC", OICRegionAll, "OIC", "OICRegion")
  add_spec("Countdown", CountdownAll, "Countdown", "CountdownCountries")
  add_spec("ECAAfrica", ECAAfricaRegionAll, "ECAAfrica", "ECAAfricaRegion")
  add_spec("AU", AURegionAll, "AURegion2", "AURegion")
  add_spec("Fragile2013", Fragile2013All, "FragileCountries2013", "FragileCountries2013")
  add_spec("Fragile2014", Fragile2014All, "FragileCountries2014", "FragileCountries2014")
  add_spec("Fragile2015", Fragile2015All, "FragileCountries2015", "FragileCountries2015")
  add_spec("USAID", USAIDAll, "USAID", "USAIDCountries")
  add_spec("M49", M49RegionAll, "M49", "M49Region")
  add_spec(
    "AfricanEconomicCommunity",
    AfricanEconomicCommunityAll,
    "AfricanEconomicCommunity",
    "AfricanEconomicCommunityRegion"
  )

  specs
}

BuildRegionalSpecs5_24 <- function(country.info, regiontypes.select) {
  if (is.null(regiontypes.select)) {
    return(list())
  }

  specs <- list()
  if (is.element("Adhoc", regiontypes.select)) {
    adhoc.spec <- BuildAdhocRegionalSpec5_24(country.info)
    if (!is.null(adhoc.spec)) {
      specs[[length(specs) + 1L]] <- adhoc.spec
    }
  }

  c(specs, BuildStandardRegionalSpecs5_24(country.info, regiontypes.select))
}

RegionalBundleExists5_24 <- function(output.dir.samples.region, j) {
  files <- c(
    paste0("q0.rt_", j, ".rda"),
    paste0("q1to4.rt_", j, ".rda"),
    paste0("q5.rt_", j, ".rda"),
    paste0("death0.all.rt_", j, ".rda"),
    paste0("death1to4.all.rt_", j, ".rda"),
    paste0("deathu5.all.rt_", j, ".rda")
  )
  all(file.exists(file.path(output.dir.samples.region, files)))
}

SaveRegionalBundle5_24 <- function(bundle, output.dir.samples.region, j) {
  dir.create(output.dir.samples.region, recursive = TRUE, showWarnings = FALSE)
  q0.rt <- bundle$q0.rt
  q1to4.rt <- bundle$q1to4.rt
  q5.rt <- bundle$q5.rt
  death0.all.rt <- bundle$death0.all.rt
  death1to4.all.rt <- bundle$death1to4.all.rt
  deathu5.all.rt <- bundle$deathu5.all.rt
  save(q0.rt, file = file.path(output.dir.samples.region, paste0("q0.rt_", j, ".rda")))
  save(q1to4.rt, file = file.path(output.dir.samples.region, paste0("q1to4.rt_", j, ".rda")))
  save(q5.rt, file = file.path(output.dir.samples.region, paste0("q5.rt_", j, ".rda")))
  save(death0.all.rt, file = file.path(output.dir.samples.region, paste0("death0.all.rt_", j, ".rda")))
  save(death1to4.all.rt, file = file.path(output.dir.samples.region, paste0("death1to4.all.rt_", j, ".rda")))
  save(deathu5.all.rt, file = file.path(output.dir.samples.region, paste0("deathu5.all.rt_", j, ".rda")))
}

RunRegionalResults5_24 <- function(
    spec,
    ctx,
    run.on.server,
    percentiles,
    ndigits,
    replace.rates.reg = NULL
) {
  region.start <- proc.time()[["elapsed"]]
  cat(paste0("Generating output for ", spec$filename, "...\n"))

  output.dir.samples.region <- file.path(ctx$output.dir.samples, spec$filename)
  dir.create(output.dir.samples.region, recursive = TRUE, showWarnings = FALSE)

  regions <- spec$regions
  regions[is.na(regions)] <- 0

  if (ctx$nsim >= 1) {
    CalculateRegionalDeaths5_24(
      j = 1,
      output.dir.samples = ctx$output.dir.samples,
      output.dir.samplescombined = ctx$output.dir.samplescombined,
      regions = regions,
      regiontypes = spec$regiontypes,
      filename = spec$filename
    )
    cat(paste0("Output generated for trajectory 1 out of ", ctx$nsim,
               ifelse(ctx$nsim > 1, " trajectories.\n", " trajectory.\n")))
  }

  if (ctx$nsim > 1) {
    if (run.on.server) {
      registerDoMC()
      foreach (j = 2:ctx$nsim) %dopar% {
        CalculateRegionalDeaths5_24(
          j = j,
          output.dir.samples = ctx$output.dir.samples,
          output.dir.samplescombined = ctx$output.dir.samplescombined,
          regions = regions,
          regiontypes = spec$regiontypes,
          filename = spec$filename
        )
        cat(paste0("Output generated for trajectory ", j, " out of ", ctx$nsim,
                   ifelse(ctx$nsim > 1, " trajectories.\n", " trajectory.\n")))
      }
    } else {
      for (j in 2:ctx$nsim) {
        CalculateRegionalDeaths5_24(
          j = j,
          output.dir.samples = ctx$output.dir.samples,
          output.dir.samplescombined = ctx$output.dir.samplescombined,
          regions = regions,
          regiontypes = spec$regiontypes,
          filename = spec$filename
        )
        cat(paste0("Output generated for trajectory ", j, " out of ", ctx$nsim,
                   ifelse(ctx$nsim > 1, " trajectories.\n", " trajectory.\n")))
      }
    }
  }

  retry.count <- 0L
  max.retries <- 10L
  retried.trajs <- integer(0)
  missing.trajs <- which(!vapply(seq_len(ctx$nsim), function(j) {
    RegionalBundleExists5_24(output.dir.samples.region, j)
  }, logical(1)))

  while (length(missing.trajs) > 0 && retry.count < max.retries) {
    retry.count <- retry.count + 1L
    retried.trajs <- unique(c(retried.trajs, missing.trajs))

    if (run.on.server && length(missing.trajs) > 1) {
      registerDoMC()
      foreach (j = missing.trajs) %dopar% {
        CalculateRegionalDeaths5_24(
          j = j,
          output.dir.samples = ctx$output.dir.samples,
          output.dir.samplescombined = ctx$output.dir.samplescombined,
          regions = regions,
          regiontypes = spec$regiontypes,
          filename = spec$filename
        )
      }
    } else {
      for (j in missing.trajs) {
        CalculateRegionalDeaths5_24(
          j = j,
          output.dir.samples = ctx$output.dir.samples,
          output.dir.samplescombined = ctx$output.dir.samplescombined,
          regions = regions,
          regiontypes = spec$regiontypes,
          filename = spec$filename
        )
      }
    }

    missing.trajs <- which(!vapply(seq_len(ctx$nsim), function(j) {
      RegionalBundleExists5_24(output.dir.samples.region, j)
    }, logical(1)))
  }

  if (length(missing.trajs) > 0) {
    stop(
      sprintf(
        "Missing regional bundles for %s after %d retries: %s",
        spec$filename,
        max.retries,
        paste(missing.trajs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  cat("Combining and outputting regional results...\n")
  CombineAndOutputRegionalResults5_24(
    output.dir = ctx$output.dir,
    output.dir.samples = ctx$output.dir.samples,
    output.dir.samplescombined = ctx$output.dir.samplescombined,
    regiontypes = spec$regiontypes,
    filename = spec$filename,
    percentiles = percentiles,
    ndigits = ndigits,
    replace.rates.reg = replace.rates.reg
  )

  elapsed <- GetPhaseElapsed5_24(region.start)
  cat(sprintf(
    "Regional summary for %s: %.2f s, retries=%d, retried_trajectories=%d\n",
    spec$filename,
    elapsed,
    retry.count,
    length(retried.trajs)
  ))
  invisible(list(
    filename = spec$filename,
    elapsed = elapsed,
    retries = retry.count,
    retried_trajectories = length(retried.trajs)
  ))
}

RunAllRegionalResults5_24 <- function(ctx, regiontypes.select, run.on.server, percentiles, ndigits) {
  if (is.null(regiontypes.select)) {
    return(0)
  }

  regional.start <- proc.time()[["elapsed"]]
  specs <- BuildRegionalSpecs5_24(ctx$country.info, regiontypes.select)
  cat("Generating regional results...\n")

  for (spec in specs) {
    if (!is.null(spec$announce) && nzchar(spec$announce)) {
      message(spec$announce, "\n")
    }
    RunRegionalResults5_24(
      spec = spec,
      ctx = ctx,
      run.on.server = run.on.server,
      percentiles = percentiles,
      ndigits = ndigits
    )
  }

  GetPhaseElapsed5_24(regional.start)
}

OutputAggregates.ori <- function( # Calculate and output aggregated rates and numbers of deaths at the country,
  ## regional and global level.
  runname.U5MR = NULL, ##<< Either specify 1) \code{runname.U5MR}
  runname.IMR = NULL, ##<< and \code{runname.IMR}, or
  results.U5MR.file = NULL, ##<< 2) and \code{results.U5MR.file} (for median only)
  results.IMR.file = NULL, ##<< and \code{results.IMR.file} (for median only).
  filename.U5MR = NULL, ##<< Alternative file name of trajectory array, used if \code{runname.U5MR} is specified.
  ## Default is \code{u5mrfinal.ctj.rda}.
  filename.IMR = NULL, ##<< Alternative file name of trajectory array, used if \code{runname.IMR} is specified.
  ## Default is \code{imrfinal.ctj.rda}.

  country.info.file = NULL,
  population.file = NULL,
  data.a0.file = NULL, ##<< File path to a0 data. If \code{NULL}, a0 data included in package is used.

  ## package from World Population Prospects 2012 is used.
  run.on.server = FALSE, ##<< Running on server? Set \code{TRUE} to run in parallel.
  regiontypes.select = c("Adhoc"), ##<< Output regional aggregates for which region types?
  ## Input a character vector of more than one of the possible options if desired.
  ## If \code{NULL}, output will not be generated at the region level.
  est.years = seq(1950.5, year.target, 1), ##<< Years of estimation.
  year1 = 1990.5, ##<< First year used for ARR calculation.
  year2 = 1999.5, ##<< Second year used for ARR calculation.
  year3 = 2000.5, ##<< Third year used for ARR calculation.
  year4 = 2009.5, ##<< Fourth year used for ARR calculation.
  year5 = 2010.5, ##<< Fifth year used for ARR calculation.
  year6 = 2020.5, ##<< Last year used for ARR calculation.
  year.target = year.lastestimatepublished, ##<< MDG target year.
  factor.target = 1/3, ##<< MDG target factor (Reduce to one third).
  percentiles = c(0.05, 0.5, 0.95), ##<< Vector of percentiles.
  ndigits = 10, ##<< Number of decimal places to use for analysis (e.g. to calcalate ARR, decline).
  output.dir = NULL, ##<< Output directory to save all results.
  test = FALSE ##<< Use a subset of 10 trajectories to test function.
) {
  source("R/chooseregion.R")
  overall.start <- proc.time()[["elapsed"]]
  phase.times <- list()

  output.dirs <- ResolveOutputDirs5_24(output.dir)
  preprocess.start <- proc.time()[["elapsed"]]
  inputs <- LoadCountryInputs5_24(
    country.info.file = country.info.file,
    population.file = population.file,
    data.a0.file = data.a0.file
  )
  trajectories <- LoadTrajectoryArrays5_24(
    runname.U5MR = runname.U5MR,
    runname.IMR = runname.IMR,
    results.U5MR.file = results.U5MR.file,
    results.IMR.file = results.IMR.file,
    filename.U5MR = filename.U5MR,
    filename.IMR = filename.IMR,
    country.info = inputs$country.info,
    output.dirs = output.dirs,
    est.years = est.years,
    test = test
  )
  ctx <- BuildOlderChildrenContext5_24(
    country.info = inputs$country.info,
    data.pop = inputs$data.pop,
    data.a0 = inputs$data.a0,
    u5mr.ctj = trajectories$u5mr.ctj,
    imr.ctj = trajectories$imr.ctj,
    est.years = est.years,
    output.dirs = output.dirs,
    year1 = year1,
    year2 = year2,
    year3 = year3,
    year4 = year4,
    year5 = year5,
    year6 = year6,
    year.target = year.target,
    factor.target = factor.target
  )
  phase.times$preprocess <- GetPhaseElapsed5_24(preprocess.start)

  country.info <- ctx$country.info
  output.dir <- ctx$output.dir
  output.dir.samples <- ctx$output.dir.samples
  output.dir.samplescombined <- ctx$output.dir.samplescombined

  phase.times$country <- RunCountryResults5_24(
    ctx = ctx,
    percentiles = percentiles,
    ndigits = ndigits,
    run.on.server = run.on.server
  )
  phase.times$world <- RunWorldResults5_24(
    ctx = ctx,
    percentiles = percentiles,
    ndigits = ndigits
  )

  phase.times$regional <- RunAllRegionalResults5_24(
    ctx = ctx,
    regiontypes.select = regiontypes.select,
    run.on.server = run.on.server,
    percentiles = percentiles,
    ndigits = ndigits
  )
  phase.times$total <- GetPhaseElapsed5_24(overall.start)
  cat("OutputAggregates.ori timing summary:\n")
  cat(sprintf("  preprocess    %7.2f s\n", phase.times$preprocess))
  cat(sprintf("  country       %7.2f s\n", phase.times$country))
  cat(sprintf("  world         %7.2f s\n", phase.times$world))
  cat(sprintf("  regional      %7.2f s\n", phase.times$regional))
  cat(sprintf("  total         %7.2f s\n", phase.times$total))
  invisible(phase.times)
}
#----------------------------------------------------------------------
CalculateCountryDeaths <- function(
  j, ##<< Index number of trajectory.
  u5mr.ctj,
  imr.ctj,
  a0.c,
  a1to4.c,
  pop0.orig.ct,
  pop1to4.orig.ct,
  iso.c,
  est.years,
  year1,
  year2,
  year3,
  year4,
  year5,
  year6,
  year.target,
  factor.target,
  ndigits,
  output.dir
) {
  pop0.ct <- pop0.orig.ct
  pop1to4.ct <- pop1to4.orig.ct
  # set population to 0 if rate data not available
  med.imr.ct <- apply(imr.ctj,1:2, median,  na.rm=T)#LH 20211207
  med.u5mr.ct <- apply(imr.ctj,1:2, median,  na.rm=T)#LH 20211207
  arr.ind.select <- which(is.na(med.imr.ct) | is.na(med.u5mr.ct), arr.ind = TRUE) #LH 20211207 to avoid population set 0 if only some trajectories are 0
  #arr.ind.select <- which(is.na(imr.ctj[, , 1]) | is.na(u5mr.ctj[, , 1]), arr.ind = TRUE)

  pop0.ct[arr.ind.select] <- 0
  pop1to4.ct[arr.ind.select] <- 0

  # round off to 1 d.p. before calculation (for median only)
  if(FALSE){#bmedit20180307
  if (dim(u5mr.ctj)[3] == 1) {
    u5mr.ctj <- u5mr.ctj
    imr.ctj <- imr.ctj
  }
  }
  u5mr.ct <- u5mr.ctj[, , j]
  imr.ct <- imr.ctj[, , j]
  C <- nrow(u5mr.ct)
  nyears <- ncol(u5mr.ct)
  cmr.ct<-death0.ct <- death1to4.ct <- deathu5.ct <- matrix(NA, C, nyears)
  arr.ind.select.ct <- which(is.na(imr.ct) | is.na(u5mr.ct), arr.ind = TRUE) #lh edit 20211207
  # calculate parameters
  q1to4.ct <- 1-(1-u5mr.ct/1000)/(1-imr.ct/1000)
  #q1to4.ct[q1to4.ct<0]<-NA #set zero if u5mr<imr
  cmr.ct<-q1to4.ct*1000

  #bmedit M0.ct <- (imr.ct/1000)/(1-(imr.ct/1000)*(1-a0.c))
  M0.ct <- (imr.ct/1000)/(5*(1-(imr.ct/1000)*(1-a0.c)))
  #M0.ct[arr.ind.select] <- 0
  M0.ct[arr.ind.select.ct] <- 0 #lh edit 20211207
  #bmedit  M1to4.ct <- q1to4.ct/(4*(1-q1to4.ct*(1-a1to4.c)))
  M1to4.ct <- q1to4.ct/(5*(1-q1to4.ct*(1-a1to4.c)))
  #M1to4.ct[arr.ind.select] <- 0
  M1to4.ct[arr.ind.select.ct] <- 0 #lh edit 20211207
  # calculate country deaths
  for (i in 1:nyears) {
    death0.ct[,i] <- roundoff(M0.ct[,i]*pop0.ct[,i],0) #LH edit 08172020
    death1to4.ct[,i] <- roundoff(M1to4.ct[,i]*pop1to4.ct[,i],0) #LH edit 08172020
    deathu5.ct[,i] <- roundoff(death0.ct[,i] + death1to4.ct[,i],0)
  }
  # set deaths to NA if rate data is not available
  # cmr.ct[arr.ind.select] <- NA #LH edit 08172020
  # death0.ct[arr.ind.select] <- NA #LH edit 08172020
  # death1to4.ct[arr.ind.select] <- NA #LH edit 08172020
  # deathu5.ct[arr.ind.select] <- NA #LH edit 08172020
  cmr.ct[arr.ind.select.ct] <- NA
  death0.ct[arr.ind.select.ct] <- NA
  death1to4.ct[arr.ind.select.ct] <- NA
  deathu5.ct[arr.ind.select.ct] <- NA
  # calculate country rates of decline
  ARR.year1.year6.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year6)
  ARR.year1.year2.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
  ARR.year3.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year3, year.end = year4)
  ARR.year5.year6.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year5, year.end = year6)

  ARR.IMR.year1.year6.c <- CalculateARR(u5mr = imr.ct, years = est.years, year.start = year1, year.end = year6)
  ARR.IMR.year1.year2.c <- CalculateARR(u5mr = imr.ct, years = est.years, year.start = year1, year.end = year2)
  ARR.IMR.year3.year4.c <- CalculateARR(u5mr = imr.ct, years = est.years, year.start = year3, year.end = year4)
  ARR.IMR.year5.year6.c <- CalculateARR(u5mr = imr.ct, years = est.years, year.start = year5, year.end = year6)

  ARR.CMR.year1.year6.c <- CalculateARR(u5mr = cmr.ct, years = est.years, year.start = year1, year.end = year6)
  ARR.CMR.year1.year2.c <- CalculateARR(u5mr = cmr.ct, years = est.years, year.start = year1, year.end = year2)
  ARR.CMR.year3.year4.c <- CalculateARR(u5mr = cmr.ct, years = est.years, year.start = year3, year.end = year4)
  ARR.CMR.year5.year6.c <- CalculateARR(u5mr = cmr.ct, years = est.years, year.start = year5, year.end = year6)

  required.ARR.c <- ifelse(year6 < year.target,
                           1/(year.target-year6)*
                             log(u5mr.ct[, est.years == year1]*factor.target/
                                   u5mr.ct[, est.years == year6])*-100, NA)
  changeinARR.c <- ARR.year1.year6.c - ARR.year5.year6.c
  decline.year1.year6.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year6)
  decline.year1.year2.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
  decline.year3.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year3, year.end = year4)
  decline.year5.year6.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year5, year.end = year6)

  if (!file.exists(file.path(output.dir, "info.rda"))) {
    info <- list(iso.c = iso.c,
                 C = C,
                 est.years = est.years,
                 est.years.floor = est.years-0.5,
                 nyears = nyears,
                 a0.c = a0.c,
                 a1to4.c = a1to4.c,
                 pop0.ct = pop0.ct,
                 pop1to4.ct = pop1to4.ct,
                 pop0.orig.ct = pop0.orig.ct,
                 pop1to4.orig.ct = pop1to4.orig.ct,
                 year1 = year1,
                 year2 = year2,
                 year3 = year3,
                 year4 = year4,
                 year5 = year5,
                 year6 = year6,
                 year.target = year.target,
                 factor.target = factor.target)
    if(!dir.exists(output.dir))dir.create(output.dir)
    save(info, file = file.path(output.dir, "info.rda"))
    cat(paste0("Information about the aggregates have been saved to ", output.dir, ".\n"))
  }
  # save samples
  save(cmr.ct, file = file.path(output.dir, paste0("cmr.ct_", j, ".rda")))
  save(death0.ct, file = file.path(output.dir, paste0("death0.ct_", j, ".rda")))
  save(death1to4.ct, file = file.path(output.dir, paste0("death1to4.ct_", j, ".rda")))
  save(deathu5.ct, file = file.path(output.dir, paste0("deathu5.ct_", j, ".rda")))
  save(ARR.year1.year6.c, file = file.path(output.dir, paste0("ARR.year1.year6.c_", j, ".rda")))
  save(ARR.year1.year2.c, file = file.path(output.dir, paste0("ARR.year1.year2.c_", j, ".rda")))
  save(ARR.year3.year4.c, file = file.path(output.dir, paste0("ARR.year3.year4.c_", j, ".rda")))
  save(ARR.year5.year6.c, file = file.path(output.dir, paste0("ARR.year5.year6.c_", j, ".rda")))

  save(ARR.IMR.year1.year6.c, file = file.path(output.dir, paste0("ARR.IMR.year1.year6.c_", j, ".rda")))
  save(ARR.IMR.year1.year2.c, file = file.path(output.dir, paste0("ARR.IMR.year1.year2.c_", j, ".rda")))
  save(ARR.IMR.year3.year4.c, file = file.path(output.dir, paste0("ARR.IMR.year3.year4.c_", j, ".rda")))
  save(ARR.IMR.year5.year6.c, file = file.path(output.dir, paste0("ARR.IMR.year5.year6.c_", j, ".rda")))

  save(ARR.CMR.year1.year6.c, file = file.path(output.dir, paste0("ARR.CMR.year1.year6.c_", j, ".rda")))
  save(ARR.CMR.year1.year2.c, file = file.path(output.dir, paste0("ARR.CMR.year1.year2.c_", j, ".rda")))
  save(ARR.CMR.year3.year4.c, file = file.path(output.dir, paste0("ARR.CMR.year3.year4.c_", j, ".rda")))
  save(ARR.CMR.year5.year6.c, file = file.path(output.dir, paste0("ARR.CMR.year5.year6.c_", j, ".rda")))


  save(required.ARR.c, file = file.path(output.dir, paste0("required.ARR.c_", j, ".rda")))
  save(changeinARR.c, file = file.path(output.dir, paste0("changeinARR.c_", j, ".rda")))
  save(decline.year1.year6.c, file = file.path(output.dir, paste0("decline.year1.year6.c_", j, ".rda")))
  save(decline.year1.year2.c, file = file.path(output.dir, paste0("decline.year1.year2.c_", j, ".rda")))
  save(decline.year3.year4.c, file = file.path(output.dir, paste0("decline.year3.year4.c_", j, ".rda")))
  save(decline.year5.year6.c, file = file.path(output.dir, paste0("decline.year5.year6.c_", j, ".rda")))
}
#-------------------------------------------------------------------------
CombineAndOutputCountryResults <- function(
  u5mr.ctj,
  imr.ctj,
  country.info,
  percentiles,
  ndigits,
  output.dir,
  output.dir.samples,
  output.dir.samplescombined
) {
  load(file.path(output.dir.samples, "info.rda"))
  list2env(info, envir = environment())

  nsim <- dim(u5mr.ctj)[3]
  est.years.floor <- est.years-0.5

  # combine all the samples into their respective arrays
  cmr.ctj<-death0.ctj <- death1to4.ctj <- deathu5.ctj <- array(NA, c(C, nyears, nsim))
  ARR.year1.year6.cj <- ARR.year1.year2.cj <- ARR.year3.year4.cj <- ARR.year5.year6.cj <-required.ARR.cj <-
    ARR.IMR.year1.year6.cj <- ARR.IMR.year1.year2.cj <- ARR.IMR.year3.year4.cj <- ARR.IMR.year5.year6.cj <-
    ARR.CMR.year1.year6.cj <- ARR.CMR.year1.year2.cj <- ARR.CMR.year3.year4.cj <- ARR.CMR.year5.year6.cj <-changeinARR.cj <-
    decline.year1.year6.cj <- decline.year1.year2.cj <- decline.year3.year4.cj <- decline.year5.year6.cj <- array(NA, c(C, nsim))

  for (j in 1:nsim) {
    load(file.path(output.dir.samples, paste0("cmr.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("death0.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("death1to4.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("deathu5.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.year1.year6.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.year1.year2.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.year3.year4.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.year5.year6.c_", j, ".rda")))

    load(file.path(output.dir.samples, paste0("ARR.IMR.year1.year6.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.IMR.year1.year2.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.IMR.year3.year4.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.IMR.year5.year6.c_", j, ".rda")))

    load(file.path(output.dir.samples, paste0("ARR.CMR.year1.year6.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.CMR.year1.year2.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.CMR.year3.year4.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.CMR.year5.year6.c_", j, ".rda")))

    load(file.path(output.dir.samples, paste0("required.ARR.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("changeinARR.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("decline.year1.year6.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("decline.year1.year2.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("decline.year3.year4.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("decline.year5.year6.c_", j, ".rda")))
    cmr.ctj[, , j] <- cmr.ct
    death0.ctj[, , j] <- death0.ct
    death1to4.ctj[, , j] <- death1to4.ct
    deathu5.ctj[, , j] <- deathu5.ct
    ARR.year1.year6.cj[, j] <- ARR.year1.year6.c
    ARR.year1.year2.cj[, j] <- ARR.year1.year2.c
    ARR.year3.year4.cj[, j] <- ARR.year3.year4.c
    ARR.year5.year6.cj[, j] <- ARR.year5.year6.c
    required.ARR.cj[, j] <- required.ARR.c
    changeinARR.cj[, j] <- changeinARR.c
    decline.year1.year6.cj[, j] <- decline.year1.year6.c
    decline.year1.year2.cj[, j] <- decline.year1.year2.c
    decline.year3.year4.cj[, j] <- decline.year3.year4.c
    decline.year5.year6.cj[, j] <- decline.year5.year6.c
  }
  # save combined results
  save(cmr.ctj, file = file.path(output.dir.samplescombined, "cmr.ctj.rda"))
  save(death0.ctj, file = file.path(output.dir.samplescombined, "death0.ctj.rda"))
  save(death1to4.ctj, file = file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  save(deathu5.ctj, file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  save(ARR.year1.year6.cj, file = file.path(output.dir.samplescombined, "ARR.year1.year6.cj.rda"))
  save(ARR.year1.year2.cj, file = file.path(output.dir.samplescombined, "ARR.year1.year2.cj.rda"))
  save(ARR.year3.year4.cj, file = file.path(output.dir.samplescombined, "ARR.year3.year4.cj.rda"))
  save(ARR.year5.year6.cj, file = file.path(output.dir.samplescombined, "ARR.year5.year6.cj.rda"))
  save(required.ARR.cj, file = file.path(output.dir.samplescombined, "required.ARR.cj.rda"))
  save(changeinARR.cj, file = file.path(output.dir.samplescombined, "changeinARR.cj.rda"))
  save(decline.year1.year6.cj, file = file.path(output.dir.samplescombined, "decline.year1.year6.cj.rda"))
  save(decline.year1.year2.cj, file = file.path(output.dir.samplescombined, "decline.year1.year2.cj.rda"))
  save(decline.year3.year4.cj, file = file.path(output.dir.samplescombined, "decline.year3.year4.cj.rda"))
  save(decline.year5.year6.cj, file = file.path(output.dir.samplescombined, "decline.year5.year6.cj.rda"))
  save(info, file = file.path(output.dir.samplescombined, "info.rda"))
  # delete samples
  unlink(file.path(output.dir.samples, paste0("cmr.ct_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("death0.ct_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("death1to4.ct_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("deathu5.ct_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.year1.year6.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.year1.year2.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.year3.year4.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.year5.year6.c_", 1:nsim, ".rda")))

  unlink(file.path(output.dir.samples, paste0("ARR.IMR.year1.year6.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.IMR.year1.year2.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.IMR.year3.year4.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.IMR.year5.year6.c_", 1:nsim, ".rda")))

  unlink(file.path(output.dir.samples, paste0("ARR.CMR.year1.year6.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.CMR.year1.year2.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.CMR.year3.year4.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.CMR.year5.year6.c_", 1:nsim, ".rda")))

  unlink(file.path(output.dir.samples, paste0("required.ARR.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("changeinARR.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("decline.year1.year6.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("decline.year1.year2.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("decline.year3.year4.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("decline.year5.year6.c_", 1:nsim, ".rda")))
  #----------------------------------------------------------------------
  # output country summaries
  u5mr.qct <- apply(u5mr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qct <- apply(imr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  cmr.qct <- apply(cmr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.qct <- apply(deathu5.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.qct <- apply(death0.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death1to4.qct <- apply(death1to4.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  u5mr.ui <- imr.ui <- cmr.ui<-deathu5.ui <- death0.ui <- death1to4.ui<- NULL
  for (c in 1:C) {
    u5mr.ui <- rbind(u5mr.ui, u5mr.qct[, c, ])
    imr.ui <- rbind(imr.ui, imr.qct[, c, ])
    cmr.ui <- rbind(cmr.ui, cmr.qct[, c, ])
    deathu5.ui <- rbind(deathu5.ui, deathu5.qct[, c, ])
    death0.ui <- rbind(death0.ui, death0.qct[, c, ])
    death1to4.ui<-rbind(death1to4.ui, death1to4.qct[, c, ])
  }
  # output to .csv
  colnames(u5mr.ui) <- paste0("U5MR ", est.years.floor)
  colnames(imr.ui) <- paste0("IMR ", est.years.floor)
  colnames(cmr.ui) <- paste0("CMR ", est.years.floor)
  colnames(deathu5.ui) <- paste0("Under-five deaths ", est.years.floor)
  colnames(death0.ui) <- paste0("Infant deaths ", est.years.floor)
  colnames(death1to4.ui) <- paste0("Child deaths ", est.years.floor)
  country.info.output <- matrix(rep(unlist(country.info), each = 3), C*3, ncol(country.info))
  colnames(country.info.output) <- colnames(country.info)
  if (nsim == 1) {
    select.rows <- seq(1, nrow(u5mr.ui), 3)+1
  } else {
    select.rows <- seq(1, nrow(u5mr.ui), 1)
  }
  write.csv(cbind(country.info.output,
                  rep(c("Lower", "Median", "Upper"), C),
                  u5mr.ui, imr.ui,cmr.ui,
                  roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0),roundoff(death1to4.ui, digits = 0))[select.rows, ],
            file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
            row.names = F, na = "")
  #----------------------------------------------------------------------
  # output country summaries - ARR
  # ARR.year1.year6.ui <- apply(ARR.year1.year6.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.year1.year2.ui <- apply(ARR.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.year3.year4.ui <- apply(ARR.year3.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.year5.year6.ui <- apply(ARR.year5.year6.cj, 1, quantile, probs = percentiles, na.rm = T)
  #
  # ARR.IMR.year1.year6.ui <- apply(ARR.IMR.year1.year6.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.IMR.year1.year2.ui <- apply(ARR.IMR.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.IMR.year3.year4.ui <- apply(ARR.IMR.year3.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.IMR.year5.year6.ui <- apply(ARR.IMR.year5.year6.cj, 1, quantile, probs = percentiles, na.rm = T)
  #
  # ARR.CMR.year1.year6.ui <- apply(ARR.CMR.year1.year6.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.CMR.year1.year2.ui <- apply(ARR.CMR.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.CMR.year3.year4.ui <- apply(ARR.CMR.year3.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.CMR.year5.year6.ui <- apply(ARR.CMR.year5.year6.cj, 1, quantile, probs = percentiles, na.rm = T)
  #
  # required.ARR.ui <- apply(required.ARR.cj, 1, quantile, probs = percentiles, na.rm = T)
  # changeinARR.ui <- apply(changeinARR.cj, 1, quantile, probs = percentiles, na.rm = T)
  # decline.year1.year6.ui <- apply(decline.year1.year6.cj, 1, quantile, probs = percentiles, na.rm = T)
  # decline.year1.year2.ui <- apply(decline.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
  # decline.year3.year4.ui <- apply(decline.year3.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  # decline.year5.year6.ui <- apply(decline.year5.year6.cj, 1, quantile, probs = percentiles, na.rm = T)
  # country.RoDs.ui <- cbind(t(ARR.year1.year6.ui), t(ARR.year1.year2.ui), t(ARR.year3.year4.ui),t(ARR.year5.year6.ui),
  #                          t(ARR.IMR.year1.year6.ui), t(ARR.IMR.year1.year2.ui), t(ARR.IMR.year3.year4.ui),t(ARR.IMR.year5.year6.ui),
  #                          t(ARR.CMR.year1.year6.ui), t(ARR.CMR.year1.year2.ui), t(ARR.CMR.year3.year4.ui),t(ARR.CMR.year1.year6.ui),
  #                          t(required.ARR.ui), t(changeinARR.ui), t(decline.year1.year6.ui),
  #                          t(decline.year1.year2.ui), t(decline.year3.year4.ui),t(decline.year5.year6.ui))
  # # output to .csv
  # ui.colnames <- c(" lower bound", " median", " upper bound")
  # colnames(country.RoDs.ui) <- c(paste0("ARR.U5MR.", year1-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("ARR.U5MR.", year1-0.5, "-", year2-0.5, ui.colnames),
  #                                paste0("ARR.U5MR.", year3-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("ARR.U5MR.", year5-0.5, "-", year6-0.5, ui.colnames),
  #
  #                                paste0("ARR.IMR.", year1-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("ARR.IMR.", year1-0.5, "-", year2-0.5, ui.colnames),
  #                                paste0("ARR.IMR.", year3-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("ARR.IMR.", year5-0.5, "-", year6-0.5, ui.colnames),
  #
  #                                paste0("ARR.CMR.", year1-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("ARR.CMR.", year1-0.5, "-", year2-0.5, ui.colnames),
  #                                paste0("ARR.CMR.", year3-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("ARR.CMR.", year5-0.5, "-", year6-0.5, ui.colnames),
  #
  #                                paste0("Required ARR U5MR", ui.colnames),
  #                                paste0("Change in ARR U5MR", ui.colnames),
  #                                paste0("Percentage decline U5MR", year1-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("Percentage decline U5MR", year1-0.5, "-", year2-0.5, ui.colnames),
  #                                paste0("Percentage decline U5MR", year3-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("Percentage decline U5MR", year5-0.5, "-", year6-0.5, ui.colnames))
  # if (nsim == 1)
  #   country.RoDs.ui <- country.RoDs.ui[, !grepl("bound", colnames(country.RoDs.ui))]
  # write.csv(cbind(country.info, country.RoDs.ui),
  #           file = file.path(output.dir, "Rates of Decline_Country Summary.csv"),
  #           row.names = FALSE, na = "")
}
#----------------------------------------------------------------------
CalculateWorldDeaths <- function(
  output.dir.samplescombined,
  output.dir,
  percentiles,
  ndigits
) {
  load(file.path(output.dir.samplescombined, "info.rda"))
  list2env(info, envir = environment())
  load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
  load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  nsim <- dim(deathu5.ctj)[3]

  # Note: w stands for w, and w = 1
  death0.wtj <- death1to4.wtj <- deathu5.wtj <-
    death0.all.wtj <- death1to4.all.wtj <- deathu5.all.wtj <-
    M0.wtj <- M1to4.wtj <- q0.wtj <- q1to4.wtj <- q5.wtj <-
    u5mr.wtj <- imr.wtj <- array(data = NA, c(1, nyears, nsim))
  pop0.wt <- pop1to4.wt <- pop0.orig.wt <- pop1to4.orig.wt <- popu5.orig.wt <-
    coverage0.wt <- coverageu5.wt <- matrix(NA, 1, nyears)
  
  # OPTIMIZATION: Calculate population coverage once (not per trajectory)
  for (i in 1:nyears) {
    # Check that population coverage > 50%
    pop0.wt[, i] <- sum(pop0.ct[, i])
    pop1to4.wt[, i] <- sum(pop1to4.ct[, i])
    pop0.orig.wt[, i] <- sum(pop0.orig.ct[, i])
    pop1to4.orig.wt[, i] <- sum(pop1to4.orig.ct[, i])
    popu5.orig.wt[, i] <- pop0.orig.wt[, i] + pop1to4.orig.wt[, i]
    coverage0.wt[, i] <- pop0.wt[, i]/pop0.orig.wt[, i]
    coverageu5.wt[, i] <- (pop0.wt[, i] + pop1to4.wt[, i])/(popu5.orig.wt[, i])
  }
  
  for (j in 1:nsim) {
    # OPTIMIZATION: Vectorize death summing (1.5-2x speedup)
    death0.ct_j <- death0.ctj[,,j]
    death1to4.ct_j <- death1to4.ctj[,,j]
    deathu5.ct_j <- deathu5.ctj[,,j]
    
    death0.wtj[,,j] <- matrix(colSums(death0.ct_j, na.rm=T), nrow=1)
    death1to4.wtj[,,j] <- matrix(colSums(death1to4.ct_j, na.rm=T), nrow=1)
    deathu5.wtj[,,j] <- matrix(colSums(deathu5.ct_j, na.rm=T), nrow=1)
    
    # Calculate mortality rates
    for (i in 1:nyears) {
      M0.wtj[, i, j] <- death0.wtj[, i, j]/pop0.wt[,i]
      M1to4.wtj[, i, j] <- death1to4.wtj[, i, j]/pop1to4.wt[,i]

      #bmedit q0.wtj[, i, j] <- M0.wtj[, i, j]/(1+(1-ifelse(mean(a0.c, na.rm = T) < 0.2, 0.1, 0.3))*M0.wtj[, i, j])
      q0.wtj[, i, j] <- 5*M0.wtj[, i, j]/(1+(5-5*mean(a0.c))*M0.wtj[, i, j])
      # bmedit q1to4.wtj[, i, j] <- 4*M1to4.wtj[, i, j]/(1+(4-4*mean(a1to4.c, na.rm = T))*M1to4.wtj[, i, j])
      q1to4.wtj[, i, j] <- 5*M1to4.wtj[, i, j]/(1+(5-5*mean(a1to4.c, na.rm = T))*M1to4.wtj[, i, j])

      q5.wtj[, i, j] <- 1-(1-q0.wtj[, i, j])*(1-q1to4.wtj[, i, j])
      death0.all.wtj[, i, j] <- M0.wtj[, i, j]*pop0.orig.wt[,i]
      death1to4.all.wtj[, i, j] <- M1to4.wtj[, i, j]*pop1to4.orig.wt[,i]
      deathu5.all.wtj[, i, j] <- death0.all.wtj[, i, j] + death1to4.all.wtj[, i, j]
    }
  }
  u5mr.wtj <- q5.wtj*1000
  imr.wtj <- q0.wtj*1000
  cmr.wtj <- q1to4.wtj*1000

  # world summary
  u5mr.qwt <- apply(u5mr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qwt <- apply(imr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  cmr.qwt <- apply(cmr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.all.qwt <- apply(deathu5.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.all.qwt <- apply(death0.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death1to4.all.qwt <- apply(death1to4.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  # NA if coverage < 0.5
  for (q in 1:length(percentiles)) {
    u5mr.qwt[q, , ][coverageu5.wt < 0.5] <- NA
    imr.qwt[q, , ][coverage0.wt < 0.5] <- NA
    cmr.qwt[q, , ][coverageu5.wt < 0.5] <- NA
    deathu5.all.qwt[q, , ][coverageu5.wt < 0.5] <- NA
    death0.all.qwt[q, , ][coverage0.wt < 0.5] <- NA
    death1to4.all.qwt[q, , ][coverage0.wt < 0.5] <- NA
  }
  # world summary
  res.world <- cbind(est.years.floor,
                     roundoff(t(popu5.orig.wt), digits = 0),
                     roundoff(t(pop0.orig.wt), digits = 0),
                     roundoff(t(coverageu5.wt)*100, digits = 1),
                     roundoff(t(coverage0.wt)*100, digits = 1),
                     t(u5mr.qwt[, 1, ]),
                     t(imr.qwt[, 1, ]),
                     t(cmr.qwt[, 1, ]),
                     roundoff(t(deathu5.all.qwt[, 1, ]), digits = 0),
                     roundoff(t(death0.all.qwt[, 1, ]), digits = 0),
                     roundoff(t(death1to4.all.qwt[, 1, ]), digits = 0))
  ui.colnames <- c(" lower bound", " median", " upper bound")
  colnames(res.world) <- c("Year", "Under-five population", "Infant population",
                           "Population coverage (under 5)",
                           "Population coverage (age 0)",
                           paste0("U5MR", ui.colnames),
                           paste0("IMR", ui.colnames),
                           paste0("CMR", ui.colnames),
                           paste0("Under-five deaths", ui.colnames),
                           paste0("Infant deaths", ui.colnames),
                           paste0("Child deaths", ui.colnames))
  save(res.world, file = file.path(output.dir.samplescombined, "res.world.rda"))
  if (nsim == 1) {
    res.world <- res.world[, !grepl("bound", colnames(res.world))]
  }
  write.csv(res.world, file = file.path(output.dir, "Rates & Deaths_World.csv"),
            row.names = F, na = "")
  # save all quantities # change JR, 26 Aug 2013
  save(u5mr.wtj, file = file.path(output.dir.samplescombined, "u5mr.wtj.rda"))
  save(imr.wtj, file = file.path(output.dir.samplescombined, "imr.wtj.rda"))
  save(cmr.wtj, file = file.path(output.dir.samplescombined, "cmr.wtj.rda"))
  save(deathu5.all.wtj, file = file.path(output.dir.samplescombined, "deathu5.all.wtj.rda"))
  save(death0.all.wtj, file = file.path(output.dir.samplescombined, "death0.all.wtj.rda"))
  save(death1to4.all.wtj, file = file.path(output.dir.samplescombined, "death1to4.all.wtj.rda"))
  save(pop0.wt, file = file.path(output.dir.samplescombined, "pop0.wt.rda"))
  save(pop1to4.wt, file = file.path(output.dir.samplescombined, "pop1to4.wt.rda"))
  save(pop0.orig.wt, file = file.path(output.dir.samplescombined, "pop0.orig.wt.rda"))
  save(pop1to4.orig.wt, file = file.path(output.dir.samplescombined, "pop1to4.orig.wt.rda"))
  save(popu5.orig.wt, file = file.path(output.dir.samplescombined, "popu5.orig.wt.rda"))
  save(coverage0.wt, file = file.path(output.dir.samplescombined, "coverage0.wt.rda"))
  save(coverageu5.wt, file = file.path(output.dir.samplescombined, "coverageu5.wt.rda"))

  # round off to 1 d.p. before calculation (for median only)
  if(FALSE){#bmedit20180307
  if (dim(u5mr.wtj)[3] == 1) { # change JR, 26 Aug 2013
    u5mr.wtj <- u5mr.wtj
    imr.wtj <- imr.wtj
  }
  }
  # world summary - rates of decline
  # ARR.year1.year6.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                   year.start = year1, year.end = year6)
  # ARR.year1.year2.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                   year.start = year1, year.end = year2)
  # ARR.year3.year4.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                   year.start = year3, year.end = year4)
  # ARR.year5.year6.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                   year.start = year5, year.end = year6)
  #
  # ARR.IMR.year1.year6.j <- CalculateARR(u5mr = imr.wtj[1, , ], years = est.years,
  #                                   year.start = year1, year.end = year6)
  # ARR.IMR.year1.year2.j <- CalculateARR(u5mr = imr.wtj[1, , ], years = est.years,
  #                                   year.start = year1, year.end = year2)
  # ARR.IMR.year3.year4.j <- CalculateARR(u5mr = imr.wtj[1, , ], years = est.years,
  #                                   year.start = year3, year.end = year4)
  # ARR.IMR.year5.year6.j <- CalculateARR(u5mr = imr.wtj[1, , ], years = est.years,
  #                                   year.start = year5, year.end = year6)
  #
  # ARR.CMR.year1.year6.j <- CalculateARR(u5mr = cmr.wtj[1, , ], years = est.years,
  #                                       year.start = year1, year.end = year6)
  # ARR.CMR.year1.year2.j <- CalculateARR(u5mr = cmr.wtj[1, , ], years = est.years,
  #                                       year.start = year1, year.end = year2)
  # ARR.CMR.year3.year4.j <- CalculateARR(u5mr = cmr.wtj[1, , ], years = est.years,
  #                                       year.start = year3, year.end = year4)
  # ARR.CMR.year5.year6.j <- CalculateARR(u5mr = cmr.wtj[1, , ], years = est.years,
  #                                       year.start = year5, year.end = year6)
  #
  # required.ARR.j <- ifelse(year6 < year.target,
  #                          1/(year.target-year6)*
  #                            log(u5mr.wtj[1, est.years == year1, ]*factor.target/
  #                                  u5mr.wtj[1, est.years == year6, ])*-100, NA)
  # changeinARR.j <- ARR.year1.year6.j - ARR.year5.year6.j
  # decline.year1.year6.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                           year.start = year1, year.end = year6)
  # decline.year1.year2.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                           year.start = year1, year.end = year2)
  # decline.year3.year4.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                           year.start = year3, year.end = year4)
  # decline.year5.year6.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                           year.start = year5, year.end = year6)
  # ARR.year1.year6.ui <- quantile(ARR.year1.year6.j, probs = percentiles)
  # ARR.year1.year2.ui <- quantile(ARR.year1.year2.j, probs = percentiles)
  # ARR.year3.year4.ui <- quantile(ARR.year3.year4.j, probs = percentiles)
  # ARR.year5.year6.ui <- quantile(ARR.year5.year6.j, probs = percentiles)
  #
  # ARR.IMR.year1.year6.ui <- quantile(ARR.IMR.year1.year6.j, probs = percentiles)
  # ARR.IMR.year1.year2.ui <- quantile(ARR.IMR.year1.year2.j, probs = percentiles)
  # ARR.IMR.year3.year4.ui <- quantile(ARR.IMR.year3.year4.j, probs = percentiles)
  # ARR.IMR.year5.year6.ui <- quantile(ARR.IMR.year5.year6.j, probs = percentiles)
  #
  # ARR.CMR.year1.year6.ui <- quantile(ARR.CMR.year1.year6.j, probs = percentiles)
  # ARR.CMR.year1.year2.ui <- quantile(ARR.CMR.year1.year2.j, probs = percentiles)
  # ARR.CMR.year3.year4.ui <- quantile(ARR.CMR.year3.year4.j, probs = percentiles)
  # ARR.CMR.year5.year6.ui <- quantile(ARR.CMR.year5.year6.j, probs = percentiles)
  #
  # # change JR, 20150605: set na.rm = TRUE if all required.ARR are NAs,
  # # indicating that year4 = year.target
  # required.ARR.ui <- quantile(required.ARR.j, probs = percentiles,
  #                             na.rm = all(is.na(required.ARR.j)))
  # changeinARR.ui <- quantile(changeinARR.j, probs = percentiles)
  # decline.year1.year6.ui <- quantile(decline.year1.year6.j, probs = percentiles)
  # decline.year1.year2.ui <- quantile(decline.year1.year2.j, probs = percentiles)
  # decline.year3.year4.ui <- quantile(decline.year3.year4.j, probs = percentiles)
  # decline.year5.year6.ui <- quantile(decline.year5.year6.j, probs = percentiles)
  # global.RoDs.ui <- rbind(c(ARR.year1.year6.ui, ARR.year1.year2.ui, ARR.year3.year4.ui,ARR.year5.year6.ui,
  #                           ARR.IMR.year1.year6.ui, ARR.IMR.year1.year2.ui, ARR.IMR.year3.year4.ui,ARR.IMR.year5.year6.ui,
  #                           ARR.CMR.year1.year6.ui, ARR.CMR.year1.year2.ui, ARR.CMR.year3.year4.ui,ARR.CMR.year5.year6.ui,
  #                           required.ARR.ui, changeinARR.ui, decline.year1.year6.ui,
  #                           decline.year1.year2.ui, decline.year3.year4.ui,decline.year5.year6.ui))
  # colnames(global.RoDs.ui) <- c(paste0("ARR.U5MR.", year1-0.5, "-", year6-0.5, ui.colnames),
  #                               paste0("ARR.U5MR.", year1-0.5, "-", year2-0.5, ui.colnames),
  #                               paste0("ARR.U5MR.", year3-0.5, "-", year4-0.5, ui.colnames),
  #                               paste0("ARR.U5MR.", year5-0.5, "-", year6-0.5, ui.colnames),
  #                               paste0("ARR.IMR.", year1-0.5, "-", year6-0.5, ui.colnames),
  #                               paste0("ARR.IMR.", year1-0.5, "-", year2-0.5, ui.colnames),
  #                               paste0("ARR.IMR.", year3-0.5, "-", year4-0.5, ui.colnames),
  #                               paste0("ARR.IMR.", year5-0.5, "-", year6-0.5, ui.colnames),
  #                               paste0("ARR.CMR.", year1-0.5, "-", year6-0.5, ui.colnames),
  #                               paste0("ARR.CMR.", year1-0.5, "-", year2-0.5, ui.colnames),
  #                               paste0("ARR.CMR.", year3-0.5, "-", year4-0.5, ui.colnames),
  #                               paste0("ARR.CMR.", year5-0.5, "-", year6-0.5, ui.colnames),
  #                               paste0("Required ARR U5MR", ui.colnames),
  #                               paste0("Change in ARR U5MR", ui.colnames),
  #                               paste0("Percentage decline U5MR", year1-0.5, "-", year6-0.5, ui.colnames),
  #                               paste0("Percentage decline U5MR", year1-0.5, "-", year2-0.5, ui.colnames),
  #                               paste0("Percentage decline U5MR", year3-0.5, "-", year4-0.5, ui.colnames),
  #                               paste0("Percentage decline U5MR", year5-0.5, "-", year6-0.5, ui.colnames))
  # save(global.RoDs.ui, file = file.path(output.dir.samplescombined, "global.RoDs.ui.rda"))
  # if (nsim == 1) {
  #   global.RoDs.ui <- global.RoDs.ui[, !grepl("bound", colnames(global.RoDs.ui))]
  #   global.RoDs.ui.output <- rbind(colnames(global.RoDs.ui), global.RoDs.ui)
  # } else {
  #   global.RoDs.ui.output <- cbind(data.frame(Region = "World"), global.RoDs.ui)
  # }
  # write.csv(global.RoDs.ui.output,
  #           file = file.path(output.dir, "Rates of Decline_World.csv"), row.names = F, na = "")
}
#----------------------------------------------------------------------
GetRegionalResults5_24 <- function(
  output.dir,
  output.dir.samples,
  output.dir.samplescombined,
  regions, regiontypes,
  filename,
  run.on.server,
  percentiles,
  ndigits,
  replace.rates.reg = NULL
) {
  cat(paste0("Generating output for ", filename, "...\n"))
  nregs <- length(regiontypes)
  regions[is.na(regions)] <- 0 # to remove NAs

  load(file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  nsim <- dim(deathu5.ctj)[3]

  if (run.on.server) {
    # calculate once to get population arrays, because multiple chains will be running at once for parallel computing
    CalculateRegionalDeaths5_24(j = 1, output.dir.samples = output.dir.samples,
                                output.dir.samplescombined = output.dir.samplescombined,
                                regions = regions, regiontypes = regiontypes, filename = filename)
    cat(paste0("Output generated for trajectory ", 1, " out of ", nsim,
               ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
    if (nsim > 1) {
      registerDoMC()
      foreach (j=2:nsim) %dopar% {
        CalculateRegionalDeaths5_24(j = j, output.dir.samples = output.dir.samples,
                                    output.dir.samplescombined = output.dir.samplescombined,
                                    regions = regions, regiontypes = regiontypes, filename = filename)
        cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                   ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
      }
    }
  } else {
    for (j in 1:nsim) {
      CalculateRegionalDeaths5_24(j = j, output.dir.samples = output.dir.samples,
                                  output.dir.samplescombined = output.dir.samplescombined,
                                  regions = regions, regiontypes = regiontypes, filename = filename)
      cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                 ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
    }
  }
  cat(paste0("Combining and outputting regional results...\n"))
  CombineAndOutputRegionalResults5_24(output.dir = output.dir,
                                      output.dir.samples = output.dir.samples,
                                      output.dir.samplescombined = output.dir.samplescombined,
                                      regiontypes = regiontypes,
                                      filename = filename,
                                      percentiles = percentiles,
                                      ndigits = ndigits,
                                      replace.rates.reg = replace.rates.reg)
}
#----------------------------------------------------------------------
CalculateRegionalDeaths5_24 <- function(
  j,
  output.dir.samples,
  output.dir.samplescombined,
  regions,
  regiontypes,
  filename
) {
  load(file.path(output.dir.samplescombined, "info.rda"))
  list2env(info, envir = environment())
  load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
  load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  nregs <- length(regiontypes)
  regions[is.na(regions)] <- 0

  # create separate directory for each region type
  output.dir.samples.region <- file.path(output.dir.samples, filename)
  dir.create(output.dir.samples.region, recursive = TRUE, showWarnings = FALSE)

  if (j == 1) { # calculate once
    pop0.rt <- pop1to4.rt <- pop0.orig.rt <- pop1to4.orig.rt <- popu5.orig.rt <-
      coverage0.rt <- coverageu5.rt <- matrix(NA, nregs, nyears)
  } else {
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop0.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.orig.rt.rda")))
  }
  M0.rt <- M1to4.rt <- q0.rt <- q1to4.rt <- q5.rt <-
    death0.rt <- death1to4.rt <- deathu5.rt <- death0.all.rt <- death1to4.all.rt <- deathu5.all.rt <-
    matrix(NA, nregs, nyears)
  for (r in 1:nregs) {
    if (filename %in% c("UNICEFReportRegion","UNICEFProgRegion", "MDGRegion", "WBRegion", "AURegion","UNPDRegion",
                        "OICRegion", "M49Region","SDGSimpleRegion", "EAPRORegion")) {
      reg.num <- ChooseRegion(region = regiontypes[r], regiontype = filename)
      select.reg <- (1:nrow(regions))[regions[, is.element(colnames(regions),
                                                           paste0(filename, reg.num))] == regiontypes[r]]
    } else if (filename %in% c("WHORegion", "CountdownCountries", "ECAAfricaRegion",
                                "FragileCountries2013", "FragileCountries2014", "FragileCountries2015",
                               "USAIDCountries","HAC","Conflict", "GBDRegion","AfricanEconomicCommunityRegion",
                               "SPhumanitarianRegion", "SPhighburdenRegion", # YL added 2022.02
                               "AdhocCountries"
                               )) {
      # Handle both vector (single column) and data frame (multiple columns) cases
      if (is.data.frame(regions) || is.matrix(regions)) {
        # Multi-column case: find rows where any column matches the region name
        select.reg <- which(apply(regions, 1, function(row) any(row == regiontypes[r])))
      } else {
        # Single column case: traditional vector matching
        select.reg <- (1:length(regions))[regions == regiontypes[r]]
      }
    }
    if (j == 1) { # calculate the first time
      for (i in 1:nyears) {
        # check that population coverage > 50% per region
        pop0.rt[r, i] <- sum(pop0.ct[select.reg,i])
        pop1to4.rt[r, i] <- sum(pop1to4.ct[select.reg,i])
        pop0.orig.rt[r, i] <- sum(pop0.orig.ct[select.reg,i])
        pop1to4.orig.rt[r, i] <- sum(pop1to4.orig.ct[select.reg,i])
        popu5.orig.rt[r, i] <- pop0.orig.rt[r, i] + pop1to4.orig.rt[r, i]
        coverage0.rt[r, i] <- pop0.rt[r, i]/pop0.orig.rt[r, i]
        coverageu5.rt[r, i] <- (pop0.rt[r, i] + pop1to4.rt[r, i])/(popu5.orig.rt[r, i])
      }
      # save the first time
      if(!dir.exists(output.dir.samplescombined)) dir.create(output.dir.samplescombined, recursive = TRUE)
      save(pop0.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.rt.rda")))
      save(pop1to4.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.rt.rda")))
      save(popu5.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
      save(pop0.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
      save(pop1to4.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.orig.rt.rda")))
      save(coverageu5.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_coverageu5.rt.rda")))
      save(coverage0.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_coverage0.rt.rda")))
    }
    for (i in 1:nyears) {
      # calculate values
      death0.rt[r, i] <- sum(death0.ctj[select.reg, i, j], na.rm = T)
      death1to4.rt[r, i] <- sum(death1to4.ctj[select.reg, i, j], na.rm = T)
      deathu5.rt[r, i] <- sum(deathu5.ctj[select.reg, i, j], na.rm = T)
      M0.rt[r, i] <- death0.rt[r, i]/pop0.rt[r, i]
      M1to4.rt[r, i] <- death1to4.rt[r, i]/pop1to4.rt[r, i]

      #bmedit q0.rt[r, i] <- M0.rt[r, i]/(1+(1-ifelse(mean(a0.c[select.reg], na.rm = T) < 0.2, 0.1, 0.3))*M0.rt[r, i])
      q0.rt[r, i] <- 5*M0.rt[r, i]/(1+(5-5*0.5)*M0.rt[r, i])
      #bmedit q1to4.rt[r, i] <- 4*M1to4.rt[r, i]/(1+(4-4*mean(a1to4.c[select.reg], na.rm = T))*M1to4.rt[r, i])
      q1to4.rt[r, i] <- 5*M1to4.rt[r, i]/(1+(5-5*mean(a1to4.c[select.reg], na.rm = T))*M1to4.rt[r, i])

      q5.rt[r, i] <- 1-(1-q0.rt[r, i])*(1-q1to4.rt[r, i])
      death0.all.rt[r, i] <- M0.rt[r, i]*pop0.orig.rt[r, i]
      death1to4.all.rt[r, i] <- M1to4.rt[r, i]*pop1to4.orig.rt[r, i]
      deathu5.all.rt[r, i] <- death0.all.rt[r, i] + death1to4.all.rt[r, i]
    }
  }
  SaveRegionalBundle5_24(
    bundle = list(
      q0.rt = q0.rt,
      q1to4.rt = q1to4.rt,
      q5.rt = q5.rt,
      death0.all.rt = death0.all.rt,
      death1to4.all.rt = death1to4.all.rt,
      deathu5.all.rt = deathu5.all.rt
    ),
    output.dir.samples.region = output.dir.samples.region,
    j = j
  )
}
#----------------------------------------------------------------------
CombineAndOutputRegionalResults5_24 <- function(
  output.dir,
  output.dir.samples,
  output.dir.samplescombined,
  regiontypes,
  filename,
  percentiles,
  ndigits,
  replace.rates.reg = NULL
) {
  # load one file first to get dimensions
  load(file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  nsim <- dim(deathu5.ctj)[3]
  nregs <- length(regiontypes)
  load(file.path(output.dir.samplescombined, "info.rda"))
  list2env(info, envir = environment())

  # load world results
  load(file = file.path(output.dir.samplescombined, "res.world.rda"))
  # load(file = file.path(output.dir.samplescombined, "global.RoDs.ui.rda"))

  # create separate directory for each region type
  output.dir.samples.region <- file.path(output.dir.samples, filename)

  death0.rtj <- death1to4.rtj <- deathu5.rtj <- death0.all.rtj <- death1to4.all.rtj <- deathu5.all.rtj <-
    M0.rtj <- M1to4.rtj <- q0.rtj <- q1to4.rtj <- q5.rtj <- u5mr.rtj <- imr.rtj <-
    array(NA, c(nregs, nyears, nsim))
  pop0.rt <- pop1to4.rt <- pop0.orig.rt <- pop1to4.orig.rt <- popu5.orig.rt <-
    coverage0.rt <- coverageu5.rt <- matrix(NA, nregs, nyears)
  for (j in 1:nsim) {
    load(file = file.path(output.dir.samples.region, paste0("q0.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("q1to4.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("q5.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("death0.all.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("death1to4.all.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("deathu5.all.rt_", j, ".rda")))
    q0.rtj[ , , j] <- q0.rt
    q1to4.rtj[ , , j] <- q1to4.rt
    q5.rtj[ , , j] <- q5.rt
    death0.all.rtj[ , , j] <- death0.all.rt
    death1to4.all.rtj[ , , j] <- death1to4.all.rt
    deathu5.all.rtj[ , , j] <- deathu5.all.rt
  }
  u5mr.rtj <- q5.rtj*1000
  imr.rtj <- q0.rtj*1000
  cmr.rtj <- q1to4.rtj*1000
  # save the samples
  save(u5mr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_u5mr.rtj.rda")))
  save(imr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_imr.rtj.rda")))
  save(cmr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_cmr.rtj.rda")))
  save(deathu5.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_deathu5.all.rtj.rda")))
  save(death0.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_death0.all.rtj.rda")))
  save(death1to4.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_death1to4.all.rtj.rda")))
  # delete samples
  unlink(file.path(output.dir.samples.region, paste0("q0.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("q1to4.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("q5.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("death0.all.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("death1to4.all.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("deathu5.all.rt_", 1:nsim, ".rda")))

  # load population and coverage info
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_coverageu5.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_coverage0.rt.rda"))  )

  # regional summaries
  u5mr.qrt <- apply(u5mr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qrt <- apply(imr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  cmr.qrt <- apply(cmr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.all.qrt <- apply(deathu5.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.all.qrt <- apply(death0.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death1to4.all.qrt <- apply(death1to4.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  # NA if coverage < 0.5
  for (q in 1:length(percentiles)) {
    u5mr.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    imr.qrt[q, , ][coverage0.rt < 0.5] <- NA
    cmr.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    deathu5.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    death0.all.qrt[q, , ][coverage0.rt < 0.5] <- NA
    death1to4.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
  }
  # regional summary
  res.year <- NULL
  for (i in 1:nyears) {
    res.year <- rbind(res.year,
                      rbind(
                        cbind(est.years.floor[i],
                              roundoff(popu5.orig.rt[,i], digits = 0),
                              roundoff(pop0.orig.rt[,i], digits = 0),
                              roundoff(coverageu5.rt[,i]*100, digits = 2),
                              roundoff(coverage0.rt[,i]*100, digits = 2),
                              t(u5mr.qrt[,,i]),
                              t(imr.qrt[,,i]),
                              t(cmr.qrt[,,i]),
                              roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                              roundoff(t(death0.all.qrt[,,i]), digits = 0),
                              roundoff(t(death1to4.all.qrt[,,i]), digits = 0)),
                        res.world[res.world[, 1] == est.years.floor[i], ]
                      ))
  }
  res.region <- cbind(rep(c(regiontypes, "World"), nyears), res.year)
  # output to .csv
  ui.colnames <- c(" lower bound", " median", " upper bound")
  colnames(res.region) <- c("Region", "Year",
                            "Under-five population", "Infant population",
                            "Population coverage (under 5)", "Population coverage (age 0)",
                            paste0("U5MR", ui.colnames),
                            paste0("IMR", ui.colnames),
                            paste0("CMR", ui.colnames),
                            paste0("Under-five deaths", ui.colnames),
                            paste0("Infant deaths", ui.colnames),
                            paste0("Child deaths", ui.colnames))
  if (nsim == 1)
    res.region <- res.region[, !grepl("bound", colnames(res.region))]
  write.csv(res.region, file = file.path(output.dir, paste0("Rates & Deaths_", filename, ".csv")),
            row.names = F, na = "")

  # round off to 1 d.p. before calculation (for median only)
  if(FALSE){#bmedit20180307
  if (dim(u5mr.rtj)[3] == 1) { # change JR, 26 Aug 2013
    u5mr.rtj <- u5mr.rtj
    imr.rtj <- imr.rtj
    cmr.rtj <- cmr.rtj
  }
  }
  # regional summary - rates of decline
  #  region.RoDs.ui <- NULL
  # for (r in 1:nregs) {
  #   ARR.year1.year6.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                     year.start = year1, year.end = year6)
  #   ARR.year1.year2.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                     year.start = year1, year.end = year2)
  #   ARR.year3.year4.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                     year.start = year3, year.end = year4)
  #   ARR.year5.year6.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                     year.start = year5, year.end = year6)
  #
  #   ARR.IMR.year1.year6.j <- CalculateARR(u5mr = imr.rtj[r, , ], years = est.years,
  #                                     year.start = year1, year.end = year6)
  #   ARR.IMR.year1.year2.j <- CalculateARR(u5mr = imr.rtj[r, , ], years = est.years,
  #                                     year.start = year1, year.end = year2)
  #   ARR.IMR.year3.year4.j <- CalculateARR(u5mr = imr.rtj[r, , ], years = est.years,
  #                                     year.start = year3, year.end = year4)
  #   ARR.IMR.year5.year6.j <- CalculateARR(u5mr = imr.rtj[r, , ], years = est.years,
  #                                     year.start = year5, year.end = year6)
  #
  #   ARR.CMR.year1.year6.j <- CalculateARR(u5mr = cmr.rtj[r, , ], years = est.years,
  #                                         year.start = year1, year.end = year6)
  #   ARR.CMR.year1.year2.j <- CalculateARR(u5mr = cmr.rtj[r, , ], years = est.years,
  #                                     year.start = year1, year.end = year2)
  #   ARR.CMR.year3.year4.j <- CalculateARR(u5mr = cmr.rtj[r, , ], years = est.years,
  #                                     year.start = year3, year.end = year4)
  #   ARR.CMR.year5.year6.j <- CalculateARR(u5mr = cmr.rtj[r, , ], years = est.years,
  #                                     year.start = year5, year.end = year6)
  #   required.ARR.j <- ifelse(year4 < year.target,
  #                            1/(year.target-year4)*
  #                              log(u5mr.rtj[r, est.years == year1, ]*factor.target/
  #                                    u5mr.rtj[1, est.years == year4, ])*-100, NA)
  #   changeinARR.j <- ARR.year1.year6.j - ARR.year5.year6.j
  #   decline.year1.year6.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                             year.start = year1, year.end = year6)
  #   decline.year1.year2.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                             year.start = year1, year.end = year2)
  #   decline.year3.year4.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                             year.start = year2, year.end = year4)
  #   decline.year5.year6.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                             year.start = year5, year.end = year6)
  #   ARR.year1.year6.ui <- quantile(ARR.year1.year6.j, probs = percentiles)
  #   ARR.year1.year2.ui <- quantile(ARR.year1.year2.j, probs = percentiles)
  #   ARR.year3.year4.ui <- quantile(ARR.year3.year4.j, probs = percentiles)
  #   ARR.year5.year6.ui <- quantile(ARR.year5.year6.j, probs = percentiles)
  #
  #   ARR.IMR.year1.year6.ui <- quantile(ARR.IMR.year1.year6.j, probs = percentiles)
  #   ARR.IMR.year1.year2.ui <- quantile(ARR.IMR.year1.year2.j, probs = percentiles)
  #   ARR.IMR.year3.year4.ui <- quantile(ARR.IMR.year3.year4.j, probs = percentiles)
  #   ARR.IMR.year5.year6.ui <- quantile(ARR.IMR.year5.year6.j, probs = percentiles)
  #
  #   ARR.CMR.year1.year6.ui <- quantile(ARR.CMR.year1.year6.j, probs = percentiles)
  #   ARR.CMR.year1.year2.ui <- quantile(ARR.CMR.year1.year2.j, probs = percentiles)
  #   ARR.CMR.year3.year4.ui <- quantile(ARR.CMR.year3.year4.j, probs = percentiles)
  #   ARR.CMR.year5.year6.ui <- quantile(ARR.CMR.year5.year6.j, probs = percentiles)
  #
  #   # change JR, 20150605: set na.rm = TRUE if all required.ARR are NAs,
  #   # indicating that year4 = year.target
  #   required.ARR.ui <- quantile(required.ARR.j, probs = percentiles,
  #                               na.rm = all(is.na(required.ARR.j)))
  #   changeinARR.ui <- quantile(changeinARR.j, probs = percentiles)
  #   decline.year1.year6.ui <- quantile(decline.year1.year6.j, probs = percentiles)
  #   decline.year1.year2.ui <- quantile(decline.year1.year2.j, probs = percentiles)
  #   decline.year3.year4.ui <- quantile(decline.year3.year4.j, probs = percentiles)
  #   decline.year5.year6.ui <- quantile(decline.year5.year6.j, probs = percentiles)
  #   #bmedit to add decline in number of deaths
  #   # pcdeclinedeaths.1990.2016.region <- -100*(t(deathu5.all.qrt[,,length(1950:2016)]) - deathu5.all.qrt[,,length(1990:2016)])/deathu5.all.qrt[,,length(1990:2016)]
  #
  #   region.RoDs.ui <- rbind(region.RoDs.ui,
  #                           c(ARR.year1.year6.ui, ARR.year1.year2.ui, ARR.year3.year4.ui,ARR.year5.year6.ui,
  #                             ARR.IMR.year1.year6.ui, ARR.IMR.year1.year2.ui, ARR.IMR.year3.year4.ui,ARR.IMR.year5.year6.ui,
  #                             ARR.CMR.year1.year6.ui, ARR.CMR.year1.year2.ui, ARR.CMR.year3.year4.ui,ARR.CMR.year5.year6.ui,
  #                             required.ARR.ui, changeinARR.ui, decline.year1.year6.ui,
  #                             decline.year1.year2.ui, decline.year3.year4.ui,decline.year5.year6.ui))
  #
  # }
  # colnames(region.RoDs.ui) <- c(paste0("ARR.U5MR.", year1-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("ARR.U5MR.", year1-0.5, "-", year2-0.5, ui.colnames),
  #                                paste0("ARR.U5MR.", year3-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("ARR.U5MR.", year5-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("ARR.IMR.", year1-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("ARR.IMR.", year1-0.5, "-", year2-0.5, ui.colnames),
  #                                paste0("ARR.IMR.", year3-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("ARR.IMR.", year5-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("ARR.CMR.", year1-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("ARR.CMR.", year1-0.5, "-", year2-0.5, ui.colnames),
  #                                paste0("ARR.CMR.", year3-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("ARR.CMR.", year5-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("Required ARR", ui.colnames),
  #                                paste0("Change in ARR", ui.colnames),
  #                                paste0("Percentage decline ", year1-0.5, "-", year6-0.5, ui.colnames),
  #                                paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
  #                                paste0("Percentage decline ", year3-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("Percentage decline ", year5-0.5, "-", year6-0.5, ui.colnames))
  # region.RoDs <- data.frame(Region = c(regiontypes, "World"), rbind(region.RoDs.ui, global.RoDs.ui))
  # if (nsim == 1)
  #   region.RoDs <- region.RoDs[, !grepl("bound", colnames(region.RoDs))]
  # write.csv(region.RoDs, file = file.path(output.dir, paste0("Rates of Decline_", filename, ".csv")),
  #           row.names = F, na = "")
  cat(paste0("Output generated for ", filename, ".\n"))
}
