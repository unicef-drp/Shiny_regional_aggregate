# outputaggregates.R
# Jin Rou New, 2012-2013
# David Sharrow, 2019
# Updates
# 2020-03: added SDGRC
# 2021-02: added JHU, LiST_all, EAPRO
# 2021-11/2022-07: added FCS2021/22
# 2024-01: added FCSCountries (replace older FCS2021,22,23)
# 2024-02: added SDG2015 -- checking countries with data after 2015
# 2024-03: added UNICEFReportRegion_nohigh -- UNICEF regions without high income countries
# 2024-11: add AURECRegion -- Africa Union REC Regions
# 2026-01: added FragileCountries2025OECD (replacing 2022OECD)
# version 2.0
# check if country results have been run already, if so skip country agg
# check for missing trajectories when run in parallel
# version 3.0
# Please refer to "record of improvement.md" for details of improvements in version 3.0
# 

# What it does: Resolves a safe number of CPU cores to use for parallel work.
# Why it is needed: The aggregation pipeline runs on different machines, so core requests need validation and sensible defaults.
ResolveParallelCoresBWC <- function(parallel.cores = NULL) {
  detected.cores <- parallel::detectCores()
  if (is.na(detected.cores) || detected.cores < 1) {
    detected.cores <- 1L
  }
  
  if (is.null(parallel.cores)) {
    return(as.integer(max(1L, min(detected.cores - 2L, 50L))))
  }
  
  requested.cores <- suppressWarnings(as.integer(parallel.cores))
  if (is.na(requested.cores) || requested.cores < 1) {
    requested.cores <- 1L
  }
  as.integer(max(1L, min(requested.cores, detected.cores)))
}

# What it does: Returns elapsed wall-clock time for the current phase.
# Why it is needed: Timing is reused across stages for progress logs and runtime summaries.
GetPhaseElapsedBWC <- function(start.time) {
  proc.time()[["elapsed"]] - start.time
}

# What it does: Formats elapsed seconds into a readable hours string.
# Why it is needed: Long-running jobs need consistent, human-readable timing messages.
FormatRoutElapsedBWC <- function(elapsed) {
  elapsed <- suppressWarnings(as.numeric(elapsed))
  if (is.na(elapsed) || !is.finite(elapsed) || elapsed < 0) {
    elapsed <- 0
  }
  sprintf("%.1f hours", elapsed/3600)
}

# What it does: Chooses how many items to process before reporting progress.
# Why it is needed: Chunking keeps logs informative without adding too much progress-tracking overhead.
ResolveProgressChunkSizeBWC <- function(total, parallel.cores, target.updates = 20L, progress.every = NULL) {
  total <- suppressWarnings(as.integer(total))
  parallel.cores <- suppressWarnings(as.integer(parallel.cores))
  target.updates <- suppressWarnings(as.integer(target.updates))
  progress.every <- suppressWarnings(as.integer(progress.every))
  if (is.na(total) || total < 1L) {
    return(1L)
  }
  if (!is.na(progress.every) && progress.every >= 1L) {
    return(as.integer(min(total, progress.every)))
  }
  if (is.na(parallel.cores) || parallel.cores < 1L) {
    parallel.cores <- 1L
  }
  if (is.na(target.updates) || target.updates < 1L) {
    target.updates <- 1L
  }
  as.integer(min(total, max(parallel.cores, ceiling(total/target.updates))))
}

# What it does: Splits a total workload into index chunks.
# Why it is needed: The parallel and sequential loops use these chunks to batch work and progress updates.
BuildProgressChunksBWC <- function(total, chunk.size) {
  total <- suppressWarnings(as.integer(total))
  chunk.size <- suppressWarnings(as.integer(chunk.size))
  if (is.na(total) || total < 1L) {
    return(list())
  }
  if (is.na(chunk.size) || chunk.size < 1L) {
    chunk.size <- 1L
  }
  chunk.starts <- seq.int(1L, total, by = chunk.size)
  lapply(chunk.starts, function(start.idx) {
    seq.int(start.idx, min(total, start.idx + chunk.size - 1L))
  })
}

# What it does: Logs progress with counts, percent complete, elapsed time, and optional extra detail.
# Why it is needed: Regional and country runs can take a long time, so we need lightweight visibility into job status.
LogRoutProgressBWC <- function(stage, completed, total, start.time, extra = NULL) {
  completed <- suppressWarnings(as.integer(completed))
  total <- suppressWarnings(as.integer(total))
  if (is.na(total) || total < 1L) {
    total <- 1L
  }
  if (is.na(completed) || completed < 0L) {
    completed <- 0L
  }
  completed <- min(completed, total)
  elapsed <- GetPhaseElapsedBWC(start.time)
  pct <- 100 * completed/total
  log.line <- sprintf("[%s] %d/%d (%.1f%%) complete after %s",
                      stage, completed, total, pct, FormatRoutElapsedBWC(elapsed))
  if (!is.null(extra) && nzchar(extra)) {
    log.line <- paste0(log.line, " | ", extra)
  }
  message(log.line)
  try(flush(stderr()), silent = TRUE)
  invisible(NULL)
}

# What it does: Creates a text progress bar when there is measurable work to track.
# Why it is needed: Sequential loops benefit from immediate console feedback without duplicating setup logic.
CreateLoopProgressBarBWC <- function(total) {
  total <- suppressWarnings(as.integer(total))
  if (is.na(total) || total <= 0L) {
    return(NULL)
  }
  utils::txtProgressBar(min = 0L, max = total, style = 3)
}

# What it does: Updates an existing text progress bar to the latest position.
# Why it is needed: Centralizing the NULL-safe update avoids repeated checks inside loops.
UpdateLoopProgressBarBWC <- function(progress.bar, value) {
  if (is.null(progress.bar)) {
    return(invisible(NULL))
  }
  utils::setTxtProgressBar(progress.bar, value)
  invisible(NULL)
}

# What it does: Closes an existing text progress bar.
# Why it is needed: Progress bars should be cleaned up safely at the end of a run.
CloseLoopProgressBarBWC <- function(progress.bar) {
  if (is.null(progress.bar)) {
    return(invisible(NULL))
  }
  close(progress.bar)
  invisible(NULL)
}

# What it does: Precomputes year grids and weight matrices used in country life-table calculations.
# Why it is needed: Reusing this setup avoids rebuilding the same structures for every country and trajectory.
BuildCountryCalculationContextBWC <- function(nyears, years.start = 1950L, nn.exists = FALSE) {
  nyears <- suppressWarnings(as.integer(nyears))
  years.start <- suppressWarnings(as.integer(years.start))
  if (is.na(nyears) || nyears < 1L) {
    stop("nyears must be a positive integer")
  }
  if (is.na(years.start)) {
    years.start <- 1950L
  }
  
  years <- seq.int(years.start, length.out = nyears)
  weight.j.1 <- (53 - seq_len(52L))/52
  wgt.mat <- t(matrix(cbind(weight.j.1, 1 - weight.j.1), ncol = 10L, nrow = 52L))
  years.mat <- vapply(years, function(yr) seq(yr + 0.5, yr + 5, 0.5), numeric(10L))
  dimnames(years.mat) <- list(NULL, c("years.mat", rep("", max(0L, nyears - 1L))))
  wgt.mat.cache <- lapply(seq_len(nyears), function(n) {
    matrix(rep(wgt.mat, n), nrow = 10L, ncol = n * 52L)
  })
  
  wgt.nmr.mat <- wgt.nmr.mat.cache <- NULL
  if (isTRUE(nn.exists)) {
    weight.nmr.j.1 <- c(rep(1, 49), ((53 - (50:52))/4))
    wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1 - weight.nmr.j.1), ncol = 2L, nrow = 52L))
    wgt.nmr.mat.cache <- lapply(seq_len(nyears), function(n) {
      matrix(rep(wgt.nmr.mat, n), nrow = 2L, ncol = n * 52L)
    })
  }
  
  list(
    years = years,
    years.mat = years.mat,
    total.week.cols = nyears * 52L,
    wgt.mat = wgt.mat,
    wgt.mat.cache = wgt.mat.cache,
    wgt.nmr.mat = wgt.nmr.mat,
    wgt.nmr.mat.cache = wgt.nmr.mat.cache
  )
}

# What it does: Builds the weekly life-table objects for one country trajectory from rates and live births.
# Why it is needed: This is the core BWC step that turns mortality trajectories into death counts and cohort survival paths.
BuildCountryLifeTableBWC <- function(
    u5mr.row,
    imr.row,
    livebirths.row,
    country.ctx,
    nmr.row = NULL,
    replace.u5mr.row = NULL,
    replace.imr.row = NULL,
    replace.nmr.row = NULL,
    use.full.years = FALSE
) {
  years <- country.ctx$years
  if (isTRUE(use.full.years)) {
    year.pos.k <- seq_along(years)
  } else {
    year.pos.k <- which(!is.na(u5mr.row))
  }
  if (length(year.pos.k) < 1L) {
    return(NULL)
  }
  
  years.k <- years[year.pos.k]
  max.pos.k <- max(year.pos.k)
  year1.est.u5 <- years.k[1]
  imr.pos.k <- which(!is.na(imr.row))
  year1.est.u1 <- if (length(imr.pos.k) > 0L) years[imr.pos.k[1]] else NA_real_
  nn.exists <- !is.null(nmr.row)
  year1.est.nn <- NA_real_
  if (nn.exists) {
    nmr.pos.k <- which(!is.na(nmr.row))
    year1.est.nn <- if (length(nmr.pos.k) > 0L) years[nmr.pos.k[1]] else NA_real_
  }
  
  livebirth.pos.k <- if (isTRUE(use.full.years)) seq_along(years) else year.pos.k
  wpp.livebirths.k <- as.numeric(livebirths.row[livebirth.pos.k])
  bwc.vec.k <- rep(wpp.livebirths.k/52, each = 52L)
  
  n.k <- length(year.pos.k)
  nmx.mat.k <- matrix(NA_real_, nrow = 10L, ncol = n.k * 52L)
  nmr.mat.k <- NULL
  if (nn.exists) {
    nmr.mat.k <- matrix(NA_real_, nrow = 2L, ncol = n.k * 52L)
  }
  
  for (i in seq_len(n.k)) {
    pos.i <- year.pos.k[i]
    if ((pos.i + 5L) > max.pos.k) {
      select.idx <- year.pos.k[i:n.k]
      nmx.u1.i <- imr.row[select.idx]
      if (!is.null(replace.imr.row)) {
        missing.idx <- is.na(nmx.u1.i)
        if (any(missing.idx)) {
          nmx.u1.i[missing.idx] <- replace.imr.row[select.idx][missing.idx]
        }
      }
      nmx.u5.i <- u5mr.row[select.idx]
      if (!is.null(replace.u5mr.row)) {
        missing.idx <- is.na(nmx.u5.i)
        if (any(missing.idx)) {
          nmx.u5.i[missing.idx] <- replace.u5mr.row[select.idx][missing.idx]
        }
      }
    } else {
      select.idx <- pos.i:(pos.i + 5L)
      nmx.u1.i <- imr.row[select.idx]
      if (!is.null(replace.imr.row)) {
        missing.idx <- is.na(nmx.u1.i)
        if (any(missing.idx)) {
          nmx.u1.i[missing.idx] <- replace.imr.row[select.idx][missing.idx]
        }
      }
      nmx.u5.i <- u5mr.row[select.idx]
      if (!is.null(replace.u5mr.row)) {
        missing.idx <- is.na(nmx.u5.i)
        if (any(missing.idx)) {
          nmx.u5.i[missing.idx] <- replace.u5mr.row[select.idx][missing.idx]
        }
      }
    }
    
    nmx.1to4.i <- ((nmx.u5.i - nmx.u1.i)/(1000 - nmx.u1.i)) * 1000
    nmx.1to4.i.alt <- (1 - ((1 - (nmx.1to4.i/1000))^(1/4))) * 1000
    nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
    nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2], 2),
               rep(nmx.1to4.i.alt[3], 2), rep(nmx.1to4.i.alt[4], 2), nmx.1to4.i.alt[5])
    block.cols <- ((i - 1L) * 52L + 1L):(i * 52L)
    nmx.mat.k[, block.cols] <- matrix(nmx.i, nrow = 10L, ncol = 52L)
    
    if (nn.exists) {
      if ((pos.i + 1L) > max.pos.k) {
        nmr.i <- c(nmr.row[pos.i], NA_real_)
      } else {
        select.nn.idx <- pos.i:year.pos.k[i + 1L]
        nmr.i <- nmr.row[select.nn.idx]
        if (!is.null(replace.nmr.row)) {
          missing.idx <- is.na(nmr.i)
          if (any(missing.idx)) {
            nmr.i[missing.idx] <- replace.nmr.row[select.nn.idx][missing.idx]
          }
        }
      }
      nmr.mat.k[, block.cols] <- matrix(nmr.i, nrow = 2L, ncol = 52L)
    }
  }
  
  wgt.mat.k <- country.ctx$wgt.mat.cache[[n.k]]
  nqx.mat.k <- wgt.mat.k * (nmx.mat.k/1000)
  npx.mat.k <- 1 - nqx.mat.k
  lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k), 2, cumprod)
  dx.mat.k <- -apply(lx.mat.k, 2, diff)
  
  lx.nn.mat.k <- dx.nn.mat.k <- NULL
  if (nn.exists) {
    wgt.nn.mat.k <- country.ctx$wgt.nmr.mat.cache[[n.k]]
    nqx.nn.mat.k <- wgt.nn.mat.k * (nmr.mat.k/1000)
    npx.nn.mat.k <- 1 - nqx.nn.mat.k
    lx.nn.mat.k <- apply(rbind(bwc.vec.k, npx.nn.mat.k), 2, cumprod)
    dx.nn.mat.k <- -apply(lx.nn.mat.k, 2, diff)
  }
  
  years.k.mat <- matrix(rep(country.ctx$years.mat[, year.pos.k, drop = FALSE], 52L),
                        nrow = 10L, ncol = n.k * 52L)
  years.k.mat <- years.k.mat[, order(years.k.mat[1, ]), drop = FALSE]
  cols.k <- (country.ctx$total.week.cols - (n.k * 52L) + 1L):country.ctx$total.week.cols
  
  list(
    years.k = years.k,
    year1.est.u5 = year1.est.u5,
    year1.est.u1 = year1.est.u1,
    year1.est.nn = year1.est.nn,
    cols.k = cols.k,
    dx.mat.k = dx.mat.k,
    lx.mat.k = lx.mat.k,
    dx.nn.mat.k = dx.nn.mat.k,
    lx.nn.mat.k = lx.nn.mat.k,
    years.k.mat = years.k.mat
  )
}

# What it does: Flags countries whose regional inputs still contain missing rates.
# Why it is needed: Regional rebuilding only needs to target countries that would otherwise leave incomplete aggregates.
IdentifyCountriesNeedingRegionalRebuildBWC <- function(
    u5mr.temp.rt,
    imr.temp.rt,
    nmr.temp.rt = NULL
) {
  missing.mask <- is.na(u5mr.temp.rt) | is.na(imr.temp.rt)
  if (!is.null(nmr.temp.rt)) {
    missing.mask <- missing.mask | is.na(nmr.temp.rt)
  }
  rowSums(missing.mask) > 0L
}

# What it does: Collapses one country life table into annual infant, under-five, and optional neonatal deaths.
# Why it is needed: Regional aggregation works on yearly totals, not the weekly life-table detail.
SummariseRegionalLifeTableDeathsBWC <- function(
    country.life.table,
    years,
    nn.exists = FALSE
) {
  death0.row <- deathu5.row <- numeric(length(years))
  deathnn.row <- if (isTRUE(nn.exists)) numeric(length(years)) else NULL
  years.k <- country.life.table$years.k
  years.k.mat <- country.life.table$years.k.mat
  dx.mat.k <- country.life.table$dx.mat.k
  nn.year.mask <- floor(years.k.mat[1:2, , drop = FALSE])
  
  for (yrk in seq.int(min(years.k), max(years.k))) {
    year.pos <- match(yrk, years)
    deathu5.row[year.pos] <- sum(dx.mat.k[floor(years.k.mat) == yrk], na.rm = TRUE)
    death0.row[year.pos] <- sum(dx.mat.k[1:2, , drop = FALSE][nn.year.mask == yrk], na.rm = TRUE)
    if (isTRUE(nn.exists)) {
      deathnn.row[year.pos] <- sum(country.life.table$dx.nn.mat.k[nn.year.mask == yrk], na.rm = TRUE)
    }
  }
  
  list(
    death0 = roundoff(death0.row, digits = 0),
    deathu5 = roundoff(deathu5.row, digits = 0),
    deathnn = if (isTRUE(nn.exists)) roundoff(deathnn.row, digits = 0) else NULL
  )
}

# What it does: Computes ARR and decline metrics for every country across all simulation draws.
# Why it is needed: These summary indicators feed the country, regional, and world decline outputs.
BuildCountryRatesOfDeclineArraysBWC <- function(
    u5mr.ctj,
    est.years,
    year1,
    year2,
    year4,
    year.target,
    factor.target,
    ndigits
) {
  C <- dim(u5mr.ctj)[1]
  nsim <- dim(u5mr.ctj)[3]
  ARR.year1.year4.cj <- ARR.year1.year2.cj <- ARR.year2.year4.cj <- required.ARR.cj <- changeinARR.cj <-
    decline.year1.year4.cj <- decline.year1.year2.cj <- decline.year2.year4.cj <- array(NA_real_, c(C, nsim))

  for (j in seq_len(nsim)) {
    u5mr.ct <- u5mr.ctj[, , j]
    ARR.year1.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year4)
    ARR.year1.year2.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
    ARR.year2.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year2, year.end = year4)
    required.ARR.c <- ifelse(
      year4 < year.target,
      1 / (year.target - year4) *
        log(roundoff(u5mr.ct[, est.years == year1] * factor.target, digits = ndigits) /
              u5mr.ct[, est.years == year4]) * -100,
      NA
    )
    changeinARR.c <- ARR.year2.year4.c - ARR.year1.year2.c
    decline.year1.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year4)
    decline.year1.year2.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
    decline.year2.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year2, year.end = year4)

    ARR.year1.year4.cj[, j] <- ARR.year1.year4.c
    ARR.year1.year2.cj[, j] <- ARR.year1.year2.c
    ARR.year2.year4.cj[, j] <- ARR.year2.year4.c
    required.ARR.cj[, j] <- required.ARR.c
    changeinARR.cj[, j] <- changeinARR.c
    decline.year1.year4.cj[, j] <- decline.year1.year4.c
    decline.year1.year2.cj[, j] <- decline.year1.year2.c
    decline.year2.year4.cj[, j] <- decline.year2.year4.c
  }

  list(
    ARR.year1.year4.cj = ARR.year1.year4.cj,
    ARR.year1.year2.cj = ARR.year1.year2.cj,
    ARR.year2.year4.cj = ARR.year2.year4.cj,
    required.ARR.cj = required.ARR.cj,
    changeinARR.cj = changeinARR.cj,
    decline.year1.year4.cj = decline.year1.year4.cj,
    decline.year1.year2.cj = decline.year1.year2.cj,
    decline.year2.year4.cj = decline.year2.year4.cj
  )
}

# What it does: Builds the cache file path for one regional simulation bundle.
# Why it is needed: A single naming rule keeps save, load, validation, and cleanup steps aligned.
GetRegionalBundlePathBWC <- function(output.dir.samples.region, j) {
  file.path(output.dir.samples.region, paste0("regional_rt_", j, ".rds"))
}

# What it does: Checks whether a saved regional bundle has the expected objects and dimensions.
# Why it is needed: Parallel runs can leave partial or corrupt files, and those should never be reused as valid caches.
RegionalBundleIsValidBWC <- function(bundle) {
  required.names <- c("q0.rt", "q1to4.rt", "q5.rt", "death0.all.rt",
                      "death1to4.all.rt", "deathu5.all.rt", "nn.exists")
  if (!is.list(bundle) || !all(required.names %in% names(bundle))) {
    return(FALSE)
  }
  if (!is.logical(bundle$nn.exists) || length(bundle$nn.exists) != 1L || is.na(bundle$nn.exists)) {
    return(FALSE)
  }
  
  matrix.names <- c("q0.rt", "q1to4.rt", "q5.rt", "death0.all.rt",
                    "death1to4.all.rt", "deathu5.all.rt")
  if (isTRUE(bundle$nn.exists)) {
    matrix.names <- c(matrix.names, "qnn.rt", "deathnn.all.rt")
  }
  
  ref.dim <- dim(bundle$q0.rt)
  if (length(ref.dim) != 2L) {
    return(FALSE)
  }
  
  for (matrix.name in matrix.names) {
    matrix.value <- bundle[[matrix.name]]
    if (!is.matrix(matrix.value) || !is.numeric(matrix.value) || !identical(dim(matrix.value), ref.dim)) {
      return(FALSE)
    }
  }
  
  TRUE
}

# What it does: Confirms that a regional bundle exists on disk and passes validation.
# Why it is needed: The refactored regional runner uses this to skip good caches and retry missing or bad bundles.
RegionalBundleExistsBWC <- function(output.dir.samples.region, j) {
  bundle.path <- GetRegionalBundlePathBWC(output.dir.samples.region, j)
  if (!file.exists(bundle.path)) {
    return(FALSE)
  }
  bundle.info <- file.info(bundle.path)
  if (is.na(bundle.info$size) || bundle.info$size <= 0) {
    return(FALSE)
  }
  bundle <- tryCatch(readRDS(bundle.path), error = function(...) NULL)
  RegionalBundleIsValidBWC(bundle)
}

# What it does: Loads one saved regional simulation bundle from disk.
# Why it is needed: Path resolution stays in one place instead of being repeated in each caller.
LoadRegionalBundleBWC <- function(output.dir.samples.region, j) {
  readRDS(GetRegionalBundlePathBWC(output.dir.samples.region, j))
}

# What it does: Saves one regional simulation bundle using a temporary file and rename step.
# Why it is needed: Atomic writes reduce the risk of readers seeing half-written bundle files during parallel execution.
SaveRegionalBundleBWC <- function(bundle, output.dir.samples.region, j) {
  dir.create(output.dir.samples.region, showWarnings = FALSE, recursive = TRUE)
  final.path <- GetRegionalBundlePathBWC(output.dir.samples.region, j)
  temp.path <- paste0(final.path, ".tmp")
  if (file.exists(temp.path)) {
    unlink(temp.path)
  }
  saveRDS(bundle, file = temp.path, compress = FALSE)
  if (file.exists(final.path)) {
    unlink(final.path)
  }
  if (!file.rename(temp.path, final.path)) {
    stop(paste0("Failed to move completed regional bundle into place: ", final.path))
  }
  invisible(final.path)
}

# What it does: Divides two matrices and converts non-finite results to `NA`.
# Why it is needed: Aggregate rate calculations can hit zero denominators, and those should not leak `Inf` or `NaN` into outputs.
SafeDivideMatrixBWC <- function(numerator, denominator) {
  result <- numerator/denominator
  result[!is.finite(result)] <- NA_real_
  result
}

# What it does: Computes quantiles after dropping non-finite values, or returns an all-`NA` template if nothing is usable.
# Why it is needed: Summary output should remain stable even when a draw vector is empty or numerically invalid.
SafeQuantileBWC <- function(x, probs) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    template <- stats::quantile(c(0, 1), probs = probs, na.rm = TRUE)
    template[] <- NA_real_
    return(template)
  }
  stats::quantile(x, probs = probs, na.rm = TRUE)
}

# What it does: Collects adhoc region columns and names from the country info file.
# Why it is needed: Adhoc regional output uses dynamic group names instead of a fixed region lookup table.
ResolveAdhocRegionSelectionBWC <- function(country.info) {
  matching.cols <- colnames(country.info)[grepl("^AdhocCountries", colnames(country.info), ignore.case = TRUE)]
  if (length(matching.cols) < 1L) {
    return(NULL)
  }

  regiontypes <- unique(unlist(lapply(matching.cols, function(col) {
    values <- as.character(country.info[[col]])
    values <- values[!is.na(values) & nzchar(values)]
    unique(values)
  }), use.names = FALSE))
  regiontypes <- unique(regiontypes[nzchar(regiontypes)])
  if (length(regiontypes) < 1L) {
    return(NULL)
  }

  regions <- if (inherits(country.info, "data.table")) {
    country.info[, ..matching.cols]
  } else {
    country.info[, matching.cols, drop = FALSE]
  }

  list(
    regions = regions,
    regiontypes = regiontypes
  )
}

# What it does: Builds the member-country index list for each requested regional grouping.
# Why it is needed: Every regional calculation depends on a consistent mapping from region labels to country rows.
BuildRegionalIndexListBWC <- function(regions, regiontypes, filename, which.no.rates = integer(0)) {
  select.reg.list <- vector("list", length(regiontypes))
  
  for (r in seq_along(regiontypes)) {
    if (identical(filename, "AdhocCountries")) {
      if (is.data.frame(regions) || is.matrix(regions)) {
        region.matrix <- as.matrix(regions)
        select.reg.og <- which(apply(region.matrix, 1, function(row) {
          any(!is.na(row) & nzchar(row) & row == regiontypes[r])
        }))
      } else {
        region.values <- as.character(regions)
        select.reg.og <- which(!is.na(region.values) & nzchar(region.values) & region.values == regiontypes[r])
      }
    } else if (filename %in% c("UNICEFProgRegion", "UNICEFReportRegion", "MDGRegion", "SDGRegion", "SDGSimpleRegion",
                        "SDGRCRegion", "EAPRORegion", "WBRegion", "UNPDRegion", "OICRegion", "M49Region", "Wealthall", "Wealthdata", "AURegion", "AURECRegion",
                        "FCSCountries", "FragileCountries2025OECD", "SDG2015", "UNICEFReportRegion_nohigh")) {
      reg.num <- ChooseRegion(region = regiontypes[r], regiontype = filename)
      select.reg.og <- (1:nrow(regions))[regions[, is.element(colnames(regions),
                                                               paste0(filename, reg.num))] == regiontypes[r]]
    } else if (filename %in% c("WHORegion", "CountdownCountries", "ECAAfricaRegion","GlobalStrategyCountries",
                               "FragileCountries2013", "FragileCountries2014", "FragileCountries2015",
                               "FragileCountries2017", "FragileCountries2018", "FragileCountries2018OECD1", "FragileCountries2018OECD2", "FragileCountries2019", "JHUFragile2021",
                               "WealthallGlobal", "WealthdataGlobal", "WorldBankReg2", "NewWorldBank", "USAIDCountries", "AfricanEconomicCommunityRegion",
                               "ECACountries", "GAVICountries", "SPhumanitarianRegion","SPhighburdenRegion","JHURegion","LiSTRegion","MENAEMRORegion","EECARegion")) {
      select.reg.og <- (1:length(regions))[regions == regiontypes[r]]
    } else {
      stop(paste0("Unhandled regional filename: ", filename))
    }
    
    if (length(which.no.rates) > 0) {
      select.reg <- setdiff(select.reg.og, which.no.rates)
    } else {
      select.reg <- select.reg.og
    }
    select.reg.list[[r]] <- as.integer(select.reg)
  }
  
  select.reg.list
}

# What it does: Adjusts the set of region types to run in missing-rate replacement mode.
# Why it is needed: Replacement runs need small guardrails to avoid overwriting standard aggregate outputs by mistake.
ResolveRegiontypesForReplacementBWC <- function(regiontypes.select, replace.rates.reg, regiontypes.select.missing = FALSE) {
  if (!isTRUE(regiontypes.select.missing) || is.null(replace.rates.reg)) {
    return(regiontypes.select)
  }
  if (identical(replace.rates.reg, "M49Region") && "M49" %in% regiontypes.select) {
    cat("Skipping M49 during replacement-run regional output so the conventional M49 aggregate is not overwritten.\n")
    return(setdiff(regiontypes.select, "M49"))
  }
  regiontypes.select
}

# What it does: Converts a user-selected ISO list into validated country indices.
# Why it is needed: Partial rebuilds depend on exact country matching and should fail fast on unknown ISO codes.
ResolveSelectedIsoIndicesBWC <- function(selected_iso, iso.c) {
  if (is.null(selected_iso)) {
    return(NULL)
  }
  selected_iso <- unique(as.character(selected_iso))
  selected_iso <- selected_iso[nzchar(selected_iso)]
  if (length(selected_iso) < 1L) {
    return(NULL)
  }
  missing_iso <- setdiff(selected_iso, iso.c)
  if (length(missing_iso) > 0L) {
    stop(paste0("selected_iso not found in country.info: ", paste(missing_iso, collapse = ", ")))
  }
  match(selected_iso, iso.c)
}

# What it does: Finds the first year with any non-missing trajectory estimate for each selected country.
# Why it is needed: Partial rebuild logic needs the earliest available estimate year when reconstructing metadata.
BuildYear1EstimatesFromTrajectoriesBWC <- function(rate.ctj, years, country.idx) {
  if (is.null(rate.ctj)) {
    return(rep(NA_real_, length(country.idx)))
  }
  vapply(country.idx, function(idx) {
    year.available <- apply(!is.na(rate.ctj[idx, , , drop = FALSE]), 2, any)
    first.pos <- which(year.available)[1]
    if (length(first.pos) < 1L || is.na(first.pos)) {
      return(NA_real_)
    }
    years[first.pos]
  }, numeric(1))
}

# What it does: Loads a cached country-level object, or creates an `NA` template when allowed.
# Why it is needed: Partial replacement runs must preserve untouched slices while rebuilding only selected countries.
LoadReplacementCountryTemplateBWC <- function(path, object.name, dim.template = NULL) {
  value <- NULL
  if (file.exists(path)) {
    load(path)
    value <- get(object.name)
  } else if (!is.null(dim.template)) {
    value <- array(NA_real_, dim = dim.template)
  } else {
    stop(paste0("Missing cached replacement file required for partial rebuild: ", path))
  }
  value
}

# What it does: Loads a country-level cache from the sample file first, then falls back to the combined cache if needed.
# Why it is needed: Partial reruns need to reuse existing results safely when a per-draw intermediate file is not present.
LoadReplacementCountryTemplateWithFallbackBWC <- function(
    sample.path,
    sample.object.name,
    combined.path = NULL,
    combined.object.name = NULL,
    slice.index = NULL
) {
  if (file.exists(sample.path)) {
    load(sample.path)
    return(get(sample.object.name))
  }
  if (!is.null(combined.path) && file.exists(combined.path)) {
    load(combined.path)
    combined.value <- get(combined.object.name)
    if (is.null(slice.index)) {
      return(combined.value)
    }
    dims <- length(dim(combined.value))
    if (dims == 3L) {
      return(combined.value[, , slice.index, drop = FALSE][, , 1L])
    }
    if (dims == 4L) {
      return(combined.value[, , , slice.index, drop = FALSE][, , , 1L])
    }
    stop(paste0("Unsupported fallback object dimensions for ", combined.object.name))
  }
  stop(paste0("Missing cached replacement file required for partial rebuild: ", sample.path))
}

# What it does: Lists the country cache files used by a missing-rate replacement run.
# Why it is needed: Cleanup and cache checks need one authoritative definition of the replacement file set.
BuildReplacementCountryCacheFilesBWC <- function(replace.rates.reg, nn.exists = FALSE) {
  if (is.null(replace.rates.reg)) {
    return(character(0))
  }
  files.country.replace <- c(
    paste0("death0.ctj.", replace.rates.reg, "-replace.rda"),
    paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda"),
    paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")
  )
  if (isTRUE(nn.exists)) {
    files.country.replace <- c(
      files.country.replace,
      paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")
    )
  }
  files.country.replace
}

ShouldGenerateOrdinaryCountryResultsBWC <- function(
    country.combined.exists,
    country.trajectories.exist,
    ordinary.selected.active,
    replace.rates.reg,
    reuse.replacement.country
) {
  if (isTRUE(reuse.replacement.country) && !is.null(replace.rates.reg)) {
    return(FALSE)
  }

  !isTRUE(country.combined.exists) ||
    !isTRUE(country.trajectories.exist) ||
    isTRUE(ordinary.selected.active)
}

# What it does: Deletes cached world-level output files from a previous run.
# Why it is needed: World summaries have to be regenerated from fresh country data when upstream inputs change.
DeleteWorldOutputsBWC <- function(output.dir.samplescombined, nn.exists = FALSE) {
  files.world <- c("res.world.rda", "global.RoDs.ui.rda", "u5mr.wtj.rda", "imr.wtj.rda",
                   "deathu5.all.wtj.rda", "death0.all.wtj.rda",
                   "pop0.wt.rda", "pop1to4.wt.rda",
                   "pop0.orig.wt.rda", "pop1to4.orig.wt.rda", "popu5.orig.wt.rda",
                   "coverage0.wt.rda", "coverageu5.wt.rda")
  if (isTRUE(nn.exists)) {
    files.world <- c(files.world, "nmr.wtj.rda", "deathnn.all.wtj.rda")
  }
  unlink(file.path(output.dir.samplescombined, files.world))
  invisible(files.world)
}

# What it does: Lists the combined regional cache files for one region family.
# Why it is needed: The refactored regional flow uses this list to detect whether partial updates are safe.
RegionalCombinedCacheFilesBWC <- function(filename, nn.exists = FALSE) {
  files.region <- c(
    paste0(filename, "_u5mr.rtj.rda"),
    paste0(filename, "_imr.rtj.rda"),
    paste0(filename, "_deathu5.all.rtj.rda"),
    paste0(filename, "_death0.all.rtj.rda")
  )
  if (isTRUE(nn.exists)) {
    files.region <- c(
      files.region,
      paste0(filename, "_nmr.rtj.rda"),
      paste0(filename, "_deathnn.all.rtj.rda")
    )
  }
  files.region
}

# What it does: Orchestrates the full country, world, and regional aggregation workflow.
# Why it is needed: This is the main entry point that wires together input loading, calculations, caching, and final outputs.
OutputAggregates <- function( # Calculate and output aggregated rates and numbers of deaths at the country,
  ## regional and global level.
  runname.U5MR = NULL, ##<< Either specify 1) \code{runname.U5MR}
  runname.IMR = NULL, ##<< and \code{runname.IMR},
  runname.NMR = NULL, ##<< and/or \code{runname.NMR}
  results.U5MR.file = NULL, ##<< 2) and \code{results.U5MR.file} (for median only)
  results.IMR.file = NULL, ##<< and \code{results.IMR.file} (for median only)
  results.NMR.file = NULL, ##<< and/or \code{results.NMR.file} (for median only)
  filename.U5MR = NULL, ##<< Alternative file name of trajectory array, used if \code{runname.U5MR} is specified.
  ## Default is \code{u5mrfinal.ctj.rda}.
  filename.IMR = NULL, ##<< Alternative file name of trajectory array, used if \code{runname.IMR} is specified.
  filename.NMR = NULL, ##<< Alternative file name of trajectory array, used if \code{runname.NMR} is specified.
  ## Default is \code{imrfinal.ctj.rda}.
  country.info.file = NULL, ##<< If \code{NULL}, country info included in package is used.
  population.file = NULL, ##<< File path to population data. If \code{NULL}, population data included in package is used.
  livebirths.file = NULL,
  data.a0.file = NULL, ##<< File path to a0 data. If \code{NULL}, a0 data included in package is used.
  ## package from World Population Prospects 2012 is used.
  run.on.server = TRUE, ##<< Running on server? Set \code{TRUE} to run in parallel.
  parallel.cores = NULL, ##<< Number of cores to use when \code{run.on.server = TRUE}. Defaults to min(detectCores()-2, 24).
  regiontypes.select = c("UNICEFProg", "UNICEFReport", "MDG", "SDGSimple", "WHO", "WB", "UNPD", "OIC",
                         "Countdown", "ECAAfrica", "AU", "AUREC",
                         "Fragile2013", "Fragile2014", "Fragile2015", "Fragile2017", "Fragile2018","Fragile2019", 
                         "FCSCountries",
                         "USAID", "M49", "Wealthdata", "Wealthall","GlobalStrategy","JHU","LiST_all","MENAEMRO","EECA", "SDG2015", "UNICEFReport_nohigh"), ##<< Output regional aggregates for which region types?
  ## Input a character vector of more than one of the possible options if desired.
  ## If \code{NULL}, output will not be generated at the region level.
  ### ---- note 7-27-17: Add new UNICEF regions?
  year1 = 1990.5, ##<< First year used for ARR calculation.
  year2 = 2000.5, ##<< Second year used for ARR calculation.
  year4 = 2019.5, ##<< Last year used for ARR calculation.
  year.target = 2019.5, ##<< MDG target year.
  est.years = seq(1950.5, 2019.5, 1), ##<< Years of estimation.
  factor.target = 1/3, ##<< MDG target factor (Reduce to one third).
  percentiles = c(0.05, 0.5, 0.95), ##<< Vector of percentiles.
  ndigits = 1, ##<< Number of decimal places to use for analysis (e.g. to calcalate ARR, decline).
  output.dir = NULL, ##<< Output directory to save all results.
  get.world.results = TRUE, ## Should the world results be calculated? If running to get regional aggregate for replace, world results can be silenced
  round.output = FALSE,
  output.rates.of.decline = FALSE,
  replace.rates.reg="M49Region", # Regional Aggregate to use for replacing -- must be one of regiontypes.select
  replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", country.info$M49Region2[country.info$M49Region1=="Americas"]),  # regiontypes from aggregate (e.g. M49Region1) -- must be vector with 1 regional type for each country and types must be from replace.rates.reg, this argument is necessary for creating the country trajectories with missing rates replaced with regional, not used if these country trajectories already exist
  reuse.replacement.country = FALSE,
  selected_iso = NULL,
  test = FALSE ##<< Use a subset of first 5 trajectories to test function; must be using trajectory files not just Results.csv
) {
  regiontypes.select.missing <- missing(regiontypes.select)
  regiontypes.select <- ResolveRegiontypesForReplacementBWC(
    regiontypes.select = regiontypes.select,
    replace.rates.reg = replace.rates.reg,
    regiontypes.select.missing = regiontypes.select.missing
  )

  source("R/chooseregion.R")
  overall.start <- proc.time()[["elapsed"]]
  phase.times <- list()
  parallel.cores.resolved <- if (run.on.server) ResolveParallelCoresBWC(parallel.cores) else 1L
  
  if (is.null(output.dir))
    output.dir <- "output_numberofdeaths"
  output.dir.samples <- file.path(output.dir, "samples")
  output.dir.samplescombined <- file.path(output.dir, "samples_combined")
  dir.create(output.dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(output.dir.samples, showWarnings = FALSE, recursive = TRUE)
  dir.create(output.dir.samplescombined, showWarnings = FALSE, recursive = TRUE)
  
  # DJS edit 2018-07-23 for using historical regional constant for aggregation
  if(!is.null(replace.rates.reg)){
    if(file.exists(file.path(output.dir.samplescombined, paste0(replace.rates.reg, "_u5mr.rtj.rda")))){
      cat(paste0("World and regional aggregate results will be calculated with country deaths calculated with missing historical rates replaced with ", replace.rates.reg,"...\n" ))
    } else
      cat(paste0("Aggregate results for ",replace.rates.reg, " do not exist yet. \n Must generate aggregate results for ",replace.rates.reg, " with replace.rates.reg=NULL first to get regional aggregate to use as replacement rates...\n" ))
  }
  
  ## Add live births to this input list? Will need for the BWC calculations
  if (is.null(country.info.file))
    country.info.file <- file.path("input", "country.info.CME.csv")
  if (is.null(population.file)) # same file used as country.info.CME.csv contains both country and population info
    population.file <- file.path("input", "country.info.CME.csv")
  if (is.null(data.a0.file))
    data.a0.file <- file.path("input", "a0.csv")
  if (is.null(livebirths.file)) ## added DJS 2017-07-27 for BWC method
    livebirths.file <- file.path("input", "data_livebirths.csv")
  
  # read in data
  country.info <- read.csv(file = country.info.file, header = T, stringsAsFactors = F,
                           strip.white = T)
  country.info <- country.info[, !grepl("pop", colnames(country.info))]
  data.pop <- read.csv(file = population.file, header = T, stringsAsFactors = F, strip.white = T)
  data.a0 <- read.csv(file = data.a0.file, header = T, stringsAsFactors = F, strip.white = T)
  data.livebirths <- read.csv(file = livebirths.file, header = T, stringsAsFactors = F, strip.white = T)
  
  # reformat data (same country order)
  data.pop <- dplyr::left_join(data.frame(ISO3Code = country.info$ISO3Code), data.pop, by = "ISO3Code")
  data.a0 <- dplyr::left_join(data.frame(iso = country.info$ISO3Code), data.a0, by = "iso")
  data.livebirths <- reshape(data=data.livebirths, idvar=c("country", "uncode", "sex"), timevar = "year", direction = "wide")
  data.livebirths <- data.livebirths[match(country.info$UNCode, data.livebirths$uncode),]
  data.livebirths$iso <- country.info$ISO3Code[match(country.info$UNCode, data.livebirths$uncode)]
  
  if (sum(is.na(data.a0$a0)) > 0)
    cat(paste0("Warning: a0 is NA for ", paste(data.a0$iso[is.na(data.a0$a0)], collapse = ", "), ".\n"))
  
  # read in B3 U5MR and IMR trajectories
  if (!is.null(runname.U5MR) & !is.null(runname.IMR)) {
    cat("Reading in results from output/runname.U5MR and output/runname.IMR.\n")
  } else if (!is.null(results.U5MR.file) & !is.null(results.IMR.file)) {
    cat("Reading in results from results.U5MR.file and results.IMR.file.\n")
  } else {
    cat("Error: Either runname.U5MR and runname.IMR or results.U5MR.file and results.IMR.file must be specified.\n")
  }
  if (is.null(filename.U5MR))
    filename.U5MR <- "u5mrfinal.ctj.rda" ## check structure here to fit into BWC code
  if (is.null(filename.IMR))
    filename.IMR <- "imrfinal.ctj.rda" ## check structure here to fit into BWC code
  if (is.null(filename.NMR))
    filename.NMR <- "finalresults.jtc.Rda" ## check structure here to fit into BWC code
  if (!is.null(results.U5MR.file) & !is.null(results.IMR.file)) { ## may need to add NMR file here if SummariseResults works the same for it
    SummariseResults(results.file = results.U5MR.file,
                     output.dir = file.path(output.dir, "U5MR"),
                     filename.output = gsub(".rda", "", filename.U5MR))
    SummariseResults(results.file = results.IMR.file,
                     output.dir = file.path(output.dir, "IMR"),
                     filename.output = gsub(".rda", "", filename.IMR))
    
    # load required U5MR files
    load(file = file.path(output.dir, "U5MR", filename.U5MR))
    eval(parse(text = paste0("u5mrfinal.ctj <- ", gsub(".rda", "", filename.U5MR))))
    load(file = file.path(output.dir, "U5MR", "iso.c.rda"))
    load(file = file.path(output.dir, "U5MR", "year.t.rda"))
    isoU5MR.c <- iso.c
    yearU5MR.t <- year.t
    
    # load required IMR files
    load(file = file.path(output.dir, "IMR", filename.IMR))
    eval(parse(text = paste0("imrfinal.ctj <- ", gsub(".rda", "", filename.IMR))))
    load(file = file.path(output.dir, "IMR", "iso.c.rda"))
    load(file = file.path(output.dir, "IMR", "year.t.rda"))
    isoIMR.c <- iso.c
    yearIMR.t <- year.t
    
    # nmr
    if(!is.null(results.NMR.file)) {
      SummariseResults(results.file = results.NMR.file,
                       output.dir = file.path(output.dir, "NMR"),
                       filename.output = gsub(".Rda", "", filename.NMR))
      
      load(file = file.path(output.dir, "NMR", gsub(".Rda", ".rda", filename.NMR)))
      eval(parse(text = paste0("nmrfinal.ctj <- ", gsub(".Rda", "", filename.NMR))))
      load(file = file.path(output.dir, "NMR", "iso.c.rda"))
      load(file = file.path(output.dir, "NMR", "year.t.rda"))
      isoNMR.c <- iso.c
      yearNMR.t <- year.t
    }
  } else {
    # load required U5MR files
    load(file = file.path("output", runname.U5MR, filename.U5MR))
    if(!grepl("crisisfree", filename.U5MR)) eval(parse(text = paste0("u5mrfinal.ctj <- ", gsub(".rda", "", filename.U5MR))))
    load(file = file.path("output", runname.U5MR, "iso.c.rda"))
    load(file = file.path("output", runname.U5MR, "year.t.rda"))
    isoU5MR.c <- iso.c
    yearU5MR.t <- year.t
    
    # load required IMR files
    load(file = file.path("output", runname.IMR, filename.IMR))
    if(!grepl("crisisfree", filename.IMR)) eval(parse(text = paste0("imrfinal.ctj <- ", gsub(".rda", "", filename.IMR))))
    load(file = file.path("output", runname.IMR, "iso.c.rda"))
    load(file = file.path("output", runname.IMR, "year.t.rda"))
    isoIMR.c <- iso.c
    yearIMR.t <- year.t
    
    if(!is.null(runname.NMR)) {
      load(file = file.path("output", runname.NMR, filename.NMR))
      eval(parse(text = paste0("nmrfinal.ctj <- ", gsub("final","",gsub(".Rda", "", filename.NMR)))))
      nmrfinal.ctj <- aperm(nmrfinal.ctj, c(3,2,1))
      isoNMR.c <- dimnames(nmrfinal.ctj)[[1]]
      yearNMR.t <- as.numeric(dimnames(nmrfinal.ctj)[[2]])
    }
  }
  
  # get dimensions
  nyears <- length(est.years) ## may use this for years or length of some dimension of output files
  est.years.floor <- floor(est.years)
  iso.c <- country.info$ISO3Code
  C <- length(iso.c)
  selected.country.idx <- ResolveSelectedIsoIndicesBWC(selected_iso = selected_iso, iso.c = iso.c)
  if (isTRUE(reuse.replacement.country) && is.null(replace.rates.reg)) {
    stop("reuse.replacement.country=TRUE can only be used when replace.rates.reg is not NULL.")
  }
  if (isTRUE(reuse.replacement.country) && !is.null(selected.country.idx)) {
    stop("reuse.replacement.country=TRUE cannot be combined with selected_iso because selected-country reruns require rebuilding replacement-country cache rows.")
  }
  
  # test
  if (test) {
    nsim.available <- dim(u5mrfinal.ctj)[3]
    if (!is.na(nsim.available) && nsim.available > 0) {
      n.test.sim <- min(5L, nsim.available)
      rand.select <- sample(seq_len(nsim.available), n.test.sim)
      u5mrfinal.ctj <- u5mrfinal.ctj[, , rand.select, drop = FALSE]
      imrfinal.ctj <- imrfinal.ctj[, , rand.select, drop = FALSE]
      if(exists("nmrfinal.ctj")) nmrfinal.ctj <- nmrfinal.ctj[, , rand.select, drop = FALSE]
    }
  }
  nsim <- dim(u5mrfinal.ctj)[3]
  cat(paste0("Number of simulations is ", nsim, "\n"))
  
  if (sum(!file.exists(file.path(output.dir.samplescombined, "u5mr.ctj.rda"),
                       file.path(output.dir.samplescombined, "imr.ctj.rda"))) > 0) { # change to > 1 for conditional that both U5MR and IMR files are present
    u5mr.ctj <- imr.ctj <- array(NA, c(C, nyears, nsim))
    u5mr.ctj[match(isoU5MR.c, iso.c), is.element(est.years, yearU5MR.t), ] <-
      u5mrfinal.ctj[, is.element(yearU5MR.t, est.years), ]
    imr.ctj[match(isoIMR.c, iso.c), is.element(est.years, yearIMR.t), ] <-
      imrfinal.ctj[, is.element(yearIMR.t, est.years), ]
    
    
    # check that U5MR and IMR <= 1000
    u5mr.ctj[u5mr.ctj > 1000] <- 1000
    imr.ctj[imr.ctj > 1000] <- 1000
    
    # calculate q1to4.ctj
    q1to4.ctj <- 1-(1-u5mr.ctj/1000)/(1-imr.ctj/1000)
    # make 1-year rate for BWC method
    #q1to4.ctj <- (1-((1-(q1to4.ctj))^(1/4)))
    arr.ind.select <- which(is.na(q1to4.ctj), arr.ind = TRUE)
    # set u5mr.ctj and imr.ctj to NA wherever the other rate is NA in that country-year
    u5mr.ctj[arr.ind.select] <- NA
    imr.ctj[arr.ind.select] <- NA
    # check for countries with all NA values
    select.NA.c <- apply(u5mr.ctj[, , 1], 1, function(x) sum(!is.na(x)) == 0)
    cat(paste0("Warning: U5MR estimates are NA for all years of estimation for ",
               paste(iso.c[select.NA.c], collapse = ", "), ".\n"))
    select.NA.c <- apply(imr.ctj[, , 1], 1, function(x) sum(!is.na(x)) == 0)
    cat(paste0("Warning: IMR estimates are NA for all years of estimation for ",
               paste(iso.c[select.NA.c], collapse = ", "), ".\n"))
    
    rownames(u5mr.ctj) <- rownames(imr.ctj) <- iso.c
    colnames(u5mr.ctj) <- colnames(imr.ctj) <- est.years.floor
    
    
    # save
    save(u5mr.ctj, file = file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
    save(imr.ctj, file = file.path(output.dir.samplescombined, "imr.ctj.rda"))
    cat(paste0("Processed trajectories saved to ", output.dir.samplescombined, "\n"))
  } else {
    load(file = file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
    load(file = file.path(output.dir.samplescombined, "imr.ctj.rda"))
    cat(paste0("Processed trajectories loaded from ", output.dir.samplescombined, "\n"))
  }
  
  #NMR
  if (!file.exists(file.path(output.dir.samplescombined, "nmr.ctj.rda"))&exists("nmrfinal.ctj")) {
    nmr.ctj  <- array(NA, c(C, nyears, nsim))
    nmr.ctj[match(isoNMR.c, iso.c), is.element(est.years, yearNMR.t), ] <-
      nmrfinal.ctj[, is.element(yearNMR.t, est.years), ]
    
    # check that U5MR and IMR <= 1000
    nmr.ctj[nmr.ctj > 1000] <- 1000
    
    arr.ind.select <- which(is.na(q1to4.ctj), arr.ind = TRUE)
    # set nmr.ctj to NA wherever the u5mr or imr is NA in that country-year
    nmr.ctj[arr.ind.select] <- NA
    
    # check for countries with all NA values
    select.NA.c <- apply(nmr.ctj[, , 1], 1, function(x) sum(!is.na(x)) == 0)
    cat(paste0("Warning: NMR estimates are NA for all years of estimation for ",
               paste(iso.c[select.NA.c], collapse = ", "), ".\n"))
    
    rownames(nmr.ctj) <- iso.c
    colnames(nmr.ctj) <- est.years.floor
    # save
    save(nmr.ctj, file = file.path(output.dir.samplescombined, "nmr.ctj.rda"))
    cat(paste0("Processed NMR trajectories saved to ", output.dir.samplescombined, "\n"))
  } else {
    if(!is.null(results.NMR.file)|!is.null(runname.NMR)){
      load(file = file.path(output.dir.samplescombined, "nmr.ctj.rda"))
      cat(paste0("Processed NMR trajectories loaded from ", output.dir.samplescombined, "\n"))
    } else {
      nmr.ctj <- NULL
    } #if/else
  } # if/else

  files.country.replace <- BuildReplacementCountryCacheFilesBWC(
    replace.rates.reg = replace.rates.reg,
    nn.exists = !is.null(nmr.ctj)
  )
  if (isTRUE(reuse.replacement.country)) {
    missing.replace.cache <- files.country.replace[
      !file.exists(file.path(output.dir.samplescombined, files.country.replace))
    ]
    if (length(missing.replace.cache) > 0) {
      stop(paste0(
        "reuse.replacement.country=TRUE requested, but replacement-country cache for ",
        replace.rates.reg, " is missing from ", output.dir.samplescombined, ":\n  ",
        paste(missing.replace.cache, collapse = "\n  "),
        "\nRun OutputAggregates(..., replace.rates.reg = '", replace.rates.reg,
        "') once without reuse.replacement.country=TRUE first."
      ))
    }
    if (!file.exists(file.path(output.dir.samplescombined, "info.rda"))) {
      stop(paste0(
        "reuse.replacement.country=TRUE requested, but info.rda is missing from ",
        output.dir.samplescombined,
        ". Rebuild the country cache once without reuse.replacement.country=TRUE first."
      ))
    }
  }
  
  phase.times$preprocess <- GetPhaseElapsedBWC(overall.start)
  
  #-------------------------------------------------------------------------
  a0.c <- data.a0$a0
  a1to4.c <- rep(0.4, length(a0.c))
  pop0.orig.ct <- data.pop[, is.element(colnames(data.pop), paste0("pop0", est.years.floor))]
  pop1to4.orig.ct <- data.pop[, is.element(colnames(data.pop), paste0("pop1to4", est.years.floor))]
  # get live birth matrix like pop; added 2017-07-27 DJS for BWC method
  lb.ct <- data.livebirths[,which(names(data.livebirths)==paste0("lb.",est.years[1])):which(names(data.livebirths)==paste0("lb.",est.years[length(est.years)]))]
  country.calc.ctx <- BuildCountryCalculationContextBWC(
    nyears = nyears,
    years.start = 1950L,
    nn.exists = !is.null(nmr.ctj)
  )
  #-------------------------------------------------------------------------
  # get country results
  country.phase.start <- proc.time()[["elapsed"]]
  if(!is.null(nmr.ctj)){
    files.country <- c("death0.ctj.rda", "death1to4.ctj.rda", "deathu5.ctj.rda", "deathnn.ctj.rda",
                       # "dx.array.ctj.rda", "lx.array.ctj.rda", "dx.nn.array.ctj.rda", "lx.nn.array.ctj.rda",
                       "ARR.year1.year4.cj.rda", "ARR.year1.year2.cj.rda",
                       "ARR.year2.year4.cj.rda",
                       "decline.year1.year4.cj.rda", "decline.year1.year2.cj.rda",
                       "decline.year2.year4.cj.rda")
  } else {
    files.country <- c("death0.ctj.rda", "death1to4.ctj.rda", "deathu5.ctj.rda",
                       # "dx.array.ctj.rda", "lx.array.ctj.rda",
                       "ARR.year1.year4.cj.rda", "ARR.year1.year2.cj.rda",
                       "ARR.year2.year4.cj.rda",
                       "decline.year1.year4.cj.rda", "decline.year1.year2.cj.rda",
                       "decline.year2.year4.cj.rda")
  }
  
  # Also check for individual trajectory files (dx/lx arrays needed for cache)
  sample_trajectory_files <- c("dx.array.ct_1.rda", "lx.array.ct_1.rda")
  sample_info_exists <- file.exists(file.path(output.dir.samples, "info.rda"))
  country_combined_exists <- sum(!file.exists(file.path(output.dir.samplescombined, files.country))) == 0
  country_trajectories_exist <- sum(!file.exists(file.path(output.dir.samples, sample_trajectory_files))) == 0
  ordinary.selected.active <- is.null(replace.rates.reg) &&
    !is.null(selected.country.idx) &&
    country_combined_exists &&
    country_trajectories_exist &&
    sample_info_exists
  ordinary.selected.ignored <- is.null(replace.rates.reg) &&
    !is.null(selected.country.idx) &&
    !ordinary.selected.active
  
  if (ordinary.selected.ignored) {
    cat(paste0("Ignoring selected_iso for ordinary country generation because no existing country cache was found in ",
               output.dir, ". Running full country generation instead.\n"))
  }
  
  ordinary.country.generation.required <- ShouldGenerateOrdinaryCountryResultsBWC(
    country.combined.exists = country_combined_exists,
    country.trajectories.exist = country_trajectories_exist,
    ordinary.selected.active = ordinary.selected.active,
    replace.rates.reg = replace.rates.reg,
    reuse.replacement.country = reuse.replacement.country
  )

  if (ordinary.country.generation.required) {
    if (ordinary.selected.active) {
      cat(paste0("Updating ordinary country results for ", length(selected.country.idx), " selected ISO code(s): ",
                 paste(iso.c[selected.country.idx], collapse = ", "), "\n"))
    }
    cat(paste("Generating country results...\n"))
    if (run.on.server) {
      country.progress.start <- proc.time()[["elapsed"]]
      country.chunks <- BuildProgressChunksBWC(
        total = nsim,
        chunk.size = ResolveProgressChunkSizeBWC(
          total = nsim,
          parallel.cores = parallel.cores.resolved,
          progress.every = 1000L
        )
      )
      for (chunk in country.chunks) {
        registerDoMC(cores = min(parallel.cores.resolved, length(chunk)))
        foreach (j = chunk) %dopar% {
          CalculateCountryDeathsBWC(j = j, u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                    livebirths.ct = lb.ct,
                                    a0.c = a0.c, a1to4.c = a1to4.c,
                                    pop0.orig.ct = pop0.orig.ct, pop1to4.orig.ct = pop1to4.orig.ct,
                                    iso.c = iso.c, est.years = est.years,
                                    year1 = year1, year2 = year2, year4 = year4,
                                    year.target = year.target, factor.target = factor.target, ndigits = ndigits,
                                    output.dir = output.dir.samples,
                                    country.ctx = country.calc.ctx,
                                    output.dir.samplescombined = output.dir.samplescombined,
                                    selected.country.idx = if (ordinary.selected.active) selected.country.idx else NULL)
        }
        LogRoutProgressBWC(stage = "country generation",
                           completed = max(chunk),
                           total = nsim,
                           start.time = country.progress.start)
      }
    } else {
      progress.bar <- CreateLoopProgressBarBWC(nsim)
      on.exit(CloseLoopProgressBarBWC(progress.bar), add = TRUE)
      for (j in seq_len(nsim)) {
        CalculateCountryDeathsBWC(j = j, u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                  livebirths.ct = lb.ct,
                                  a0.c = a0.c, a1to4.c = a1to4.c,
                                  pop0.orig.ct = pop0.orig.ct, pop1to4.orig.ct = pop1to4.orig.ct,
                                  iso.c = iso.c, est.years = est.years,
                                  year1 = year1, year2 = year2, year4 = year4,
                                  year.target = year.target, factor.target = factor.target, ndigits = ndigits,
                                  output.dir = output.dir.samples,
                                  country.ctx = country.calc.ctx,
                                  output.dir.samplescombined = output.dir.samplescombined,
                                  selected.country.idx = if (ordinary.selected.active) selected.country.idx else NULL)
        UpdateLoopProgressBarBWC(progress.bar, j)
      }
      CloseLoopProgressBarBWC(progress.bar)
      progress.bar <- NULL
    }
    cat(paste0("Combining and outputting country results...\n"))
    CombineAndOutputCountryResults.BWC(u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                   country.info = country.info,
                                   percentiles = percentiles, ndigits = ndigits,
                                   output.dir = output.dir,
                                   output.dir.samples = output.dir.samples,
                                   output.dir.samplescombined = output.dir.samplescombined,
                                   output.rates.of.decline = output.rates.of.decline,
                                   round.output = round.output,
                                   selected.country.idx = if (ordinary.selected.active) selected.country.idx else NULL)
  } else {
    if (isTRUE(reuse.replacement.country) && !is.null(replace.rates.reg)) {
      cat(paste0(
        "Skipping ordinary country generation because replacement-country cache for ",
        replace.rates.reg,
        " is being reused from ",
        output.dir.samplescombined,
        ".\n"
      ))
    } else if(is.null(replace.rates.reg)){
      sapply(files.country, LoadFile, output.dir = output.dir.samplescombined,
             envir = environment())
      cat(paste("Country results loaded from ", output.dir.samplescombined, "\n"))
    } else {
      cat(paste("Country results without regional replacement not loaded. Calculating country deaths with regional replacement instead.\n"))
    }
  }
  phase.times$country <- GetPhaseElapsedBWC(country.phase.start)
  #-------------------------------------------------------------------------
  # get country results replacing missing rates with regional aggregate
  file.check.agg.replace <- file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda"))
  replacement.needs.rebuild <- !is.null(replace.rates.reg) &&
    (!isTRUE(reuse.replacement.country) || !file.exists(file.check.agg.replace) || !is.null(selected.country.idx))
  if(replacement.needs.rebuild){
    # Check if required regional aggregate files exist before parallel processing
    required.files <- c(paste0(replace.rates.reg,"_u5mr.rtj.rda"),
                       paste0(replace.rates.reg,"_imr.rtj.rda"))
    if(!is.null(nmr.ctj)) {
      required.files <- c(required.files, paste0(replace.rates.reg,"_nmr.rtj.rda"))
    }
    missing.files <- required.files[!file.exists(file.path(output.dir.samplescombined, required.files))]
    if(length(missing.files) > 0) {
      stop(paste0("Error: Required regional aggregate files are missing from ", output.dir.samplescombined, ":\n  ",
                  paste(missing.files, collapse="\n  "),
                  "\nPlease run regional aggregates first before calculating country results with replaced missing rates."))
    }
    # if (sum(!file.exists(file.path(output.dir.samplescombined, files.coun0try))) > 0) {
    cat(paste0("Generating country results (missing rates replaced with ", replace.rates.reg, ")...\n"))
    if (run.on.server) {
      replace.progress.start <- proc.time()[["elapsed"]]
      replace.chunks <- BuildProgressChunksBWC(
        total = nsim,
        chunk.size = ResolveProgressChunkSizeBWC(
          total = nsim,
          parallel.cores = parallel.cores.resolved,
          progress.every = 1000L
        )
      )
      for (chunk in replace.chunks) {
        registerDoMC(cores = min(parallel.cores.resolved, length(chunk)))
        foreach (j = chunk) %dopar% {
          CalculateCountryDeathsBWC.replacemissingrates(j = j, u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                                        livebirths.ct = lb.ct,
                                                        a0.c = a0.c, a1to4.c = a1to4.c,
                                                        pop0.orig.ct = pop0.orig.ct, pop1to4.orig.ct = pop1to4.orig.ct,
                                                        iso.c = iso.c, est.years = est.years,
                                                        year1 = year1, year2 = year2, year4 = year4,
                                                        year.target = year.target, factor.target = factor.target, ndigits = ndigits,
                                                        output.dir = output.dir.samples,
                                                        output.dir.samplescombined = output.dir.samplescombined,
                                                        replace.rates.reg=replace.rates.reg,
                                                        replace.rates.cat=replace.rates.cat,
                                                        country.ctx = country.calc.ctx,
                                                        selected.country.idx = selected.country.idx)
        }
        LogRoutProgressBWC(stage = paste0("replacement-country generation: ", replace.rates.reg),
                           completed = max(chunk),
                           total = nsim,
                           start.time = replace.progress.start)
      }
    } else {
      progress.bar <- CreateLoopProgressBarBWC(nsim)
      on.exit(CloseLoopProgressBarBWC(progress.bar), add = TRUE)
      for (j in seq_len(nsim)) {
        CalculateCountryDeathsBWC.replacemissingrates(j = j, u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                                      livebirths.ct = lb.ct,
                                                      a0.c = a0.c, a1to4.c = a1to4.c,
                                                      pop0.orig.ct = pop0.orig.ct, pop1to4.orig.ct = pop1to4.orig.ct,
                                                      iso.c = iso.c, est.years = est.years,
                                                      year1 = year1, year2 = year2, year4 = year4,
                                                      year.target = year.target, factor.target = factor.target, ndigits = ndigits,
                                                      output.dir = output.dir.samples,
                                                      output.dir.samplescombined = output.dir.samplescombined,
                                                      replace.rates.reg=replace.rates.reg,
                                                      replace.rates.cat=replace.rates.cat,
                                                      country.ctx = country.calc.ctx,
                                                      selected.country.idx = selected.country.idx)
        UpdateLoopProgressBarBWC(progress.bar, j)
      }
      CloseLoopProgressBarBWC(progress.bar)
      progress.bar <- NULL
    }
    cat(paste0("Combining and outputting country results (missing rates replaced with ", replace.rates.reg, ")...\n"))
    CombineAndOutputCountryResults.BWC.replacemissingrates(u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                                       country.info = country.info,
                                                       percentiles = percentiles, ndigits = ndigits,
                                                       output.dir = output.dir,
                                                       output.dir.samples = output.dir.samples,
                                                       output.dir.samplescombined = output.dir.samplescombined,
                                                       replace.rates.reg=replace.rates.reg,
                                                       selected.country.idx = selected.country.idx)
  } else {
    if(is.null(replace.rates.reg)){
      cat(paste0("Did not generate country results with missing rates replaced...\n"))
    } else {
      sapply(files.country.replace, LoadFile, output.dir = output.dir.samplescombined,
             envir = environment())
      if (isTRUE(reuse.replacement.country)) {
        cat(paste0("Reusing existing replacement-country cache for ", replace.rates.reg,
                   " from ", output.dir.samplescombined, ".\n"))
      } else {
        cat(paste("Country results (with missing rates replaced with regional) loaded from ", output.dir.samplescombined, "\n"))
      }
    }
  }
  #-------------------------------------------------------------------------
  # get world results
  world.phase.start <- proc.time()[["elapsed"]]
  if(get.world.results){
    if (isTRUE(replacement.needs.rebuild)) {
      DeleteWorldOutputsBWC(
        output.dir.samplescombined = output.dir.samplescombined,
        nn.exists = !is.null(nmr.ctj)
      )
    }
    if (!is.null(selected.country.idx) && !is.null(replace.rates.reg)) {
      DeleteWorldOutputsBWC(
        output.dir.samplescombined = output.dir.samplescombined,
        nn.exists = !is.null(nmr.ctj)
      )
    }
    if (isTRUE(ordinary.selected.active)) {
      DeleteWorldOutputsBWC(
        output.dir.samplescombined = output.dir.samplescombined,
        nn.exists = !is.null(nmr.ctj)
      )
    }
    if(is.null(nmr.ctj)){
      files.world <- c("res.world.rda", "u5mr.wtj.rda", "imr.wtj.rda",
                       "deathu5.all.wtj.rda", "death0.all.wtj.rda",
                       "pop0.wt.rda", "pop1to4.wt.rda",
                       "pop0.orig.wt.rda", "pop1to4.orig.wt.rda", "popu5.orig.wt.rda",
                       #"livebirths.wt.rda",
                       "coverage0.wt.rda", "coverageu5.wt.rda") # change JR, 26 Aug 2013
    } else {
      files.world <- c("res.world.rda", "u5mr.wtj.rda", "imr.wtj.rda", "nmr.wtj.rda",
                       "deathu5.all.wtj.rda", "death0.all.wtj.rda", "deathnn.all.wtj.rda",
                       "pop0.wt.rda", "pop1to4.wt.rda",
                       "pop0.orig.wt.rda", "pop1to4.orig.wt.rda", "popu5.orig.wt.rda",
                       #"livebirths.wt.rda",
                       "coverage0.wt.rda", "coverageu5.wt.rda")
    }
    if (isTRUE(output.rates.of.decline)) {
      files.world <- c(files.world, "global.RoDs.ui.rda")
    }
    
    if (sum(!file.exists(file.path(output.dir.samplescombined, files.world))) > 0) {
      if(is.null(replace.rates.reg)){
        cat(paste("Generating world results...\n"))
      } else {
        cat(paste0("Generating world results with country deaths calculated with missing historical rates replaced with ",replace.rates.reg,"...\n"))
      }
      # CalculateWorldDeaths(output.dir.samplescombined = output.dir.samplescombined,
      #                      output.dir = output.dir,
      #                      percentiles = percentiles,
      #                      ndigits = ndigits)
      CalculateWorldDeathsBWC(output.dir.samplescombined = output.dir.samplescombined,
                              output.dir.samples = output.dir.samples,
                              output.dir = output.dir,
                              percentiles = percentiles,
                              ndigits = ndigits,
                              run.on.server = run.on.server,
                              parallel.cores = parallel.cores.resolved,
                              replace.rates.reg = replace.rates.reg,
                              output.rates.of.decline = output.rates.of.decline,
                              round.output = round.output)
      
      cat(paste("Output generated for world.\n"))
    } else {
      sapply(files.world, LoadFile, output.dir = output.dir.samplescombined,
             envir = environment())
      cat(paste("World results loaded from ", output.dir.samplescombined, "\n"))
    }
  } # if(get.world.results)
  phase.times$world <- GetPhaseElapsedBWC(world.phase.start)
  #-------------------------------------------------------------------------
  # get regional results
  regional.phase.start <- proc.time()[["elapsed"]]
  selected.iso.regional <- if (!is.null(selected.country.idx)) iso.c[selected.country.idx] else NULL
  if (!is.null(regiontypes.select)) {    #PROBABLY DELETE THIS ONE
    cat(paste("Generating regional results...\n"))
    if (is.element("Adhoc", regiontypes.select)) {
      adhoc.selection <- ResolveAdhocRegionSelectionBWC(country.info)
      if (is.null(adhoc.selection)) {
        warning("Adhoc regional output requested, but no populated AdhocCountries columns were found in country.info.")
      } else {
        cat(paste0("Processing ", length(adhoc.selection$regiontypes), " adhoc regions.\n"))
        GetRegionalResultsBWC(regiontypes = adhoc.selection$regiontypes,
                              regions = adhoc.selection$regions,
                              filename = "AdhocCountries",
                              output.dir = output.dir, output.dir.samples = output.dir.samples,
                              output.dir.samplescombined = output.dir.samplescombined,
                              run.on.server = run.on.server,
                              percentiles = percentiles, ndigits = ndigits,
                              replace.rates.reg = replace.rates.reg,
                              parallel.cores = parallel.cores.resolved,
                              round.output = round.output)
      }
    }
    if (is.element("SDGSimple", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = SDGSimpleRegionAll,
                            regions = country.info[, grepl("SDGSimple", colnames(country.info))],
                            filename = "SDGSimpleRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            parallel.cores = parallel.cores.resolved,
                            round.output = round.output)
    if (is.element("SDG2015", regiontypes.select)) # only select those with data after 2015
      GetRegionalResultsBWC(regiontypes = SDG2015RegionAll,
                            regions = country.info[, grepl("SDG2015", colnames(country.info))],
                            filename = "SDG2015",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            parallel.cores = parallel.cores.resolved,
                            round.output = round.output)
    if (is.element("UNICEFProg", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = UNICEFProgRegionAll, ## func at end; think about new regions
                            regions = country.info[, grepl("UNICEFProg", colnames(country.info))],
                            filename = "UNICEFProgRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            parallel.cores = parallel.cores.resolved,
                            round.output = round.output)
    if (is.element("UNICEFReport", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = UNICEFReportRegionAll,
                            regions = country.info[, grepl("UNICEFReport", colnames(country.info))],
                            filename = "UNICEFReportRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            parallel.cores = parallel.cores.resolved,
                            round.output = round.output)
    if (is.element("UNICEFReport_nohigh", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = UNICEFReportRegionAll_nohigh,
                            regions = country.info[, grepl("UNICEFReportRegion_nohigh", colnames(country.info))],
                            filename = "UNICEFReportRegion_nohigh",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            parallel.cores = parallel.cores.resolved,
                            round.output = round.output)
    if (is.element("Wealthall", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = WealthallRegionAll,
                            regions = country.info[, grepl("Wealthall", colnames(country.info))],
                            filename = "Wealthall",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("WealthallGlobal", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = WealthallGlobalAll,
                            regions = country.info[, grepl("WealthallGlobal", colnames(country.info))],
                            filename = "WealthallGlobal",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Wealthdata", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = WealthdataRegionAll,
                            regions = country.info[, grepl("Wealthdata", colnames(country.info))],
                            filename = "Wealthdata",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("WealthdataGlobal", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = WealthdataGlobalAll,
                            regions = country.info[, grepl("WealthdataGlobal", colnames(country.info))],
                            filename = "WealthdataGlobal",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("MDG", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = MDGRegionAll,
                            regions = country.info[, grepl("MDG", colnames(country.info))],
                            filename = "MDGRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("SDG", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = SDGRegionAll,
                            regions = country.info[, grepl("SDG", colnames(country.info))],
                            filename = "SDGRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("WHO", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = WHORegionAll,
                            regions = country.info[, grepl("WHO", colnames(country.info))],
                            filename = "WHORegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("WB", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = WBRegionAll,
                            regions = country.info[, grepl("WB", colnames(country.info))],
                            filename = "WBRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    # if (is.element("WorldBankReg2", regiontypes.select))
    #   GetRegionalResultsBWC(regiontypes = WorldBankReg2All,
    #                         regions = country.info[, grepl("WorldBankReg2", colnames(country.info))],
    #                         filename = "WorldBankReg2",
    #                         output.dir = output.dir, output.dir.samples = output.dir.samples,
    #                         output.dir.samplescombined = output.dir.samplescombined,
    #                         run.on.server = run.on.server,
    #                         percentiles = percentiles, ndigits = ndigits,
    #                          replace.rates.reg = replace.rates.reg,
    # round.output = round.output)
    # if (is.element("NewWorldBank", regiontypes.select))
    #   GetRegionalResultsBWC(regiontypes = NewWorldBankAll,
    #                         regions = country.info[, grepl("NewWorldBank", colnames(country.info))],
    #                         filename = "NewWorldBank",
    #                         output.dir = output.dir, output.dir.samples = output.dir.samples,
    #                         output.dir.samplescombined = output.dir.samplescombined,
    #                         run.on.server = run.on.server,
    #                         percentiles = percentiles, ndigits = ndigits,
    #                          replace.rates.reg = replace.rates.reg,
    # round.output = round.output)
    if (is.element("UNPD", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = UNPDRegionAll,
                            regions = country.info[, grepl("UNPD", colnames(country.info))],
                            filename = "UNPDRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("OIC", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = OICRegionAll,
                            regions = country.info[, grepl("OIC", colnames(country.info))],
                            filename = "OICRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Countdown", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = CountdownAll,
                            regions = country.info[, grepl("Countdown", colnames(country.info))],
                            filename = "CountdownCountries",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("ECAAfrica", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = ECAAfricaRegionAll,
                            regions = country.info[, grepl("ECAAfrica", colnames(country.info))],
                            filename = "ECAAfricaRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("AU", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = AURegionAll,
                            regions = country.info[, grepl("AURegion", colnames(country.info))],
                            filename = "AURegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("AUREC", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = AURECRegionAll,
                            regions = country.info[, grepl("AURECRegion", colnames(country.info))],
                            filename = "AURECRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Fragile2012", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2012All,
                            regions = country.info[, grepl("FragileCountries2012", colnames(country.info))],
                            filename = "FragileCountries2012",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Fragile2013", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2013All,
                            regions = country.info[, grepl("FragileCountries2013", colnames(country.info))],
                            filename = "FragileCountries2013",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Fragile2014", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2014All,
                            regions = country.info[, grepl("FragileCountries2014", colnames(country.info))],
                            filename = "FragileCountries2014",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Fragile2015", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2015All,
                            regions = country.info[, grepl("FragileCountries2015", colnames(country.info))],
                            filename = "FragileCountries2015",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Fragile2017", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2017All,
                            regions = country.info[, grepl("FragileCountries2017", colnames(country.info))],
                            filename = "FragileCountries2017",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Fragile2018", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2018All,
                            regions = country.info[, grepl("FragileCountries2018", colnames(country.info))],
                            filename = "FragileCountries2018",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Fragile2018OECD1", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2018OECD1All,
                            regions = country.info[, grepl("FragileCountries2018OECD1", colnames(country.info))],
                            filename = "FragileCountries2018OECD1",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Fragile2018OECD2", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2018OECD2All,
                            regions = country.info[, grepl("FragileCountries2018OECD2", colnames(country.info))],
                            filename = "FragileCountries2018OECD2",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("Fragile2019", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2018All,
                            regions = country.info[, grepl("FragileCountries2019", colnames(country.info))],
                            filename = "FragileCountries2019",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("JHUFragile2021", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = JHUFragile2021All,
                            regions = country.info[, grepl("JHUFragile2021", colnames(country.info))],
                            filename = "JHUFragile2021",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("FCSCountries", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = FCSCountriesAll,
                            regions = country.info[, grepl("FCSCountries", colnames(country.info))],
                            filename = "FCSCountries",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("FragileCountries2025OECD", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2025OECDRegionAll,
                            regions = country.info[, grepl("FragileCountries2025OECD", colnames(country.info))],
                            filename = "FragileCountries2025OECD",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    # if (is.element("Fragile2018OECD", regiontypes.select))
    #   GetRegionalResultsBWC(regiontypes = Fragile2018OECDRegionAll,
    #                         regions = country.info[, grepl("Fragile2018OECD", colnames(country.info))],
    #                         filename = "Fragile2018OECDRegion",
    #                         output.dir = output.dir, output.dir.samples = output.dir.samples,
    #                         output.dir.samplescombined = output.dir.samplescombined,
    #                         run.on.server = run.on.server,
    #                         percentiles = percentiles, ndigits = ndigits,
    #                         replace.rates.reg = replace.rates.reg,
    # round.output = round.output)
    if (is.element("USAID", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = USAIDAll,
                            regions = country.info[, grepl("USAID", colnames(country.info))],
                            filename = "USAIDCountries",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("ECA", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = ECAAll,
                            regions = country.info[, grepl("ECACountries", colnames(country.info))],
                            filename = "ECACountries",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("GAVI", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = GAVIAll,
                            regions = country.info[, grepl("GAVICountries", colnames(country.info))],
                            filename = "GAVICountries",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("GlobalStrategy", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = GlobalStrategyAll,
                            regions = country.info[, grepl("GlobalStrategy", colnames(country.info))],
                            filename = "GlobalStrategyCountries",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("M49", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = M49RegionAll,
                            regions = country.info[, grepl("M49", colnames(country.info))],
                            filename = "M49Region",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            selected.iso = selected.iso.regional,
                            round.output = round.output)
    if (is.element("AfricanEconomicCommunity", regiontypes.select)) # WCARO economic communities
      GetRegionalResultsBWC(regiontypes = AfricanEconomicCommunityAll,
                            regions = country.info[, grepl("AfricanEconomicCommunity", colnames(country.info))],
                            filename = "AfricanEconomicCommunityRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("SPhumanitarian", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = SPhumanitarianAll,
                            regions = country.info[, grepl("SPhumanitarian", colnames(country.info))],
                            filename = "SPhumanitarianRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("SPhighburden", regiontypes.select)) # WCARO economic communities
      GetRegionalResultsBWC(regiontypes = SPhighburdenAll,
                            regions = country.info[, grepl("SPhighburden", colnames(country.info))],
                            filename = "SPhighburdenRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("SDGRC", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = SDGRCRegionAll,
                            regions = country.info[, grepl("SDGRCRegion", colnames(country.info))],
                            filename = "SDGRCRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("EAPRO", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = EAPRORegionAll,
                            regions = country.info[, grepl("EAPRORegion", colnames(country.info))],
                            filename = "EAPRORegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("JHU", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = JHUAll,
                            regions = country.info[, grepl("JHU", colnames(country.info))],
                            filename = "JHURegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("LiST_all", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = LiST_allAll,
                            regions = country.info[, grepl("LiST_all", colnames(country.info))],
                            filename = "LiSTRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("MENAEMRO", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = MENAEMRORegionAll,
                            regions = country.info[, grepl("MENAEMRO", colnames(country.info))],
                            filename = "MENAEMRORegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    if (is.element("EECA", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = EECARegionAll,
                            regions = country.info[, grepl("EECA", colnames(country.info))],
                            filename = "EECARegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
  }
  phase.times$regional <- GetPhaseElapsedBWC(regional.phase.start)
  phase.times$total <- GetPhaseElapsedBWC(overall.start)
  cat("OutputAggregates timing summary:\n")
  cat(sprintf("  preprocess    %7.2f s\n", phase.times$preprocess))
  cat(sprintf("  country       %7.2f s\n", phase.times$country))
  cat(sprintf("  world         %7.2f s\n", phase.times$world))
  cat(sprintf("  regional      %7.2f s\n", phase.times$regional))
  cat(sprintf("  total         %7.2f s\n", phase.times$total))
}
#-------------------------------------------------------------------------
# What it does: Calculates country-level deaths and cohort arrays for one simulation draw.
# Why it is needed: Country draw outputs are the foundation for all later world and regional aggregation steps.
CalculateCountryDeathsBWC <- function(
    j, ##<< Index number of trajectory.
    u5mr.ctj,
    imr.ctj,
    nmr.ctj=NULL,
    a0.c,
    a1to4.c,
    pop0.orig.ct,
    pop1to4.orig.ct,
    livebirths.ct,
    iso.c,
    est.years,
    year1,
    year2,
    year4,
    year.target, ## final year of estimates
    factor.target,
    ndigits,
    output.dir,
    country.ctx,
    output.dir.samplescombined = NULL,
    selected.country.idx = NULL
) {
  pop0.ct <- pop0.orig.ct
  pop1to4.ct <- pop1to4.orig.ct
  # set population to 0 if rate data not available
  arr.ind.select <- which(is.na(imr.ctj[, , 1]) | is.na(u5mr.ctj[, , 1]), arr.ind = TRUE)
  # arr.ind.select.nmr <- which(is.na(imr.ctj[, , 1]) | is.na(u5mr.ctj[, , 1]) | is.na(nmr.ctj[, , 1]), arr.ind = TRUE)
  pop0.ct[arr.ind.select] <- 0
  pop1to4.ct[arr.ind.select] <- 0
  
  # edit DJS 2018-03-09 remove rounding for median calculation
  # round off to 1 d.p. before calculation (for median only)
  # if (dim(u5mr.ctj)[3] == 1) {
  #   u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
  #   imr.ctj <- roundoff(imr.ctj, digits = ndigits)
  #   if(!is.null(nmr.ctj)) nmr.ctj <- roundoff(nmr.ctj, digits = ndigits)
  # }
  u5mr.ct <- u5mr.ctj[, , j]
  imr.ct <- imr.ctj[, , j]
  if(!is.null(nmr.ctj)){
    nmr.ct <- nmr.ctj[, , j]
  }
  # end DJS edit 2018-03-09
  
  C <- nrow(u5mr.ct)
  nyears <- ncol(u5mr.ct)
  if (is.null(selected.country.idx)) {
    ifelse(is.null(nmr.ctj), death0.ct <- death1to4.ct <- deathu5.ct <- matrix(NA, C, nyears), death0.ct <- death1to4.ct <- deathu5.ct <- deathnn.ct <- matrix(NA, C, nyears))
    
    years <- country.ctx$years
    dx.array.by.c <- array(NA, dim=c(3, country.ctx$total.week.cols, nrow(u5mr.ct)))
    lx.array.by.c <- array(NA, dim=c(3, country.ctx$total.week.cols, nrow(u5mr.ct)))
    if(!is.null(nmr.ctj)){
      dx.nn.array.by.c <- array(NA, dim=c(2, country.ctx$total.week.cols, nrow(u5mr.ct)))
      lx.nn.array.by.c <- array(NA, dim=c(2, country.ctx$total.week.cols, nrow(u5mr.ct)))
    }
  } else {
    death0.ct <- LoadReplacementCountryTemplateWithFallbackBWC(
      sample.path = file.path(output.dir, paste0("death0.ct_", j, ".rda")),
      sample.object.name = "death0.ct",
      combined.path = file.path(output.dir.samplescombined, "death0.ctj.rda"),
      combined.object.name = "death0.ctj",
      slice.index = j
    )
    death1to4.ct <- LoadReplacementCountryTemplateWithFallbackBWC(
      sample.path = file.path(output.dir, paste0("death1to4.ct_", j, ".rda")),
      sample.object.name = "death1to4.ct",
      combined.path = file.path(output.dir.samplescombined, "death1to4.ctj.rda"),
      combined.object.name = "death1to4.ctj",
      slice.index = j
    )
    deathu5.ct <- LoadReplacementCountryTemplateWithFallbackBWC(
      sample.path = file.path(output.dir, paste0("deathu5.ct_", j, ".rda")),
      sample.object.name = "deathu5.ct",
      combined.path = file.path(output.dir.samplescombined, "deathu5.ctj.rda"),
      combined.object.name = "deathu5.ctj",
      slice.index = j
    )
    dx.array.by.c <- LoadReplacementCountryTemplateWithFallbackBWC(
      sample.path = file.path(output.dir, paste0("dx.array.ct_", j, ".rda")),
      sample.object.name = "dx.array.by.c",
      combined.path = file.path(output.dir.samplescombined, "dx.array.ctj.rda"),
      combined.object.name = "dx.array.ctj",
      slice.index = j
    )
    lx.array.by.c <- LoadReplacementCountryTemplateWithFallbackBWC(
      sample.path = file.path(output.dir, paste0("lx.array.ct_", j, ".rda")),
      sample.object.name = "lx.array.by.c",
      combined.path = file.path(output.dir.samplescombined, "lx.array.ctj.rda"),
      combined.object.name = "lx.array.ctj",
      slice.index = j
    )
    if(!is.null(nmr.ctj)){
      deathnn.ct <- LoadReplacementCountryTemplateWithFallbackBWC(
        sample.path = file.path(output.dir, paste0("deathnn.ct_", j, ".rda")),
        sample.object.name = "deathnn.ct",
        combined.path = file.path(output.dir.samplescombined, "deathnn.ctj.rda"),
        combined.object.name = "deathnn.ctj",
        slice.index = j
      )
      dx.nn.array.by.c <- LoadReplacementCountryTemplateWithFallbackBWC(
        sample.path = file.path(output.dir, paste0("dx.nn.array.ct_", j, ".rda")),
        sample.object.name = "dx.nn.array.by.c",
        combined.path = file.path(output.dir.samplescombined, "dx.nn.array.ctj.rda"),
        combined.object.name = "dx.nn.array.ctj",
        slice.index = j
      )
      lx.nn.array.by.c <- LoadReplacementCountryTemplateWithFallbackBWC(
        sample.path = file.path(output.dir, paste0("lx.nn.array.ct_", j, ".rda")),
        sample.object.name = "lx.nn.array.by.c",
        combined.path = file.path(output.dir.samplescombined, "lx.nn.array.ctj.rda"),
        combined.object.name = "lx.nn.array.ctj",
        slice.index = j
      )
    }
    years <- country.ctx$years
  }
  year1.est.u5 <- rep(NA, C)
  year1.est.u1 <- rep(NA, C)
  year1.est.nn <- rep(NA, C)
  
  # k loop for countries
  k.seq <- if (is.null(selected.country.idx)) seq_len(nrow(u5mr.ct)) else selected.country.idx
  for(k in k.seq){
    country.life.table <- BuildCountryLifeTableBWC(
      u5mr.row = u5mr.ct[k, ],
      imr.row = imr.ct[k, ],
      nmr.row = if (!is.null(nmr.ctj)) nmr.ct[k, ] else NULL,
      livebirths.row = livebirths.ct[k, ],
      country.ctx = country.ctx
    )
    if (is.null(country.life.table)) next
    years.k <- country.life.table$years.k
    year1.est.u5[k] <- country.life.table$year1.est.u5
    year1.est.u1[k] <- country.life.table$year1.est.u1
    if(!is.null(nmr.ctj)) year1.est.nn[k] <- country.life.table$year1.est.nn
    
    dx.mat.k <- country.life.table$dx.mat.k
    lx.mat.k <- country.life.table$lx.mat.k
    cols.k <- country.life.table$cols.k
    dx.array.by.c[,cols.k,k] <- dx.mat.k[1:3,, drop = FALSE]
    lx.array.by.c[,cols.k,k] <- lx.mat.k[1:3,, drop = FALSE]
    if(!is.null(nmr.ctj)){
      dx.nn.mat.k <- country.life.table$dx.nn.mat.k
      lx.nn.mat.k <- country.life.table$lx.nn.mat.k
      dx.nn.array.by.c[,cols.k,k] <- dx.nn.mat.k[1:2,, drop = FALSE]
      lx.nn.array.by.c[,cols.k,k] <- lx.nn.mat.k[1:2,, drop = FALSE]
    }
    
    ## sum deaths by year
    years.k.mat <- country.life.table$years.k.mat
    for(yrk in (year1.est.u5[k]+5):max(years.k)){
      deathu5.ct[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T)
    } # for loop for u5 deaths
    for(yrk in (year1.est.u1[k]+1):max(years.k)){
      death0.ct[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
    } # for loop for infant deaths
    death1to4.ct[k,] <- deathu5.ct[k,]-death0.ct[k,]
    if(!is.null(nmr.ctj)){
      for(yrk in (year1.est.nn[k]+1):max(years.k)){
        deathnn.ct[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
      } ## for loop nn deaths
    } # if
  } # k loop: countries
  
  # # set deaths to NA if rate data is not available, and for first 5 years for U5MR and 1 year for IMR and NMR
  # --- REPLACED in k loop with year1.est.XX to calculate deaths only at year1.est.nn+1 for neoanatal and infant deaths and year1.est.nn+5 for U5 deaths; need t-5 years of cohorts surviving for complete under-5 deaths with BWC
  # death0.ct[arr.ind.select] <- NA
  # death1to4.ct[arr.ind.select] <- NA
  # deathu5.ct[arr.ind.select] <- NA
  
  if (!file.exists(file.path(output.dir, "info.rda"))) {
    info <- list(iso.c = iso.c,
                 C = C,
                 est.years = est.years,
                 est.years.floor = est.years-0.5,
                 year1.est.nn = year1.est.nn, ## will be NA if no nmr
                 year1.est.u1 = year1.est.u1,
                 year1.est.u5 = year1.est.u5,
                 nyears = nyears,
                 a0.c = a0.c,
                 a1to4.c = a1to4.c,
                 pop0.ct = pop0.ct,
                 pop1to4.ct = pop1to4.ct,
                 pop0.orig.ct = pop0.orig.ct,
                 pop1to4.orig.ct = pop1to4.orig.ct,
                 livebirths.ct = livebirths.ct,
                 year1 = year1,
                 year2 = year2,
                 year4 = year4,
                 year.target = year.target,
                 factor.target = factor.target)
    save(info, file = file.path(output.dir, "info.rda"))
    cat(paste0("Information about the aggregates have been saved to ", output.dir, ".\n"))
  }
  # save samples
  save(death0.ct, file = file.path(output.dir, paste0("death0.ct_", j, ".rda")))
  save(death1to4.ct, file = file.path(output.dir, paste0("death1to4.ct_", j, ".rda")))
  save(deathu5.ct, file = file.path(output.dir, paste0("deathu5.ct_", j, ".rda")))
  save(dx.array.by.c, file = file.path(output.dir, paste0("dx.array.ct_", j, ".rda")))
  save(lx.array.by.c, file = file.path(output.dir, paste0("lx.array.ct_", j, ".rda")))
  if(!is.null(nmr.ctj)){
    save(deathnn.ct, file = file.path(output.dir, paste0("deathnn.ct_", j, ".rda")))
    save(dx.nn.array.by.c, file = file.path(output.dir, paste0("dx.nn.array.ct_", j, ".rda")))
    save(lx.nn.array.by.c, file = file.path(output.dir, paste0("lx.nn.array.ct_", j, ".rda")))
  }
}
#-------------------------------------------------------------------------
# What it does: Recalculates one country draw after filling missing country rates with a chosen regional aggregate.
# Why it is needed: Replacement mode lets the pipeline produce complete historical deaths when country trajectories have gaps.
CalculateCountryDeathsBWC.replacemissingrates <- function( # DJS add 2018-07-24 for using defined historical regional rates to replace missing country rates -- this function requires existing regional aggregate output for replacement
  j, ##<< Index number of trajectory.
  u5mr.ctj,
  imr.ctj,
  nmr.ctj=NULL, # if NULL will only calculate U5 and U1 deaths; supplying NMR wil give nn deaths from seperate BWC calculation for NMR
  a0.c,
  a1to4.c,
  pop0.orig.ct,
  pop1to4.orig.ct,
  livebirths.ct,
  iso.c,
  est.years,
  year1,
  year2,
  year4,
  year.target, ## final year of estimates
  factor.target,
  ndigits,
  output.dir,
  output.dir.samplescombined,
  replace.rates.reg, # Regional Aggregate to use for replacing -- must be one of regiontypes.select
  replace.rates.cat,  # Regional categories from aggregate (e.g. M49Region1) -- must be vector with 1 regional category for each country and categories must be from replace.rates.reg
  country.ctx,
  selected.country.idx = NULL
) {
  pop0.ct <- pop0.orig.ct
  pop1to4.ct <- pop1to4.orig.ct
  # set population to 0 if rate data not available
  arr.ind.select <- which(is.na(imr.ctj[, , 1]) | is.na(u5mr.ctj[, , 1]), arr.ind = TRUE)
  # arr.ind.select.nmr <- which(is.na(imr.ctj[, , 1]) | is.na(u5mr.ctj[, , 1]) | is.na(nmr.ctj[, , 1]), arr.ind = TRUE)
  pop0.ct[arr.ind.select] <- 0
  pop1to4.ct[arr.ind.select] <- 0
  
  nn.exists <- !is.null(nmr.ctj)
  
  regions.constant <- replace.rates.cat
  
  # edit DJS 2018-03-09 remove rounding for median calculation
  # round off to 1 d.p. before calculation (for median only)
  # if (dim(u5mr.ctj)[3] == 1) {
  #   u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
  #   imr.ctj <- roundoff(imr.ctj, digits = ndigits)
  #   if(!is.null(nmr.ctj)) nmr.ctj <- roundoff(nmr.ctj, digits = ndigits)
  # }
  u5mr.ct <- u5mr.ctj[, , j]
  imr.ct <- imr.ctj[, , j]
  if(nn.exists){
    nmr.ct <- nmr.ctj[, , j]
  }
  # end DJS edit 2018-03-09
  
  # get deaths for calculating replace.rates.reg
  load(file.path(output.dir.samplescombined, paste0(replace.rates.reg,"_u5mr.rtj.rda")))
  u5mr.replace.rt <- u5mr.rtj[,,j] # will have same columns as u5mr.ct
  rm(u5mr.rtj)
  rownames(u5mr.replace.rt) <- M49RegionAll
  load(file.path(output.dir.samplescombined, paste0(replace.rates.reg,"_imr.rtj.rda")))
  imr.replace.rt <- imr.rtj[,,j]
  rm(imr.rtj)
  if(nn.exists){
    load(file.path(output.dir.samplescombined, paste0(replace.rates.reg,"_nmr.rtj.rda")))
    nmr.replace.rt <- nmr.rtj[,,j]
    rm(nmr.rtj)
  }
  
  
  C <- nrow(u5mr.ct)
  nyears <- ncol(u5mr.ct)
  sample.suffix <- paste0("_", replace.rates.reg, "-replace.rda")
  if (is.null(selected.country.idx)) {
    ifelse(is.null(nmr.ctj), death0.ct <- death1to4.ct <- deathu5.ct <- matrix(NA, C, nyears), death0.ct <- death1to4.ct <- deathu5.ct <- deathnn.ct <- matrix(NA, C, nyears))
    dx.array.by.c <- array(NA, dim=c(3, country.ctx$total.week.cols, nrow(u5mr.ct)))
    lx.array.by.c <- array(NA, dim=c(3, country.ctx$total.week.cols, nrow(u5mr.ct)))
    if(!is.null(nmr.ctj)){
      dx.nn.array.by.c <- array(NA, dim=c(2, country.ctx$total.week.cols, nrow(u5mr.ct)))
      lx.nn.array.by.c <- array(NA, dim=c(2, country.ctx$total.week.cols, nrow(u5mr.ct)))
    }
  } else {
    death0.ct <- LoadReplacementCountryTemplateWithFallbackBWC(
      sample.path = file.path(output.dir, paste0("death0.ct_", j, sample.suffix)),
      sample.object.name = "death0.ct",
      combined.path = file.path(output.dir.samplescombined, paste0("death0.ctj.", replace.rates.reg, "-replace.rda")),
      combined.object.name = "death0.ctj",
      slice.index = j
    )
    death1to4.ct <- LoadReplacementCountryTemplateWithFallbackBWC(
      sample.path = file.path(output.dir, paste0("death1to4.ct_", j, sample.suffix)),
      sample.object.name = "death1to4.ct",
      combined.path = file.path(output.dir.samplescombined, paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda")),
      combined.object.name = "death1to4.ctj",
      slice.index = j
    )
    deathu5.ct <- LoadReplacementCountryTemplateWithFallbackBWC(
      sample.path = file.path(output.dir, paste0("deathu5.ct_", j, sample.suffix)),
      sample.object.name = "deathu5.ct",
      combined.path = file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")),
      combined.object.name = "deathu5.ctj",
      slice.index = j
    )
    dx.array.by.c <- LoadReplacementCountryTemplateWithFallbackBWC(
      sample.path = file.path(output.dir, paste0("dx.array.ct_", j, sample.suffix)),
      sample.object.name = "dx.array.by.c",
      combined.path = file.path(output.dir.samplescombined, paste0("dx.array.ctj_", replace.rates.reg, "-replace.rda")),
      combined.object.name = "dx.array.ctj",
      slice.index = j
    )
    lx.array.by.c <- LoadReplacementCountryTemplateWithFallbackBWC(
      sample.path = file.path(output.dir, paste0("lx.array.ct_", j, sample.suffix)),
      sample.object.name = "lx.array.by.c",
      combined.path = file.path(output.dir.samplescombined, paste0("lx.array.ctj_", replace.rates.reg, "-replace.rda")),
      combined.object.name = "lx.array.ctj",
      slice.index = j
    )
    if(!is.null(nmr.ctj)){
      deathnn.ct <- LoadReplacementCountryTemplateWithFallbackBWC(
        sample.path = file.path(output.dir, paste0("deathnn.ct_", j, sample.suffix)),
        sample.object.name = "deathnn.ct",
        combined.path = file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")),
        combined.object.name = "deathnn.ctj",
        slice.index = j
      )
      dx.nn.array.by.c <- LoadReplacementCountryTemplateWithFallbackBWC(
        sample.path = file.path(output.dir, paste0("dx.nn.array.ct_", j, sample.suffix)),
        sample.object.name = "dx.nn.array.by.c",
        combined.path = file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj_", replace.rates.reg, "-replace.rda")),
        combined.object.name = "dx.nn.array.ctj",
        slice.index = j
      )
      lx.nn.array.by.c <- LoadReplacementCountryTemplateWithFallbackBWC(
        sample.path = file.path(output.dir, paste0("lx.nn.array.ct_", j, sample.suffix)),
        sample.object.name = "lx.nn.array.by.c",
        combined.path = file.path(output.dir.samplescombined, paste0("lx.nn.array.ctj_", replace.rates.reg, "-replace.rda")),
        combined.object.name = "lx.nn.array.ctj",
        slice.index = j
      )
    }
  }
  
  years <- country.ctx$years
  year1.est.u5 <- rep(NA, C)
  year1.est.u1 <- rep(NA, C)
  year1.est.nn <- rep(NA, C)
  
  # k loop for countries
  k.seq <- if (is.null(selected.country.idx)) seq_len(nrow(u5mr.ct)) else selected.country.idx
  for(k in k.seq){
    replace.row.idx <- match(regions.constant[k], M49RegionAll)
    if (is.na(replace.row.idx)) next
    country.life.table <- BuildCountryLifeTableBWC(
      u5mr.row = u5mr.ct[k, ],
      imr.row = imr.ct[k, ],
      nmr.row = if (!is.null(nmr.ctj)) nmr.ct[k, ] else NULL,
      livebirths.row = livebirths.ct[k, ],
      country.ctx = country.ctx,
      replace.u5mr.row = u5mr.replace.rt[replace.row.idx, ],
      replace.imr.row = imr.replace.rt[replace.row.idx, ],
      replace.nmr.row = if (!is.null(nmr.ctj)) nmr.replace.rt[replace.row.idx, ] else NULL,
      use.full.years = TRUE
    )
    if (is.null(country.life.table)) next
    years.k <- country.life.table$years.k
    year1.est.u5[k] <- country.life.table$year1.est.u5
    year1.est.u1[k] <- country.life.table$year1.est.u1
    if(!is.null(nmr.ctj)) year1.est.nn[k] <- country.life.table$year1.est.nn
    
    dx.mat.k <- country.life.table$dx.mat.k
    lx.mat.k <- country.life.table$lx.mat.k
    cols.k <- country.life.table$cols.k
    dx.array.by.c[,cols.k,k] <- dx.mat.k[1:3,, drop = FALSE]
    lx.array.by.c[,cols.k,k] <- lx.mat.k[1:3,, drop = FALSE]
    if(!is.null(nmr.ctj)){
      dx.nn.mat.k <- country.life.table$dx.nn.mat.k
      lx.nn.mat.k <- country.life.table$lx.nn.mat.k
      dx.nn.array.by.c[,cols.k,k] <- dx.nn.mat.k[1:2,, drop = FALSE]
      lx.nn.array.by.c[,cols.k,k] <- lx.nn.mat.k[1:2,, drop = FALSE]
    }
    
    ## sum deaths by year
    years.k.mat <- country.life.table$years.k.mat
    for(yrk in (years.k[1]+5):max(years.k)){
      deathu5.ct[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T)
    } # for loop for u5 deaths
    for(yrk in (years.k[1]+1):max(years.k)){
      death0.ct[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
    } # for loop for infant deaths
    death1to4.ct[k,] <- deathu5.ct[k,]-death0.ct[k,]
    if(!is.null(nmr.ctj)){
      for(yrk in (years.k[1] + 1):max(years.k)){
        deathnn.ct[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
      } ## for loop nn deaths
    } # if
  } # k loop: countries
  
  # # set deaths to NA if rate data is not available, and for first 5 years for U5MR and 1 year for IMR and NMR
  # --- REPLACED in k loop with year1.est.XX to calculate deaths only at year1.est.nn+1 for neoanatal and infant deaths and year1.est.nn+5 for U5 deaths; need t-5 years of cohorts surviving for complete under-5 deaths with BWC
  # death0.ct[arr.ind.select] <- NA
  # death1to4.ct[arr.ind.select] <- NA
  # deathu5.ct[arr.ind.select] <- NA
  
  # calculate country rates of decline
  # ARR.year1.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year4)
  # ARR.year1.year2.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
  # ARR.year2.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year2, year.end = year4)
  # required.ARR.c <- ifelse(year4 < year.target,
  #                          1/(year.target-year4)*
  #                            log(roundoff(u5mr.ct[, est.years == year1]*factor.target, digits = ndigits)/
  #                                  u5mr.ct[, est.years == year4])*-100, NA)
  # changeinARR.c <- ARR.year2.year4.c - ARR.year1.year2.c
  # decline.year1.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year4)
  # decline.year1.year2.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
  # decline.year2.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year2, year.end = year4)
  
  # if (!file.exists(file.path(output.dir, "info.rda"))) {
  #   info <- list(iso.c = iso.c,
  #                C = C,
  #                est.years = est.years,
  #                est.years.floor = est.years-0.5,
  #                year1.est.nn = year1.est.nn, ## will be NA if no nmr
  #                year1.est.u1 = year1.est.u1,
  #                year1.est.u5 = year1.est.u5,
  #                nyears = nyears,
  #                a0.c = a0.c,
  #                a1to4.c = a1to4.c,
  #                pop0.ct = pop0.ct,
  #                pop1to4.ct = pop1to4.ct,
  #                pop0.orig.ct = pop0.orig.ct,
  #                pop1to4.orig.ct = pop1to4.orig.ct,
  #                livebirths.ct = livebirths.ct,
  #                year1 = year1,
  #                year2 = year2,
  #                year4 = year4,
  #                year.target = year.target,
  #                factor.target = factor.target)
  #   save(info, file = file.path(output.dir, "info.rda"))
  #   cat(paste0("Information about the aggregates have been saved to ", output.dir, ".\n"))
  # }
  # save samples
  save(death0.ct, file = file.path(output.dir, paste0("death0.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  save(death1to4.ct, file = file.path(output.dir, paste0("death1to4.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  save(deathu5.ct, file = file.path(output.dir, paste0("deathu5.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  save(dx.array.by.c, file = file.path(output.dir, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  save(lx.array.by.c, file = file.path(output.dir, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  if(!is.null(nmr.ctj)){
    save(deathnn.ct, file = file.path(output.dir, paste0("deathnn.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    save(dx.nn.array.by.c, file = file.path(output.dir, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    save(lx.nn.array.by.c, file = file.path(output.dir, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  }
  # save(ARR.year1.year4.c, file = file.path(output.dir, paste0("ARR.year1.year4.c_", j, ".rda")))
  # save(ARR.year1.year2.c, file = file.path(output.dir, paste0("ARR.year1.year2.c_", j, ".rda")))
  # save(ARR.year2.year4.c, file = file.path(output.dir, paste0("ARR.year2.year4.c_", j, ".rda")))
  # save(required.ARR.c, file = file.path(output.dir, paste0("required.ARR.c_", j, ".rda")))
  # save(changeinARR.c, file = file.path(output.dir, paste0("changeinARR.c_", j, ".rda")))
  # save(decline.year1.year4.c, file = file.path(output.dir, paste0("decline.year1.year4.c_", j, ".rda")))
  # save(decline.year1.year2.c, file = file.path(output.dir, paste0("decline.year1.year2.c_", j, ".rda")))
  # save(decline.year2.year4.c, file = file.path(output.dir, paste0("decline.year2.year4.c_", j, ".rda")))
}
#-------------------------------------------------------------------------
# What it does: Combines saved country draw files into arrays and writes country summary outputs.
# Why it is needed: The per-draw calculation step saves intermediates, and this function turns them into final country deliverables.
CombineAndOutputCountryResults.BWC <- function(
    u5mr.ctj,
    imr.ctj,
    nmr.ctj=NULL,
    country.info,
    percentiles,
    ndigits,
    output.dir,
    output.dir.samples,
    output.dir.samplescombined,
    output.rates.of.decline,
    round.output,
    selected.country.idx = NULL
) {
  load(file.path(output.dir.samples, "info.rda"))
  list2env(info, envir = environment())
  
  nsim <- dim(u5mr.ctj)[3]
  est.years.floor <- est.years-0.5
  if (!is.null(selected.country.idx)) {
    info$year1.est.u5[selected.country.idx] <- BuildYear1EstimatesFromTrajectoriesBWC(
      rate.ctj = u5mr.ctj,
      years = est.years.floor,
      country.idx = selected.country.idx
    )
    info$year1.est.u1[selected.country.idx] <- BuildYear1EstimatesFromTrajectoriesBWC(
      rate.ctj = imr.ctj,
      years = est.years.floor,
      country.idx = selected.country.idx
    )
    if(!is.null(nmr.ctj)){
      if (is.null(info$year1.est.nn)) {
        info$year1.est.nn <- rep(NA_real_, C)
      }
      info$year1.est.nn[selected.country.idx] <- BuildYear1EstimatesFromTrajectoriesBWC(
        rate.ctj = nmr.ctj,
        years = est.years.floor,
        country.idx = selected.country.idx
      )
    }
  }
  
  # combine all the samples into their respective arrays
  if (is.null(selected.country.idx)) {
    if(is.null(nmr.ctj)){
      death0.ctj<-death1to4.ctj<-deathu5.ctj<-array(NA, c(C, nyears, nsim))
    } else {
      death0.ctj<-death1to4.ctj<-deathu5.ctj<-deathnn.ctj<-array(NA, c(C, nyears, nsim))
    }
  } else {
    load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
    load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
    load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
    if(!is.null(nmr.ctj)){
      load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
    }
  }
  
  ARR.year1.year4.cj <- ARR.year1.year2.cj <- ARR.year2.year4.cj <- required.ARR.cj <- changeinARR.cj <-
    decline.year1.year4.cj <- decline.year1.year2.cj <- decline.year2.year4.cj <- array(NA, c(C, nsim))
  
  # lx.array.ctj <- dx.array.ctj <- array(NA, dim=c(3, nyears*52, C, nsim))
  # if(!is.null(nmr.ctj)) lx.nn.array.ctj <- dx.nn.array.ctj <- array(NA, dim=c(2, nyears*52, C, nsim))
  
  for (j in 1:nsim) {
    load(file.path(output.dir.samples, paste0("death0.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("death1to4.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("deathu5.ct_", j, ".rda")))
    #load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
    #load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
    if (is.null(selected.country.idx)) {
      death0.ctj[, , j] <- death0.ct
      death1to4.ctj[, , j] <- death1to4.ct
      deathu5.ctj[, , j] <- deathu5.ct
    } else {
      death0.ctj[selected.country.idx, , j] <- death0.ct[selected.country.idx, , drop = FALSE]
      death1to4.ctj[selected.country.idx, , j] <- death1to4.ct[selected.country.idx, , drop = FALSE]
      deathu5.ctj[selected.country.idx, , j] <- deathu5.ct[selected.country.idx, , drop = FALSE]
    }
    # dx.array.ctj[,,,j] <- dx.array.by.c
    # lx.array.ctj[,,,j] <- lx.array.by.c
    if(!is.null(nmr.ctj)){
      load(file.path(output.dir.samples, paste0("deathnn.ct_", j, ".rda")))
      if (is.null(selected.country.idx)) {
        deathnn.ctj[, , j] <- deathnn.ct
      } else {
        deathnn.ctj[selected.country.idx, , j] <- deathnn.ct[selected.country.idx, , drop = FALSE]
      }
      # load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
      # load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
      # dx.nn.array.ctj[,,,j] <- dx.nn.array.by.c
      # lx.nn.array.ctj[,,,j] <- lx.nn.array.by.c
    }
  }
  if (isTRUE(output.rates.of.decline)) {
    country.rods <- BuildCountryRatesOfDeclineArraysBWC(
      u5mr.ctj = u5mr.ctj,
      est.years = est.years,
      year1 = year1,
      year2 = year2,
      year4 = year4,
      year.target = year.target,
      factor.target = factor.target,
      ndigits = ndigits
    )
    ARR.year1.year4.cj <- country.rods$ARR.year1.year4.cj
    ARR.year1.year2.cj <- country.rods$ARR.year1.year2.cj
    ARR.year2.year4.cj <- country.rods$ARR.year2.year4.cj
    required.ARR.cj <- country.rods$required.ARR.cj
    changeinARR.cj <- country.rods$changeinARR.cj
    decline.year1.year4.cj <- country.rods$decline.year1.year4.cj
    decline.year1.year2.cj <- country.rods$decline.year1.year2.cj
    decline.year2.year4.cj <- country.rods$decline.year2.year4.cj
  }
  
  rownames(death0.ctj) <- rownames(deathu5.ctj) <- rownames(death1to4.ctj) <- iso.c
  if (isTRUE(output.rates.of.decline)) {
    rownames(ARR.year1.year4.cj) <- rownames(ARR.year1.year2.cj) <- rownames(ARR.year2.year4.cj) <- rownames(required.ARR.cj) <- rownames(changeinARR.cj) <- rownames(decline.year1.year4.cj) <- rownames(decline.year1.year2.cj) <- rownames(decline.year2.year4.cj) <- iso.c
  }
  
  colnames(death0.ctj) <- colnames(death1to4.ctj) <- colnames(deathu5.ctj) <- est.years.floor
  
  if(exists("deathnn.ctj")){
    rownames(deathnn.ctj) <- iso.c
    colnames(deathnn.ctj) <- est.years.floor
  }
  
  # save combined results
  save(death0.ctj, file = file.path(output.dir.samplescombined, "death0.ctj.rda"))
  save(death1to4.ctj, file = file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  save(deathu5.ctj, file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  if (isTRUE(output.rates.of.decline)) {
    save(ARR.year1.year4.cj, file = file.path(output.dir.samplescombined, "ARR.year1.year4.cj.rda"))
    save(ARR.year1.year2.cj, file = file.path(output.dir.samplescombined, "ARR.year1.year2.cj.rda"))
    save(ARR.year2.year4.cj, file = file.path(output.dir.samplescombined, "ARR.year2.year4.cj.rda"))
    save(required.ARR.cj, file = file.path(output.dir.samplescombined, "required.ARR.cj.rda"))
    save(changeinARR.cj, file = file.path(output.dir.samplescombined, "changeinARR.cj.rda"))
    save(decline.year1.year4.cj, file = file.path(output.dir.samplescombined, "decline.year1.year4.cj.rda"))
    save(decline.year1.year2.cj, file = file.path(output.dir.samplescombined, "decline.year1.year2.cj.rda"))
    save(decline.year2.year4.cj, file = file.path(output.dir.samplescombined, "decline.year2.year4.cj.rda"))
  }
  # save(dx.array.ctj, file = file.path(output.dir.samplescombined, "dx.array.ctj.rda"))
  # save(lx.array.ctj, file = file.path(output.dir.samplescombined, "lx.array.ctj.rda"))
  save(info, file = file.path(output.dir.samples, "info.rda"))
  save(info, file = file.path(output.dir.samplescombined, "info.rda"))
  
  if(!is.null(nmr.ctj)){
    save(deathnn.ctj, file = file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
    # save(dx.nn.array.ctj, file = file.path(output.dir.samplescombined, "dx.nn.array.ctj.rda"))
    # save(lx.nn.array.ctj, file = file.path(output.dir.samplescombined, "lx.nn.array.ctj.rda"))
  }
  # delete samples
  unlink(file.path(output.dir.samples, paste0("death0.ct_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("death1to4.ct_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("deathu5.ct_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples, paste0("dx.array.ct_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples, paste0("lx.array.ct_", 1:nsim, ".rda")))
  
  if(!is.null(nmr.ctj)){
    unlink(file.path(output.dir.samples, paste0("deathnn.ct_", 1:nsim, ".rda")))
    # unlink(file.path(output.dir.samples, paste0("dx.nn.array.ct_", 1:nsim, ".rda")))
    # unlink(file.path(output.dir.samples, paste0("lx.nn.array.ct_", 1:nsim, ".rda")))
  }
  #----------------------------------------------------------------------
  # output country summaries
  u5mr.qct <- apply(u5mr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qct <- apply(imr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.qct <- apply(deathu5.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.qct <- apply(death0.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  u5mr.ui <- imr.ui <- deathu5.ui <- death0.ui <- NULL
  for (c in 1:C) {
    u5mr.ui <- rbind(u5mr.ui, u5mr.qct[, c, ])
    imr.ui <- rbind(imr.ui, imr.qct[, c, ])
    deathu5.ui <- rbind(deathu5.ui, deathu5.qct[, c, ])
    death0.ui <- rbind(death0.ui, death0.qct[, c, ])
  }
  
  if(!is.null(nmr.ctj)){
    nmr.qct <- apply(nmr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
    deathnn.qct <- apply(deathnn.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
    nmr.ui <- deathnn.ui <- NULL
    for (c in 1:C) {
      nmr.ui <- rbind(nmr.ui, nmr.qct[, c, ])
      deathnn.ui <- rbind(deathnn.ui, deathnn.qct[, c, ])
    }
    colnames(nmr.ui) <- paste0("NMR ", est.years.floor)
    colnames(deathnn.ui) <- paste0("Neonatal Deaths ", est.years.floor)
  }
  # output to .csv
  colnames(u5mr.ui) <- paste0("U5MR ", est.years.floor)
  colnames(imr.ui) <- paste0("IMR ", est.years.floor)
  colnames(deathu5.ui) <- paste0("Under-five Deaths ", est.years.floor)
  colnames(death0.ui) <- paste0("Infant Deaths ", est.years.floor)
  country.info.output <- matrix(rep(unlist(country.info), each = 3), C*3, ncol(country.info))
  colnames(country.info.output) <- colnames(country.info)
  if (nsim == 1) {
    select.rows <- seq(1, nrow(u5mr.ui), 3)+1
  } else {
    select.rows <- seq(1, nrow(u5mr.ui), 1)
  }
  
  ifelse(round.output, u5mr.ui <- roundoff(u5mr.ui, digits = ndigits), u5mr.ui <- u5mr.ui)
  ifelse(round.output, imr.ui <- roundoff(imr.ui, digits = ndigits), imr.ui <- imr.ui)
  if(!is.null(nmr.ctj)){
    ifelse(round.output, nmr.ui <- roundoff(nmr.ui, digits = ndigits), nmr.ui <- nmr.ui)
  }
  
  if(is.null(nmr.ctj)){
    write.csv(cbind(country.info.output,
                    rep(c("Lower", "Median", "Upper"), C),
                    u5mr.ui,
                    imr.ui,
                    roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0))[select.rows, ],
              file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
              row.names = F, na = "")
  } else {
    write.csv(cbind(country.info.output,
                    rep(c("Lower", "Median", "Upper"), C),
                    u5mr.ui,
                    imr.ui,
                    nmr.ui,
                    roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0), roundoff(deathnn.ui, digits = 0))[select.rows, ],
              file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
              row.names = F, na = "")
  }
  
  #----------------------------------------------------------------------
  # output country summaries - ARR
  if (isTRUE(output.rates.of.decline)) {
    ARR.year1.year4.ui <- apply(ARR.year1.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
    ARR.year1.year2.ui <- apply(ARR.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
    ARR.year2.year4.ui <- apply(ARR.year2.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
    required.ARR.ui <- apply(required.ARR.cj, 1, quantile, probs = percentiles, na.rm = T)
    changeinARR.ui <- apply(changeinARR.cj, 1, quantile, probs = percentiles, na.rm = T)
    decline.year1.year4.ui <- apply(decline.year1.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
    decline.year1.year2.ui <- apply(decline.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
    decline.year2.year4.ui <- apply(decline.year2.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
    country.RoDs.ui <- cbind(t(ARR.year1.year4.ui), t(ARR.year1.year2.ui), t(ARR.year2.year4.ui),
                             t(required.ARR.ui), t(changeinARR.ui), t(decline.year1.year4.ui),
                             t(decline.year1.year2.ui), t(decline.year2.year4.ui))
    ui.colnames <- c(" lower bound", " median", " upper bound")
    colnames(country.RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5, ui.colnames),
                                   paste0("ARR ", year1-0.5, "-", year2-0.5, ui.colnames),
                                   paste0("ARR ", year2-0.5, "-", year4-0.5, ui.colnames),
                                   paste0("Required ARR", ui.colnames),
                                   paste0("Change in ARR", ui.colnames),
                                   paste0("Percentage decline ", year1-0.5, "-", year4-0.5, ui.colnames),
                                   paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
                                   paste0("Percentage decline ", year2-0.5, "-", year4-0.5, ui.colnames))
    if (nsim == 1)
      country.RoDs.ui <- country.RoDs.ui[, !grepl("bound", colnames(country.RoDs.ui))]
    write.csv(cbind(country.info, country.RoDs.ui),
              file = file.path(output.dir, "Rates of Decline_Country Summary.csv"),
              row.names = FALSE, na = "")
  }
}
#-------------------------------------------------------------------------
# What it does: Combines country draw files from replacement mode and updates the replacement caches.
# Why it is needed: Missing-rate runs need their own combined country outputs without disturbing the standard caches.
CombineAndOutputCountryResults.BWC.replacemissingrates <- function(
    u5mr.ctj,
    imr.ctj,
    nmr.ctj=NULL,
    country.info,
    percentiles,
    ndigits,
    output.dir,
    output.dir.samples,
    output.dir.samplescombined,
    replace.rates.reg,
    selected.country.idx = NULL
) {
  load(file.path(output.dir.samples, "info.rda"))
  list2env(info, envir = environment())
  
  nsim <- as.numeric(dim(u5mr.ctj)[3])
  nyears <- as.numeric(dim(u5mr.ctj)[2])
  ncountry <- as.numeric(dim(u5mr.ctj)[1])
  # est.years.floor <- est.years-0.5
  
  # combine all the samples into their respective arrays
  cat(paste0("Generating arrays for deaths by country..."))
  if(is.null(selected.country.idx)){
    if(is.null(nmr.ctj)){
      death0.ctj<-death1to4.ctj<-deathu5.ctj<-array(NA, dim=c(ncountry, nyears, nsim))
      rownames(death0.ctj) <- rownames(deathu5.ctj) <- rownames(death1to4.ctj)  <- iso.c
      colnames(death0.ctj) <- colnames(death1to4.ctj) <- colnames(deathu5.ctj)  <- est.years.floor
    } else {
      death0.ctj<-death1to4.ctj<-deathu5.ctj<-deathnn.ctj<-array(NA, dim=c(ncountry, nyears, nsim))
      rownames(death0.ctj) <- rownames(deathu5.ctj) <- rownames(death1to4.ctj) <- rownames(deathnn.ctj) <- iso.c
      colnames(death0.ctj) <- colnames(death1to4.ctj) <- colnames(deathu5.ctj) <- colnames(deathnn.ctj) <- est.years.floor
    }
  } else {
    death0.ctj <- LoadReplacementCountryTemplateBWC(
      path = file.path(output.dir.samplescombined, paste0("death0.ctj.", replace.rates.reg, "-replace.rda")),
      object.name = "death0.ctj"
    )
    death1to4.ctj <- LoadReplacementCountryTemplateBWC(
      path = file.path(output.dir.samplescombined, paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda")),
      object.name = "death1to4.ctj"
    )
    deathu5.ctj <- LoadReplacementCountryTemplateBWC(
      path = file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")),
      object.name = "deathu5.ctj"
    )
    if(!is.null(nmr.ctj)){
      deathnn.ctj <- LoadReplacementCountryTemplateBWC(
        path = file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")),
        object.name = "deathnn.ctj"
      )
    }
  }
  
  
  # cat(paste0("Generating arrays for lx and dx..."))
  # lx.array.ctj <- dx.array.ctj <- array(NA, dim=c(3, nyears*52, ncountry, nsim))
  # if(!is.null(nmr.ctj)) lx.nn.array.ctj <- dx.nn.array.ctj <- array(NA, dim=c(2, nyears*52, ncountry, nsim))
  
  for (j in 1:nsim) {
    load(file.path(output.dir.samples, paste0("death0.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samples, paste0("death1to4.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samples, paste0("deathu5.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    # load(file.path(output.dir.samples, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    # load(file.path(output.dir.samples, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    if (is.null(selected.country.idx)) {
      death0.ctj[, , j] <- death0.ct
      death1to4.ctj[, , j] <- death1to4.ct
      deathu5.ctj[, , j] <- deathu5.ct
    } else {
      death0.ctj[selected.country.idx, , j] <- death0.ct[selected.country.idx, , drop = FALSE]
      death1to4.ctj[selected.country.idx, , j] <- death1to4.ct[selected.country.idx, , drop = FALSE]
      deathu5.ctj[selected.country.idx, , j] <- deathu5.ct[selected.country.idx, , drop = FALSE]
    }
    # dx.array.ctj[,,,j] <- dx.array.by.c
    # lx.array.ctj[,,,j] <- lx.array.by.c
    if(!is.null(nmr.ctj)){
      load(file.path(output.dir.samples, paste0("deathnn.ct_", j, "_", replace.rates.reg, "-replace.rda")))
      if (is.null(selected.country.idx)) {
        deathnn.ctj[, , j] <- deathnn.ct
      } else {
        deathnn.ctj[selected.country.idx, , j] <- deathnn.ct[selected.country.idx, , drop = FALSE]
      }
      # load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
      # load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
      # dx.nn.array.ctj[,,,j] <- dx.nn.array.by.c
      # lx.nn.array.ctj[,,,j] <- lx.nn.array.by.c
    }
  }
  # save combined results
  save(death0.ctj, file = file.path(output.dir.samplescombined, paste0("death0.ctj.", replace.rates.reg, "-replace.rda")))
  save(death1to4.ctj, file = file.path(output.dir.samplescombined, paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda")))
  save(deathu5.ctj, file = file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
  # save(dx.array.ctj, file = file.path(output.dir.samplescombined, paste0("dx.array.ctj.", replace.rates.reg, "-replace.rda")))
  # save(lx.array.ctj, file = file.path(output.dir.samplescombined, paste0("lx.array.ctj.", replace.rates.reg, "-replace.rda")))
  # save(info, file = file.path(output.dir.samplescombined, "info.rda"))
  
  if(!is.null(nmr.ctj)){
    save(deathnn.ctj, file = file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    # save(dx.nn.array.ctj, file = file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.", replace.rates.reg, "-replace.rda")))
    # save(lx.nn.array.ctj, file = file.path(output.dir.samplescombined, paste0("lx.nn.array.ctj.", replace.rates.reg, "-replace.rda")))
  }
  # delete samples
  unlink(file.path(output.dir.samples, paste0("death0.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  unlink(file.path(output.dir.samples, paste0("death1to4.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  unlink(file.path(output.dir.samples, paste0("deathu5.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  # unlink(file.path(output.dir.samples, paste0("dx.array.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  # unlink(file.path(output.dir.samples, paste0("lx.array.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  
  if(!is.null(nmr.ctj)){
    unlink(file.path(output.dir.samples, paste0("deathnn.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
    # unlink(file.path(output.dir.samples, paste0("dx.nn.array.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
    # unlink(file.path(output.dir.samples, paste0("lx.nn.array.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  }
  #----------------------------------------------------------------------
  # # output country summaries
  # u5mr.qct <- apply(u5mr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  # imr.qct <- apply(imr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  # deathu5.qct <- apply(deathu5.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  # death0.qct <- apply(death0.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  # u5mr.ui <- imr.ui <- deathu5.ui <- death0.ui <- NULL
  # for (c in 1:C) {
  #   u5mr.ui <- rbind(u5mr.ui, u5mr.qct[, c, ])
  #   imr.ui <- rbind(imr.ui, imr.qct[, c, ])
  #   deathu5.ui <- rbind(deathu5.ui, deathu5.qct[, c, ])
  #   death0.ui <- rbind(death0.ui, death0.qct[, c, ])
  # }
  #
  # if(!is.null(nmr.ctj)){
  #   nmr.qct <- apply(nmr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  #   deathnn.qct <- apply(deathnn.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  #   nmr.ui <- deathnn.ui <- NULL
  #   for (c in 1:C) {
  #     nmr.ui <- rbind(nmr.ui, nmr.qct[, c, ])
  #     deathnn.ui <- rbind(deathnn.ui, deathnn.qct[, c, ])
  #   }
  #   colnames(nmr.ui) <- paste0("NMR ", est.years.floor)
  #   colnames(deathnn.ui) <- paste0("Neonatal Deaths ", est.years.floor)
  # }
  # # output to .csv
  # colnames(u5mr.ui) <- paste0("U5MR ", est.years.floor)
  # colnames(imr.ui) <- paste0("IMR ", est.years.floor)
  # colnames(deathu5.ui) <- paste0("Under-five Deaths ", est.years.floor)
  # colnames(death0.ui) <- paste0("Infant Deaths ", est.years.floor)
  # country.info.output <- matrix(rep(unlist(country.info), each = 3), C*3, ncol(country.info))
  # colnames(country.info.output) <- colnames(country.info)
  # if (nsim == 1) {
  #   select.rows <- seq(1, nrow(u5mr.ui), 3)+1
  # } else {
  #   select.rows <- seq(1, nrow(u5mr.ui), 1)
  # }
  #
  # if(is.null(nmr.ctj)){
  #   write.csv(cbind(country.info.output,
  #                   rep(c("Lower", "Median", "Upper"), C),
  #                   roundoff(u5mr.ui, digits = ndigits), roundoff(imr.ui, digits = ndigits),
  #                   roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0))[select.rows, ],
  #             file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
  #             row.names = F, na = "")
  # } else {
  #   write.csv(cbind(country.info.output,
  #                   rep(c("Lower", "Median", "Upper"), C),
  #                   roundoff(u5mr.ui, digits = ndigits), roundoff(imr.ui, digits = ndigits), roundoff(nmr.ui, digits = ndigits),
  #                   roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0), roundoff(deathnn.ui, digits = 0))[select.rows, ],
  #             file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
  #             row.names = F, na = "")
  # }
}
#----------------------------------------------------------------------
# What it does: Aggregates country results to the world level and writes world summaries.
# Why it is needed: Global outputs require separate coverage checks, BWC recomputation, and rates-of-decline summaries.
CalculateWorldDeathsBWC <- function(
    output.dir.samplescombined,
    output.dir.samples,
    output.dir,
    percentiles,
    ndigits,
    run.on.server=run.on.server,
    parallel.cores = NULL,
    replace.rates.reg,
    output.rates.of.decline,
    round.output
) {
  if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
    load(file.path(output.dir.samplescombined, "info.rda"))
    list2env(info, envir = environment())
    load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
    load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
    load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
    
    nn.exists <- file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
    if(nn.exists) load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  } else {
    load(file.path(output.dir.samplescombined, "info.rda"))
    list2env(info, envir = environment())
    load(file.path(output.dir.samplescombined, paste0("death0.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined,paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
    
    nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    if(nn.exists) load(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
  } # if/else  replace.rates.reg
  
  nsim <- dim(deathu5.ctj)[3]
  
  ## load country mortality rates for later death calc -- these do not have missing rates replaced
  load(file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
  load(file.path(output.dir.samplescombined, "imr.ctj.rda"))
  if(nn.exists) load(file.path(output.dir.samplescombined, "nmr.ctj.rda"))
  
  # edit DJS 2018-03-09 remove rounding for median calculation
  # round off to 1 d.p. before calculation (for median only)
  # if (nsim == 1) {
  #   u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
  #   imr.ctj <- roundoff(imr.ctj, digits = ndigits)
  #   if(!is.null(nmr.ctj)) nmr.ctj <- roundoff(nmr.ctj, digits = ndigits)
  # }
  # edit DJS 2018-03-09
  
  # edit DJS 2018-07-27 round deaths at country level before summing to region and world
  deathu5.ctj <- roundoff(deathu5.ctj, digits = 0)
  death0.ctj <- roundoff(death0.ctj, digits = 0)
  death1to4.ctj <- roundoff(death1to4.ctj, digits = 0)
  if(nn.exists) deathnn.ctj <- roundoff(deathnn.ctj, digits = 0)
  # edit DJS 2018-07-27
  
  # ## load country deaths and cohorts for later death calc
  # load(file.path(output.dir.samplescombined, "dx.array.ctj.rda"))
  # load(file.path(output.dir.samplescombined, "lx.array.ctj.rda"))
  # if(nn.exists) load(file.path(output.dir.samplescombined, "dx.nn.array.ctj.rda"))
  # if(nn.exists) load(file.path(output.dir.samplescombined, "lx.nn.array.ctj.rda"))
  
  ## wgt and year matrixes for later death calc
  # wgt.mat
  weight.j.1 <- (53-(1:52))/52 #weight for mortality rate in year 1
  wgt.mat <- t(matrix(cbind(weight.j.1, 1-weight.j.1), ncol=10, nrow=52))
  if(nn.exists){
    # wgt.nmr.mat
    weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4)) #weight for mortality rate in year 1
    wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1-weight.nmr.j.1), ncol=2, nrow=52))
  }
  # years.mat
  years <- years.k <- seq(1950,1950+ncol(u5mr.ctj)-1,1)
  for(yr in years){
    ifelse(yr==1950, years.mat <- seq(yr+.5,yr+5,.5), years.mat <- cbind(years.mat, seq(yr+.5,yr+5,.5)))
  }
  
  # Note: w stands for w, and w = 1
  # death0.wtj <- death1to4.wtj <- deathu5.wtj <-
  #   death0.all.wtj <- death1to4.all.wtj <- deathu5.all.wtj <-
  #   M0.wtj <- M1to4.wtj <- q0.wtj <- q1to4.wtj <- q5.wtj <-
  #   u5mr.wtj <- imr.wtj <- array(data = NA, c(1, nyears, nsim))
  
  death0.wtj <- death1to4.wtj <- deathu5.wtj <- deathnn.wtj <-
    death0.all.wtj <- death1to4.all.wtj <- deathu5.all.wtj <- deathnn.all.wtj <-
    M0.wtj <- M1to4.wtj <- q0.wtj <- q1to4.wtj <- q5.wtj <- qnn.wtj <-
    u5mr.wtj <- imr.wtj <- nmr.wtj <- array(data = NA, c(1, nyears, nsim))
  
  # imr.dx.bwc1 <- imr.lx.bwc1 <- cmr.dx.bwc1 <- cmr.lx.bwc1 <-
  #   nmr.dx.bwc1 <- nmr.lx.bwc1 <- array(data = NA, c(1, nyears, nsim))
  
  nyears <- ncol(u5mr.ctj)
  death0.temp.ct <- deathu5.temp.ct <- deathnn.temp.ct <- matrix(NA, C, nyears)
  
  pop0.wt <- pop1to4.wt <- pop0.orig.wt <- pop1to4.orig.wt <- popu5.orig.wt <-
    coverage0.wt <- coverageu5.wt <- matrix(NA, 1, nyears)
  
  # What it does: Converts a year and optional week offsets into BWC column positions.
  # Why it is needed: The world calculation repeatedly indexes weekly cohort arrays by year.
  getBWC <- function(bwc=NULL, year){
    if(is.null(bwc)){
      bwc <- 1:52
    }
    return(((year-1950)*52)+bwc)
  }
  
  removeNA <- T
  
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
  
  if(run.on.server){
    # for (j in 1:nsim) {
    registerDoMC(cores = ResolveParallelCoresBWC(parallel.cores))
    foreach (j = 1:nsim) %dopar% {
      # dx.array.by.c <- dx.array.ctj[,,,j]
      # lx.array.by.c <- lx.array.ctj[,,,j]
      # if(nn.exists){
      # dx.nn.array.by.c <- dx.nn.array.ctj[,,,j]
      # lx.nn.array.by.c <- lx.nn.array.ctj[,,,j]
      # }
      
      if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
        load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
        load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
        if(nn.exists){
          load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
          load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
        }
      } else {
        load(file.path(output.dir.samples, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
        load(file.path(output.dir.samples, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
        if(nn.exists){
          load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
          load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
        }
      } # if/else is.null(replace.rates.reg)
      
      death0.wt <- deathu5.wt <- deathnn.wt <-
        death0.all.wt <- deathu5.all.wt <- deathnn.all.wt <-
        q0.wt <- q1to4.wt <- q5.wt <- qnn.wt <- matrix(data = NA, 1, nyears)
      
      for (i in 1:nyears) {
        death0.wt[, i] <- sum(death0.ctj[, i, j], na.rm = T)
        deathu5.wt[, i] <- sum(deathu5.ctj[, i, j], na.rm = T)
        if(nn.exists) deathnn.wt[, i] <- sum(deathnn.ctj[, i, j], na.rm = T)
        
        q0.wt[, i] <- sum(dx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)/sum(lx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)
        if(i>1){
          q1to4.wt[, i]<- 1-((1-(sum(dx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),], na.rm=removeNA)/sum(lx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),], na.rm=removeNA)))^4)
        }
        q5.wt[, i] <- 1-(1-q0.wt[, i])*(1-q1to4.wt[, i])
        
        if(nn.exists){
          qnn.wt[, i] <- sum(dx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)/sum(lx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)
        }
      } #i loop years
      
      ## save qs and deaths for trajectories to combine outside jnsim loop
      save(death0.wt, file = file.path(output.dir.samples, paste0("death0.wt_", j, ".rda")))
      save(deathu5.wt, file = file.path(output.dir.samples, paste0("deathu5.wt_", j, ".rda")))
      if(nn.exists) save(deathnn.wt, file = file.path(output.dir.samples, paste0("deathnn.wt_", j, ".rda")))
      save(q0.wt, file = file.path(output.dir.samples, paste0("q0.wt_", j, ".rda")))
      #save(q1to4.wt, file = file.path(output.dir.samples, paste0("q1to4.wt_", j, ".rda")))
      save(q5.wt, file = file.path(output.dir.samples, paste0("q5.wt_", j, ".rda")))
      if(nn.exists) save(qnn.wt, file = file.path(output.dir.samples, paste0("qnn.wt_", j, ".rda")))
    } # j sim loop 1
  } else {
    for (j in 1:nsim) {
      # registerDoMC()
      # foreach (j = 1:nsim) %dopar% {
      # dx.array.by.c <- dx.array.ctj[,,,j]
      # lx.array.by.c <- lx.array.ctj[,,,j]
      # if(nn.exists){
      # dx.nn.array.by.c <- dx.nn.array.ctj[,,,j]
      # lx.nn.array.by.c <- lx.nn.array.ctj[,,,j]
      # }
      
      if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
        load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
        load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
        if(nn.exists){
          load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
          load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
        }
      } else {
        load(file.path(output.dir.samples, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
        load(file.path(output.dir.samples, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
        if(nn.exists){
          load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
          load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
        }
      } # if/else is.null(replace.rates.reg)
      
      death0.wt <- deathu5.wt <- deathnn.wt <-
        death0.all.wt <- deathu5.all.wt <- deathnn.all.wt <-
        q0.wt <- q1to4.wt <- q5.wt <- qnn.wt <- matrix(data = NA, 1, nyears)
      
      for (i in 1:nyears) {
        death0.wt[, i] <- sum(death0.ctj[, i, j], na.rm = T)
        deathu5.wt[, i] <- sum(deathu5.ctj[, i, j], na.rm = T)
        if(nn.exists) deathnn.wt[, i] <- sum(deathnn.ctj[, i, j], na.rm = T)
        
        q0.wt[, i] <- sum(dx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)/sum(lx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)
        if(i>1){
          q1to4.wt[, i]<- 1-((1-(sum(dx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),], na.rm=removeNA)/sum(lx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),], na.rm=removeNA)))^4)
        }
        q5.wt[, i] <- 1-(1-q0.wt[, i])*(1-q1to4.wt[, i])
        
        if(nn.exists){
          qnn.wt[, i] <- sum(dx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)/sum(lx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)
        }
      } #i loop years
      
      ## save qs and deaths for trajectories to combine outside jnsim loop
      save(death0.wt, file = file.path(output.dir.samples, paste0("death0.wt_", j, ".rda")))
      save(deathu5.wt, file = file.path(output.dir.samples, paste0("deathu5.wt_", j, ".rda")))
      if(nn.exists) save(deathnn.wt, file = file.path(output.dir.samples, paste0("deathnn.wt_", j, ".rda")))
      save(q0.wt, file = file.path(output.dir.samples, paste0("q0.wt_", j, ".rda")))
      #save(q1to4.wt, file = file.path(output.dir.samples, paste0("q1to4.wt_", j, ".rda")))
      save(q5.wt, file = file.path(output.dir.samples, paste0("q5.wt_", j, ".rda")))
      if(nn.exists) save(qnn.wt, file = file.path(output.dir.samples, paste0("qnn.wt_", j, ".rda")))
    } # j sim loop 1
  } # else - run.on.server
  
  ## combine trajectory files from j loop
  for(j in 1:nsim){
    load(file.path(output.dir.samples, paste0("death0.wt_", j, ".rda")))
    death0.wtj[,,j] <- death0.wt
    load(file.path(output.dir.samples, paste0("deathu5.wt_", j, ".rda")))
    deathu5.wtj[,,j] <- deathu5.wt
    
    load(file.path(output.dir.samples, paste0("q0.wt_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("q5.wt_", j, ".rda")))
    q0.wtj[,,j] <- q0.wt
    q5.wtj[,,j] <- q5.wt
    
    if(nn.exists){
      load(file.path(output.dir.samples, paste0("deathnn.wt_", j, ".rda")))
      load(file.path(output.dir.samples, paste0("qnn.wt_", j, ".rda")))
      deathnn.wtj[,,j] <- deathnn.wt
      qnn.wtj[,,j] <- qnn.wt
    } # if nn.exists
  }# j loop for combining
  
  if(is.null(replace.rates.reg)){
    ## do BWC method again for all countries replacing missing rates with world rates, then sum deaths at world level for deathXX.all.wtj
    if(run.on.server){
      # for (j in 1:nsim) {
      registerDoMC(cores = ResolveParallelCoresBWC(parallel.cores))
      foreach (j = 1:nsim) %dopar% {
        for(k in 1:dim(u5mr.ctj)[1]){
          u5mr.temp.ct <- u5mr.ctj[,,j]
          imr.temp.ct <- imr.ctj[,,j]
          if(nn.exists) nmr.temp.ct <- nmr.ctj[,,j]
          
          ## get live births for years.k
          wpp.livebirths.k <- as.numeric(livebirths.ct[k,match(years.k,years)])
          #if(nrow(wpp.livebirths.k)<1) next
          bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
          
          for(ik in 1:length(years.k)){
            if(years.k[ik]+5>max(years.k)){
              # u1 mortality rates; has IMR for year[i] and year[i+1]
              nmx.u1.i <- imr.temp.ct[k,match(years.k[ik:length(years.k)],years)]
              nmx.u1.i[is.na(nmx.u1.i)] <- q0.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmx.u1.i)],j]*1000
              # u5 mortality rates; need same length as u1 to convert to 4q1
              nmx.u5.i <- u5mr.temp.ct[k,match(years.k[ik:length(years.k)],years)]
              nmx.u5.i[is.na(nmx.u5.i)] <- q5.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmx.u5.i)],j]*1000
              # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
              nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
              nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
              nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
              # combine appropriate rates in mortality rate vector
              nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
            } else {
              # u1 mortality rates; has IMR for year[i] and year[i+1]
              nmx.u1.i <- imr.temp.ct[k,match(years.k[ik]:(years.k[ik]+5),years)]
              nmx.u1.i[is.na(nmx.u1.i)] <- q0.wtj[,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u1.i)],j]*1000
              # u5 mortality rates; need same length as u1 to convert to 4q1
              nmx.u5.i <- u5mr.temp.ct[k,match(years.k[ik]:(years.k[ik]+5),years)]
              nmx.u5.i[is.na(nmx.u5.i)] <- q5.wtj[,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u5.i)],j]*1000
              # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
              nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
              nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
              nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
              # combine in mortality rate vector for lifetable function
              nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
            } #if/else
            
            ## turn nmx.i into matrix
            ifelse(ik==1, nmx.mat.k <- matrix(nmx.i, nrow=10, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=10, ncol=52)))
            if(nsim==1) nmx.mat.k <- roundoff(nmx.mat.k, digits=1)
            
            # nmr
            if(nn.exists){
              if(years.k[ik]+1>max(years.k)){
                nmr.i <- c(nmr.temp.ct[k,match(years.k[ik],years)], NA)
                nmr.i[is.na(nmr.i)] <- qnn.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmr.i)],j]*1000
              } else {
                nmr.i <- nmr.temp.ct[k,match(years.k[ik]:years.k[ik+1],years)]
                nmr.i[is.na(nmr.i)] <- qnn.wtj[,match(years.k[ik]:years.k[ik+1],years)[is.na(nmr.i)],j]*1000
              } # if/else
              
              ## turn nmr.i into matrix
              ifelse(ik==1, nmr.mat.k <- matrix(nmr.i, nrow=2, ncol=52), nmr.mat.k <- cbind(nmr.mat.k ,matrix(nmr.i, nrow=2, ncol=52)))
              if(nsim==1) nmr.mat.k <- roundoff(nmr.mat.k,digits=1)
            }
          } # i loop for nmx matrix
          
          ## get infant and u5 deaths
          wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=10, ncol=length(years.k)*52)
          nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
          npx.mat.k <- 1-nqx.mat.k
          lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
          dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
          
          ## get nn deaths
          if(nn.exists){
            wgt.nn.mat.k <- matrix(rep(wgt.nmr.mat,length(years.k)),nrow=2, ncol=length(years.k)*52)
            nqx.nn.mat.k <- wgt.nn.mat.k*(nmr.mat.k/1000)
            npx.nn.mat.k <- 1-nqx.nn.mat.k
            lx.nn.mat.k <- apply(rbind(bwc.vec.k, npx.nn.mat.k),2,cumprod)
            dx.nn.mat.k <- -apply(lx.nn.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
          }
          
          years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=10, ncol=length(years.k)*52)
          years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
          
          for(yrk in min(years.k):max(years.k)){
            deathu5.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T)
            death0.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
            if(nn.exists) deathnn.temp.ct[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
          } # for loop for summing deaths
          # edit DJS 2018-07-27 round deaths at country level before summing to region and world
          deathu5.temp.ct <- roundoff(deathu5.temp.ct, digits = 0)
          death0.temp.ct <- roundoff(death0.temp.ct, digits = 0)
          if(nn.exists) deathnn.temp.ct <- roundoff(deathnn.temp.ct, digits = 0)
          # edit DJS 2018-07-27
        } # k loop
        
        # to remove LIE from regional aggregate calcualtions -- to implement in 2018
        death0.all.wt <- apply(death0.temp.ct[apply(!is.na(death0.ctj),1,sum)>0,], 2, sum, na.rm=F)
        deathu5.all.wt <- apply(deathu5.temp.ct[apply(!is.na(deathu5.ctj),1,sum)>0,], 2, sum, na.rm=F)
        if(nn.exists) deathnn.all.wt <- apply(deathnn.temp.ct[apply(!is.na(deathnn.ctj),1,sum)>0,], 2, sum, na.rm=F)
        
        # death0.all.wt <- apply(death0.temp.ct, 2, sum, na.rm=F)
        # deathu5.all.wt <- apply(deathu5.temp.ct, 2, sum, na.rm=F)
        # if(nn.exists) deathnn.all.wt <- apply(deathnn.temp.ct, 2, sum, na.rm=F)
        
        ## save deaths.all objects for combining outside j loop
        save(death0.all.wt, file = file.path(output.dir.samples, paste0("death0.all.wt_", j, ".rda")))
        save(deathu5.all.wt, file = file.path(output.dir.samples, paste0("deathu5.all.wt_", j, ".rda")))
        if(nn.exists) save(deathnn.all.wt, file = file.path(output.dir.samples, paste0("deathnn.all.wt_", j, ".rda")))
      } # j loop nsim 2
    } else {
      for (j in 1:nsim) {
        # registerDoMC()
        # foreach (j = 1:nsim) %dopar% {
        for(k in 1:dim(u5mr.ctj)[1]){
          u5mr.temp.ct <- u5mr.ctj[,,j]
          imr.temp.ct <- imr.ctj[,,j]
          if(nn.exists) nmr.temp.ct <- nmr.ctj[,,j]
          
          ## get live births for years.k
          wpp.livebirths.k <- as.numeric(livebirths.ct[k,match(years.k,years)])
          #if(nrow(wpp.livebirths.k)<1) next
          bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
          
          for(ik in 1:length(years.k)){
            if(years.k[ik]+5>max(years.k)){
              # u1 mortality rates; has IMR for year[i] and year[i+1]
              nmx.u1.i <- imr.temp.ct[k,match(years.k[ik:length(years.k)],years)]
              nmx.u1.i[is.na(nmx.u1.i)] <- q0.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmx.u1.i)],j]*1000
              # u5 mortality rates; need same length as u1 to convert to 4q1
              nmx.u5.i <- u5mr.temp.ct[k,match(years.k[ik:length(years.k)],years)]
              nmx.u5.i[is.na(nmx.u5.i)] <- q5.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmx.u5.i)],j]*1000
              # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
              nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
              nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
              nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
              # combine appropriate rates in mortality rate vector
              nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
            } else {
              # u1 mortality rates; has IMR for year[i] and year[i+1]
              nmx.u1.i <- imr.temp.ct[k,match(years.k[ik]:(years.k[ik]+5),years)]
              nmx.u1.i[is.na(nmx.u1.i)] <- q0.wtj[,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u1.i)],j]*1000
              # u5 mortality rates; need same length as u1 to convert to 4q1
              nmx.u5.i <- u5mr.temp.ct[k,match(years.k[ik]:(years.k[ik]+5),years)]
              nmx.u5.i[is.na(nmx.u5.i)] <- q5.wtj[,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u5.i)],j]*1000
              # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
              nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
              nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
              nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
              # combine in mortality rate vector for lifetable function
              nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
            } #if/else
            
            ## turn nmx.i into matrix
            ifelse(ik==1, nmx.mat.k <- matrix(nmx.i, nrow=10, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=10, ncol=52)))
            #nmx.mat.k <- roundoff(nmx.mat.k, digits = 1)
            # nmr
            if(nn.exists){
              if(years.k[ik]+1>max(years.k)){
                nmr.i <- c(nmr.temp.ct[k,match(years.k[ik],years)], NA)
                nmr.i[is.na(nmr.i)] <- qnn.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmr.i)],j]*1000
              } else {
                nmr.i <- nmr.temp.ct[k,match(years.k[ik]:years.k[ik+1],years)]
                nmr.i[is.na(nmr.i)] <- qnn.wtj[,match(years.k[ik]:years.k[ik+1],years)[is.na(nmr.i)],j]*1000
              } # if/else
              
              ## turn nmr.i into matrix
              ifelse(ik==1, nmr.mat.k <- matrix(nmr.i, nrow=2, ncol=52), nmr.mat.k <- cbind(nmr.mat.k ,matrix(nmr.i, nrow=2, ncol=52)))
              # nmr.mat.k <- roundoff(nmr.mat.k, digits=1)
            }
          } # i loop for nmx matrix
          
          ## get infant and u5 deaths
          wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=10, ncol=length(years.k)*52)
          nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
          npx.mat.k <- 1-nqx.mat.k
          lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
          dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
          
          ## get nn deaths
          if(nn.exists){
            wgt.nn.mat.k <- matrix(rep(wgt.nmr.mat,length(years.k)),nrow=2, ncol=length(years.k)*52)
            nqx.nn.mat.k <- wgt.nn.mat.k*(nmr.mat.k/1000)
            npx.nn.mat.k <- 1-nqx.nn.mat.k
            lx.nn.mat.k <- apply(rbind(bwc.vec.k, npx.nn.mat.k),2,cumprod)
            dx.nn.mat.k <- -apply(lx.nn.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
          }
          
          years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=10, ncol=length(years.k)*52)
          years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
          
          for(yrk in min(years.k):max(years.k)){
            deathu5.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T)
            death0.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
            if(nn.exists) deathnn.temp.ct[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
          } # for loop for summing deaths
          # edit DJS 2018-07-27 round deaths at country level before summing to region and world
          deathu5.temp.ct <- roundoff(deathu5.temp.ct, digits = 0)
          death0.temp.ct <- roundoff(death0.temp.ct, digits = 0)
          if(nn.exists) deathnn.temp.ct <- roundoff(deathnn.temp.ct, digits = 0)
          # edit DJS 2018-07-27
        } # k loop
        
        
        # to remove LIE from world calculation -- to be implemented in 2018
        death0.all.wt <- apply(death0.temp.ct[apply(!is.na(death0.ctj),1,sum)>0,], 2, sum, na.rm=F)
        deathu5.all.wt <- apply(deathu5.temp.ct[apply(!is.na(deathu5.ctj),1,sum)>0,], 2, sum, na.rm=F)
        if(nn.exists) deathnn.all.wt <- apply(deathnn.temp.ct[apply(!is.na(deathnn.ctj),1,sum)>0,], 2, sum, na.rm=F)
        # death0.all.wt <- apply(death0.temp.ct, 2, sum, na.rm=F)
        # deathu5.all.wt <- apply(deathu5.temp.ct, 2, sum, na.rm=F)
        # if(nn.exists) deathnn.all.wt <- apply(deathnn.temp.ct, 2, sum, na.rm=F)
        
        ## save deaths.all objects for combining outside j loop
        save(death0.all.wt, file = file.path(output.dir.samples, paste0("death0.all.wt_", j, ".rda")))
        save(deathu5.all.wt, file = file.path(output.dir.samples, paste0("deathu5.all.wt_", j, ".rda")))
        if(nn.exists) save(deathnn.all.wt, file = file.path(output.dir.samples, paste0("deathnn.all.wt_", j, ".rda")))
      } # j loop nsim 2
    }
    
    ## combine trajectory files from j loop
    for(j in 1:nsim){
      load(file.path(output.dir.samples, paste0("death0.all.wt_", j, ".rda")))
      death0.all.wtj[,,j] <- death0.all.wt
      load(file.path(output.dir.samples, paste0("deathu5.all.wt_", j, ".rda")))
      deathu5.all.wtj[,,j] <- deathu5.all.wt
      
      if(nn.exists){
        load(file.path(output.dir.samples, paste0("deathnn.all.wt_", j, ".rda")))
        deathnn.all.wtj[,,j] <- deathnn.all.wt
      } # is nn.exists
    }# j loop for combining
    
    unlink(file.path(output.dir.samples, paste0("death0.all.wt_", 1:nsim, ".rda")))
    unlink(file.path(output.dir.samples, paste0("deathu5.all.wt_", 1:nsim, ".rda")))
    if(nn.exists){
      unlink(file.path(output.dir.samples, paste0("deathnn.all.wt_", 1:nsim, ".rda")))
    }
    
  } else {
    death0.all.wtj <- death0.wtj
    deathu5.all.wtj <- deathu5.wtj
    if(nn.exists){
      deathnn.all.wtj <- deathnn.wtj
    } # if nn.exists
  } # if replace.reg
  
  # delete samples
  unlink(file.path(output.dir.samples, paste0("death0.wt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("deathu5.wt_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples, paste0("death0.all.wt_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples, paste0("deathu5.all.wt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("q0.wt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("q5.wt_", 1:nsim, ".rda")))
  if(nn.exists){
    unlink(file.path(output.dir.samples, paste0("deathnn.wt_", 1:nsim, ".rda")))
    # unlink(file.path(output.dir.samples, paste0("deathnn.all.wt_", 1:nsim, ".rda")))
    unlink(file.path(output.dir.samples, paste0("qnn.wt_", 1:nsim, ".rda")))
  }
  
  ## NA for columns (years) where BWC method doesn't have full count yet
  death0.all.wtj[,1:3,] <- NA
  deathnn.all.wtj[,1:3,] <- NA
  deathu5.all.wtj[,1:8,] <- NA # first year of complete nmx schedule for deaths + 5 years
  
  u5mr.wtj <- q5.wtj*1000
  imr.wtj <- q0.wtj*1000
  if(nn.exists) nmr.wtj <- qnn.wtj*1000
  
  # world summary
  u5mr.qwt <- apply(u5mr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qwt <- apply(imr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  if(nn.exists) nmr.qwt <- apply(nmr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.all.qwt <- apply(deathu5.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.all.qwt <- apply(death0.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  if(nn.exists) deathnn.all.qwt <- apply(deathnn.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  
  # NA if coverage < 0.5 and for some years beyond that for BWC method
  for (q in 1:length(percentiles)) {
    u5mr.qwt[q, , ][coverageu5.wt < 0.5] <- NA
    imr.qwt[q, , ][coverage0.wt < 0.5] <- NA
    if(nn.exists) nmr.qwt[q, , ][coverage0.wt < 0.5] <- NA
    deathu5.all.qwt[q, , ][c(which(coverageu5.wt < 0.5), (max(which(coverageu5.wt < 0.5))+1):(max(which(coverageu5.wt < 0.5))+5))] <- NA
    death0.all.qwt[q, , ][c(which(coverageu5.wt < 0.5), (max(which(coverageu5.wt < 0.5))+1))] <- NA
    if(nn.exists) deathnn.all.qwt[q, , ][c(which(coverageu5.wt < 0.5), (max(which(coverageu5.wt < 0.5))+1))] <- NA
  }
  
  # world summary
  if(nn.exists){
    ifelse(round.output, t.u5mr.qwt <- roundoff(t(u5mr.qwt[, 1, ]), digits = ndigits), t.u5mr.qwt <- t(u5mr.qwt[, 1, ]))
    ifelse(round.output, t.imr.qwt <- roundoff(t(imr.qwt[, 1, ]), digits = ndigits),  t.imr.qwt <- t(imr.qwt[, 1, ]))
    ifelse(round.output, t.nmr.qwt <- roundoff(t(nmr.qwt[, 1, ]), digits = ndigits), t.nmr.qwt <- t(nmr.qwt[, 1, ]))
    
    res.world <- cbind(est.years.floor,
                       roundoff(t(popu5.orig.wt), digits = 0),
                       roundoff(t(pop0.orig.wt), digits = 0),
                       roundoff(t(coverageu5.wt)*100, digits = 1),
                       roundoff(t(coverage0.wt)*100, digits = 1),
                       t.u5mr.qwt,
                       t.imr.qwt,
                       t.nmr.qwt,
                       roundoff(t(deathu5.all.qwt[, 1, ]), digits = 0),
                       roundoff(t(death0.all.qwt[, 1, ]), digits = 0),
                       roundoff(t(deathnn.all.qwt[, 1, ]), digits = 0))
    ui.colnames <- c(" lower bound", " median", " upper bound")
    colnames(res.world) <- c("Year", "Under-five population", "Infant population",
                             "Population coverage (under 5)",
                             "Population coverage (age 0)",
                             paste0("U5MR", ui.colnames),
                             paste0("IMR", ui.colnames),
                             paste0("NMR", ui.colnames),
                             paste0("Under-five deaths", ui.colnames),
                             paste0("Infant deaths", ui.colnames),
                             paste0("Neonatal deaths", ui.colnames))
  } else {
    ifelse(round.output, t.u5mr.qwt <- roundoff(t(u5mr.qwt[, 1, ]), digits = ndigits), t.u5mr.qwt <- t(u5mr.qwt[, 1, ]))
    ifelse(round.output, t.imr.qwt <- roundoff(t(imr.qwt[, 1, ]), digits = ndigits),  t.imr.qwt <- t(imr.qwt[, 1, ]))
    
    res.world <- cbind(est.years.floor,
                       roundoff(t(popu5.orig.wt), digits = 0),
                       roundoff(t(pop0.orig.wt), digits = 0),
                       roundoff(t(coverageu5.wt)*100, digits = 1),
                       roundoff(t(coverage0.wt)*100, digits = 1),
                       t.u5mr.qwt,
                       t.imr.qwt,
                       roundoff(t(deathu5.all.qwt[, 1, ]), digits = 0),
                       roundoff(t(death0.all.qwt[, 1, ]), digits = 0))
    ui.colnames <- c(" lower bound", " median", " upper bound")
    colnames(res.world) <- c("Year", "Under-five population", "Infant population",
                             "Population coverage (under 5)",
                             "Population coverage (age 0)",
                             paste0("U5MR", ui.colnames),
                             paste0("IMR", ui.colnames),
                             paste0("Under-five deaths", ui.colnames),
                             paste0("Infant deaths", ui.colnames))
  }
  save(res.world, file = file.path(output.dir.samplescombined, "res.world.rda"))
  if (nsim == 1) {
    res.world <- res.world[, !grepl("bound", colnames(res.world))]
  }
  write.csv(res.world, file = file.path(output.dir, "Rates & Deaths_World.csv"),
            row.names = F, na = "")
  # save all quantities # change JR, 26 Aug 2013
  save(u5mr.wtj, file = file.path(output.dir.samplescombined, "u5mr.wtj.rda"))
  save(imr.wtj, file = file.path(output.dir.samplescombined, "imr.wtj.rda"))
  save(deathu5.all.wtj, file = file.path(output.dir.samplescombined, "deathu5.all.wtj.rda"))
  save(death0.all.wtj, file = file.path(output.dir.samplescombined, "death0.all.wtj.rda"))
  save(pop0.wt, file = file.path(output.dir.samplescombined, "pop0.wt.rda"))
  save(pop1to4.wt, file = file.path(output.dir.samplescombined, "pop1to4.wt.rda"))
  save(pop0.orig.wt, file = file.path(output.dir.samplescombined, "pop0.orig.wt.rda"))
  save(pop1to4.orig.wt, file = file.path(output.dir.samplescombined, "pop1to4.orig.wt.rda"))
  save(popu5.orig.wt, file = file.path(output.dir.samplescombined, "popu5.orig.wt.rda"))
  save(coverage0.wt, file = file.path(output.dir.samplescombined, "coverage0.wt.rda"))
  save(coverageu5.wt, file = file.path(output.dir.samplescombined, "coverageu5.wt.rda"))
  if(nn.exists){
    save(nmr.wtj, file = file.path(output.dir.samplescombined, "nmr.wtj.rda"))
    save(deathnn.all.wtj, file = file.path(output.dir.samplescombined, "deathnn.all.wtj.rda"))
  }
  
  # round off to 1 d.p. before calculation (for median only)
  if (dim(u5mr.wtj)[3] == 1) { # change JR, 26 Aug 2013
    u5mr.wtj <- roundoff(u5mr.wtj, digits = ndigits)
    imr.wtj <- roundoff(imr.wtj, digits = ndigits)
  }
  
  if (isTRUE(output.rates.of.decline)) {
    ARR.year1.year4.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
                                      year.start = year1, year.end = year4)
    ARR.year1.year2.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
                                      year.start = year1, year.end = year2)
    ARR.year2.year4.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
                                      year.start = year2, year.end = year4)
    required.ARR.j <- ifelse(year4 < year.target,
                             1/(year.target-year4)*
                               log(roundoff(u5mr.wtj[1, est.years == year1, ]*factor.target, digits = ndigits)/
                                     u5mr.wtj[1, est.years == year4, ])*-100, NA)
    changeinARR.j <- ARR.year2.year4.j - ARR.year1.year2.j
    decline.year1.year4.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
                                              year.start = year1, year.end = year4)
    decline.year1.year2.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
                                              year.start = year1, year.end = year2)
    decline.year2.year4.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
                                              year.start = year2, year.end = year4)
    ARR.year1.year4.ui <- SafeQuantileBWC(ARR.year1.year4.j, probs = percentiles)
    ARR.year1.year2.ui <- SafeQuantileBWC(ARR.year1.year2.j, probs = percentiles)
    ARR.year2.year4.ui <- SafeQuantileBWC(ARR.year2.year4.j, probs = percentiles)
    required.ARR.ui <- SafeQuantileBWC(required.ARR.j, probs = percentiles)
    changeinARR.ui <- SafeQuantileBWC(changeinARR.j, probs = percentiles)
    decline.year1.year4.ui <- SafeQuantileBWC(decline.year1.year4.j, probs = percentiles)
    decline.year1.year2.ui <- SafeQuantileBWC(decline.year1.year2.j, probs = percentiles)
    decline.year2.year4.ui <- SafeQuantileBWC(decline.year2.year4.j, probs = percentiles)
    global.RoDs.ui <- rbind(c(ARR.year1.year4.ui, ARR.year1.year2.ui, ARR.year2.year4.ui,
                              required.ARR.ui, changeinARR.ui, decline.year1.year4.ui,
                              decline.year1.year2.ui, decline.year2.year4.ui))
    colnames(global.RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5, ui.colnames),
                                  paste0("ARR ", year1-0.5, "-", year2-0.5, ui.colnames),
                                  paste0("ARR ", year2-0.5, "-", year4-0.5, ui.colnames),
                                  paste0("Required ARR", ui.colnames),
                                  paste0("Change in ARR", ui.colnames),
                                  paste0("Percentage decline ", year1-0.5, "-", year4-0.5, ui.colnames),
                                  paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
                                  paste0("Percentage decline ", year2-0.5, "-", year4-0.5, ui.colnames))
    save(global.RoDs.ui, file = file.path(output.dir.samplescombined, "global.RoDs.ui.rda"))
    if (nsim == 1) {
      global.RoDs.ui <- global.RoDs.ui[, !grepl("bound", colnames(global.RoDs.ui))]
      global.RoDs.ui.output <- rbind(colnames(global.RoDs.ui), global.RoDs.ui)
    } else {
      global.RoDs.ui.output <- cbind(data.frame(Region = "World"), global.RoDs.ui)
    }
    write.csv(global.RoDs.ui.output,
              file = file.path(output.dir, "Rates of Decline_World.csv"), row.names = F, na = "")
  }
}
#----------------------------------------------------------------------
# What it does: Collects caller context and dispatches regional processing through the refactored runner.
# Why it is needed: Existing call sites can keep using the same wrapper while newer logic lives in the refactor path.
GetRegionalResultsBWC <- function(
    output.dir,
    output.dir.samples,
    output.dir.samplescombined,
    regions, regiontypes,
    filename,
    run.on.server,
    percentiles,
    ndigits,
    replace.rates.reg,
    selected.iso = NULL,
    parallel.cores = NULL,
    output.rates.of.decline = NULL,
    round.output# DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
) {
  caller.country.info <- get0("country.info", envir = parent.frame(), inherits = TRUE)
  if (is.null(caller.country.info)) {
    stop("country.info was not found when preparing regional results.")
  }
  if (is.null(parallel.cores)) {
    caller.parallel.cores <- get0("parallel.cores.resolved", envir = parent.frame(), inherits = TRUE)
    parallel.cores <- if (is.null(caller.parallel.cores)) ResolveParallelCoresBWC(NULL) else caller.parallel.cores
  }
  if (is.null(selected.iso)) {
    selected.iso <- get0("selected.iso.regional", envir = parent.frame(), inherits = TRUE)
  }
  if (is.null(output.rates.of.decline)) {
    output.rates.of.decline <- get0("output.rates.of.decline", envir = parent.frame(), inherits = TRUE)
  }
  if (is.null(output.rates.of.decline)) {
    output.rates.of.decline <- FALSE
  }
  
  return(RunRegionalResultsBWCRefactor(
    output.dir = output.dir,
    output.dir.samples = output.dir.samples,
    output.dir.samplescombined = output.dir.samplescombined,
    regions = regions,
    regiontypes = regiontypes,
    filename = filename,
    run.on.server = run.on.server,
    percentiles = percentiles,
    ndigits = ndigits,
    replace.rates.reg = replace.rates.reg,
    selected.iso = selected.iso,
    iso.country.order = caller.country.info$ISO3Code,
    parallel.cores = parallel.cores,
    output.rates.of.decline = output.rates.of.decline,
    round.output = round.output
  ))
}
#----------------------------------------------------------------------
# What it does: Calculates one regional draw using the older regional aggregation implementation.
# Why it is needed: This legacy path is still kept in the file for compatibility and reference alongside the refactored flow.
CalculateRegionalDeathsBWC <- function(
    j,
    output.dir.samples,
    output.dir.samplescombined,
    regions,
    regiontypes,
    filename,
    replace.rates.reg, # DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
    iso.country.order # DJS 2022-01-06 edit to give country ISO order from cme info file, which is the order the country trajectories are in 
) {
  # load(file.path(output.dir.samplescombined, "info.rda"))
  # list2env(info, envir = environment())
  # load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
  # load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  # load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  # if(file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))){
  #   load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  # }
  if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
    load(file.path(output.dir.samplescombined, "info.rda"))
    #DJS edit 2022-03-21 reorder some elements of 'info' according to supplied country info file 
    order.country.info.file <- match(iso.country.order, info$iso.c)
    info$iso.c <- info$iso.c[order.country.info.file]
    info$year1.est.u1 <- info$year1.est.u1[order.country.info.file]
    info$year1.est.u5 <- info$year1.est.u5[order.country.info.file]
    info$a0.c <- info$a0.c[order.country.info.file]
    info$a1to4.c <- info$a1to4.c[order.country.info.file]
    info$pop0.ct <- info$pop0.ct[order.country.info.file,]
    info$pop1to4.ct <- info$pop1to4.ct[order.country.info.file,]
    load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
    load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
    load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
    
    nn.exists <- file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
    if(nn.exists){
      load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
      info$year1.est.nn <- info$year1.est.nn[order.country.info.file]
    }
    list2env(info, envir = environment())
  } else {
    load(file.path(output.dir.samplescombined, "info.rda"))
    #DJS edit 2022-03-21 reorder some elements of 'info' according to supplied country info file 
    order.country.info.file <- match(iso.country.order, info$iso.c)
    info$iso.c <- info$iso.c[order.country.info.file]
    info$year1.est.u1 <- info$year1.est.u1[order.country.info.file]
    info$year1.est.u5 <- info$year1.est.u5[order.country.info.file]
    info$a0.c <- info$a0.c[order.country.info.file]
    info$a1to4.c <- info$a1to4.c[order.country.info.file]
    info$pop0.ct <- info$pop0.ct[order.country.info.file,]
    info$pop1to4.ct <- info$pop1to4.ct[order.country.info.file,]
    load(file.path(output.dir.samplescombined, paste0("death0.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined,paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
    
    nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    if(nn.exists){
      load(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
      info$year1.est.nn <- info$year1.est.nn[order.country.info.file]
    }
    list2env(info, envir = environment())
  } # if/else  replace.rates.reg
  
  # order the country deaths in same ISO order as country info file # DS edit 2022-01-06
  # u5
  dimnames.ordered.deaths.u5 <- dimnames(deathu5.ctj)
  dimnames.ordered.deaths.u5[[1]] <- dimnames(deathu5.ctj)[[1]][match(iso.country.order, dimnames(deathu5.ctj)[[1]])]
  deathu5.ctj <- array(deathu5.ctj[match(iso.country.order, dimnames(deathu5.ctj)[[1]]),,], dim = dim(deathu5.ctj), dimnames = dimnames.ordered.deaths.u5)
  # u1
  dimnames.ordered.deaths.u1 <- dimnames(death0.ctj)
  dimnames.ordered.deaths.u1[[1]] <- dimnames(death0.ctj)[[1]][match(iso.country.order, dimnames(death0.ctj)[[1]])]
  death0.ctj <- array(death0.ctj[match(iso.country.order, dimnames(death0.ctj)[[1]]),,], dim = dim(death0.ctj), dimnames = dimnames.ordered.deaths.u1)
  # 1to4
  dimnames.ordered.deaths.1to4 <- dimnames(death1to4.ctj)
  dimnames.ordered.deaths.1to4[[1]] <- dimnames(death1to4.ctj)[[1]][match(iso.country.order, dimnames(death1to4.ctj)[[1]])]
  death1to4.ctj <- array(death1to4.ctj[match(iso.country.order, dimnames(death1to4.ctj)[[1]]),,], dim = dim(death1to4.ctj), dimnames = dimnames.ordered.deaths.1to4)
  # nn
  if(nn.exists){
    dimnames.ordered.deaths.nn <- dimnames(deathnn.ctj)
    dimnames.ordered.deaths.nn[[1]] <- dimnames(deathnn.ctj)[[1]][match(iso.country.order, dimnames(deathnn.ctj)[[1]])]
    deathnn.ctj <- array(deathnn.ctj[match(iso.country.order, dimnames(deathnn.ctj)[[1]]),,], dim = dim(deathnn.ctj), dimnames = dimnames.ordered.deaths.nn)
  }
  
  
  # edit DJS 2018-07-27 round deaths at country level before summing to region and world
  deathu5.ctj <- roundoff(deathu5.ctj, digits = 0)
  death0.ctj <- roundoff(death0.ctj, digits = 0)
  death1to4.ctj <- roundoff(death1to4.ctj, digits = 0)
  if(nn.exists) deathnn.ctj <- roundoff(deathnn.ctj, digits = 0)
  # edit DJS 2018-07-27
  
  nregs <- length(regiontypes)
  
  # infant and u5 deaths BWC method
  ## load dx and lx arrays once so not loaded at every j
  # load(file.path(output.dir.samplescombined, paste0("dx.array.ctj.rda")))
  # load(file.path(output.dir.samplescombined, paste0("lx.array.ctj.rda")))
  # nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.rda")))
  # if(nn.exists){
  #   load(file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.rda")))
  #   load(file.path(output.dir.samplescombined, paste0("lx.nn.array.ctj.rda")))
  # }
  #
  # dx.array.by.c <- dx.array.ctj[,,,j]
  # lx.array.by.c <- lx.array.ctj[,,,j]
  # if(nn.exists){
  #   dx.nn.array.by.c <- dx.nn.array.ctj[,,,j]
  #   lx.nn.array.by.c <- lx.nn.array.ctj[,,,j]
  # }
  
  # nn.exists <- file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  # load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
  # load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
  # if(nn.exists){
  # load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
  # load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
  # }
  
  if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
    load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
    if(nn.exists){
      load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
      load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
    }
  } else {
    load(file.path(output.dir.samples, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samples, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    if(nn.exists){
      load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
      load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    }
  } # if/else is.null(replace.rates.reg)
  
  # order the arrays by iso order in country.info # DS 2022-01-06 to calculate based on iso order in country.info
  load(file.path(output.dir.samples, "info.rda"))
  dx.array.by.c <- dx.array.by.c[,,match(iso.country.order, info$iso.c)]  
  lx.array.by.c <- lx.array.by.c[,,match(iso.country.order, info$iso.c)] 
  if(nn.exists){
    dx.nn.array.by.c <- dx.nn.array.by.c[,,match(iso.country.order, info$iso.c)] 
    lx.nn.array.by.c <- lx.nn.array.by.c[,,match(iso.country.order, info$iso.c)] 
  }
  
  ## load country mortality rates for later death calc
  load(file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
  load(file.path(output.dir.samplescombined, "imr.ctj.rda"))
  if(nn.exists) load(file.path(output.dir.samplescombined, "nmr.ctj.rda"))
  
  # order the country rates in same ISO order as country info file # DS edit 2022-01-06
  # u5
  dimnames.ordered.rates.u5 <- dimnames(u5mr.ctj)
  dimnames.ordered.rates.u5[[1]] <- dimnames(u5mr.ctj)[[1]][match(iso.country.order, dimnames(u5mr.ctj)[[1]])]
  u5mr.ctj <- array(u5mr.ctj[match(iso.country.order, dimnames(u5mr.ctj)[[1]]),,], dim = dim(u5mr.ctj), dimnames = dimnames.ordered.rates.u5)
  # u1
  dimnames.ordered.rates.u1 <- dimnames(imr.ctj)
  dimnames.ordered.rates.u1[[1]] <- dimnames(imr.ctj)[[1]][match(iso.country.order, dimnames(imr.ctj)[[1]])]
  imr.ctj <- array(imr.ctj[match(iso.country.order, dimnames(imr.ctj)[[1]]),,], dim = dim(imr.ctj), dimnames = dimnames.ordered.rates.u1)
  # nn
  if(nn.exists){
    dimnames.ordered.rates.nn <- dimnames(nmr.ctj)
    dimnames.ordered.rates.nn[[1]] <- dimnames(nmr.ctj)[[1]][match(iso.country.order, dimnames(nmr.ctj)[[1]])]
    nmr.ctj <- array(nmr.ctj[match(iso.country.order, dimnames(nmr.ctj)[[1]]),,], dim = dim(nmr.ctj), dimnames = dimnames.ordered.rates.nn)
  }
  
  nsim <- dim(u5mr.ctj)[3]
  
  # edit DJS 2018-03-09 remove rounding for median calculation
  # if (nsim == 1) {
  #   u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
  #   imr.ctj <- roundoff(imr.ctj, digits = ndigits)
  #   if(nn.exists) nmr.ctj <- roundoff(nmr.ctj, digits = ndigits)
  # }
  # end edit DJS 2018-03-09
  
  ## wgt and year matrixes for later death calc
  # wgt.mat
  weight.j.1 <- (53-(1:52))/52 #weight for mortality rate in year 1
  wgt.mat <- t(matrix(cbind(weight.j.1, 1-weight.j.1), ncol=10, nrow=52))
  if(nn.exists){
    # wgt.nmr.mat
    weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4)) #weight for mortality rate in year 1
    wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1-weight.nmr.j.1), ncol=2, nrow=52))
  }
  # years.mat
  years <- years.k <- seq(1950,1950+ncol(u5mr.ctj)-1,1)
  for(yr in years){
    ifelse(yr==1950, years.mat <- seq(yr+.5,yr+5,.5), years.mat <- cbind(years.mat, seq(yr+.5,yr+5,.5)))
  }
  
  # What it does: Converts a year and optional week offsets into BWC column positions.
  # Why it is needed: The legacy regional routine needs a compact helper for repeated weekly-array indexing.
  getBWC <- function(bwc=NULL, year){
    if(is.null(bwc)){
      bwc <- 1:52
    }
    return(((year-1950)*52)+bwc)
  }
  
  # create separate directory for each region type
  output.dir.samples.region <- file.path(output.dir.samples, filename)
  dir.create(file.path(getwd(), output.dir.samples.region), showWarnings = FALSE)
  
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
  M0.rt <- M1to4.rt <- q0.rt <- q1to4.rt <- q5.rt <- qnn.rt <-
    death0.rt <- death1to4.rt <- deathu5.rt <- deathnn.rt <- death0.all.rt <- death1to4.all.rt <- deathu5.all.rt <- deathnn.all.rt <- matrix(NA, nregs, nyears)
  
  removeNA <- T
  
  which.no.rates <- which(apply(!is.na(u5mr.ctj),1,sum)<1)
  
  for (r in 1:nregs) {
    if (filename %in% c("UNICEFProgRegion", "UNICEFReportRegion",  "MDGRegion", "SDGRegion", "SDGSimpleRegion",
                        "SDGRCRegion", "EAPRORegion", "WBRegion", "UNPDRegion", "OICRegion", "M49Region", "Wealthall", "Wealthdata", "AURegion", "AURECRegion",
                        "FCSCountries", "FragileCountries2025OECD", "SDG2015", "UNICEFReportRegion_nohigh")) {
      reg.num <- ChooseRegion(region = regiontypes[r], regiontype = filename)
      select.reg <- (1:nrow(regions))[regions[, is.element(colnames(regions),
                                                           paste0(filename, reg.num))] == regiontypes[r]]
      # to remove LIE from regional aggregate calcualtions -- to implement in 2018
      select.reg.og <- select.reg
      ifelse(is.na(match(which.no.rates,select.reg.og)), select.reg <- select.reg.og, select.reg <- select.reg.og[-match(which.no.rates,select.reg.og)])
    } else if (filename %in% c("WHORegion", "CountdownCountries", "ECAAfricaRegion","GlobalStrategyCountries",
                               "FragileCountries2013", "FragileCountries2014", "FragileCountries2015",
                               "FragileCountries2017", "FragileCountries2018", "FragileCountries2018OECD1", "FragileCountries2018OECD2", "FragileCountries2019", "JHUFragile2021", 
                               "WealthallGlobal", "WealthdataGlobal", "WorldBankReg2", "NewWorldBank", "USAIDCountries", "AfricanEconomicCommunityRegion",
                               "ECACountries", "GAVICountries", "SPhumanitarianRegion","SPhighburdenRegion","JHURegion","LiSTRegion","MENAEMRORegion","EECARegion")) {
      select.reg <- (1:length(regions))[regions == regiontypes[r]]
      # to remove LIE from regional aggregate calcualtions -- to implement in 2018
      select.reg.og <- (1:length(regions))[regions == regiontypes[r]]
      ifelse(is.na(match(which.no.rates,select.reg.og)), select.reg <- select.reg.og, select.reg <- select.reg.og[-match(which.no.rates,select.reg.og)])
    }
    if (j == 1) { # calculate the first time
      for (i in 1:nyears) {
        # check that population coverage > 50% per region
        pop0.rt[r, i] <- sum(pop0.ct[select.reg,i])
        pop1to4.rt[r, i] <- sum(pop1to4.ct[select.reg,i])
        pop0.orig.rt[r, i] <- sum(pop0.orig.ct[order.country.info.file,][select.reg,i])
        pop1to4.orig.rt[r, i] <- sum(pop1to4.orig.ct[order.country.info.file,][select.reg,i])
        popu5.orig.rt[r, i] <- pop0.orig.rt[r, i] + pop1to4.orig.rt[r, i]
        coverage0.rt[r, i] <- pop0.rt[r, i]/pop0.orig.rt[r, i]
        coverageu5.rt[r, i] <- (pop0.rt[r, i] + pop1to4.rt[r, i])/(popu5.orig.rt[r, i])
      }
      # save the first time
      save(pop0.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.rt.rda")))
      save(pop1to4.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.rt.rda")))
      save(popu5.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
      save(pop0.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
      save(pop1to4.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.orig.rt.rda")))
      save(coverageu5.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_coverageu5.rt.rda")))
      save(coverage0.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_coverage0.rt.rda")))
    }
    for (i in 1:nyears) {
      # calculate deaths
      death0.rt[r, i] <- sum(death0.ctj[select.reg, i, j], na.rm = T)
      death1to4.rt[r, i] <- sum(death1to4.ctj[select.reg, i, j], na.rm = T)
      deathu5.rt[r, i] <- sum(deathu5.ctj[select.reg, i, j], na.rm = T)
      if(nn.exists) deathnn.rt[r, i] <- sum(deathnn.ctj[select.reg, i, j], na.rm = T)
      
      # calculate rates
      q0.rt[r, i] <- sum(dx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)/sum(lx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)
      
      if(i>1){
        q1to4.rt[r, i] <- 1-((1-(sum(dx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),select.reg], na.rm=removeNA)/sum(lx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),select.reg], na.rm=removeNA)))^4)
      }
      
      if(nn.exists){
        qnn.rt[r, i] <- sum(dx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)/sum(lx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)
      }
      
      q5.rt[r, i] <- 1-(1-q0.rt[r, i])*(1-q1to4.rt[r, i])
    } # i loop for years
    
    if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
      ## do BWC method again for all countries in region replacing missing rates with regional rates, then sum deaths at world level for deathXX.all.wtj
      u5mr.temp.rt <- u5mr.ctj[select.reg,,j]
      imr.temp.rt <- imr.ctj[select.reg,,j]
      if(nn.exists) nmr.temp.rt <- nmr.ctj[select.reg,,j]
      livebirths.rt <- livebirths.ct[select.reg,]
      
      deathu5.temp.rt <- death0.temp.rt <- deathnn.temp.rt <- matrix(NA, nrow(u5mr.temp.rt), nyears)
      
      for(k in 1:dim(u5mr.temp.rt)[1]){
        ## get live births for years.k
        wpp.livebirths.k <- as.numeric(livebirths.rt[k,match(years.k,years)])
        #if(nrow(wpp.livebirths.k)<1) next
        bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
        
        for(ik in 1:length(years.k)){
          if(years.k[ik]+5>max(years.k)){
            # u1 mortality rates; has IMR for year[i] and year[i+1]
            nmx.u1.i <- imr.temp.rt[k,match(years.k[ik:length(years.k)],years)]
            nmx.u1.i[is.na(nmx.u1.i)] <- q0.rt[r,match(years.k[ik:length(years.k)],years)[is.na(nmx.u1.i)]]*1000
            # u5 mortality rates; need same length as u1 to convert to 4q1
            nmx.u5.i <- u5mr.temp.rt[k,match(years.k[ik:length(years.k)],years)]
            nmx.u5.i[is.na(nmx.u5.i)] <- q5.rt[r,match(years.k[ik:length(years.k)],years)[is.na(nmx.u5.i)]]*1000
            # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
            nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
            nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
            nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
            # combine appropriate rates in mortality rate vector
            nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
          } else {
            # u1 mortality rates; has IMR for year[i] and year[i+1]
            nmx.u1.i <- imr.temp.rt[k,match(years.k[ik]:(years.k[ik]+5),years)]
            nmx.u1.i[is.na(nmx.u1.i)] <- q0.rt[r,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u1.i)]]*1000
            # u5 mortality rates; need same length as u1 to convert to 4q1
            nmx.u5.i <- u5mr.temp.rt[k,match(years.k[ik]:(years.k[ik]+5),years)]
            nmx.u5.i[is.na(nmx.u5.i)] <- q5.rt[r,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u5.i)]]*1000
            # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
            nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
            nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
            nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
            # combine in mortality rate vector for lifetable function
            nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
          } #if/else
          
          ## turn nmx.i into matrix
          ifelse(ik==1, nmx.mat.k <- matrix(nmx.i, nrow=10, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=10, ncol=52)))
          #if(nsim==1) nmx.mat.k <- roundoff(nmx.mat.k, digits=1)
          # nmr
          if(nn.exists){
            if(years.k[ik]+1>max(years.k)){
              nmr.i <- c(nmr.temp.rt[k,match(years.k[ik],years)], NA)
              nmr.i[is.na(nmr.i)] <- qnn.rt[r,match(years.k[ik:length(years.k)],years)[is.na(nmr.i)]]*1000
            } else {
              nmr.i <- nmr.temp.rt[k,match(years.k[ik]:years.k[ik+1],years)]
              nmr.i[is.na(nmr.i)] <- qnn.rt[r,match(years.k[ik]:years.k[ik+1],years)[is.na(nmr.i)]]*1000
            } # if/else
            
            ## turn nmr.i into matrix
            ifelse(ik==1, nmr.mat.k <- matrix(nmr.i, nrow=2, ncol=52), nmr.mat.k <- cbind(nmr.mat.k ,matrix(nmr.i, nrow=2, ncol=52)))
            # if(nsim==1) nmr.mat.k <- roundoff(nmr.mat.k,digits=1)
          }
        } # ik loop for nmx matrix
        
        ## get infant and u5 deaths
        wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=10, ncol=length(years.k)*52)
        nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
        npx.mat.k <- 1-nqx.mat.k
        lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
        dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
        
        ## get nn deaths
        if(nn.exists){
          wgt.nn.mat.k <- matrix(rep(wgt.nmr.mat,length(years.k)),nrow=2, ncol=length(years.k)*52)
          nqx.nn.mat.k <- wgt.nn.mat.k*(nmr.mat.k/1000)
          npx.nn.mat.k <- 1-nqx.nn.mat.k
          lx.nn.mat.k <- apply(rbind(bwc.vec.k, npx.nn.mat.k),2,cumprod)
          dx.nn.mat.k <- -apply(lx.nn.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
        }
        
        years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=10, ncol=length(years.k)*52)
        years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
        
        for(yrk in min(years.k):max(years.k)){
          deathu5.temp.rt[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T)
          death0.temp.rt[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
          if(nn.exists) deathnn.temp.rt[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
        } # for loop for summing deaths
        # edit DJS 2018-07-27 round deaths at country level before summing to region and world
        deathu5.temp.rt <- roundoff(deathu5.temp.rt, digits = 0)
        death0.temp.rt <- roundoff(death0.temp.rt, digits = 0)
        if(nn.exists) deathnn.temp.rt <- roundoff(deathnn.temp.rt, digits = 0)
        # edit DJS 2018-07-27
      } # k loop for countries in the region
      
      
      death0.all.rt[r,] <- apply(death0.temp.rt, 2, sum, na.rm=F)
      deathu5.all.rt[r,] <- apply(deathu5.temp.rt, 2, sum, na.rm=F)
      if(nn.exists) deathnn.all.rt[r,] <- apply(deathnn.temp.rt, 2, sum, na.rm=F)
      
    } else {# if replace.rates.reg
      death0.all.rt <- death0.rt
      deathu5.all.rt <- deathu5.rt
      if(nn.exists) deathnn.all.rt <- deathnn.rt
    } # else
    
    
    
    save(q0.rt, file = file.path(output.dir.samples.region, paste0("q0.rt_", j, ".rda")))
    save(q1to4.rt, file = file.path(output.dir.samples.region, paste0("q1to4.rt_", j, ".rda")))
    save(q5.rt, file = file.path(output.dir.samples.region, paste0("q5.rt_", j, ".rda")))
    save(death0.all.rt, file = file.path(output.dir.samples.region, paste0("death0.all.rt_", j, ".rda")))
    save(death1to4.all.rt, file = file.path(output.dir.samples.region, paste0("death1to4.all.rt_", j, ".rda")))
    save(deathu5.all.rt, file = file.path(output.dir.samples.region, paste0("deathu5.all.rt_", j, ".rda")))
    if(nn.exists){
      save(qnn.rt, file = file.path(output.dir.samples.region, paste0("qnn.rt_", j, ".rda")))
      save(deathnn.all.rt, file = file.path(output.dir.samples.region, paste0("deathnn.all.rt_", j, ".rda")))
    } # if
  } # r loop regions
}
#----------------------------------------------------------------------
# What it does: Loads and aligns all regional inputs, caches, memberships, and metadata into one context object.
# Why it is needed: The refactored regional runner depends on one prepared context instead of repeatedly reloading shared data.
PrepareRegionalContextBWCRefactor <- function(
    output.dir.samples,
    output.dir.samplescombined,
    regions,
    regiontypes,
    filename,
    replace.rates.reg,
    iso.country.order,
    selected.iso = NULL
) {
  regions[is.na(regions)] <- 0
  load(file.path(output.dir.samplescombined, "info.rda"))
  order.country.info.file <- match(iso.country.order, info$iso.c)
  if (any(is.na(order.country.info.file))) {
    stop("Could not align the regional context to the requested country order.")
  }
  
  info$iso.c <- info$iso.c[order.country.info.file]
  info$year1.est.u1 <- info$year1.est.u1[order.country.info.file]
  info$year1.est.u5 <- info$year1.est.u5[order.country.info.file]
  info$a0.c <- info$a0.c[order.country.info.file]
  info$a1to4.c <- info$a1to4.c[order.country.info.file]
  info$pop0.ct <- as.matrix(info$pop0.ct[order.country.info.file, , drop = FALSE])
  info$pop1to4.ct <- as.matrix(info$pop1to4.ct[order.country.info.file, , drop = FALSE])
  info$pop0.orig.ct <- as.matrix(info$pop0.orig.ct[order.country.info.file, , drop = FALSE])
  info$pop1to4.orig.ct <- as.matrix(info$pop1to4.orig.ct[order.country.info.file, , drop = FALSE])
  info$livebirths.ct <- as.matrix(info$livebirths.ct[order.country.info.file, , drop = FALSE])
  storage.mode(info$pop0.ct) <- "double"
  storage.mode(info$pop1to4.ct) <- "double"
  storage.mode(info$pop0.orig.ct) <- "double"
  storage.mode(info$pop1to4.orig.ct) <- "double"
  storage.mode(info$livebirths.ct) <- "double"
  if (!is.null(info$year1.est.nn)) {
    info$year1.est.nn <- info$year1.est.nn[order.country.info.file]
  }
  
  if (is.null(replace.rates.reg)) {
    load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
    load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
    load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
    nn.exists <- file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
    if (nn.exists) {
      load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
    }
  } else {
    load(file.path(output.dir.samplescombined, paste0("death0.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined, paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
    nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    if (nn.exists) {
      load(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    }
  }
  
  death0.ctj <- roundoff(death0.ctj[order.country.info.file, , , drop = FALSE], digits = 0)
  death1to4.ctj <- roundoff(death1to4.ctj[order.country.info.file, , , drop = FALSE], digits = 0)
  deathu5.ctj <- roundoff(deathu5.ctj[order.country.info.file, , , drop = FALSE], digits = 0)
  if (nn.exists) {
    deathnn.ctj <- roundoff(deathnn.ctj[order.country.info.file, , , drop = FALSE], digits = 0)
  } else {
    deathnn.ctj <- NULL
  }
  
  load(file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
  load(file.path(output.dir.samplescombined, "imr.ctj.rda"))
  u5mr.ctj <- u5mr.ctj[order.country.info.file, , , drop = FALSE]
  imr.ctj <- imr.ctj[order.country.info.file, , , drop = FALSE]
  nmr.ctj <- NULL
  if (nn.exists) {
    load(file.path(output.dir.samplescombined, "nmr.ctj.rda"))
    nmr.ctj <- nmr.ctj[order.country.info.file, , , drop = FALSE]
  }
  
  which.no.rates <- which(apply(!is.na(u5mr.ctj), 1, sum) < 1)
  select.reg.list <- BuildRegionalIndexListBWC(
    regions = regions,
    regiontypes = regiontypes,
    filename = filename,
    which.no.rates = which.no.rates
  )
  
  membership.mat <- matrix(0, nrow = length(regiontypes), ncol = dim(u5mr.ctj)[1])
  for (r in seq_along(select.reg.list)) {
    if (length(select.reg.list[[r]]) > 0) {
      membership.mat[r, select.reg.list[[r]]] <- 1
    }
  }

  selected.region.idx <- integer(0)
  selected.region.names <- character(0)
  selected.partial.active <- FALSE
  selected.partial.ignored <- FALSE
  selected.partial.supported <- !is.null(selected.iso) &&
    (identical(filename, "M49Region") || !is.null(replace.rates.reg))
  if (selected.partial.supported) {
    selected.country.order.idx <- match(selected.iso, info$iso.c)
    selected.country.order.idx <- selected.country.order.idx[!is.na(selected.country.order.idx)]
    if (length(selected.country.order.idx) > 0) {
      selected.region.idx <- which(rowSums(membership.mat[, selected.country.order.idx, drop = FALSE]) > 0)
      if (length(selected.region.idx) > 0) {
        region.cache.exists <- all(file.exists(file.path(
          output.dir.samplescombined,
          RegionalCombinedCacheFilesBWC(filename = filename, nn.exists = nn.exists)
        )))
        selected.partial.active <- region.cache.exists
        selected.partial.ignored <- !region.cache.exists
        if (selected.partial.active) {
          selected.region.names <- regiontypes[selected.region.idx]
        } else {
          selected.region.idx <- integer(0)
        }
      }
    }
  }
  
  output.dir.samples.region <- file.path(output.dir.samples, filename)
  dir.create(output.dir.samples.region, showWarnings = FALSE, recursive = TRUE)
  
  pop.files <- c(
    file.path(output.dir.samplescombined, paste0(filename, "_pop0.rt.rda")),
    file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.rt.rda")),
    file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")),
    file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")),
    file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.orig.rt.rda")),
    file.path(output.dir.samplescombined, paste0(filename, "_coverageu5.rt.rda")),
    file.path(output.dir.samplescombined, paste0(filename, "_coverage0.rt.rda"))
  )
  
  if (all(file.exists(pop.files))) {
    load(pop.files[1]); load(pop.files[2]); load(pop.files[3]); load(pop.files[4]); load(pop.files[5]); load(pop.files[6]); load(pop.files[7])
  } else {
    pop0.rt <- membership.mat %*% info$pop0.ct
    pop1to4.rt <- membership.mat %*% info$pop1to4.ct
    pop0.orig.rt <- membership.mat %*% info$pop0.orig.ct
    pop1to4.orig.rt <- membership.mat %*% info$pop1to4.orig.ct
    popu5.orig.rt <- pop0.orig.rt + pop1to4.orig.rt
    coverage0.rt <- pop0.rt/pop0.orig.rt
    coverageu5.rt <- (pop0.rt + pop1to4.rt)/popu5.orig.rt
    save(pop0.rt, file = pop.files[1])
    save(pop1to4.rt, file = pop.files[2])
    save(popu5.orig.rt, file = pop.files[3])
    save(pop0.orig.rt, file = pop.files[4])
    save(pop1to4.orig.rt, file = pop.files[5])
    save(coverageu5.rt, file = pop.files[6])
    save(coverage0.rt, file = pop.files[7])
  }
  
  years <- seq(1950, 1950 + dim(u5mr.ctj)[2] - 1, 1)
  years.mat <- NULL
  for (yr in years) {
    if (is.null(years.mat)) {
      years.mat <- seq(yr + .5, yr + 5, .5)
    } else {
      years.mat <- cbind(years.mat, seq(yr + .5, yr + 5, .5))
    }
  }
  weight.j.1 <- (53 - (1:52))/52
  wgt.mat <- t(matrix(cbind(weight.j.1, 1 - weight.j.1), ncol = 10, nrow = 52))
  if (nn.exists) {
    weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4))
    wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1 - weight.nmr.j.1), ncol = 2, nrow = 52))
  } else {
    wgt.nmr.mat <- NULL
  }
  country.calc.ctx <- BuildCountryCalculationContextBWC(
    nyears = dim(u5mr.ctj)[2],
    years.start = years[1],
    nn.exists = nn.exists
  )
  
  list(
    output.dir.samples = output.dir.samples,
    output.dir.samples.region = output.dir.samples.region,
    filename = filename,
    replace.rates.reg = replace.rates.reg,
    info = info,
    regiontypes = regiontypes,
    nregs = length(regiontypes),
    nsim = dim(deathu5.ctj)[3],
    nyears = dim(u5mr.ctj)[2],
    nn.exists = nn.exists,
    order.country.info.file = order.country.info.file,
    select.reg.list = select.reg.list,
    membership.mat = membership.mat,
    selected.region.idx = selected.region.idx,
    selected.region.names = selected.region.names,
    selected.partial.active = selected.partial.active,
    selected.partial.ignored = selected.partial.ignored,
    death0.ctj = death0.ctj,
    death1to4.ctj = death1to4.ctj,
    deathu5.ctj = deathu5.ctj,
    deathnn.ctj = deathnn.ctj,
    u5mr.ctj = u5mr.ctj,
    imr.ctj = imr.ctj,
    nmr.ctj = nmr.ctj,
    years = years,
    years.mat = years.mat,
    q0.cohort.idx = ((floor(info$est.years) - 1950) * 52) + 1L,
    q1.cohort.idx = c(NA_integer_, (((floor(info$est.years)[-length(info$est.years)] - 1950) * 52) + 1L)),
    wgt.mat = wgt.mat,
    wgt.nmr.mat = wgt.nmr.mat,
    country.calc.ctx = country.calc.ctx
  )
}
#----------------------------------------------------------------------
# What it does: Runs the refactored regional workflow, including generation, retries, and final combination.
# Why it is needed: Regional processing now needs centralized control over caching, progress logging, and partial rebuild logic.
RunRegionalResultsBWCRefactor <- function(
    output.dir,
    output.dir.samples,
    output.dir.samplescombined,
    regions,
    regiontypes,
    filename,
    run.on.server,
    percentiles,
    ndigits,
    replace.rates.reg,
    selected.iso,
    iso.country.order,
    parallel.cores,
    output.rates.of.decline,
    round.output
) {
  region.start <- proc.time()[["elapsed"]]
  cat(paste0("Generating output for ", filename, "...\n"))
  
  regional.ctx <- PrepareRegionalContextBWCRefactor(
    output.dir.samples = output.dir.samples,
    output.dir.samplescombined = output.dir.samplescombined,
    regions = regions,
    regiontypes = regiontypes,
    filename = filename,
    replace.rates.reg = replace.rates.reg,
    iso.country.order = iso.country.order,
    selected.iso = selected.iso
  )
  nsim <- regional.ctx$nsim
  if (isTRUE(regional.ctx$selected.partial.ignored)) {
    cat(paste0("Ignoring selected_iso for ", filename,
               " generation because no existing ", filename, " cache was found in ",
               output.dir.samplescombined, ". Running full regeneration instead.\n"))
  }
  if (isTRUE(regional.ctx$selected.partial.active)) {
    cat(paste0("Updating ", filename, " results for ", length(regional.ctx$selected.region.idx),
               " affected region(s): ", paste(regional.ctx$selected.region.names, collapse = ", "), "\n"))
  }
  
  if (run.on.server) {
    regional.progress.start <- proc.time()[["elapsed"]]
    regional.chunks <- BuildProgressChunksBWC(
      total = nsim,
      chunk.size = ResolveProgressChunkSizeBWC(
        total = nsim,
        parallel.cores = parallel.cores,
        progress.every = 1000L
      )
    )
    for (chunk in regional.chunks) {
      if (length(chunk) == 1L) {
        CalculateRegionalDeathsBWCAtomic(j = chunk[[1]], regional.ctx = regional.ctx)
      } else {
        registerDoMC(cores = min(parallel.cores, length(chunk)))
        foreach (j = chunk) %dopar% {
          CalculateRegionalDeathsBWCAtomic(j = j, regional.ctx = regional.ctx)
        }
      }
      launched <- max(chunk)
      valid.bundles <- sum(vapply(seq_len(launched), function(idx) {
        RegionalBundleExistsBWC(regional.ctx$output.dir.samples.region, idx)
      }, logical(1)))
      LogRoutProgressBWC(stage = paste0("regional generation: ", filename),
                         completed = launched,
                         total = nsim,
                         start.time = regional.progress.start,
                         extra = sprintf("valid bundles=%d", valid.bundles))
    }
  } else {
    for (j in seq_len(nsim)) {
      CalculateRegionalDeathsBWCAtomic(j = j, regional.ctx = regional.ctx)
    }
  }
  
  retry.count <- 0L
  max.retries <- 10L
  retried.trajs <- integer(0)
  missing.trajs <- which(!vapply(seq_len(nsim), function(j) {
    RegionalBundleExistsBWC(regional.ctx$output.dir.samples.region, j)
  }, logical(1)))
  
  while (length(missing.trajs) > 0 && retry.count < max.retries) {
    retry.count <- retry.count + 1L
    retried.trajs <- unique(c(retried.trajs, missing.trajs))
    missing.preview <- paste(utils::head(missing.trajs, 25), collapse = ", ")
    if (length(missing.trajs) > 25) {
      missing.preview <- paste0(missing.preview, ", ...")
    }
    cat(paste0("Warning: ", length(missing.trajs), " regional bundle(s) missing for ", filename,
               " (attempt ", retry.count, "/", max.retries, "). Re-running: ", missing.preview, "\n"))
    
    if (run.on.server && length(missing.trajs) > 1) {
      registerDoMC(cores = parallel.cores)
      foreach (j = missing.trajs) %dopar% {
        CalculateRegionalDeathsBWCAtomic(j = j, regional.ctx = regional.ctx)
      }
    } else {
      for (j in missing.trajs) {
        CalculateRegionalDeathsBWCAtomic(j = j, regional.ctx = regional.ctx)
      }
    }
    
    missing.trajs <- which(!vapply(seq_len(nsim), function(j) {
      RegionalBundleExistsBWC(regional.ctx$output.dir.samples.region, j)
    }, logical(1)))
    LogRoutProgressBWC(stage = paste0("regional retry: ", filename),
                       completed = nsim - length(missing.trajs),
                       total = nsim,
                       start.time = region.start,
                       extra = sprintf("attempt=%d remaining=%d", retry.count, length(missing.trajs)))
  }
  
  if (length(missing.trajs) > 0) {
    stop(paste0("ERROR: ", length(missing.trajs), " regional bundle(s) still missing for ", filename,
                " after ", max.retries, " retries: ", paste(missing.trajs, collapse = ", ")))
  }
  
  cat(paste0("Combining and outputting regional results...\n"))
  CombineAndOutputRegionalResultsBWC(output.dir = output.dir,
                                     output.dir.samples = output.dir.samples,
                                     output.dir.samplescombined = output.dir.samplescombined,
                                     regiontypes = regiontypes,
                                     filename = filename,
                                     percentiles = percentiles,
                                     ndigits = ndigits,
                                     replace.rates.reg = replace.rates.reg,
                                     selected.region.idx = if (isTRUE(regional.ctx$selected.partial.active)) regional.ctx$selected.region.idx else NULL,
                                     output.rates.of.decline = output.rates.of.decline,
                                     round.output = round.output)
  elapsed <- GetPhaseElapsedBWC(region.start)
  cat(sprintf("Regional summary for %s: %.2f s, retries=%d, retried_trajectories=%d\n",
              filename, elapsed, retry.count, length(retried.trajs)))
  invisible(list(filename = filename, elapsed = elapsed, retries = retry.count,
                 retried_trajectories = length(retried.trajs)))
}
#----------------------------------------------------------------------
# What it does: Calculates and saves one regional draw from the prepared context.
# Why it is needed: The refactored runner parallelizes work at the per-draw level, so each draw needs an isolated atomic worker.
CalculateRegionalDeathsBWCAtomic <- function(
    j,
    regional.ctx
) {
  if (is.null(regional.ctx$replace.rates.reg)) {
    load(file.path(regional.ctx$output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
    load(file.path(regional.ctx$output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
    if (regional.ctx$nn.exists) {
      load(file.path(regional.ctx$output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
      load(file.path(regional.ctx$output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
    }
  } else {
    load(file.path(regional.ctx$output.dir.samples, paste0("dx.array.ct_", j, "_", regional.ctx$replace.rates.reg, "-replace.rda")))
    load(file.path(regional.ctx$output.dir.samples, paste0("lx.array.ct_", j, "_", regional.ctx$replace.rates.reg, "-replace.rda")))
    if (regional.ctx$nn.exists) {
      load(file.path(regional.ctx$output.dir.samples, paste0("dx.nn.array.ct_", j, "_", regional.ctx$replace.rates.reg, "-replace.rda")))
      load(file.path(regional.ctx$output.dir.samples, paste0("lx.nn.array.ct_", j, "_", regional.ctx$replace.rates.reg, "-replace.rda")))
    }
  }
  
  dx.array.by.c <- dx.array.by.c[, , regional.ctx$order.country.info.file, drop = FALSE]
  lx.array.by.c <- lx.array.by.c[, , regional.ctx$order.country.info.file, drop = FALSE]
  if (regional.ctx$nn.exists) {
    dx.nn.array.by.c <- dx.nn.array.by.c[, , regional.ctx$order.country.info.file, drop = FALSE]
    lx.nn.array.by.c <- lx.nn.array.by.c[, , regional.ctx$order.country.info.file, drop = FALSE]
  }
  
  # What it does: Pulls one age slice from the cohort array and reshapes it for regional matrix multiplication.
  # Why it is needed: Regional rate aggregation repeatedly needs the same extraction and reshape step.
  extract.cohort.matrix <- function(arr, age.index, cohort.indices) {
    result <- t(vapply(cohort.indices, function(idx) arr[age.index, idx, ], numeric(ncol(regional.ctx$membership.mat))))
    result[is.na(result)] <- 0
    result
  }
  
  death0.slice <- regional.ctx$death0.ctj[, , j]
  death0.slice[is.na(death0.slice)] <- 0
  death1to4.slice <- regional.ctx$death1to4.ctj[, , j]
  death1to4.slice[is.na(death1to4.slice)] <- 0
  deathu5.slice <- regional.ctx$deathu5.ctj[, , j]
  deathu5.slice[is.na(deathu5.slice)] <- 0
  
  death0.rt <- regional.ctx$membership.mat %*% death0.slice
  death1to4.rt <- regional.ctx$membership.mat %*% death1to4.slice
  deathu5.rt <- regional.ctx$membership.mat %*% deathu5.slice
  
  q0.num <- regional.ctx$membership.mat %*% t(extract.cohort.matrix(dx.array.by.c, 1L, regional.ctx$q0.cohort.idx))
  q0.den <- regional.ctx$membership.mat %*% t(extract.cohort.matrix(lx.array.by.c, 1L, regional.ctx$q0.cohort.idx))
  q0.rt <- SafeDivideMatrixBWC(q0.num, q0.den)
  
  q1to4.rt <- matrix(NA_real_, nrow = regional.ctx$nregs, ncol = regional.ctx$nyears)
  if (regional.ctx$nyears > 1) {
    q1.num <- regional.ctx$membership.mat %*% t(extract.cohort.matrix(dx.array.by.c, 3L, regional.ctx$q1.cohort.idx[-1]))
    q1.den <- regional.ctx$membership.mat %*% t(extract.cohort.matrix(lx.array.by.c, 3L, regional.ctx$q1.cohort.idx[-1]))
    q1.base <- SafeDivideMatrixBWC(q1.num, q1.den)
    q1to4.rt[, -1] <- 1 - ((1 - q1.base)^4)
  }
  q5.rt <- 1 - (1 - q0.rt) * (1 - q1to4.rt)
  
  if (regional.ctx$nn.exists) {
    deathnn.slice <- regional.ctx$deathnn.ctj[, , j]
    deathnn.slice[is.na(deathnn.slice)] <- 0
    deathnn.rt <- regional.ctx$membership.mat %*% deathnn.slice
    qnn.num <- regional.ctx$membership.mat %*% t(extract.cohort.matrix(dx.nn.array.by.c, 1L, regional.ctx$q0.cohort.idx))
    qnn.den <- regional.ctx$membership.mat %*% t(extract.cohort.matrix(lx.nn.array.by.c, 1L, regional.ctx$q0.cohort.idx))
    qnn.rt <- SafeDivideMatrixBWC(qnn.num, qnn.den)
  } else {
    deathnn.rt <- NULL
    qnn.rt <- NULL
  }
  
  if (is.null(regional.ctx$replace.rates.reg)) {
    death0.all.rt <- death0.rt
    deathu5.all.rt <- deathu5.rt
    deathnn.all.rt <- deathnn.rt
    region.loop.idx <- if (isTRUE(regional.ctx$selected.partial.active)) regional.ctx$selected.region.idx else seq_len(regional.ctx$nregs)
    
    for (r in region.loop.idx) {
      select.reg <- regional.ctx$select.reg.list[[r]]
      if (length(select.reg) == 0) {
        next
      }
      
      u5mr.temp.rt <- matrix(regional.ctx$u5mr.ctj[select.reg, , j], nrow = length(select.reg), ncol = regional.ctx$nyears)
      imr.temp.rt <- matrix(regional.ctx$imr.ctj[select.reg, , j], nrow = length(select.reg), ncol = regional.ctx$nyears)
      nmr.temp.rt <- NULL
      if (regional.ctx$nn.exists) {
        nmr.temp.rt <- matrix(regional.ctx$nmr.ctj[select.reg, , j], nrow = length(select.reg), ncol = regional.ctx$nyears)
      }
      
      countries.needing.rebuild <- IdentifyCountriesNeedingRegionalRebuildBWC(
        u5mr.temp.rt = u5mr.temp.rt,
        imr.temp.rt = imr.temp.rt,
        nmr.temp.rt = nmr.temp.rt
      )
      if (!any(countries.needing.rebuild)) {
        next
      }
      rebuild.local.idx <- which(countries.needing.rebuild)
      rebuild.country.idx <- select.reg[rebuild.local.idx]
      replace.u5mr.row <- q5.rt[r, ] * 1000
      replace.imr.row <- q0.rt[r, ] * 1000
      replace.nmr.row <- if (regional.ctx$nn.exists) qnn.rt[r, ] * 1000 else NULL
      rebuilt.death0 <- numeric(regional.ctx$nyears)
      rebuilt.deathu5 <- numeric(regional.ctx$nyears)
      rebuilt.deathnn <- if (regional.ctx$nn.exists) numeric(regional.ctx$nyears) else NULL
      
      for (local.idx in rebuild.local.idx) {
        country.life.table <- BuildCountryLifeTableBWC(
          u5mr.row = u5mr.temp.rt[local.idx, ],
          imr.row = imr.temp.rt[local.idx, ],
          nmr.row = if (regional.ctx$nn.exists) nmr.temp.rt[local.idx, ] else NULL,
          livebirths.row = regional.ctx$info$livebirths.ct[select.reg[local.idx], ],
          country.ctx = regional.ctx$country.calc.ctx,
          replace.u5mr.row = replace.u5mr.row,
          replace.imr.row = replace.imr.row,
          replace.nmr.row = replace.nmr.row,
          use.full.years = TRUE
        )
        death.summary <- SummariseRegionalLifeTableDeathsBWC(
          country.life.table = country.life.table,
          years = regional.ctx$years,
          nn.exists = regional.ctx$nn.exists
        )
        rebuilt.death0 <- rebuilt.death0 + death.summary$death0
        rebuilt.deathu5 <- rebuilt.deathu5 + death.summary$deathu5
        if (regional.ctx$nn.exists) {
          rebuilt.deathnn <- rebuilt.deathnn + death.summary$deathnn
        }
      }
      death0.all.rt[r, ] <- death0.all.rt[r, ] -
        colSums(death0.slice[rebuild.country.idx, , drop = FALSE], na.rm = TRUE) +
        rebuilt.death0
      deathu5.all.rt[r, ] <- deathu5.all.rt[r, ] -
        colSums(deathu5.slice[rebuild.country.idx, , drop = FALSE], na.rm = TRUE) +
        rebuilt.deathu5
      if (regional.ctx$nn.exists) {
        deathnn.all.rt[r, ] <- deathnn.all.rt[r, ] -
          colSums(deathnn.slice[rebuild.country.idx, , drop = FALSE], na.rm = TRUE) +
          rebuilt.deathnn
      }
    }
  } else {
    death0.all.rt <- death0.rt
    deathu5.all.rt <- deathu5.rt
    deathnn.all.rt <- deathnn.rt
  }

  if (isTRUE(regional.ctx$selected.partial.active)) {
    unaffected.reg.idx <- setdiff(seq_len(regional.ctx$nregs), regional.ctx$selected.region.idx)
    if (length(unaffected.reg.idx) > 0) {
      q0.rt[unaffected.reg.idx, ] <- NA_real_
      q1to4.rt[unaffected.reg.idx, ] <- NA_real_
      q5.rt[unaffected.reg.idx, ] <- NA_real_
      death0.all.rt[unaffected.reg.idx, ] <- NA_real_
      death1to4.rt[unaffected.reg.idx, ] <- NA_real_
      deathu5.all.rt[unaffected.reg.idx, ] <- NA_real_
      if (regional.ctx$nn.exists) {
        qnn.rt[unaffected.reg.idx, ] <- NA_real_
        deathnn.all.rt[unaffected.reg.idx, ] <- NA_real_
      }
    }
  }
  
  SaveRegionalBundleBWC(bundle = list(q0.rt = q0.rt, q1to4.rt = q1to4.rt,
                                      q5.rt = q5.rt, death0.all.rt = death0.all.rt, death1to4.all.rt = death1to4.rt,
                                      deathu5.all.rt = deathu5.all.rt, qnn.rt = qnn.rt, deathnn.all.rt = deathnn.all.rt,
                                      nn.exists = regional.ctx$nn.exists),
                        output.dir.samples.region = regional.ctx$output.dir.samples.region,
                        j = j)
}
#----------------------------------------------------------------------
# What it does: Combines saved regional draw bundles and writes the final regional summary outputs.
# Why it is needed: Regional calculations are saved per draw, so a final pass is required to assemble summaries and export files.
CombineAndOutputRegionalResultsBWC <- function(
    output.dir,
    output.dir.samples,
    output.dir.samplescombined,
    regiontypes,
    filename,
    percentiles,
    ndigits,
    replace.rates.reg,
    selected.region.idx = NULL,
    output.rates.of.decline,
    round.output# DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
) {
  # load one file first to get dimensions
  if(is.null(replace.rates.reg)){ # DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
    load(file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  } else {
    load(file = file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
  }
  
  nsim <- dim(deathu5.ctj)[3]
  nregs <- length(regiontypes)
  load(file.path(output.dir.samplescombined, "info.rda"))
  list2env(info, envir = environment())
  
  # load world results
  world.results.exist <- file.exists(file.path(output.dir.samplescombined, "res.world.rda"))
  world.rods.exist <- isTRUE(output.rates.of.decline) && file.exists(file.path(output.dir.samplescombined, "global.RoDs.ui.rda"))
  if(world.results.exist){
    load(file = file.path(output.dir.samplescombined, "res.world.rda"))
    if (world.rods.exist) {
      load(file = file.path(output.dir.samplescombined, "global.RoDs.ui.rda"))
    }
  }
  
  # create separate directory for each region type
  output.dir.samples.region <- file.path(output.dir.samples, filename)

  nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0(filename, "_nmr.rtj.rda")))
  if (is.null(selected.region.idx)) {
    death0.rtj <- death1to4.rtj <- deathu5.rtj <- deathnn.rtj <- death0.all.rtj <- death1to4.all.rtj <- deathu5.all.rtj <- deathnn.all.rtj <-
      M0.rtj <- M1to4.rtj <- q0.rtj <- q1to4.rtj <- q5.rtj <- qnn.rtj <- u5mr.rtj <- imr.rtj <- nmr.rtj <- array(NA, c(nregs, nyears, nsim))
  } else {
    load(file.path(output.dir.samplescombined, paste0(filename, "_u5mr.rtj.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_imr.rtj.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_deathu5.all.rtj.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_death0.all.rtj.rda")))
    if (nn.exists) {
      load(file.path(output.dir.samplescombined, paste0(filename, "_nmr.rtj.rda")))
      load(file.path(output.dir.samplescombined, paste0(filename, "_deathnn.all.rtj.rda")))
    } else {
      nmr.rtj <- array(NA, c(nregs, nyears, nsim))
      deathnn.all.rtj <- array(NA, c(nregs, nyears, nsim))
    }
    q0.rtj <- q1to4.rtj <- q5.rtj <- qnn.rtj <- death1to4.rtj <- array(NA, c(nregs, nyears, nsim))
  }
  
  pop0.rt <- pop1to4.rt <- pop0.orig.rt <- pop1to4.orig.rt <- popu5.orig.rt <-
    coverage0.rt <- coverageu5.rt <- matrix(NA, nregs, nyears)
  
  for (j in 1:nsim) {
    bundle <- LoadRegionalBundleBWC(output.dir.samples.region, j)
    nn.exists <- isTRUE(bundle$nn.exists)
    if (is.null(selected.region.idx)) {
      q0.rtj[, , j] <- bundle$q0.rt
      q1to4.rtj[, , j] <- bundle$q1to4.rt
      q5.rtj[, , j] <- bundle$q5.rt
      if(nn.exists) qnn.rtj[, , j] <- bundle$qnn.rt
      death0.all.rtj[, , j] <- bundle$death0.all.rt
      deathu5.all.rtj[, , j] <- bundle$deathu5.all.rt
      if(nn.exists) deathnn.all.rtj[, , j] <- bundle$deathnn.all.rt
    } else {
      u5mr.rtj[selected.region.idx, , j] <- bundle$q5.rt[selected.region.idx, , drop = FALSE] * 1000
      imr.rtj[selected.region.idx, , j] <- bundle$q0.rt[selected.region.idx, , drop = FALSE] * 1000
      death0.all.rtj[selected.region.idx, , j] <- bundle$death0.all.rt[selected.region.idx, , drop = FALSE]
      deathu5.all.rtj[selected.region.idx, , j] <- bundle$deathu5.all.rt[selected.region.idx, , drop = FALSE]
      if(nn.exists) {
        nmr.rtj[selected.region.idx, , j] <- bundle$qnn.rt[selected.region.idx, , drop = FALSE] * 1000
        deathnn.all.rtj[selected.region.idx, , j] <- bundle$deathnn.all.rt[selected.region.idx, , drop = FALSE]
      }
    }
  }
  if (is.null(selected.region.idx)) {
    u5mr.rtj <- q5.rtj*1000
    imr.rtj <- q0.rtj*1000
    if(nn.exists) nmr.rtj <- qnn.rtj*1000
  }
  
  
  # save the samples
  dimnames(u5mr.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(u5mr.rtj)[2]-1)))
  save(u5mr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_u5mr.rtj.rda")))
  dimnames(imr.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(imr.rtj)[2]-1)))
  save(imr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_imr.rtj.rda")))
  dimnames(deathu5.all.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(deathu5.all.rtj)[2]-1)))
  save(deathu5.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_deathu5.all.rtj.rda")))
  dimnames(death0.all.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(death0.all.rtj)[2]-1)))
  save(death0.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_death0.all.rtj.rda")))
  if(nn.exists){
    dimnames(nmr.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(nmr.rtj)[2]-1)))
    save(nmr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_nmr.rtj.rda")))
    dimnames(deathnn.all.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(deathnn.all.rtj)[2]-1)))
    save(deathnn.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_deathnn.all.rtj.rda")))
  }
  
  # delete samples
  unlink(vapply(1:nsim, function(j) GetRegionalBundlePathBWC(output.dir.samples.region, j), character(1)))
  unlink(file.path(output.dir.samples.region, paste0("q0.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("q1to4.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("q5.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("death0.all.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("death1to4.all.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("deathu5.all.rt_", 1:nsim, ".rda")))
  if(nn.exists){
    unlink(file.path(output.dir.samples.region, paste0("qnn.rt_", 1:nsim, ".rda")))
    unlink(file.path(output.dir.samples.region, paste0("deathnn.all.rt_", 1:nsim, ".rda")))
  }
  
  # load population and coverage info
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_coverageu5.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_coverage0.rt.rda")))
  
  # regional summaries
  u5mr.qrt <- apply(u5mr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qrt <- apply(imr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.all.qrt <- apply(deathu5.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.all.qrt <- apply(death0.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  if(nn.exists){
    nmr.qrt <- apply(nmr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
    deathnn.all.qrt <- apply(deathnn.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  }
  
  # NA if coverage < 0.5
  for (q in 1:length(percentiles)) {
    u5mr.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    imr.qrt[q, , ][coverage0.rt < 0.5] <- NA
    if(nn.exists) nmr.qrt[q, , ][coverage0.rt < 0.5] <- NA
    # deathu5.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1):(max(which(coverageu5.rt < 0.5))+5))] <- NA
    # death0.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1))] <- NA
    # if(nn.exists) deathnn.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1))] <- NA
    deathu5.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    death0.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    if(nn.exists) deathnn.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    for(regy in 1:dim(u5mr.qrt)[2]){
      # use presence of rate to put NA for deaths
      missing.u5 <- which(is.na(u5mr.qrt[q, regy, ]))
      if (length(missing.u5) > 0) {
        start.idx <- max(missing.u5)
        end.idx <- min(dim(deathu5.all.qrt)[3], start.idx + 5)
        deathu5.all.qrt[q, regy, start.idx:end.idx] <- NA
      } else {
        deathu5.all.qrt[q, regy, 1:min(5, dim(deathu5.all.qrt)[3])] <- NA
      }
      missing.imr <- which(is.na(imr.qrt[q, regy, ]))
      if (length(missing.imr) > 0) {
        start.idx <- max(missing.imr)
        end.idx <- min(dim(death0.all.qrt)[3], start.idx + 1)
        death0.all.qrt[q, regy, start.idx:end.idx] <- NA
      } else {
        death0.all.qrt[q, regy, 1] <- NA
      }
      if(nn.exists){
        missing.nmr <- which(is.na(nmr.qrt[q, regy, ]))
        if (length(missing.nmr) > 0) {
          start.idx <- max(missing.nmr)
          end.idx <- min(dim(deathnn.all.qrt)[3], start.idx + 1)
          deathnn.all.qrt[q, regy, start.idx:end.idx] <- NA
        } else {
          deathnn.all.qrt[q, regy, 1] <- NA
        }
      }
      
      # use coverage to put NAs for death
      # if(length(which(coverageu5.rt[regy,]<0.5))>0){
      # deathu5.all.qrt[q,regy,(max(which(coverageu5.rt[regy,] < 0.5))+1):(max(which(coverageu5.rt[regy,] < 0.5))+5)] <- NA
      # death0.all.qrt[q,regy,(max(which(coverageu5.rt[regy,] < 0.5))+1)] <- NA
      # if(nn.exists) deathnn.all.qrt[q,regy,(max(which(coverageu5.rt[regy,] < 0.5))+1)] <- NA
      # } else {
      # deathu5.all.qrt[q,regy,1:5] <- NA
      # death0.all.qrt[q,regy,1] <- NA
      # if(nn.exists) deathnn.all.qrt[q,regy,1] <- NA
      # }
    } # regy loop
  }
  
  # regional summary
  res.year <- NULL
  for (i in 1:nyears) {
    if(nn.exists){
      ifelse(round.output, t.u5mr.qrt <- roundoff(t(u5mr.qrt[,,i]), digits = ndigits), t.u5mr.qrt <- t(u5mr.qrt[,,i]))
      ifelse(round.output, t.imr.qrt <- roundoff(t(imr.qrt[,,i]), digits = ndigits), t.imr.qrt <- t(imr.qrt[,,i]))
      ifelse(round.output, t.nmr.qrt <- roundoff(t(nmr.qrt[,,i]), digits = ndigits), t.nmr.qrt <- t(nmr.qrt[,,i]))
      
      if(world.results.exist){
        res.year <- rbind(res.year,
                          rbind(
                            cbind(est.years.floor[i],
                                  roundoff(popu5.orig.rt[,i], digits = 0),
                                  roundoff(pop0.orig.rt[,i], digits = 0),
                                  roundoff(coverageu5.rt[,i]*100, digits = 2),
                                  roundoff(coverage0.rt[,i]*100, digits = 2),
                                  t.u5mr.qrt,
                                  t.imr.qrt,
                                  t.nmr.qrt,
                                  roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                  roundoff(t(death0.all.qrt[,,i]), digits = 0),
                                  roundoff(t(deathnn.all.qrt[,,i]), digits = 0)),
                            res.world[res.world[, 1] == est.years.floor[i], ]
                          ))
      } else {
        res.year <- rbind(res.year,
                          rbind(
                            cbind(est.years.floor[i],
                                  roundoff(popu5.orig.rt[,i], digits = 0),
                                  roundoff(pop0.orig.rt[,i], digits = 0),
                                  roundoff(coverageu5.rt[,i]*100, digits = 2),
                                  roundoff(coverage0.rt[,i]*100, digits = 2),
                                  t.u5mr.qrt,
                                  t.imr.qrt,
                                  t.nmr.qrt,
                                  roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                  roundoff(t(death0.all.qrt[,,i]), digits = 0),
                                  roundoff(t(deathnn.all.qrt[,,i]), digits = 0))
                          ))
      } # if(world.results.exist)
    } else {
      ifelse(round.output, t.u5mr.qrt <- roundoff(t(u5mr.qrt[,,i]), digits = ndigits), t.u5mr.qrt <- t(u5mr.qrt[,,i]))
      ifelse(round.output, t.imr.qrt <- roundoff(t(imr.qrt[,,i]), digits = ndigits), t.imr.qrt <- t(imr.qrt[,,i]))
      
      if(world.results.exist){
        res.year <- rbind(res.year,
                          rbind(
                            cbind(est.years.floor[i],
                                  roundoff(popu5.orig.rt[,i], digits = 0),
                                  roundoff(pop0.orig.rt[,i], digits = 0),
                                  roundoff(coverageu5.rt[,i]*100, digits = 2),
                                  roundoff(coverage0.rt[,i]*100, digits = 2),
                                  t.u5mr.qrt,
                                  t.imr.qrt,
                                  roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                  roundoff(t(death0.all.qrt[,,i]), digits = 0)),
                            res.world[res.world[, 1] == est.years.floor[i], ]
                          ))
      } else {
        res.year <- rbind(res.year,
                          rbind(
                            cbind(est.years.floor[i],
                                  roundoff(popu5.orig.rt[,i], digits = 0),
                                  roundoff(pop0.orig.rt[,i], digits = 0),
                                  roundoff(coverageu5.rt[,i]*100, digits = 2),
                                  roundoff(coverage0.rt[,i]*100, digits = 2),
                                  t.u5mr.qrt,
                                  t.imr.qrt,
                                  roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                  roundoff(t(death0.all.qrt[,,i]), digits = 0))
                          ))
      } # if(world.results.exist)
    } # if/else
  }
  ifelse(world.results.exist,
         res.region <- cbind(rep(c(regiontypes, "World"), nyears), res.year),
         res.region <- cbind(rep(c(regiontypes), nyears), res.year)
  )
  
  # output to .csv
  ui.colnames <- c(" lower bound", " median", " upper bound")
  if(nn.exists){
    colnames(res.region) <- c("Region", "Year",
                              "Under-five population", "Infant population",
                              "Population coverage (under 5)", "Population coverage (age 0)",
                              paste0("U5MR", ui.colnames),
                              paste0("IMR", ui.colnames),
                              paste0("NMR", ui.colnames),
                              paste0("Under-five deaths", ui.colnames),
                              paste0("Infant deaths", ui.colnames),
                              paste0("Neonatal deaths", ui.colnames))
  } else {
    colnames(res.region) <- c("Region", "Year",
                              "Under-five population", "Infant population",
                              "Population coverage (under 5)", "Population coverage (age 0)",
                              paste0("U5MR", ui.colnames),
                              paste0("IMR", ui.colnames),
                              paste0("Under-five deaths", ui.colnames),
                              paste0("Infant deaths", ui.colnames))
  }
  
  if (nsim == 1) res.region <- res.region[, !grepl("bound", colnames(res.region))]
  write.csv(res.region, file = file.path(output.dir, paste0("Rates & Deaths_", filename, ".csv")),
            row.names = F, na = "")
  
  # round off to 1 d.p. before calculation (for median only)
  if (dim(u5mr.rtj)[3] == 1) { # change JR, 26 Aug 2013
    u5mr.rtj <- roundoff(u5mr.rtj, digits = ndigits)
    imr.rtj <- roundoff(imr.rtj, digits = ndigits)
  }
  
  # regional summary - rates of decline
  if (isTRUE(output.rates.of.decline)) {
    region.RoDs.ui <- NULL
    for (r in 1:nregs) {
      ARR.year1.year4.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
                                        year.start = year1, year.end = year4)
      ARR.year1.year2.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
                                        year.start = year1, year.end = year2)
      ARR.year2.year4.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
                                        year.start = year2, year.end = year4)
      required.ARR.j <- ifelse(year4 < year.target,
                               1/(year.target-year4)*
                                 log(roundoff(u5mr.rtj[r, est.years == year1, ]*factor.target, digits = ndigits)/
                                       u5mr.rtj[1, est.years == year4, ])*-100, NA)
      changeinARR.j <- ARR.year2.year4.j - ARR.year1.year2.j
      decline.year1.year4.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
                                                year.start = year1, year.end = year4)
      decline.year1.year2.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
                                                year.start = year1, year.end = year2)
      decline.year2.year4.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
                                                year.start = year2, year.end = year4)
      ARR.year1.year4.ui <- SafeQuantileBWC(ARR.year1.year4.j, probs = percentiles)
      ARR.year1.year2.ui <- SafeQuantileBWC(ARR.year1.year2.j, probs = percentiles)
      ARR.year2.year4.ui <- SafeQuantileBWC(ARR.year2.year4.j, probs = percentiles)
      required.ARR.ui <- SafeQuantileBWC(required.ARR.j, probs = percentiles)
      changeinARR.ui <- SafeQuantileBWC(changeinARR.j, probs = percentiles)
      decline.year1.year4.ui <- SafeQuantileBWC(decline.year1.year4.j, probs = percentiles)
      decline.year1.year2.ui <- SafeQuantileBWC(decline.year1.year2.j, probs = percentiles)
      decline.year2.year4.ui <- SafeQuantileBWC(decline.year2.year4.j, probs = percentiles)
      region.RoDs.ui <- rbind(region.RoDs.ui,
                              c(ARR.year1.year4.ui, ARR.year1.year2.ui, ARR.year2.year4.ui,
                                required.ARR.ui, changeinARR.ui, decline.year1.year4.ui,
                                decline.year1.year2.ui, decline.year2.year4.ui))
    }
    colnames(region.RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5, ui.colnames),
                                  paste0("ARR ", year1-0.5, "-", year2-0.5, ui.colnames),
                                  paste0("ARR ", year2-0.5, "-", year4-0.5, ui.colnames),
                                  paste0("Required ARR", ui.colnames),
                                  paste0("Change in ARR", ui.colnames),
                                  paste0("Percentage decline ", year1-0.5, "-", year4-0.5, ui.colnames),
                                  paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
                                  paste0("Percentage decline ", year2-0.5, "-", year4-0.5, ui.colnames))
    ifelse(world.rods.exist,
           region.RoDs <- data.frame(Region = c(regiontypes, "World"), rbind(region.RoDs.ui, global.RoDs.ui)),
           region.RoDs <- data.frame(Region = c(regiontypes), region.RoDs.ui)
    )
    if (nsim == 1)
      region.RoDs <- region.RoDs[, !grepl("bound", colnames(region.RoDs))]
    write.csv(region.RoDs, file = file.path(output.dir, paste0("Rates of Decline_", filename, ".csv")),
              row.names = F, na = "")
  }
  cat(paste0("Output generated for ", filename, ".\n"))
  
}
