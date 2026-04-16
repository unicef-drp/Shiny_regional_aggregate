source("R/cleanup_outputaggregates_cache.R")

tmp <- tempfile("outputaggregate-cache-")
dir.create(file.path(tmp, "samples_combined"), recursive = TRUE)
dir.create(file.path(tmp, "samples"), recursive = TRUE)

touch <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  file.create(path)
}

combined.keep <- c(
  "info.rda",
  "u5mr.ctj.rda",
  "imr.ctj.rda",
  "nmr.ctj.rda",
  "death0.ctj.rda",
  "death1to4.ctj.rda",
  "deathu5.ctj.rda",
  "deathnn.ctj.rda",
  "death0.ctj.M49Region-replace.rda",
  "death1to4.ctj.M49Region-replace.rda",
  "deathu5.ctj.M49Region-replace.rda",
  "deathnn.ctj.M49Region-replace.rda"
)
combined.remove <- c(
  "M49Region_u5mr.rtj.rda",
  "AdhocCountries_u5mr.rtj.rda",
  "res.world.rda"
)
sample.keep <- c(
  "dx.array.ct_1_M49Region-replace.rda",
  "lx.array.ct_1_M49Region-replace.rda",
  "dx.nn.array.ct_1_M49Region-replace.rda",
  "lx.nn.array.ct_1_M49Region-replace.rda"
)
sample.remove <- c(
  "dx.array.ct_1.rda",
  "lx.array.ct_1.rda",
  "info.rda"
)

invisible(vapply(
  file.path(tmp, "samples_combined", c(combined.keep, combined.remove)),
  touch,
  logical(1)
))
invisible(vapply(
  file.path(tmp, "samples", c(sample.keep, sample.remove)),
  touch,
  logical(1)
))

result <- cleanup.outputaggregate.cache(tmp, replace.rates.reg = "M49Region")

remaining.combined <- sort(basename(list.files(file.path(tmp, "samples_combined"))))
remaining.samples <- sort(basename(list.files(file.path(tmp, "samples"))))

if (!identical(remaining.combined, sort(combined.keep))) {
  stop("samples_combined cleanup did not keep the expected minimum files.", call. = FALSE)
}

if (!identical(remaining.samples, sort(sample.keep))) {
  stop("samples cleanup did not keep the expected replacement dx/lx files.", call. = FALSE)
}

if (length(result$deleted) != length(combined.remove) + length(sample.remove)) {
  stop("cleanup result did not report the expected number of deleted files.", call. = FALSE)
}

unlink(tmp, recursive = TRUE, force = TRUE)

cat("OutputAggregates cache cleanup regression checks passed.\n")

tmp <- tempfile("older-child-cache-")
dir.create(file.path(tmp, "samples_combined"), recursive = TRUE)
dir.create(file.path(tmp, "samples", "AdhocCountries"), recursive = TRUE)

older.keep <- c(
  "info.rda",
  "u5mr.ctj.rda",
  "imr.ctj.rda",
  "cmr.ctj.rda",
  "death0.ctj.rda",
  "death1to4.ctj.rda",
  "deathu5.ctj.rda",
  "ARR.year1.year6.cj.rda",
  "ARR.year1.year2.cj.rda",
  "ARR.year3.year4.cj.rda",
  "ARR.year5.year6.cj.rda",
  "decline.year1.year6.cj.rda",
  "decline.year1.year2.cj.rda",
  "decline.year3.year4.cj.rda",
  "decline.year5.year6.cj.rda"
)
older.remove <- c(
  "AdhocCountries_u5mr.rtj.rda",
  "res.world.rda",
  "required.ARR.cj.rda",
  "SDGSimpleRegion_u5mr.rtj.rda"
)

invisible(vapply(
  file.path(tmp, "samples_combined", c(older.keep, older.remove)),
  touch,
  logical(1)
))
invisible(touch(file.path(tmp, "samples", "info.rda")))
invisible(touch(file.path(tmp, "samples", "AdhocCountries", "bundle_1.rds")))

result <- cleanup.outputaggregate.cache(
  tmp,
  replace.rates.reg = "M49Region",
  cache.type = "older_children"
)

remaining.combined <- sort(basename(list.files(file.path(tmp, "samples_combined"))))
remaining.samples <- list.files(file.path(tmp, "samples"), recursive = TRUE)

if (!identical(remaining.combined, sort(older.keep))) {
  stop("older-child samples_combined cleanup did not keep the expected country cache files.", call. = FALSE)
}

if (length(remaining.samples) != 0) {
  stop("older-child samples cleanup should remove all temporary sample files.", call. = FALSE)
}

if (length(result$deleted) != length(older.remove) + 2L) {
  stop("older-child cleanup result did not report the expected number of deleted files.", call. = FALSE)
}

unlink(tmp, recursive = TRUE, force = TRUE)

cat("Older-child cache cleanup regression checks passed.\n")
