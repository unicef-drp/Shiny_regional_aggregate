# Keep only the OutputAggregates cache files needed for fast replacement reuse.
cleanup.outputaggregate.cache <- function(
    output.dir,
    replace.rates.reg = "M49Region",
    cache.type = c("under_five", "older_children"),
    dry.run = FALSE
) {
  cache.type <- match.arg(cache.type)
  samples.combined.dir <- file.path(output.dir, "samples_combined")
  samples.dir <- file.path(output.dir, "samples")
  
  if (identical(cache.type, "under_five")) {
    combined.keep <- c(
      "info.rda",
      "u5mr.ctj.rda",
      "imr.ctj.rda",
      "nmr.ctj.rda",
      "death0.ctj.rda",
      "death1to4.ctj.rda",
      "deathu5.ctj.rda",
      "deathnn.ctj.rda",
      paste0("death0.ctj.", replace.rates.reg, "-replace.rda"),
      paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda"),
      paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda"),
      paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")
    )
  } else {
    combined.keep <- c(
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
  }
  
  replacement.sample.suffix <- paste0("_", replace.rates.reg, "-replace.rda")
  is.replacement.sample <- function(filename) {
    starts.with.life.table <- grepl(
      "^(dx|lx)(\\.nn)?\\.array\\.ct_[0-9]+_",
      filename
    )
    starts.with.life.table & endsWith(filename, replacement.sample.suffix)
  }
  
  list.cache.files <- function(path) {
    if (!dir.exists(path)) {
      return(character(0))
    }
    files <- list.files(path, full.names = TRUE, all.files = FALSE, no.. = TRUE)
    files[!file.info(files)$isdir]
  }
  
  combined.files <- list.cache.files(samples.combined.dir)
  sample.files <- list.files(
    samples.dir,
    full.names = TRUE,
    recursive = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )
  if (length(sample.files) > 0) {
    sample.files <- sample.files[!file.info(sample.files)$isdir]
  }
  
  combined.delete <- combined.files[
    !(basename(combined.files) %in% combined.keep)
  ]
  sample.delete <- if (identical(cache.type, "under_five")) {
    sample.files[!is.replacement.sample(basename(sample.files))]
  } else {
    sample.files
  }
  
  delete.files <- c(combined.delete, sample.delete)
  if (!dry.run && length(delete.files) > 0) {
    unlink(delete.files)
  }
  
  message(
    "Cache cleanup for ", output.dir, ": kept ",
    length(combined.files) + length(sample.files) - length(delete.files),
    " file(s), ",
    if (dry.run) "would delete " else "deleted ",
    length(delete.files),
    " file(s)."
  )
  
  invisible(list(
    kept = setdiff(c(combined.files, sample.files), delete.files),
    deleted = delete.files,
    dry.run = dry.run
  ))
}
