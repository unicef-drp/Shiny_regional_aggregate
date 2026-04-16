#----------------------------------------------------------------------
# summariseresults.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
SaveRdaViaTempFile <- function(object.name, target.file, env = parent.frame()) {
  dir.create(dirname(target.file), showWarnings = FALSE, recursive = TRUE)

  tmp.file <- tempfile(pattern = "aggregate-results-", fileext = ".rda")
  on.exit(unlink(tmp.file), add = TRUE)

  save(list = object.name, file = tmp.file, envir = env)

  if (!file.copy(tmp.file, target.file, overwrite = TRUE)) {
    unlink(target.file)
    if (!file.copy(tmp.file, target.file, overwrite = TRUE)) {
      stop("Unable to write results file: ", target.file)
    }
  }

  invisible(target.file)
}

SummariseResults <- function(# Summarise U5MR/IMR results for computing aggregates.
  results.file, ##<< File path to results file used as input.
  output.dir, ##<< Output directory for saved results.
  filename.output = "res.ctj" ##<< Alternative file name for results.
) {
  results <- read.csv(results.file, header = T, stringsAsFactors = F, strip.white = T)
  iso.c <- unique(results$ISO.Code)
  colnames.year <- colnames(results)[grepl("X", colnames(results))]
  results.processed <- results[results$Quantile == "Median", is.element(colnames(results), colnames.year)]
  colnames.year.select <- apply(results.processed, 2, function(x) sum(!is.na(x))) != 0
  year.t <- as.numeric(gsub("X", "", colnames.year[colnames.year.select]))
  res.ct <- results.processed[, colnames.year.select]
  res.ctj <- array(NA, c(nrow(res.ct), ncol(res.ct), 1)) 
  res.ctj[, , 1] <- as.matrix(res.ct)
  assign(filename.output, res.ctj, envir = environment())
  SaveRdaViaTempFile(filename.output, file.path(output.dir, paste0(filename.output, ".rda")), env = environment())
  SaveRdaViaTempFile("iso.c", file.path(output.dir, "iso.c.rda"), env = environment())
  SaveRdaViaTempFile("year.t", file.path(output.dir, "year.t.rda"), env = environment())
  ##value<< \code{NULL}; Saves results to \code{output.dir}. 
  return(invisible())
}
