#----------------------------------------------------------------------
# summariseresults.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
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
  eval(parse(text = paste0(filename.output, " <- res.ctj")))  
  dir.create(output.dir, showWarnings = F)
  eval(parse(text = paste0("save(", filename.output, 
                           ", file = file.path(output.dir, \"", 
                           filename.output, ".rda\"))")))
  save(iso.c, file = file.path(output.dir, "iso.c.rda"))
  save(year.t, file = file.path(output.dir, "year.t.rda"))
  ##value<< \code{NULL}; Saves results to \code{output.dir}. 
  return(invisible())
}
