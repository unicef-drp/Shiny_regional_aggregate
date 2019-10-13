GetValidationDatabase <- function(
  data.cmeinfo.file, ##<< File path of data from CME Info.
  year.cutoff, ##<< Cut-off year for validation.
  year.current, ## Current year.
  fit.B2.model = FALSE, ##<< Logical value indicating whether or not to fit B2 model (without data model). # change JR, 5 Jul
  output.dir = NULL ##<< Directory where processed data file will be saved. if \code{NULL}, defaults to
  ## directory \code{output/runname}.
) { 
  if (is.null(output.dir)) 
    output.dir <- file.path(getwd(), "input")
  dir.create(output.dir, showWarnings = FALSE) 
  data.cmeinfo <- read.csv(file = data.cmeinfo.file, 
                           header = T, as.is = T, stringsAsFactors = F, strip.white = T, encoding = "latin1")
  data.cmeinfo <- CleanData(data = data.cmeinfo,
                            output.dir = output.dir,
                            year.current = year.current,
                            fit.B2.model = fit.B2.model)

  # exclude validation observations
  print(paste("Number of NAs: ", sum(is.na(data.cmeinfo$surveyyear.i))))
  exclude.for.val <- data.cmeinfo$surveyyear.i >= year.cutoff
  data.cmeinfo.val <- data.cmeinfo[!exclude.for.val, ]
  
  # write to file
  write.csv(data.cmeinfo.val, file.path(output.dir, "data_U5MR_CMEInfo_validation.csv"),
            row.names = F, na = "", fileEncoding = "latin1")
  ##value<< \code{NULL}
  return(NULL)
}
