#----------------------------------------------------------------------
# outputaggregatestocsv.R
# Jin Rou New, 2013
#----------------------------------------------------------------------                  
OutputAggregatesToCSV <- function( # Output summaries
  info, 
  u5mr.ctj,
  imr.ctj,
  deathu5.ctj,
  death0.ctj,
  ARR.year1.year4.cj,
  ARR.year1.year2.cj,
  ARR.year2.year4.cj,
  required.ARR.cj,
  decline.year1.year4.cj,
  decline.year1.year2.cj,
  decline.year2.year4.cj,
  year1,
  year2,
  year4,
  est.years,
  percentiles,
  output.median.only = FALSE,
  ndigits,
  output.dir,
  filename
) {
  est.years.floor <- floor(est.years)
  # rates and deaths
  u5mr.ui <- GetFormattedQuantiles(data.ctj = u5mr.ctj, percentiles = percentiles)
  imr.ui <- GetFormattedQuantiles(data.ctj = imr.ctj, percentiles = percentiles)
  deathu5.ui <- GetFormattedQuantiles(data.ctj = deathu5.ctj, percentiles = percentiles)
  death0.ui <- GetFormattedQuantiles(data.ctj = death0.ctj, percentiles = percentiles)
  colnames(u5mr.ui) <- paste0("U5MR ", est.years.floor)
  colnames(imr.ui) <- paste0("IMR ", est.years.floor)
  colnames(deathu5.ui) <- paste0("Under-five Deaths ", est.years.floor)
  colnames(death0.ui) <- paste0("Infant Deaths ", est.years.floor)
  #----------------------------------------------------------------------
  # rates of decline
  ARR.year1.year4.ui <- GetFormattedQuantiles(data.ctj = ARR.year1.year4.cj, percentiles = percentiles)
  ARR.year1.year2.ui <- GetFormattedQuantiles(data.ctj = ARR.year1.year2.cj, percentiles = percentiles)
  ARR.year2.year4.ui <- GetFormattedQuantiles(data.ctj = ARR.year2.year4.cj, percentiles = percentiles)
  required.ARR.ui <- GetFormattedQuantiles(data.ctj = required.ARR.cj, percentiles = percentiles)
  decline.year1.year4.ui <- GetFormattedQuantiles(data.ctj = decline.year1.year4.cj, percentiles = percentiles)
  decline.year1.year2.ui <- GetFormattedQuantiles(data.ctj = decline.year1.year2.cj, percentiles = percentiles)
  decline.year2.year4.ui <- GetFormattedQuantiles(data.ctj = decline.year2.year4.cj, percentiles = percentiles)
  RoDs.ui <- cbind(ARR.year1.year4.ui, ARR.year1.year2.ui, ARR.year2.year4.ui,
                   required.ARR.ui, decline.year1.year4.ui, 
                   decline.year1.year2.ui, decline.year2.year4.ui)
  colnames(RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5), 
                         paste0("ARR ", year1-0.5, "-", year2-0.5),
                         paste0("ARR ", year2-0.5, "-", year4-0.5),                               
                         paste0("Required ARR"),
                         paste0("Percentage decline ", year1-0.5, "-", year4-0.5), 
                         paste0("Percentage decline ", year1-0.5, "-", year2-0.5), 
                         paste0("Percentage decline ", year2-0.5, "-", year4-0.5))
  #----------------------------------------------------------------------
  # format info
  if (is.vector(info)) {
    info.output <- info[rep(1:length(info), each = length(percentiles)), ]
  } else if (is.data.frame(info) | is.array(info)) {
    info.output <- info[rep(1:nrow(info), each = length(percentiles)), ]
  } else {
    cat("info is of an invalid type. Please input a vector, data frame or 2d array.\n")
    return()
  }
  if (output.median.only) {
    select.rows <- seq(1, nrow(u5mr.ui), 3)+1
  } else {
    select.rows <- seq(1, nrow(u5mr.ui), 1)
  }
  write.csv(cbind(info.output,
                  Quantile = rep(c("Lower", "Median", "Upper"), C),
                  roundoff(u5mr.ui, digits = ndigits),
                  roundoff(RoDs.ui, digits = ndigits),
                  roundoff(imr.ui, digits = ndigits), 
                  roundoff(cbind(deathu5.ui,death0.ui), digits = 0),
                  )[select.rows, ], 
            file = file.path(output.dir, filename), 
            row.names = F, na = "")
}
#----------------------------------------------------------------------
GetFormattedQuantiles <- function(
  data.ctj, ##<< Data array of dimension C x length(est.years) x nsim OR C x nsim
  percentiles) {
  if (length(dim(data.ctj)) == 3) { # dim C x length(est.years) x nsim
    data.qct <- apply(data.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  } else { # dim C x nsim
    data.qct <- apply(data.ctj, 1, quantile, probs = percentiles, na.rm = T)
  }
  C <- dim(data.ctj)[1]
  data.ui <- NULL
  if (length(dim(data.ctj)) == 3) {
    for (c in 1:C)  
      data.ui <- rbind(data.ui, data.qct[, c, ])
  } else {
    data.ui <- c(data.qct)
  }
  return(data.ui)
}
