#----------------------------------------------------------------------
# plotvalidationmeasures.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
PlotValidationMeasures <- function(# Plots validation measures mean/median error, mean/median relative error, 
  ## coverage and mean/median interval score against different kappas/alphas (1/x).
  runname = "test", ##<< Run name.
  is.validation.PPD, ## << Is this model validation or validation for PPD?
  weights.alpha = seq(0, 0.6, 0.1), ##<< Sequence of weights.alpha to plot for.
  lwd = 5,
  output.dir = NULL, ##<< Directory where validation results output is stored.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  fig.dir = NULL, ##<< Directory to store plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
  percentiles = c(0.05, 0.5, 0.95) ##<< Percentiles for 90% UIs.
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  types.subset.countries <- c("All non-HIV countries", "High mortality non-HIV countries", "Low mortality non-HIV countries")
  for (subset.countries in types.subset.countries) {
    if (is.validation.PPD) {
      res.ppd.L <- list()
      for (weight.alpha.select in weights.alpha) {
        output.dir.res <- file.path("output", runname, paste0("ValPPD (kappa ", weight.alpha.select, ")"))
        res.ppd.L[[paste0(weight.alpha.select)]] <- 
          read.csv(file.path(output.dir.res, paste0("Coverage and errors for PPD ", "(", subset.countries, ").csv")))
      }
      # before 2005
      meas1u <- c("ME", "MAE", "MRE", "MARE",  "Interval.score",
                  "ME.mean", "MAE.mean", "MRE.mean", "MARE.mean", "Interval.score.mean", 
                  "Percentage.below", "Percentage.above")
      # incl and after 2005
      meas2u <- paste0(meas1u, ".1")
      
      # output table for plotting
      res <- NULL
      for (weight.alpha.select in weights.alpha) {
        res <- rbind(res, 
                     c("Before and including 2005", weight.alpha.select, as.numeric(as.character(unlist(
                       res.ppd.L[[paste(weight.alpha.select)]][res.ppd$X == "Median over all trials", meas1u])))),
                     c("After 2005", weight.alpha.select, as.numeric(as.character(unlist(
                       res.ppd.L[[paste(weight.alpha.select)]][res.ppd$X == "Median over all trials", meas2u])))))
      }
      res <- as.data.frame(res)
      colnames(res) <- c("Indicator", "kappa", "ME", "MAE", "MRE", "MARE", "Interval.score",
                         "ME.mean", "MAE.mean", "MRE.mean", "MARE.mean", "Interval.score.mean",
                         "Percentage.below", "Percentage.above")
      res[, -1] <- apply(res[, -1], 2, as.numeric)
    } else {
      indicators.select <- c("U5MR 2000", "U5MR 2005", "ARR 1990-2005")
      res.readin <- read.csv(file.path("output", runname, 
                                       paste0("Coverage and errors ", "(", subset.countries, ").csv")), 
                             header = T, stringsAsFactors = F)
      resalpha <- c(as.numeric(res.readin[1,-1]))
      alphas <- unique(resalpha)
      resalpha <- c("tmp", resalpha)
      A <- length(alphas)
      res <- NULL
      for (a in 1:A) {
        alpha <- alphas[a]
        res <- rbind(res, 
                     cbind(res.readin$X[-1], alpha, as.matrix(res.readin[-1, resalpha==alpha])))
      }
      colnames(res)[1:2] <- c("Indicator", "kappa")
      res <- as.data.frame(res)
      # note: ignore warning message NAs introducted by coercion (due to weight.alpha)
      res$Indicator <- as.character(res$Indicator)
      res$Indicator[!grepl("ARR", res$Indicator)] <- paste("U5MR", floor(as.numeric(as.character(
        res$Indicator[!grepl("ARR", res$Indicator)]))))
      res <- res[is.element(res$Indicator, indicators.select), ]
      res[, -1] <- apply(res[, -1], 2, as.numeric)
    }
    res$Coverage <- 100 - res$Percentage.below - res$Percentage.above
    res$Coverage.below <- 100 - res$Percentage.below
    res$Coverage.above <- 100 - res$Percentage.above
    measures <- c("ME", "MRE", "Interval.score", "ME.mean", "MRE.mean", "Interval.score.mean", 
                  "Coverage", "Coverage.below", "Coverage.above")
    measure.names <- c("Median error", "Median relative error", "Median interval score",
                       "Mean error", "Mean relative error", "Mean interval score",
                       "Coverage", "Coverage lower bound", "Coverage upper bound")
    indicators <- unique(res$Indicator)
    nindicators <- length(indicators)
    if (is.validation.PPD) {
      colors.plot <- c("blue", "red")
    } else {
      colors.plot <- rainbow(nindicators, alpha = 0.8)
    }
    pdf(file = file.path(fig.dir, paste0(runname, " Validation error, coverage and interval score",
                                         ifelse(is.validation.PPD, " (PPD)", ""),
                                         " (", subset.countries, ").pdf")),
        width = 28, height = 21)
    par(mar = c(11, 12, 3, 2), mgp = c(7, 2.5, 0), cex.main = 3, cex.axis = 3, cex.lab = 4)
    layout(matrix(c(1:3, 10, 4:6, 10, 7:9, 10), 3, 4, byrow = T), widths = c(2, 2, 2, 1.5))
    for (m in 1:length(measures)) {
      measure <- res[, colnames(res) == measures[m]]
      if (grepl("Coverage", measure.names[m])) {
        ylim <- c(min(85, measure, na.rm = T), 100)
      } else if (grepl("relative error", measure.names[m])) {
        ylim <- c(min(measure[!grepl("ARR", res$Indicator)], na.rm = T), 
                  max(measure[!grepl("ARR", res$Indicator)], na.rm = T))
      } else {
        ylim <- c(min(measure, na.rm = T), max(measure, na.rm = T))
      }
      plot(measure[res$Indicator == indicators[1]] ~ res$kappa[res$Indicator == indicators[1]],
           main = "", xlab = expression(kappa), ylab = measure.names[m], type = "l", col = colors.plot[1],
           xlim = c(min(res$kappa), max(res$kappa) + 0.02), ylim = ylim, lwd = lwd)
      # do not plot ARR for mean/median relative error
      if (grepl("relative error", measures[m])) {
        indicators.orig <- indicators
        indicators <- indicators[!grepl("ARR", indicators)]
      }
      for (i in 1:length(indicators)) {
        lines(measure[res$Indicator == indicators[i]] ~ res$kappa[res$Indicator == indicators[i]],
              col = colors.plot[i], lwd = lwd)
      }
      if (grepl("error", measures[m])) {
        abline(h = 0, lty = 2, lwd = max(1, lwd-1))
      } else if (is.element(measure.names[m], c("Coverage"))) {
        abline(h = (percentiles[3]-percentiles[1])*100, lty = 2, lwd = max(1, lwd-1)) # actual coverage
      } else if (grepl("lower|upper", measure.names[m])) {
        abline(h = percentiles[3]*100, lty = 2, lwd = max(1, lwd-1)) # actual coverage
      }
      if (grepl("relative error", measures[m]))
        indicators <- indicators.orig
      cat(paste0(measure.names[m], " plotted,\n"))    
    } # end measures loop
    # legend for the indicators
    par(mar = c(1, 1, 1, 1))
    plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
    legend("left", legend = indicators, col = colors.plot, 
           cex = 3, lwd = lwd, lty = 1)
    dev.off()
    cat(paste0("Validation measure plots for ", subset.countries, " saved to ", fig.dir, ".\n"))
  } # end loop
  ##value<< \code{NULL}.
  return(invisible())
}
