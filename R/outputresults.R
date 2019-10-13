#----------------------------------------------------------------------
# outputresults.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
OutputResults <- function(# Output results as a .CSV file in CME Info estimates upload format.
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored and results file will be saved.
  est.years.output = NULL, ##<< Vector of estimate years to output.
  weight.alpha.select = 0.5, ##<< Result type: Value of \code{weight.alpha} for Bayesian melding.
  round.digits = NULL ##<< Number of digits to round off results to.
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  load(file.path(output.dir, "mcmc.meta.rda"))
  if (is.null(est.years.output))
    est.years.output <- 1931.5:mcmc.meta$settings$year.lastestimate
  load(file.path(output.dir, "year.t.rda"))
  load(file.path(output.dir, "res.cqt.Lw.rda"))
  res.cqt <- res.cqt.Lw[[paste0(weight.alpha.select)]]
  Q <- dim(res.cqt)[2]
  results <- NULL
  for (c in 1:mcmc.meta$data$C) {
    for (q in 1:Q) {
      results <- rbind(results, res.cqt[c, q, ])
    }
  }
  results.all <- matrix(NA, mcmc.meta$data$C*Q, length(est.years.output))
  results.all[, is.element(est.years.output, year.t)] <- results
  if (!is.null(round.digits))
    results.all <- roundoff(results.all, digits = round.digits)
  results.output <- cbind(data.frame(rep(mcmc.meta$data$name.c, each = Q), 
                                     rep(mcmc.meta$data$iso.c, each = Q),
                                     rep(c("Lower", "Median", "Upper"), mcmc.meta$data$C),
                                     rep(ifelse(mcmc.meta$settings$indicator.type == "U5MR", 
                                                "Under-five Mortality Rate", 
                                                "Infant Mortality Rate"), mcmc.meta$data$C*Q),
                                     rep("Total", mcmc.meta$data$C*3)),
                          results.all)  
  colnames(results.output) <- c("Country Name", "ISO Code",  "Quantile",  "Indicator", "Subgroup", 
                                est.years.output)
  write.csv(results.output, file = file.path(output.dir, "Results.csv"), row.names = F, na = "")
}
