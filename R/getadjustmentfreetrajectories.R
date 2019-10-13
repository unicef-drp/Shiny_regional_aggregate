#----------------------------------------------------------------------
# getadjustmentfreetrajectories.R
# Jin Rou New, 2015
#----------------------------------------------------------------------
GetAdjustmentFreeTrajectories <- function( # Get HIV and/or crisis-free trajectories
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored and new objects will be added.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  percentiles = c(0.05, 0.5, 0.95), ##<< Percentiles for 90% UIs.
  weight.alpha.select = 0.5
) {
  if (is.null(output.dir))
    output.dir <- file.path("output", runname)
  load(file.path(output.dir, "mcmc.meta.rda"))
  
  hiv.file <- mcmc.meta$files$hiv.file
  adj.file <- mcmc.meta$files$adj.file
  cat(paste0("HIV adjustment file used: ", hiv.file, "\n"))
  cat(paste0("Crisis adjustment file used: ", adj.file, "\n"))
  
  # load weights.alpha
  load(file = file.path("output", runname, "res.cqt.Lw.rda"))
  weights.alpha.plusdefault <- names(res.cqt.Lw)
  weights.alpha <- weights.alpha.plusdefault[-1]
  if (weight.alpha.select == 0) { 
    filename <- "u5.ctj"
  } else {
    w <- which(weights.alpha == weight.alpha.select)
    filename <- paste0("u5new", w, ".ctj")
  }
  load(file.path(output.dir, paste0(filename, ".rda")))
  eval(parse(text = paste0("u5.ctj <- ", filename)))
  eval(parse(text = paste0("rm(", filename, ")")))
  
  # load(file = file.path(output.dir, "year.t.rda"))
  # or for runs from the year 2015 and after:
  percentiles <- as.numeric(dimnames(res.cqt.Lw[[1]])[[2]])
  year.t <- as.numeric(dimnames(res.cqt.Lw[[1]])[[3]])
  
  if (mcmc.meta$settings$indicator.type == "U5MR") {
    data <- mcmc.meta$data
  } else {
    data <- mcmc.meta$data.all
  }
  
  u5.crisisandhivfree.ctj <- u5.crisisfree.ctj <- u5.hivfree.ctj <- u5.ctj
  dimnames(u5.crisisandhivfree.ctj) <- dimnames(u5.crisisfree.ctj) <- 
    dimnames(u5.hivfree.ctj) <- list(data$iso.c, year.t, NULL)
  
  for (c in 1:data$C) {
    if (data$crisisadj.c[c] | data$hiv.c[c])
      u.median.t <- apply(u5.ctj[c, , ], 1, median)
    # undo crisis post-adjustment for crisisadj countries
    if (data$crisisadj.c[c]) {
      cat(paste0("Undoing crisis adjustment for ", data$name.c[c], "...\n"))
      # get crisis-free trajectories that include only HIV adjustments
      propadj.t <- GetCrisisAdjustedEstimates(u.t = u.median.t, year.t = year.t, 
                                              iso = data$iso.c[c],
                                              operation = "-",
                                              adj.file = adj.file)$propadj.t
      u5.crisisandhivfree.ctj[c, , ] <- u5.crisisfree.ctj[c, , ] <- 
        apply(u5.ctj[c, , ], 2, "*", propadj.t)
    }
    # undo HIV post-adjustment for countries with high HIV prevalence  
    if (data$hiv.c[c]) {
      cat(paste0("Undoing HIV adjustment for ", data$name.c[c], "...\n"))
      # get HIV-free trajectories that include only crisis adjustments
      propadjhiv.t <- GetHIVAdjustedEstimates(u.t = u.median.t, year.t = year.t, 
                                              iso = data$iso.c[c],
                                              operation = "-",
                                              hiv.file = hiv.file)$propadjhiv.t
      u5.hivfree.ctj[c, , ] <- apply(u5.ctj[c, , ], 2, "*", propadjhiv.t)
      # get HIV and crisis-free trajectories
      u.median2.t <- apply(u5.crisisfree.ctj[c, , ], 1, median)
      propadjhiv2.t <- GetHIVAdjustedEstimates(u.t = u.median2.t, year.t = year.t, 
                                               iso = data$iso.c[c],
                                               operation = "-",
                                               hiv.file = hiv.file)$propadjhiv.t
      u5.crisisandhivfree.ctj[c, , ] <- apply(u5.crisisfree.ctj[c, , ], 2, "*", propadjhiv2.t)
    }
  }
  
  eval(parse(text = paste0("u5new", w, ".crisisandhivfree.ctj <- u5.crisisandhivfree.ctj")))
  eval(parse(text = paste0("u5new", w, ".crisisfree.ctj <- u5.crisisfree.ctj")))
  eval(parse(text = paste0("u5new", w, ".hivfree.ctj <- u5.hivfree.ctj")))
  eval(parse(text = paste0("save(u5new", w, ".crisisandhivfree.ctj, file = file.path(output.dir, \"u5new", 
                           w, ".crisisandhivfree.ctj.rda", "\"))")))
  eval(parse(text = paste0("save(u5new", w, ".crisisfree.ctj, file = file.path(output.dir, \"u5new", 
                           w, ".crisisfree.ctj.rda", "\"))")))
  eval(parse(text = paste0("save(u5new", w, ".hivfree.ctj, file = file.path(output.dir, \"u5new", 
                           w, ".hivfree.ctj.rda", "\"))")))
  cat(paste0("HIV and/or crisis-free trajectories saved to ", output.dir, ".\n"))
  return(invisible())
}
