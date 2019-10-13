#----------------------------------------------------------------------
# summariseglobalrun.R
#----------------------------------------------------------------------
SummariseGlobalRun <- function( # Summarise global run and output list to be used for country-specific runs.
  runname.global = NULL, ##<< Run name of global run.
  output.dir = NULL, ##<< Output directory containing results for \code{runname.global} and where 
  ## \code{data.global} is saved to. Defaults to \code{output/runname.global}.
  runname.all = NULL, ##<< Optional: Run name of combined country-specific runs to update country-specific parameters
  ## a.c's as well as iso.c in \code{data.global}. If this is specified, \code{runname.global} also 
  ## needs to be specified for \code{data.global} to be read in. 
  output.dir.all = NULL ##<< Optional: Output directory containing results for \code{runname.all} and where 
  ## the updated \code{data.global} is saved to. Defaults to \code{output/runname.all}. 
) {
  if (is.null(output.dir)) {
    if (!is.null(runname.global)) {
      output.dir <- file.path(getwd(), "output", runname.global)
    } else {
      cat("Error: Please specify runname.global or output.dir.\n")
      return(invisible())
    }
  }
  if (is.null(output.dir.all))
    output.dir.all <- file.path(getwd(), "output", runname.all)
  
  # load mcmc.meta and mcmc.array
  if (file.exists(file.path(output.dir, "mcmc.meta.rda"))) {
    load(file.path(output.dir, "mcmc.meta.rda"))
  } else {
    cat("Error: mcmc.meta not found!.\n")
    return(invisible())
  }

  iso.c <- mcmc.meta$data$iso.c
  C <- mcmc.meta$data$C
  use.constant.sigma.u <- mcmc.meta$settings$use.constant.sigma.u
  use.country.variance.multipliers <- mcmc.meta$settings$use.country.variance.multipliers
  if (is.null(runname.all)) {
    if (file.exists(file.path(output.dir, "mcmc.array.rda"))) {
      load(file.path(output.dir, "mcmc.array.rda"))
    } else {
      cat("Error: mcmc.array not found!.\n")
      return(invisible())
    }
    # construct data.global from mcmc.meta and mcmc.array
    year.t <- seq(floor(min(1991, mcmc.meta$data$minyear.c, na.rm = T))-0.5, 
                  mcmc.meta$settings$year.lastestimate) # change JR, 20140508
    ntypes <- mcmc.meta$jags.data$ntypes
    ntypesnoSE <- mcmc.meta$jags.data$ntypesnoSE
    
    jags.par.to.save <- c(paste0("sigma.ynonvr.t[", 1:ntypes, "]"),
                          paste0("sigma.ynonvr.tnoSE[", 1:ntypesnoSE, "]"),
                          paste0("mu.beta.tr[", 1:ntypes, ",1]"),
                          paste0("mu.beta.tr[", 1:ntypes, ",2]"),
                          paste0("sigma.beta.tr[", 1:ntypes, ",1]"),
                          paste0("sigma.beta.tr[", 1:ntypes, ",2]"),
                          "dft")
    if (use.country.variance.multipliers)
      jags.par.to.save <- c(jags.par.to.save, 
                            paste0("mu.v.r[", 1:2, "]"),
                            paste0("sigma.v.r[", 1:2, "]"))
    if (is.element("mu.biasatzerorecall", dimnames(mcmc.array)[[3]]))
      jags.par.to.save <- c(jags.par.to.save, 
                            "mu.biasatzerorecall", 
                            "sigma.biasatzerorecall",
                            "recallnobias") # change JR, 20131204
    if (!use.constant.sigma.u) {
      jags.par.to.save <- c(jags.par.to.save, "mu.a", "sigma.a", paste0("a.c[", 1:C, "]"))
    } else {
      jags.par.to.save <- c(jags.par.to.save, "a")
    }
    # check that all required parameters can be found in mcmc.array
    if (sum(!is.element(jags.par.to.save, names(mcmc.array[1, 1 , ]))) > 0) {
      cat("The following parameters cannot be found in the mcmc.array:\n")
      cat(setdiff(jags.par.to.save, names(mcmc.array[1, 1 , ])))
    }
    jags.par.to.save <- intersect(jags.par.to.save, names(mcmc.array[1, 1 , ]))
    mcmc.post <- lapply(jags.par.to.save, function(parname) median(c(mcmc.array[, , parname]), na.rm = T))
    names(mcmc.post) <- jags.par.to.save
    data.global <- list(runname.global = runname.global, # change JR, 22 May
                        year.t = year.t,
                        iso.c = iso.c,
                        recall.mid = mcmc.meta$data$recall.mid,
                        typenames = mcmc.meta$data$typenames,
                        typenoSEnames = mcmc.meta$data$typenoSEnames,
                        global.gamma.median = mcmc.meta$settings$global.gamma.median,
                        global.gamma.sd = mcmc.meta$settings$global.gamma.sd,
                        mcmc.post = mcmc.post)
    save(data.global, file = file.path(output.dir, "data.global.rda"))
  } else {
    load(file.path(output.dir, "data.global.rda"))
    load(file.path(output.dir.all, "iso.c.rda")); iso.all <- iso.c
    load(file.path(output.dir.all, "mcmc.array.rda"))
    if (!use.constant.sigma.u) {
      # remove a.c posterior medians from global run
      data.global$mcmc.post <- data.global$mcmc.post[!is.element(names(data.global$mcmc.post), 
                                                                 paste0("a.c[", 1:C, "]"))]
      jags.par.to.save <- paste0("a.c[", 1:length(iso.all), "]")
      # check that all required parameters can be found in mcmc.array
      if (sum(!is.element(jags.par.to.save, names(mcmc.array[1, 1 , ]))) > 0) {
        cat("The following parameters cannot be found in the mcmc.array:\n")
        cat(setdiff(jags.par.to.save, names(mcmc.array[1, 1 , ])))
      }
      jags.par.to.save <- intersect(jags.par.to.save, names(mcmc.array[1, 1 , ]))
      a.c.postmedians <- lapply(jags.par.to.save, function(parname) median(c(mcmc.array[, , parname]), na.rm = T))
      names(a.c.postmedians) <- jags.par.to.save
      data.global$mcmc.post <- c(data.global$mcmc.post, a.c.postmedians)
    }
    if (!use.constant.sigma.u) {
      # add runname.all
      data.global$runname.all <- runname.all
      # replace iso.c
      data.global$iso.c <- iso.all
    }
    save(data.global, file = file.path(output.dir.all, "data.global.rda"))
  }
  ##value<< \code{NULL}; Saves \code{data.global} to \code{output.dir}. 
  return(invisible())
}
