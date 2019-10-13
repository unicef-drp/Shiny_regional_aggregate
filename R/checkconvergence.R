#----------------------------------------------------------------------
# checkconvergence.R
#----------------------------------------------------------------------
CheckConvergence <- function(# Check convergence 
  ### Construct trace plots and check convergence using raftery.diag and gelman.diag.
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored and convergence results will be added.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  fig.dir = NULL, ##<< Directory to store trace plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
  plot.trace = TRUE, ##<< Construct trace plots?
  check.convergence = TRUE, ##<< Calculate and report raftery.diag and gelman.diag?
  R.threshold = 1.1, ##<< Threshold value for Gelman Rhat
  iso.select = "all", ##<< Input "all" to check convergence of all country-specific parameters, 
  ## "random" for a random subset of country-specific parameters, or 
  ## a vector of ISO country codes for country-specific parameters of selected countries. 
  fig.dir.alt = NULL ##<< Alternative directory for plots with different types plots in different folders.
  ## (Used for combined country-specific runs.)
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  load(file.path(output.dir, "mcmc.meta.rda"))
  load(file.path(output.dir, "mcmc.array.rda"))
  #----------------------------------------------------------------------
  parnames.select <- GetParameterNamesandPlotTrace(runname = runname,
                                                   plot.trace = plot.trace,
                                                   iso.select = iso.select,
                                                   fig.dir = fig.dir,
                                                   mcmc.meta = mcmc.meta,
                                                   mcmc.array =  mcmc.array,
                                                   fig.dir.alt = fig.dir.alt)
  #----------------------------------------------------------------------
  ##details<< Convergence checks:
  ##details<< Use Gelman's R after running several chains, and 
  ## Raftery-Lewis diagnostics for each chain (the median over the chains is used if there are several chains).
  if (check.convergence) {
    ##details<< Output of Gelman and Raftery-Lewis are written to \code{output.dir/convergence_check.txt}.
    filename <- file.path(output.dir, "convergence_check.txt")
    fileout <- file(filename, open = "wt")
    sink(fileout, split = T)
    cat(paste("Convergence results are written to file ", filename), "\n")
    GetRafteryLewisDiagnostics(mcmc.array = mcmc.array, parnames.select = parnames.select, 
                               output.dir = output.dir)
    GetGelmanDiagnostics(mcmc.array = mcmc.array, parnames.select = parnames.select, 
                         output.dir = output.dir, R.threshold = R.threshold)
    if (plot.trace) {
      load(file.path(output.dir, "Rhat.all.rda"))
      if (any(as.numeric(Rhat.all$Rhat) > R.threshold)) {
        parnames.plot <- Rhat.all$parameter[as.numeric(Rhat.all$Rhat) > R.threshold]
        if (is.null(fig.dir.alt)) {
          pdf(file.path(fig.dir, paste(runname, "Trace extreme Rhats.pdf")))
        } else {
          dir.create(file.path(fig.dir.alt, "Trace extreme Rhats"), showWarnings = FALSE)
          pdf(file = file.path(fig.dir.alt, "Trace extreme Rhats", 
                               paste(runname, "Trace extreme Rhats.pdf")))
        }
        for (parname in parnames.plot)
          PlotTrace(parname, mcmc.array)
        dev.off()
      }
    }
    sink()
    closeAllConnections()
  } # end check.convergence
  ##value<< NULL; Convergence diagnostics summary is written to 
  ## \code{output.dir/convergence_check.txt}.
  return(invisible(NULL))
}
#----------------------------------------------------------------------
GetParameterNamesandPlotTrace<- function(# Get parnames for convergence checks and plot trace (optional) 
  ### Get vector with parameter names to be used for convergence check
  ### and plot trace (optional)
  runname,
  plot.trace = TRUE,
  iso.select = "all",
  fig.dir,
  mcmc.meta,
  mcmc.array,
  fig.dir.alt = NULL
) {
  list2env(mcmc.meta$settings, envir = environment())
  if (!is.null(fig.dir.alt))
    dir.create(fig.dir.alt, showWarnings = FALSE)
  nchains <- dim(mcmc.array)[2]
  if (iso.select == "all") {
    c.select <- 1:mcmc.meta$data$C
  } else if (iso.select == "random") {
    c.select <- sample(1:mcmc.meta$data$C, min(20, ceiling(mcmc.meta$data$C/4)))
  } else {
    c.select <- which(is.element(mcmc.meta$data$iso.c, iso.select))
  }
  jags.par <- unlist(mcmc.meta$jags.par)
  #======================================================================
  # Spline parameters
  # Spline parameters: mu.a, sigma.a, a.c's, a
  parnames.a <- jags.par[jags.par %in% 
                           c("mu.a", "sigma.a", paste0("a.c[", c.select, "]"), "a")]
  check.parnames.in.mcmc.array <- parnames.a %in% dimnames(mcmc.array)[[3]] # change JR, 20140520
  if (any(!check.parnames.in.mcmc.array))
    cat(paste0("The following parameters are not found in the mcmc.array: ",
               paste(parnames.a[!check.parnames.in.mcmc.array], collapse = ", "), "\n"))
  parnames.a <- parnames.a[check.parnames.in.mcmc.array]
  if (length(parnames.a) > 0) {
    if (plot.trace) {
      if (is.null(fig.dir.alt)) {
        pdf(file.path(fig.dir, paste(runname, "Trace a.pdf")))
      } else {
        dir.create(file.path(fig.dir.alt, "Trace a"), showWarnings = FALSE)
        pdf(file = file.path(fig.dir.alt, "Trace a", 
                             paste(runname, "Trace a.pdf")))
      }
      for (parname in parnames.a)
        PlotTrace(parname, mcmc.array)
      dev.off()
    }
  }
  #----------------------------------------------------------------------
  # Spline parameters: b.cm's
  parnames.b <- jags.par[jags.par %in%
                           c(paste0("b.cm[", c.select, ",1]"),
                             paste0("b.cm[", c.select, ",2]"))]
  if (indicator.type == "IMR")
    parnames.b <- c(parnames.b, 
                    jags.par[jags.par %in%
                               paste0("p.c[", c.select, "]")])
  check.parnames.in.mcmc.array <- parnames.b %in% dimnames(mcmc.array)[[3]] # change JR, 20140520
  if (any(!check.parnames.in.mcmc.array))
    cat(paste0("The following parameters are not found in the mcmc.array: ",
               paste(parnames.b[!check.parnames.in.mcmc.array], collapse = ", "), "\n"))
  parnames.b <- parnames.b[check.parnames.in.mcmc.array]
  if (length(parnames.b) > 0) {
  if (plot.trace) {
    if (is.null(fig.dir.alt)) {
      pdf(file.path(fig.dir, paste(runname, "Trace b.cm.pdf")), width = 21, height = 7)
    } else {
      dir.create(file.path(fig.dir.alt, "Trace b.cm"), showWarnings = FALSE)
      pdf(file = file.path(fig.dir.alt, "Trace b.cm", 
                           paste(runname, "Trace b.cm.pdf")), width = 21, height = 7)
    }
    if (indicator.type == "U5MR") {
      for (c in c.select) {
        par(mfrow = c(2,2))
        parname <- paste0("b.cm[", c, ",", 1, "]")
        if (parname %in% dimnames(mcmc.array)[[3]]) {
        PlotTrace(parname, mcmc.array, main = mcmc.meta$data$name.c[c])
        plot(exp(c(mcmc.array[, 1, parname])), type = "l", xlab = "Iteration", ylab = "Level")
        if (nchains > 1) {
          for (chain in 2:nchains) {
            lines(exp(c(mcmc.array[, chain, parname])), col = chain)
          }
        }
        }
        if (parname %in% dimnames(mcmc.array)[[3]]) {
        parname <- paste0("b.cm[", c, ",", 2, "]")
        PlotTrace(parname, mcmc.array, main = "")
        # change JR, 20 Feb: added negative sign
        plot(-c(mcmc.array[, 1, parname])/I, type = "l", xlab = "Iteration", ylab = "ARR")
        if (nchains > 1) {
          for(chain in 2:nchains) {
            lines(-c(mcmc.array[, chain, parname])/I, col = chain) # change JR, 20 Feb: added negative sign
          }
        }
        }
      }
    } else if (indicator.type == "IMR") {
      for (c in c.select) {
        par(mfrow = c(2,2))
        parname <- paste0("b.cm[", c, ",", 1, "]")
        if (parname %in% dimnames(mcmc.array)[[3]])
        PlotTrace(parname, mcmc.array, main = mcmc.meta$data$name.c[c])
        parname <- paste0("p.c[", c, "]")
        if (parname %in% dimnames(mcmc.array)[[3]])
        PlotTrace(parname, mcmc.array, main = "")
        parname <- paste0("b.cm[", c, ",", 2, "]")
        if (parname %in% dimnames(mcmc.array)[[3]])
        PlotTrace(parname, mcmc.array, main = "")
      }
    }
    dev.off()
  }
  }
  #----------------------------------------------------------------------
  # Spline parameters: u.cq's
  parnames.u <- NULL
  for (c in c.select)
    parnames.u <- c(parnames.u, jags.par[grepl(paste0("u.cq\\[", c, ","), jags.par)])
  check.parnames.in.mcmc.array <- parnames.u %in% dimnames(mcmc.array)[[3]] # change JR, 20140520
  if (any(!check.parnames.in.mcmc.array))
    cat(paste0("The following parameters are not found in the mcmc.array: ",
               paste(parnames.u[!check.parnames.in.mcmc.array], collapse = ", "), "\n"))
  parnames.u <- parnames.u[check.parnames.in.mcmc.array]
  if (length(parnames.u) > 0) {
  if (plot.trace) {
    if (is.null(fig.dir.alt)) {
      pdf(file.path(fig.dir, paste(runname, "Trace u.cq.pdf")))
    } else {
      dir.create(file.path(fig.dir.alt, "Trace u.cq"), showWarnings = FALSE)
      pdf(file = file.path(fig.dir.alt, "Trace u.cq", 
                           paste(runname, "Trace u.cq.pdf")))
    }
    for (parname in parnames.u)
      PlotTrace(parname, mcmc.array)
    dev.off()
  }
  }
  #======================================================================
  # Non-VR parameters: dft, sigma.ynonvr.t, sigma.ynonvr.tnoSE, bias.nonvr.cs
  parnames.nonvr <- NULL
  if (!is.null(mcmc.meta$jags.par$jags.par.nonvr)) {
    if (run.type == "global") {
      parnames.nonvr <- c(parnames.nonvr, "dft")
      if (mcmc.meta$jags.data$ntypes > 0)
        parnames.nonvr <- c(parnames.nonvr, jags.par[
          jags.par %in% paste0("sigma.ynonvr.t[", seq(1, mcmc.meta$jags.data$ntypes), "]")])
      if (mcmc.meta$jags.data$ntypesnoSE > 0)
        parnames.nonvr <- c(parnames.nonvr, jags.par[
          jags.par %in% paste0("sigma.ynonvr.tnoSE[", seq(1, mcmc.meta$jags.data$ntypesnoSE), "]")])
      parnames.nonvr <- c(parnames.nonvr, jags.par[grepl("bias.nonvr.cs", jags.par)])
      if (plot.trace) {
        if (is.null(fig.dir.alt)) {
          pdf(file.path(fig.dir, paste(runname, "Trace pars.nonvr.pdf")))
        } else {
          dir.create(file.path(fig.dir.alt, "Trace pars.nonvr"), showWarnings = FALSE)
          pdf(file = file.path(fig.dir.alt, "Trace pars.nonvr", 
                               paste(runname, "Trace pars.nonvr.pdf")))
        }
        for (parname in parnames.nonvr)
          PlotTrace(parname, mcmc.array)
        dev.off()
      }
    }
  }
  #----------------------------------------------------------------------
  # Non-VR parameters: mu.beta.tr, sigma.beta.tr, beta.csr
  parnames.beta <- NULL
  if (!is.null(mcmc.meta$jags.par$jags.par.nonvr)) {
    if (run.type == "global")
      parnames.beta <- c(parnames.beta, jags.par[grep("mu.beta.tr|sigma.beta.tr", jags.par)])
    for (c in c.select)
      parnames.beta <- c(parnames.beta, jags.par[
        grepl(paste0("beta.csr\\[", c, ","), jags.par)])
    if (use.country.variance.multipliers)
      parnames.beta <- c(parnames.beta, jags.par[
        jags.par %in% c(paste0("mu.v.r[", 1:2, "]"), paste0("sigma.v.r[", 1:2, "]"), 
                        paste0("v.cr[", c.select, ",1]"), paste0("v.cr[", c.select, ",2]"))]) 
    check.parnames.in.mcmc.array <- parnames.beta %in% dimnames(mcmc.array)[[3]] # change JR, 20140520
    if (any(!check.parnames.in.mcmc.array))
    cat(paste0("The following parameters are not found in the mcmc.array: ",
               paste(parnames.beta[!check.parnames.in.mcmc.array], collapse = ", "), "\n"))
    parnames.beta <- parnames.beta[check.parnames.in.mcmc.array]
    if (length(parnames.beta) > 0) {
    if (plot.trace) {
      if (is.null(fig.dir.alt)) {
        pdf(file.path(fig.dir, paste(runname, "Trace beta.pdf")))
      } else {
        dir.create(file.path(fig.dir.alt, "Trace beta"), showWarnings = FALSE)
        pdf(file = file.path(fig.dir.alt, "Trace beta", 
                             paste(runname, "Trace beta.pdf")))
      }
      for (parname in parnames.beta)
        PlotTrace(parname, mcmc.array)
      dev.off()
    }
    }
  } 
  #----------------------------------------------------------------------
  # Non-VR parameters: mu.biasatzerorecall, tau.biasatzerorecall, recallnobias, biasatzerorecall.cs
  parnames.dhsdirectbias <- NULL
  if (add.dhsdirect.bias) {
    parnames.dhsdirectbias <- jags.par[
      jags.par %in% c("mu.biasatzerorecall", "sigma.biasatzerorecall", "recallnobias")]
    for (c in c.select)
      parnames.dhsdirectbias <- c(parnames.dhsdirectbias, jags.par[
        grepl(paste0("biasatzerorecall.cs\\[", c, ","), jags.par)])
    check.parnames.in.mcmc.array <- parnames.dhsdirectbias %in% dimnames(mcmc.array)[[3]] # change JR, 20140520
    if (any(!check.parnames.in.mcmc.array))
    cat(paste0("The following parameters are not found in the mcmc.array: ",
               paste(parnames.dhsdirectbias[!check.parnames.in.mcmc.array], collapse = ", "), "\n"))
    parnames.dhsdirectbias <- parnames.dhsdirectbias[check.parnames.in.mcmc.array]
    if (length(parnames.dhsdirectbias) > 0) {
    if (plot.trace) {
      if (is.null(fig.dir.alt)) {      
        pdf(file.path(fig.dir, paste(runname, "Trace pars.dhsdirectbias.pdf")))
      } else {
        dir.create(file.path(fig.dir.alt, "Trace pars.dhsdirectbias"), showWarnings = FALSE)
        pdf(file = file.path(fig.dir.alt, "Trace pars.dhsdirectbias", 
                             paste(runname, "Trace pars.dhsdirectbias.pdf")))
      }
      for (parname in parnames.dhsdirectbias)
        PlotTrace(parname, mcmc.array)
      dev.off()
    }
    }
  }
  #======================================================================
  # VR parameters
  parnames.vr <- mcmc.meta$jags.par$jags.par.vr
  check.parnames.in.mcmc.array <- parnames.vr %in% dimnames(mcmc.array)[[3]] # change JR, 20140520
  if (any(!check.parnames.in.mcmc.array))
  cat(paste0("The following parameters are not found in the mcmc.array: ",
             paste(parnames.vr[!check.parnames.in.mcmc.array], collapse = ", "), "\n"))
  parnames.vr<- parnames.vr[check.parnames.in.mcmc.array]
  if (length(parnames.vr) > 0) {
    if (plot.trace) {
      if (is.null(fig.dir.alt)) {
        pdf(file.path(fig.dir, paste(runname, "Trace pars.vr.pdf")))
      } else {
        dir.create(file.path(fig.dir.alt, "Trace pars.vr"), showWarnings = FALSE)
        pdf(file = file.path(fig.dir.alt, "Trace pars.vr", 
                             paste(runname, "Trace pars.vr.pdf")))
      }
      for (parname in parnames.vr)
        PlotTrace(parname, mcmc.array)
      dev.off()
    }
  }
  if (plot.trace)
    cat("Trace plots saved to ", ifelse(is.null(fig.dir.alt), fig.dir, fig.dir.alt), "\n")
  parnames.select <- c(parnames.a, parnames.b, parnames.u,
                       parnames.nonvr, parnames.beta, parnames.vr)
  ##value<< NULL
  return(parnames.select)

}
#----------------------------------------------------------------------
PlotTrace <- function( # Do trace plot for one parameter and add loess smoother for each chain
  parname, ##<< Parameter name
  mcmc.array, ##<< \code{mcmc.array}
  nchains = NULL, ##<< Number of chains
  niter = NULL, ##<< Number of simulations per chain
  main = NULL ##<< Main title of trace plot. If \code{NULL}, defaults to parname.
) {
  if (is.null(main)) main <- parname
  if (is.null(niter)) niter <- dim(mcmc.array)[1]
  if (is.null(nchains)) nchains <- dim(mcmc.array)[2]
  plot(c(mcmc.array[, 1, parname]), type = "l", ylab = parname, main = main,
       ylim = c(min(mcmc.array[, , parname]), max(mcmc.array[, , parname])))
  for (chain in 1:nchains) {
    lines(c(mcmc.array[, chain, parname]), type = "l", col = chain)
  }
  for (chain in 1:nchains) {
    curve(predict(loess(c(mcmc.array[, chain, parname]) ~ seq(1, niter)), x), 
          lty = 2, lwd = 3, add = TRUE, type = "l", col = chain)
  }
  ##value<< NULL
  return(invisible())
}
#----------------------------------------------------------------------
GetRafteryLewisDiagnostics <- function(
  mcmc.array, ##<< \code{mcmc.array} with dimensions (\code{niter}, \code{nchain}, \code{npar}), 
  ## with the third dimension named.
  parnames.select = NULL, ## Names of parameters to get diagnostics for. If \code{NULL}, all parameter 
  ## names in the \code{mcmc.array} are used
  output.dir ## Directory to save diagnostic objects to.
) {
  if (!is.null(parnames.select)) {
    parnames.select <- parnames.select[is.element(parnames.select, dimnames(mcmc.array)[[3]])]
  } else {
    parnames.select <- dimnames(mcmc.array)[[3]]
  }
  cat(paste("Start error messages based on Raftery-Lewis diagnostics.\n"))
  nchains <- dim(mcmc.array)[2]
  niterperchain <- dim(mcmc.array)[1]
  nsim <- niterperchain*nchains
  if (niterperchain <= 600) {
    cat("You need more samples to check convergence!\n")
    return()
  } 
  npar <- length(parnames.select)
  res1.pc <- res2.pc <- matrix(NA, npar, nchains)
  rownames(res1.pc) <- rownames(res2.pc) <- c(parnames.select)
  for (chain in 1:nchains) {
    res1.pc[, chain] <- raftery.diag(mcmc.array[, chain, parnames.select], 
                                     q = 0.975, r = 0.0125)$resmatrix[,"N"]
    res2.pc[, chain] <- raftery.diag(mcmc.array[, chain, parnames.select], 
                                     q = 0.025, r = 0.0125)$resmatrix[,"N"]
  }
  N1 <- apply(res1.pc, 1, median)
  N2 <- apply(res2.pc, 1, median)
  if (length(N1[N1 > nsim & !is.na(N1)]) > 0 | length(N2[N2 > nsim & !is.na(N2)]) > 0) {
    if(length(N2[N2 > nsim & !is.na(N2)]) > 0) {
      cat(paste("Additional samples needed for:", names(N2[N2 > nsim & !is.na(N2)]), round(N2[N2 > nsim & !is.na(N2)]), "\n")) # change JR, 20140520
    }
    if(length(N1[N1 > nsim & !is.na(N1)]) > 0) {
      cat(paste("Additional samples needed for:", names(N1[N1 > nsim & !is.na(N1)]), round(N1[N1 > nsim & !is.na(N1)]), "\n")) # change jR, 20140520
    }
  } else {
    cat(paste("End of Raftery-Lewis error messages, no convergence issues reported.\n"))
  }
  save(N1, file = file.path(output.dir, "N1.rda"))
  save(N2, file = file.path(output.dir, "N2.rda"))
  ##value<< NULL
}
#----------------------------------------------------------------------
GetGelmanDiagnostics <- function(
  mcmc.array, ##<< \code{mcmc.array} with dimensions (\code{niter}, \code{nchain}, \code{npar}), 
  ## with the third dimension named.
  parnames.select = NULL, ## Names of parameters to get diagnostics for. If \code{NULL}, all parameter 
  ## names in the \code{mcmc.array} are used
  output.dir, ## Directory to save diagnostic objects to.
  R.threshold = 1.1 ##<< R threshold beyond which convergence issue is diagnosed.
) {
  if (!is.null(parnames.select)) {
    parnames.select <- parnames.select[is.element(parnames.select, dimnames(mcmc.array)[[3]])]
  } else {
    parnames.select <- dimnames(mcmc.array)[[3]]
  }
  cat("Start error messages based on Gelman's R.\n")
  nchains <- dim(mcmc.array)[2]
  if (nchains < 3){
    cat("You need at least 3 chains for Gelman's R diagnostics!", "\n")
    return()
  }
  # all parameters that are positive and asym distr are transformed
  # transform = T in gelman.diag uses logit/log automatically
  # but logit sometimes a problem if close to 1
  # so decide beforehand if logit or log should be used
  parnames.gelman.logtr <- parnames.gelman.logittr <- parnames.gelman.nottr <- NULL
  for (parname in parnames.select) {
    # if positive and mean is outside 40-60 percentiles?
    outsidep <- abs(0.5 - mean(c(mcmc.array[, , parname]) <= mean(c(mcmc.array[, , parname])))) > 0.1
    if (min(c(mcmc.array[, , parname])) > 0.001 & outsidep) {
      if (max(c(mcmc.array[, , parname])) < 0.999) {
        parnames.gelman.logittr <- c(parnames.gelman.logittr, parname)
      } else {
        parnames.gelman.logtr <- c(parnames.gelman.logtr, parname)
      }
    } else {
      parnames.gelman.nottr <- c(parnames.gelman.nottr, parname)
    }
  }
  # R.all <- NULL
  Gelman.Rhat <- rep(NA, length(parnames.select))
  names(Gelman.Rhat) <- parnames.select
  par.index <- 1
  for (i in 1:3) {
    parnames <- list(parnames.gelman.logtr, parnames.gelman.logittr, parnames.gelman.nottr)[[i]]
    if (length(parnames) > 0) {
      if (i == 1) {
        mcmc.array.temp <- log(mcmc.array[, , parnames, drop = F])
      } else {
        if (i == 2) {
          mcmc.array.temp <- logit(mcmc.array[, , parnames, drop = F])
        } else {
          mcmc.array.temp <- mcmc.array[, , parnames, drop = F]
        }
      }
      for (parname in parnames) {
        post.samp <- mcmc.array.temp[, , parname]
        Gelman.Rhat[par.index] <- Rhat(post.samp)
        par.index <- par.index + 1
      }    
      # mcmc <- mcmc.list()
      # for (chain in 1:nchains) {
      #   if (length(parnames) > 1) {
      #     mcmc[[chain]] <- as.mcmc(mcmc.array.temp[, chain, ])
      #   } else {
      #     mcmc[[chain]] <- as.mcmc(mcmc.array.temp[, chain])
      #   }
      # }
      # r <- gelman.diag(mcmc, autoburnin = FALSE, transform = F)$psrf
      # R <- r[, "Point est."]
      # names(R) <- parnames
      # if (length(R[R > R.threshold]) > 0) {
      #   cat(paste("Additional samples needed for:", names(R[R > R.threshold]), 
      #             "(R =", round(R[R > R.threshold], digits = 3), ")\n"))
      # }
      # R.all <- c(R.all, R)
    }
  }
  # R <- R.all
  # save(R, file = file.path(output.dir, "R.rda"))
  Gelman.Rhat <- sort(Gelman.Rhat, decreasing = TRUE)
  if (any(Gelman.Rhat > R.threshold))
    cat(paste0("Additional samples needed for: ", 
		 paste(names(Gelman.Rhat[Gelman.Rhat > R.threshold]), collapse = ", "), "\n"))
  Gelman.Rhat <- round(Gelman.Rhat, digits = 3)
  Rhat.all <- data.frame(parameter = names(Gelman.Rhat),
			    Rhat = Gelman.Rhat)
  write.csv(Rhat.all, file = file.path(output.dir, "GelmanRhat.csv"), row.names = F)
  save(Rhat.all, file = file.path(output.dir, "Rhat.all.rda"))
  cat(paste("End of Gelman error messages (if there are no messages above, no convergence issues reported!)\n"))
  ##value<< NULL
  return(invisible())
}

Rhat <- function(arr) {
  dm <- dim(arr)
  if (length(dm)==2) return(Rhat1(arr))
  if (dm[2]==1) return(NULL)
  if (dm[3]==1) return(Rhat1(arr[,,1]))
  return(apply(arr,3,Rhat1))
}

Rhat1 <- function(mat) {
  m <- ncol(mat)
  n <- nrow(mat)
  b <- apply(mat,2,mean)
  B <- sum((b-mean(mat))^2)*n/(m-1)
  w <- apply(mat,2,var)
  W <- mean(w)
  s2hat <- (n-1)/n*W + B/n
  Vhat <- s2hat + B/m/n 
  covWB <- n /m * (cov(w,b^2)-2*mean(b)*cov(w,b))
  varV <- (n-1)^2 / n^2 * var(w)/m +
    (m+1)^2 / m^2 / n^2 * 2*B^2/(m-1) +
    2 * (m-1)*(n-1)/m/n^2 * covWB
  df <- 2 * Vhat^2 / varV
  R <- sqrt((df+3) * Vhat / (df+1) / W)
  return(R)
}
#----------------------------------------------------------------------
# Raftery-Lewis diagnostics: further analysis
# # extract country indices of countries where u.cq did not converge  
# if(sum(N1 > nsim) > 0) {
#   summary(N1[N1 > nsim] - nsim)
#   hist(N1[N1 > nsim] - nsim)
#   c.check1 <- c(as.numeric(sapply(substring(names(N1[N1 > nsim & grepl("u.cq", names(N1))]), 6, 8), 
#                                   function(x) { strsplit(x, ",")[[1]][1] })))
# } else {
#   c.check1 <- NULL
# }
# if(sum(N2 > nsim) > 0) {
#   summary(N2[N2 > nsim] - nsim)
#   hist(N2[N2 > nsim] - nsim)
#   c.check2 <- c(as.numeric(sapply(substring(names(N2[N2 > nsim & grepl("u.cq", names(N2))]), 6, 8), 
#                                   function(x) { strsplit(x, ",")[[1]][1] })))
# } else {
#   c.check2 <- NULL
# }
# # nsim = 6000
# c.check <- unique(c.check1, c.check2)
# if(length(c.check) > 0) {
#   country.info.L$name.c[c.check]
#   # which of these countries do not have VR data?
#   country.info.L$name.c[c.check][nvr.c[c.check] == 0]
# }

# # Gelman diagnostics: further analysis
# # extract country indices of countries where u.cq did not converge
# runname <- "GR20140512"
# output.dir <- file.path("output", runname)
# load(file.path(output.dir, "mcmc.meta.rda"))
# load(file.path(output.dir, "Rhat.all.rda"))
# c.check.u <- c.check.a <- NULL
# if (any(Rhat.all$Rhat > 1.1)) {
#   c.check.u <- as.numeric(sapply(substring(Rhat.all$parameter[Rhat.all$Rhat > 1.1 & 
#                                                                 grepl("u.cq", Rhat.all$parameter)], 6, 8), 
#                                  function(x) { strsplit(x, ",")[[1]][1] }))
#   c.check.a <- as.numeric(substring(Rhat.all$parameter[Rhat.all$Rhat > 1.1 & 
#                                                                   grepl("a.c", Rhat.all$parameter)], 5, 5))
# }
# c.check.u <- unique(c.check.u)
# c.check.a <- unique(c.check.a)
# 
# if (length(c.check.u) > 0)
#   mcmc.meta$data$name.c[c.check.u]
# 
# if (length(c.check.a) > 0)
#   mcmc.meta$data$name.c[c.check.a]