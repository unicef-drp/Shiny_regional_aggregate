#----------------------------------------------------------------------
# plotpriorsandposteriors.R
#----------------------------------------------------------------------
PlotPriorsAndPosteriors <- function(# Plot priors and posteriors of parameters for global run.
  ### Plot priors and posteriors of parameters for global run.
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  fig.dir = NULL ##<< Directory to store plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  load(file.path(output.dir, "mcmc.meta.rda"))
  load(file.path(output.dir, "mcmc.array.rda"))
  list2env(mcmc.meta$settings, envir = environment())
  if (run.type == "country")
    return(invisible())
  #----------------------------------------------------------------------
  ##details<< Summary of posterior samples are written to \code{output.dir/posterior_summary.txt}.
  filename <- file.path(output.dir, "posterior_summary.txt")
  fileout <- file(filename, open = "wt")
  sink(fileout, split = T)
  cat(paste("Summary of posterior samples is written to file ", filename), "\n")
  cat(paste("parname", "\t", "mean", "\t", "sd", "\n"))
  jags.par <- unlist(mcmc.meta$jags.par)
  pdf(file.path(fig.dir, paste(runname, "Priors and Posteriors hyperparameters.pdf")))
  parname <- "mu.a"
  if (parname %in% jags.par) { 
    cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
    PlotPostWithNormalPrior(post.samp = c(mcmc.array[, , parname]), 
                            priormean = -3, priorsd = sqrt(1/0.1), parname = parname)
  }
  parname <- "sigma.a"
  if (parname %in% jags.par) {
    cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
    PlotPostWithUnifPrior(post.samp = c(mcmc.array[, , parname]), 
                          priorlow = 0, priorup = 5, parname = parname)
  } 
  parname <- "a"
  if (parname %in% jags.par) {
    cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
    PlotPostWithNormalPrior(post.samp = c(mcmc.array[, , parname]), 
                            priormean = -2, priorsd = sqrt(1/0.02), parname = parname)
  }
  #----------------------------------------------------------------------
  if (mcmc.meta$jags.data$ntypes > 0) {
    for (t in 1:mcmc.meta$jags.data$ntypes) {
      parname <- paste0("sigma.ynonvr.t[", t, "]")
      if (parname %in% jags.par) {
        postsd <- c(mcmc.array[, , parname])
        cat(paste(parname, mcmc.meta$data$typenames[t], "\t", mean(postsd), "\t", sd(postsd), "\n"))
        PlotPostWithUnifPrior(post.samp = postsd, priorlow = 0, priorup = 0.5,
                              parname = parname, title = mcmc.meta$data$typenames[t])
      }
    }
  }
  if (mcmc.meta$jags.data$ntypesnoSE > 0) {
    for (tnoSE in 1:mcmc.meta$jags.data$ntypesnoSE) {
      parname <- paste0("sigma.ynonvr.tnoSE[", tnoSE, "]")
      if (parname %in% jags.par) {
        postsd <- c(mcmc.array[, , parname])
        cat(paste(parname, mcmc.meta$data$typenames[tnoSE], "\t", mean(postsd), "\t", sd(postsd), "\n"))
        PlotPostWithUnifPrior(post.samp = postsd, priorlow = 0, priorup = 0.5,
                              parname = parname, title = mcmc.meta$data$typenoSEnames[tnoSE])
      }
    }
  }
  if (mcmc.meta$jags.data$ntypes > 0) {
    for (r in 1:2) {
      for (t in 1:mcmc.meta$jags.data$ntypes) {
        parname <- paste0("sigma.beta.tr[", t, ",", r, "]")
        if (parname %in% jags.par) {
          cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
          PlotPostWithUnifPrior(post.samp = c(mcmc.array[, , parname]), priorlow = 0, priorup = 5, 
                                parname = parname, title = mcmc.meta$data$typenames[t])
        }
      }
    }
  }
  for (r in 1:2) {
    parname <- paste0("sigma.v.r[", r, "]") 
    if (parname %in% jags.par) {
      cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
      PlotPostWithUnifPrior(post.samp = c(mcmc.array[, , parname]), priorlow = 0, priorup = 5, parname = parname)
    }
  }
  parname <- "dft"
  if (parname %in% jags.par) {
    cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
    PlotPostWithUnifPrior(post.samp = c(mcmc.array[, , parname]), priorlow = 2, priorup = 30, parname = parname)
  }
  if (!is.null(mcmc.meta$jags.data$is.nonvrwithbias.cs)) {
    getc.nonvrbias.d <- seq(1, mcmc.meta$jags.data$C)[
      rowSums(mcmc.meta$jags.data$is.nonvrwithbias.cs[1:mcmc.meta$jags.data$C, , drop = FALSE], na.rm = T) > 0]
    for (c in getc.nonvrbias.d) {
      gets.nonvrbias.t <- seq(1, mcmc.meta$jags.data$S.c[c])[
        mcmc.meta$jags.data$is.nonvrwithbias.cs[c, 1:mcmc.meta$jags.data$S[c]] == 1]
      for (s in gets.nonvrbias.t) {
        parname <- paste0("bias.nonvr.cs[", c, ",", s, "]")
        cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
        PlotPostWithUnifPrior(post.samp = c(mcmc.array[, , parname]), priorlow = 0, priorup = 1, 
                              parname = parname, 
                              title = paste0(mcmc.meta$data$name.c[c], "\n",
                                             mcmc.meta$data$source.Lc.s[[c]][s], " ",
                                             mcmc.meta$data$seriesyear.Lc.s[[c]][s]))
      }
    }
  }
  parname <- "mu.biasatzerorecall"
  if (parname %in% jags.par) {
    cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
    PlotPostWithUnifPrior(post.samp = c(mcmc.array[, , parname]), priorlow = log(0.85), priorup = 10, parname = parname)
  }
  parname <- "sigma.biasatzerorecall"
  if (parname %in% jags.par) {
    cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
    PlotPostWithUnifPrior(post.samp = 1/sqrt(c(mcmc.array[, , parname])), 
                          priorlow = 0, priorup = 1, parname = parname)
  }
  parname <- "recallnobias" # change JR, 20131204
  if (parname %in% jags.par) {
    cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
    PlotPostWithUnifPrior(post.samp = c(mcmc.array[, , parname]), priorlow = 0, priorup = 7, parname = parname)
  }
  #----------------------------------------------------------------------
  for (c in mcmc.meta$jags.data$getc.vr.d[!is.na(mcmc.meta$jags.data$getc.vr.d)]) {
    parname <- paste0("sigma.yvr.c[", c, "]")
    if (parname %in% jags.par) {
      cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
      PlotPostWithUnifPrior(post.samp = c(mcmc.array[, , parname]), priorlow = 0, priorup = 0.5, 
                            parname = parname, title = mcmc.meta$data$name.c[c])
    }
  }
  for (c in mcmc.meta$jags.data$getc.vr.d[!is.na(mcmc.meta$jags.data$getc.vr.d)]) {
    parname <- paste0("bias.vr.c[", c, "]")
    if (parname %in% jags.par) {
      cat(paste(parname, "\t", mean(c(mcmc.array[, , parname])), "\t", sd(c(mcmc.array[, , parname])), "\n"))
      PlotPostWithUnifPrior(post.samp = c(mcmc.array[, , parname]), priorlow = 0, priorup = 1, 
                            parname = parname, title = mcmc.meta$data$name.c[c])
    }
  }
  dev.off()
  #----------------------------------------------------------------------
  # mu.beta.tr
  if (mcmc.meta$jags.data$ntypes > 0) {
    if (any(grepl("mu.beta.tr", jags.par))) {
    pdf(file.path(fig.dir, paste(runname, "Priors and posteriors mu.betas.pdf")))
    for (t in 1:mcmc.meta$jags.data$ntypes) {
      mu1.post.i <- c(mcmc.array[, , paste0("mu.beta.tr[", t, ",1]")])
      mu2.post.i <- c(mcmc.array[, , paste0("mu.beta.tr[", t, ",2]")])
      cat(paste(paste0("mu.beta.tr[", t, ",1]"), mcmc.meta$data$typenames[t], "\t", 
                mean(mu1.post.i), "\t", sd(mu1.post.i), "\n"))
      cat(paste(paste0("mu.beta.tr[", t, ",2]"), mcmc.meta$data$typenames[t], "\t", 
                mean(mu2.post.i), "\t", sd(mu2.post.i), "\n"))
      cat(paste(paste0("cor(mu.beta.tr[", t, ",1],mu.beta.tr[", t, "])"), "\t", 
                cor(mu1.post.i, mu2.post.i), "\n"))
      S <- length(mu2.post.i)
      mu.prior.i2 <- rmvnorm(S, mcmc.meta$jags.data$mu0.mubeta.tr[t, 1:2], 
                             solve(mcmc.meta$jags.data$Tau0.mubeta.trr[t, 1:2, 1:2]))
      par(mfrow = c(1,3))
      plot(mu.prior.i2[, 1] ~ mu.prior.i2[, 2], 
           xlab = "mu[2]", ylab = "mu[1]", main = mcmc.meta$data$typenames[t], col = 2)  
      abline(h = 0, col = 3, lwd = 3)
      abline(v = 0, col = 3, lwd = 3)
      points(mu1.post.i ~ mu2.post.i, col = 1)
      # hist(mu.prior.i2[,1], freq = F)
      # lines(density(mu1.post.i), col = 2)
      # hist(mu.prior.i2[,2], freq = F)
      # lines(density(mu2.post.i), col = 2)
      hist(mu1.post.i, freq = F, col = "grey")
      abline(v = 0, col = 3, lwd = 3)
      lines(density(mu.prior.i2[, 1]), col = 2, lwd = 3)
      hist(mu2.post.i, freq = F, col = "grey")
      abline(v = 0, col = 3, lwd = 3)
      lines(density(mu.prior.i2[, 2]), col = 2, lwd = 3)
    }
    dev.off()
    }
  }
  #----------------------------------------------------------------------
  # b.c's
  pdf(file.path(fig.dir, paste(runname, "Posts b.cs.pdf")), width = 21, height = 7)
  if (indicator.type == "U5MR") {
    # prior sample:
    S <- dim(mcmc.array)[1]
    level <- runif(S, 1, 1000) # change JR, 20 Apr: from 1 to 500
    b1.prior <- log(level)
    ARR <- runif(S, -0.2, 0.25) # change JR, 20 Apr: from -0.1 to 0.15
    b2.prior <- -ARR*I # change JR, 7 Feb: added "-" in front of ARR
    for (c in 1:mcmc.meta$data$C) {
      if (paste0("b.cm[", c, ",", 1, "]") %in% dimnames(mcmc.array)[[3]]) {
        par(mfrow = c(2,2))
        b1 <- c(mcmc.array[, , paste0("b.cm[", c, ",", 1, "]")])
        cat(paste(paste0("b.cm[", c, ",", 1, "]"), "\t", mean(b1), "\t", sd(b1), "\n"))
        # hist(b1.prior, freq = FALSE)
        # lines(density(b1), col = 2, lwd = 3)
        hist(exp(b1), freq = FALSE, col = "grey", main = mcmc.meta$data$name.c[c])
        lines(density(level), col = 2, lwd = 3)
        b2 <- c(mcmc.array[, , paste0("b.cm[", c, ",", 2, "]")])
        cat(paste(paste0("b.cm[", c, ",", 2, "]"), "\t", mean(b2), "\t", sd(b2), "\n"))
        hist(-b2/I, freq = FALSE, col = "grey", main = mcmc.meta$data$name.c[c])
        lines(density(ARR), col = 2, lwd = 3)
        plot(ARR ~ level)
        points(-b2/I ~ exp(b1), col = 2)
      }
    }
    par(mfrow = c(2,2))
    # get posterior medians
    b.cm <- matrix(NA, mcmc.meta$data$C, 2)
    for (c in 1:mcmc.meta$data$C) {
      if (paste0("b.cm[", c, ",", 1, "]") %in% dimnames(mcmc.array)[[3]]) {
        b.cm[c,1] <- median(c(mcmc.array[, , paste0("b.cm[", c, ",", 1, "]")]))
        b.cm[c,2] <- median(c(mcmc.array[, , paste0("b.cm[", c, ",", 2, "]")]))
      }
    }  
    hist(b.cm[,1], main = "Posterior medians of b.cm[c,1] by country")
    # exp(b1) corresponds to U5MR at midpoint
    hist(exp(b.cm[, 1]), main = "Posterior medians of exp(b.cm[c,1]) by country")
    hist(b.cm[, 2], main = "Posterior medians of b.cm[c,2] by country")
    # b2/interval length corresponds to ARR
    hist(-b.cm[,2 ]/I, main = "Posterior medians of -b.cm[c,2]/I by country")
  } else if (indicator.type =="IMR") {
    S <- dim(mcmc.array)[1]
    p.prior <- runif(S, 0.01, 1)
    b1.prior <- logit(p.prior)
    b2.prior <- rnorm(S, 0, sqrt(1/0.1))
    par(mfrow = c(2,2))
    for (c in 1:mcmc.meta$data$C) {
      if (paste0("b.cm[", c, ",", 1, "]") %in% dimnames(mcmc.array)[[3]]) {
      b1 <- c(mcmc.array[ , , paste0("b.cm[", c, ",", 1, "]")])
      #hist(b1.prior, freq = FALSE)
      #lines(density(b1), col = 2, lwd = 3)
      hist(b1, freq = FALSE, col = "grey", main = mcmc.meta$data$name.c[c])
      lines(density(b1.prior), col = 2, lwd = 3)
      p <- c(mcmc.array[ , , paste0("p.c[", c, "]")])
      hist(p, freq = FALSE, col = "grey", main = mcmc.meta$data$name.c[c])
      lines(density(p.prior), col = 2, lwd = 3)
      b2 <- c(mcmc.array[ , , paste0("b.cm[", c, ",", 2, "]")])
      hist(b2, freq = FALSE, col = "grey", main = mcmc.meta$data$name.c[c])
      lines(density(b2.prior), col = 2, lwd = 3)
      }
    }
    # get posterior medians
    b.cm <- matrix(NA, mcmc.meta$data$C, 2)
    p.c <- rep(NA, mcmc.meta$data$C)
    for (c in 1:mcmc.meta$data$C) {
      if (paste0("b.cm[", c, ",", 1, "]") %in% dimnames(mcmc.array)[[3]]) {
        b.cm[c, 1] <- median(c(mcmc.array[, , paste0("b.cm[", c, ",", 1, "]")]))
        b.cm[c, 2] <- median(c(mcmc.array[, , paste0("b.cm[", c, ",", 2, "]")]))
        p.c[c] <- median(c(mcmc.array[, , paste0("p.c[", c, "]")]))
      }
    }  
    hist(b.cm[, 1], main = "Posterior medians of b.cm[c,1] by country")
    hist(b.cm[, 2], main = "Posterior medians of b.cm[c,2] by country")
    hist(p.c, main = "Posterior medians of p.c[c] by country")
  }
  dev.off()
  sink()
  closeAllConnections()
  ##value<< \code{NULL}
  return(invisible())
}
