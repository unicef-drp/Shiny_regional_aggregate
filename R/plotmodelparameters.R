#----------------------------------------------------------------------
# plotmodelparameters.R
#----------------------------------------------------------------------
PlotCountrySmoothingParameters <- function(# Plot country smoothing parameter, 
  ## i.e. \code{sigma.u}, which is \code{exp(a.c)}
  runname, ##<< Run name.
  output.dir = NULL, ##<< Output directory where \code{mcmc.meta} and \code{mcmc.array} are saved.
  fig.dir = NULL, ##<< Directory to save plots.
  percentiles = c(0.05, 0.5, 0.95) ##<< Percentiles.
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname)
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  load(file.path(output.dir, "mcmc.meta.rda"))
  if (mcmc.meta$settings$run.type != "country" & !mcmc.meta$settings$use.constant.sigma.u) {
    load(file.path(output.dir, "mcmc.array.rda"))
    a.cq <- matrix(NA, mcmc.meta$data$C, 3)
    for (c in 1:mcmc.meta$data$C) {
      if (paste0("a.c[", c, "]") %in% dimnames(mcmc.array)[[3]])
        a.cq[c, ] <- quantile(mcmc.array[ , , paste0("a.c[", c, "]")], probs = percentiles)
    }
    if (mcmc.meta$settings$run.type == "global") {
      exp.mu.a <- exp(median(c(mcmc.array[, , "mu.a"])))
    } else if (mcmc.meta$settings$run.type == "combined") {
      exp.mu.a <- exp(mcmc.meta$data.global$mcmc.post$mu.a)
    }
    pdf(file = file.path(fig.dir, paste(runname, "Variability in smoothing parameter.pdf")), width = 7, height = 28)
    PlotCIDotsAndSegments(CIs.cq = exp(a.cq),
                          name.c = mcmc.meta$data$name.c, 
                          order = "median",
                          vabline = exp.mu.a,
                          main = "Variability in smoothing parameter sigma.u",
                          xlab = "sigma.u (exp(a.c))",
                          cex.axis = 0.8)
    mtext("exp(mu.a)", side = 1, at = exp.mu.a, cex = 0.6)
    dev.off()
    cat(paste0("Plot of country smoothing parameters saved to ", fig.dir, ".\n"))
  }
  ##value<< \code{NULL}.
  return(invisible())
}
#----------------------------------------------------------------------
PlotCountryVarianceMultipliers <- function(
  runname, ##<< Run name.
  output.dir = NULL, ##<< Output directory where \code{mcmc.meta} and \code{mcmc.array} are saved.
  fig.dir, ##<< Directory to save plots.
  percentiles = c(0.05,0.5,0.95) ##<< Percentiles.
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname)
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  load(file.path(output.dir, "mcmc.meta.rda"))
  if (mcmc.meta$settings$use.country.variance.multipliers & !is.null(mcmc.meta$data$Cnonvr)) {
    if (mcmc.meta$data$Cnonvr > 0) {
      load(file.path(output.dir, "mcmc.array.rda"))
      Cnonvr <- mcmc.meta$jags.data$Cnonvr
      getc.nonvr.d <- mcmc.meta$jags.data$getc.nonvr.d[!is.na(mcmc.meta$jags.data$getc.nonvr.d)]
      # analysis of the v.c's:
      # 1. check if posterior median is equal to one 
      # (else tau needs to be adjusted in PIplots, maybe in jags model in future runs)
      # and v's need to be adjusted below
      v1.sc <- v2.sc <- matrix(NA, dim(mcmc.array)[1], dim(mcmc.array)[2])
      for (s in 1:dim(mcmc.array)[1]) {
        for (c in 1:dim(mcmc.array)[2]) {
          v1.sc[s, c] <- median(mcmc.array[s, c, paste0("v.cr[", getc.nonvr.d, ",1]")])
          v2.sc[s, c] <- median(mcmc.array[s, c, paste0("v.cr[", getc.nonvr.d, ",2]")])
        }
      }
      summary(v1.sc)  
      # country-specific variance multipliers
      v1.cq <- v2.cq <-  matrix(NA, Cnonvr, 3)
      for (j in 1:Cnonvr) {
        v1.cq[j, ] <- quantile(c(mcmc.array[, , paste0("v.cr[", getc.nonvr.d[j], ",1]")]/v1.sc), percentiles)  
        v2.cq[j, ] <- quantile(c(mcmc.array[, , paste0("v.cr[", getc.nonvr.d[j], ",2]")]/v2.sc), percentiles) 
      }
      index.c <- getc.nonvr.d
      name.novr.c <- mcmc.meta$data$name.c[index.c]
      select.c <- order(v1.cq[,2])[1:10]
      pdf(file = file.path(fig.dir, paste(runname, "Variability in series level biases (lowest).pdf")))
      PlotCIDotsAndSegments(CIs.cq = sqrt(v1.cq),
                            select.c = select.c, 
                            main = "Variability in level biases",
                            name.c = name.novr.c, 
                            order = "median",
                            xlab = "St. dev. country multiplier",
                            logscale = FALSE,
                            vabline = 1, 
                            name.tif = NULL)
      dev.off()
      pdf(file = file.path(fig.dir, paste(runname, "Variability in series level biases (highest).pdf")))
      select.c <- rev(order(v1.cq[,2]))[1:10]
      PlotCIDotsAndSegments(CIs.cq =sqrt(v1.cq),
                            select.c  = select.c, 
                            main = "Variability in level biases",
                            name.c = name.novr.c, 
                            order = "median",
                            xlab = "St. dev. country multiplier",
                            logscale = FALSE,
                            vabline = 1, 
                            name.tif = NULL)
      dev.off()
      pdf(file = file.path(fig.dir, paste(runname, "Variability in series slope biases (lowest).pdf")))
      select.c <- order(v2.cq[,2])[1:10]
      PlotCIDotsAndSegments(CIs.cq = sqrt(v2.cq), 
                            select.c  = select.c, 
                            main = "Variability in slope biases",
                            name.c = name.novr.c, 
                            order = "median",
                            xlab = "St. dev. country multiplier",
                            logscale = FALSE,
                            vabline = 1, 
                            name.tif = NULL)
      dev.off()
      pdf(file = file.path(fig.dir, paste(runname, "Variability in series slope biases (highest).pdf")))
      select.c <- rev(order(v2.cq[,2]))[1:10]
      PlotCIDotsAndSegments(CIs.cq = sqrt(v2.cq), 
                            select.c  = select.c, 
                            main = "Variability in slope biases",
                            name.c = name.novr.c, 
                            order = "median",
                            xlab = "St. dev. country multiplier",
                            logscale = FALSE,
                            vabline = 1, 
                            name.tif = NULL)
      dev.off()
      # the "best behaved" countries (also taking into account upper bound)
      pdf(file = file.path(fig.dir, paste(runname, "Variability in series slope biases (best).pdf")))
      select.c <- seq(1, Cnonvr)[v2.cq[, 2] < 0.9 & v1.cq[, 2] < 0.9 & 
                                   sqrt(v2.cq[, 3]) < 1.5 & sqrt(v1.cq[, 3]) < 1.5]
      PlotCIDotsAndSegments(CIs.cq = sqrt(v1.cq), 
                            CIs2.cq = sqrt(v2.cq),
                            select.c = select.c, 
                            main = "Variability in biases",
                            name.c = name.novr.c, 
                            order = "alph",
                            xlab = "St. dev. country multiplier",
                            logscale = FALSE,
                            vabline = 1, 
                            name.tif = NULL)
      dev.off()
      cat(paste0("Plots of country variance multipliers saved to ", fig.dir, ".\n"))
    }
  }
  return(invisible())
}
#----------------------------------------------------------------------
PlotDHSDirectBias <- function(
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  fig.dir = NULL, ##<< Directory to store plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
  recall = seq(0, 20, 0.1) ##<< Vector of retrospective periods to plot bias for.
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  load(file.path(output.dir, "mcmc.meta.rda"))
  run.type <- mcmc.meta$settings$run.type
  if (run.type == "global" & mcmc.meta$settings$add.dhsdirect.bias) {
    load(file.path(output.dir, "mcmc.array.rda"))
    # Check that there are DHS Direct series
    if (mcmc.meta$data$Smax > 0) {
      getc.dhsdirect.d <- seq(1, mcmc.meta$data$C)[
        apply(mcmc.meta$jags.data$is.dhsdirect.cs[1:mcmc.meta$jags.data$C, , drop = FALSE], 1, sum, na.rm = T) > 0]
      if (length(getc.dhsdirect.d) == 0) {
        return(invisible())
      }
    } else {
      return(invisible())
    }
    # Global parameters summary
    # DHS Direct extra bias
    if (run.type == "global") {
      cat("Summary of mu.biasatzerorecall:\n")
      print(summary(c(mcmc.array[, , "mu.biasatzerorecall"])))
      cat("Summary of sigma.biasatzerorecall:\n")
      print(summary(c(mcmc.array[, , "sigma.biasatzerorecall"])))
      cat("Summary of recallnobias:\n") # change JR, 20131204
      print(summary(c(mcmc.array[, , "recallnobias"])))
      mu.biasatzerorecall <- median(c(mcmc.array[, , "mu.biasatzerorecall"]))
      recallnobias <- median(c(mcmc.array[, , "recallnobias"]))
    } else {
      cat(paste0("Summary:\n", "mu.biasatzerorecall:\n"))
      print(mcmc.meta$data.global$mcmc.post$mu.biasatzerorecall)
      cat("sigma.biasatzerorecall:\n")
      print(mcmc.meta$data.global$mcmc.post$sigma.biasatzerorecall)
      cat("recallnobias:\n") # change JR, 20131204
      print(mcmc.meta$data.global$mcmc.post$recallnobias)
      mu.biasatzerorecall <- mcmc.meta$data.global$mcmc.post$mu.biasatzerorecall
      recallnobias <- mcmc.meta$data.global$mcmc.post$recallnobias
    }
    # DHS Direct multilevel bias
    t <- which(mcmc.meta$data.global$typenames == "DHS Direct")
    mu.beta1 <- eval(parse(text = paste0("mcmc.meta$data.global$mcmc.post[['mu.beta.tr[", t, ",1]']]")))
    mu.beta2 <- eval(parse(text = paste0("mcmc.meta$data.global$mcmc.post[['mu.beta.tr[", t, ",2]']]")))
    
    # Global plot
    pdf(file.path(fig.dir, paste(runname, "DHS Direct bias (global).pdf")))
    biases.global <- #mu.beta1 + mu.beta2*(recall - mcmc.meta$data$recall.mid) +
      GetDHSDirectBias(recall = recall, biasatzerorecall = mu.biasatzerorecall, 
                       recallnobias = recallnobias)  
    plot(biases.global ~ recall, type = "l",
         main = "DHS Direct bias (global)",
         xlab = "Retrospective period (years)", ylab = "Bias (on log scale)",
         ylim = c(min(biases.global, na.rm = T), max(biases.global, 0, na.rm = T)), lwd = 3)
    abline(h = 0)
    dev.off()
    cat(paste0("Plot of global DHS Direct bias saved to ", fig.dir, ".\n"))
    
    # Plot by country by series
    Smax <- max(mcmc.meta$data$nseriesnonvr.c, na.rm = T)
    biasatzerorecall.cs <- array(NA, c(max(which(mcmc.meta$data$nseriesnonvr.c != 0), na.rm = T), Smax)) # change JR, 20131204
    biases.Lcs.j <- recall.Lcs.j <- biasmultilevel.Lcs.j <- beta1.Lc.s <- beta2.Lc.s <- list()
    pdf(file.path(fig.dir, paste(runname, "DHS Direct bias (by country).pdf")))
    for (c in getc.dhsdirect.d) {
      # if(c == 1) next()
      # print(paste0("c = ", c))
      recall.Lcs.j[[c]] <- biases.Lcs.j[[c]] <- biasmultilevel.Lcs.j[[c]] <- list()
      beta1.Lc.s[[c]] <- beta2.Lc.s[[c]] <- rep(NA, Smax)
      gets.dhsdirect.t <- which(mcmc.meta$jags.data$is.dhsdirect.cs[c, ] == 1 & 
                                  !is.na(mcmc.meta$jags.data$is.dhsdirect.cs[c, ]))
      # print(paste0("gets.dhsdirect.t = ", paste(gets.dhsdirect.t, collapse = ", ")))
      for (s in gets.dhsdirect.t) {
        # print(paste0("s = ", s))
        # DHS Direct extra bias
        biasatzerorecall.cs[c, s] <- median(c(mcmc.array[, , paste0("biasatzerorecall.cs[", 
                                                                    c, ",", s, "]")]))
        recall.Lcs.j[[c]][[s]] <- recall
        # DHS Direct multilevel bias
        beta1.Lc.s[[c]][s] <- median(c(mcmc.array[, , paste0("beta.csr[", c, ",", s, ",1]")]))
        beta2.Lc.s[[c]][s] <- median(c(mcmc.array[, , paste0("beta.csr[", c, ",", s, ",2]")]))
        biasmultilevel.Lcs.j[[c]][[s]] <- beta1.Lc.s[[c]][s] + beta2.Lc.s[[c]][s]*(recall - mcmc.meta$data$recall.mid)
        biases.Lcs.j[[c]][[s]] <- #biasmultilevel.Lcs.j[[c]][[s]] + 
          GetDHSDirectBias(recall = recall, biasatzerorecall = biasatzerorecall.cs[c, s], 
                           recallnobias = recallnobias) # change JR, 20131204
      }
      ymin <- min(c(#biases.global, 
        unlist(biases.Lcs.j[[c]])), na.rm = T)
      ymax <- max(c(#biases.global, 
        unlist(biases.Lcs.j[[c]]), 0), na.rm = T)
      par(mar = c(5, 4, 4, 2))
      # plot global bias
      #plot(biases.global ~ recall, type = "l", 
      #     ylim = c(ymin, ymax),
      #     main = mcmc.meta$data$name.c[c],
      #     xlab = "Retrospective period (years)", ylab = "Bias (on log scale)")
      plot(1, type = "l", 
           xlim = range(recall),
           ylim = c(ymin, ymax),
           main = mcmc.meta$data$name.c[c],
           xlab = "Retrospective period (years)", ylab = "Bias (on log scale)")
      
      # plot survey biases
      res <- AddSurveyData(u.Ls.i = biases.Lcs.j[[c]], year.Ls.i = recall.Lcs.j[[c]], 
                           sourcetype.s = mcmc.meta$data$sourcetype.Lc.s[[c]], 
                           method.s = mcmc.meta$data$method.Lc.s[[c]], 
                           surveyyear.s = mcmc.meta$data$seriesyear.Lc.s[[c]],
                           plot.points = FALSE, lwd = 3)
      abline(h = 0, lty = 2)
      # plot legend
      legend("bottomright", legend = c(rev(res$legendtext.s)), col = c(rev(res$legendcol.s)), 
             lty = 1, pch = 19)
    }
    dev.off()
    cat(paste0("Plot of country-specific DHS Direct bias saved to ", fig.dir, ".\n"))
  }
  ##value<< \code{NULL}.
  return(invisible())
}
#----------------------------------------------------------------------
GetDHSDirectBias <- function( # Linear function for DHS Direct bias.
  recall, ##<< Vector of retrospective periods to get bias for.
  biasatzerorecall, ##<< Bias when retrospective period is zero.
  recallnobias ##<< Retrospective period beyond which bias is zero. 
) {
  ##value<< Vector of DHS Direct biases.
  return(biasatzerorecall - biasatzerorecall/recallnobias*ifelse(recall < recallnobias, recall, recallnobias))
}
