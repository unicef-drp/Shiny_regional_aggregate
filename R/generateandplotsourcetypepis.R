#----------------------------------------------------------------------
# generateandplotsourcetypepis.R
#----------------------------------------------------------------------
GenerateAndPlotSourceTypePIs <- function(
  runname,  ##<< Run name.
  output.dir = NULL, ##<< Output directory where run objects are saved.
  fig.dir = NULL, ##<< Directory to store plots.
  recallperiod.R = c(5, 10, 20), ##<< Vector of retrospective periods to simulate for.
  igme = 100, ##<< Simulated true value.
  percentiles = c(0.05, 0.5, 0.95), ##<< Percentiles.
  plot.mean = TRUE, ##<< Plot mean of PIs?
  plot.type = "pdf" ##<< Plot "pdf" or "eps" file?
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname)
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  load(file.path(output.dir, "mcmc.meta.rda"))
  if (mcmc.meta$settings$run.type == "global") {
    load(file.path(output.dir, "mcmc.array.rda"))
    # generate/load source type PIs
    if (!(file.exists(file.path(output.dir, "sourcetype.t.rda")) &
            file.exists(file.path(output.dir, "PIs.Rtq.rda")) &
            file.exists(file.path(output.dir, "PIs.nosigmaeps.Rtq.rda")) &
            file.exists(file.path(output.dir, "PIsmean.Rt.rda")))) {
      sourcetypePI <- GenerateSourceTypePIs(mcmc.meta = mcmc.meta,
                                            mcmc.array = mcmc.array, 
                                            recallperiod.R = recallperiod.R, 
                                            sigma.eps = TRUE,
                                            percentiles = percentiles)
      sourcetypePI.nosigmaeps <- GenerateSourceTypePIs(mcmc.meta = mcmc.meta,
                                                       mcmc.array = mcmc.array, 
                                                       recallperiod.R = recallperiod.R, 
                                                       sigma.eps = FALSE,
                                                       percentiles = percentiles)
      sourcetype.t <- sourcetypePI$sourcetype.t
      PIs.Rtq <- sourcetypePI$PIs.Rtq
      PIs.nosigmaeps.Rtq <- sourcetypePI.nosigmaeps$PIs.Rtq
      PIsmean.Rt <- sourcetypePI$PIsmean.Rt
      save(sourcetype.t, file = file.path(output.dir, "sourcetype.t.rda"))
      save(PIs.Rtq, file = file.path(output.dir, "PIs.Rtq.rda"))
      save(PIs.nosigmaeps.Rtq, file = file.path(output.dir, "PIs.nosigmaeps.Rtq.rda"))
      save(PIsmean.Rt, file = file.path(output.dir, "PIsmean.Rt.rda"))
    } else {
      load(file = file.path(output.dir, "sourcetype.t.rda"))
      load(file = file.path(output.dir, "PIs.Rtq.rda"))
      load(file = file.path(output.dir, "PIs.nosigmaeps.Rtq.rda"))
      load(file = file.path(output.dir, "PIsmean.Rt.rda"))
      cat(paste0("Previously generated source type PIs.\n"))
    }
    #----------------------------------------------------------------------
    # plot source type PIs
    if (!plot.mean) PIsmean.Rt <- NULL
    if (plot.type == "pdf") {
      name.pdf <- file.path(fig.dir, paste(runname, "Source type PIs.pdf"))
      name.eps <- NULL
    } else if (plot.type == "eps") {
      name.pdf <- NULL
      name.eps <- file.path(fig.dir, paste(runname, "Source type PIs.eps"))
    }
    sourcetype.t.ordered <- c("DHS Direct", "DHS Direct no SE", 
                              "Other DHS Direct", "Other DHS Direct no SE",
                              "MICS Indirect", "MICS Indirect no SE",
                              "Census Indirect", "Others Direct",
                              "Others Indirect", "Others Household Deaths", "Others Life Table")
    sourcetype.t.ordered <- sourcetype.t.ordered[is.element(sourcetype.t.ordered, sourcetype.t)]
    order.sourcetype <- match(sourcetype.t.ordered, sourcetype.t)
    PlotSourceTypePIs(recallperiod.R = recallperiod.R,
                      PIs.Rtq = PIs.Rtq, 
                      PIs2.Rtq = PIs.nosigmaeps.Rtq,
                      PIsmean.Rt = PIsmean.Rt,
                      select.sourcetype = order.sourcetype,
                      labels.sourcetype = sourcetype.t,
                      igme = igme,
                      xlab = mcmc.meta$settings$indicator.type,
                      one.plot = TRUE,
                      name.pdf = name.pdf, 
                      name.eps = name.eps)
    cat(paste0("Plot of sourcetype PIs saved to ", fig.dir, ".\n"))
    table <- data.frame(rep(sourcetype.t[order.sourcetype], length(recallperiod.R)), 
                        rep(recallperiod.R, each = length(sourcetype.t)), 
                        c(t(round(PIs.Rtq[, order.sourcetype, 1]*igme, 1))),
                        c(t(round(PIs.Rtq[, order.sourcetype, 2]*igme, 1))),
                        c(t(round(PIs.Rtq[, order.sourcetype, 3]*igme, 1))),
                        rep(sourcetype.t[order.sourcetype], length(recallperiod.R)), 
                        rep(recallperiod.R, each = length(sourcetype.t)), 
                        c(t(round(PIs.nosigmaeps.Rtq[, order.sourcetype, 1]*igme, 1))),
                        c(t(round(PIs.nosigmaeps.Rtq[, order.sourcetype, 2]*igme, 1))),
                        c(t(round(PIs.nosigmaeps.Rtq[, order.sourcetype, 3]*igme, 1))))
    colnames(table) <- rep(c("Source type", "Retrospective period", paste0(percentiles*100, "%")), 2)
    write.csv(table, file = file.path(output.dir, "Sourcetype PIs.csv"), row.names = FALSE)
    cat(paste0("Values of sourcetype PIs saved to ", output.dir, ".\n"))
  }
  ##value<< \code{NULL}.
  return(invisible())
}
# mcmc.array.prior <- mcmc.array
# for(t in 1:nsourcetypes) {
#   for (c in 1:dim(mcmc.array)[2]) {
#     for (s in 1:dim(mcmc.array)[1]) {
#       mcmc.array.prior[s, c, paste0("mu.beta.tr[", t, ",1]")] <- mcmc.meta$jags.data$mu0.mubeta.tr[t, 1]
#       mcmc.array.prior[s, c, paste0("mu.beta.tr[", t, ",2]")] <- mcmc.meta$jags.data$mu0.mubeta.tr[t, 2]
#       mcmc.array.prior[s, c, paste0("sigma.beta.tr[", t, ",1]")] <- mcmc.meta$jags.data$sigma.beta.tr[t, 1]
#       mcmc.array.prior[s, c, paste0("sigma.beta.tr[", t, ",2]")] <- mcmc.meta$jags.data$sigma.beta.tr[t, 2]
#     }
#   }
# }
# PIs.prior.Rtq <- GenerateSourceTypePIs(mcmc.array.prior, recallperiod.R, sigma.eps = FALSE, typenames = typenames)
# PlotSourceTypePIs(PIs.Rtq = PIs.prior.Rtq, 
#                  select.sourcetype = c(2,4,1,3,5,6,7), #
#                  recallperiod.R = recallperiod.R,
#                  labels.sourcetype = typenames,
#                  name.eps = file.path(output.dir, "PIprior.pdf"))
#----------------------------------------------------------------------
GenerateSourceTypePIs <- function( # Generate prediction intervals for observations from each source type
  mcmc.meta, ##<< \code{mcmc.meta} from \code{\link{RunMCMC}}.
  mcmc.array, ##<< \code{mcmc.array} from \code{\link{ReadMCMCOutput}}.
  recallperiod.R = c(5, 10, 20), ##<< Vector of retrospective periods to simulate for.
  sigma.eps = TRUE, ##<< Logical value indicating whether or not to include sigma_eps.
  percentiles = c(0.05, 0.5, 0.95) ##<< Percentiles.
) {
  list2env(mcmc.meta$settings, envir = environment())
  list2env(mcmc.meta$jags.data, envir = environment())
  sourcetypes <- c(mcmc.meta$data$typenames, 
                   mcmc.meta$data$typenoSEnames[mcmc.meta$data$typenoSEnames != "Others"])
  nsourcetypes <- length(sourcetypes)
  nrecall <- length(recallperiod.R)
  nser.type <- NULL
  PIs.Rtq <- array(NA, c(nrecall, nsourcetypes, 3))
  PIsmean.Rt <- array(NA, c(nrecall, nsourcetypes))
  recallperiodc.R <- recallperiod.R - mcmc.meta$data$recall.mid
  v1.sc <- v2.sc <- matrix(NA, dim(mcmc.array)[1], dim(mcmc.array)[2])
  deltas.Rcs <- array(NA, c(nrecall, dim(mcmc.array)[2], dim(mcmc.array)[1]))
  for (type.select in 1:nsourcetypes) {
    sourcetype.select <- sourcetypes[type.select]
    isTypeNoSE <- grepl("no SE", sourcetype.select)
    if (!isTypeNoSE) {
      t <- which(mcmc.meta$data$typenames == sourcetype.select)
      tnoSE <- which(mcmc.meta$data$typenoSEnames == "Others")
    } else {
      t <- which(mcmc.meta$data$typenames == gsub(" no SE", "", sourcetype.select))
      tnoSE <- which(mcmc.meta$data$typenoSEnames == sourcetype.select)
    }
    # select the countries with that type of data series
    # j for country, ser for series
    jselect <- seq(1, C)[apply(type.cs == t & typenoSE.cs == tnoSE, 
                               1, sum, na.rm = T) > 0]
    j.i <- ser.i <- NULL # gives country-series combi
    for (j in jselect) {
      allseries <- seq(1, S.c[j])[type.cs[j, 1:S.c[j]] == t & 
                                    typenoSE.cs[j, 1:S.c[j]] == tnoSE]
      j.i <- c(j.i, rep(j, length(allseries)))
      ser.i <- c(ser.i, allseries)
    }
    nser <- length(ser.i)
    nser.type <- c(nser.type, nser)
    if (nser == 1) 
      cat(paste0("Warning: only one series available for source type ", 
                 sourcetype.select, "!\n"))
    for (c in 1:dim(mcmc.array)[2]) {
      for (s in 1:dim(mcmc.array)[1]) {
        mu.beta.r <- tau.beta.r <- rep(NA, 2)
        mu.beta.r[1] <- mcmc.array[s, c, paste0("mu.beta.tr[", t, ",1]")]
        mu.beta.r[2] <- mcmc.array[s, c, paste0("mu.beta.tr[", t, ",2]")]
        tau.beta.r[1] <- (mcmc.array[s, c, paste0("sigma.beta.tr[", t, ",1]")])^-2
        tau.beta.r[2] <- (mcmc.array[s, c, paste0("sigma.beta.tr[", t, ",2]")])^-2
        sigma.ynonvr <- ifelse(!isTypeNoSE, mcmc.array[s, c, paste0("sigma.ynonvr.t[", t, "]")],
                               mcmc.array[s, c, paste0("sigma.ynonvr.tnoSE[", tnoSE, "]")])
        # change JR, 20131126
        if (is.element(sourcetype.select, c("DHS Direct", "DHS Direct no SE")) & add.dhsdirect.bias) {
          biasatzerorecall <- rtnorm(1, mean = mcmc.array[s, c, "mu.biasatzerorecall"], 
                                     sd = mcmc.array[s, c, "sigma.biasatzerorecall"], upper = 0)
          recallnobias <- mcmc.array[s, c, "recallnobias"] # change JR, 20131204
        }
        # adjust for v != 1
        if (mcmc.meta$settings$use.country.variance.multipliers) {
          v1.sc[s,c] <- median(mcmc.array[s, c, paste0("v.cr[", 
                                                       mcmc.meta$jags.data$getc.nonvr.d[
                                                         !is.na(mcmc.meta$jags.data$getc.nonvr.d)], ",1]")])
          v2.sc[s,c] <- median(mcmc.array[s, c, paste0("v.cr[", 
                                                       mcmc.meta$jags.data$getc.nonvr.d[
                                                         !is.na(mcmc.meta$jags.data$getc.nonvr.d)], ",2]")])
          parnames1 <- paste0("beta.csr[", j.i, ",", ser.i, ",1]")
          parnames2 <- paste0("beta.csr[", j.i, ",", ser.i, ",2]")
          cor12 <- cor(
            mcmc.array[s, c, parnames1]/sqrt(mcmc.array[s, c, paste0("v.cr[", j.i, ",1]")]),
            mcmc.array[s, c, parnames2]/sqrt(mcmc.array[s, c, paste0("v.cr[", j.i, ",2]")]))
          var1 <- 1/tau.beta.r[1]*1/v1.sc[s, c]
          var2 <- 1/tau.beta.r[2]*1/v2.sc[s, c]
          cov12 <- var1*var2*cor12
        } else {
          var1 <- 1/tau.beta.r[1]
          var2 <- 1/tau.beta.r[2]
          cov12 <- 0
        }
        set.seed((c+100)*s) # change JR, 20140508
        bhat.r <- rmvnorm(1, mu.beta.r, cbind(c(var1, cov12), c(cov12, var2)))
        for (R in 1:nrecall) { # change JR, 20131126
          bias.dhsdirect <- ifelse(is.element(sourcetype.select, c("DHS Direct", "DHS Direct no SE")) &
                                     add.dhsdirect.bias, 
                                   GetDHSDirectBias(recall = recallperiod.R[R], 
                                                    biasatzerorecall = biasatzerorecall, 
                                                    recallnobias = recallnobias), 0)
          delta_bias <-  
            # change JR, 3 Jun
            ifelse(is.element(sourcetype.select, c("Others Household Deaths", "Others Life Table")), 
                   mu.beta.r[1], bhat.r[1] + bhat.r[2]*recallperiodc.R[R]) + bias.dhsdirect # change JR, 20131126
          if (sigma.eps) {
            if (grepl("DHS Direct|Other DHS Direct", sourcetype.select)) {
              deltas.Rcs[R, c, s] <- (delta_bias + rnorm(1, 0, sigma.ynonvr))
            } else {
              dft <- mcmc.array[s, c, "dft"]
              deltas.Rcs[R, c, s] <- (delta_bias + sigma.ynonvr*rt(1, dft))
            }
          } else {
            deltas.Rcs[R, c, s] <- delta_bias
          }
        }
      }
    } # end mcmc.array loop
    for (R in 1:nrecall) {
      PIs.Rtq[R, type.select, ] <- exp(quantile(c(deltas.Rcs[R, , ]), probs = percentiles))
      PIsmean.Rt[R, type.select] <- exp(mean(c(deltas.Rcs[R, , ])))
    }
    cat(paste("Generating prediction intervals for source type:", sourcetype.select, "\n"))
  } # end sourcetype lop
  cat("Overview of source types:\n")
  print(data.frame(Source.Type = sourcetypes, Number.Of.Series = nser.type))
  ##value<< List containing:
  return(list(sourcetype.t = sourcetypes, ##<< Vector of source types corresponding to the PI arrays.
              PIs.Rtq = PIs.Rtq, ##<< Quantiles of PIs for for retrospective period R and source type t. 
              PIsmean.Rt = PIsmean.Rt ##<< Mean of PIs for retrospective period R and source type t. 
  ))
}
#----------------------------------------------------------------------
# PlotSourceTypePIs <- function( # Plot prediction intervals for observations from each source type
#   recallperiod.R, ##<< Vector of retrospective periods. 
#   PIs.Rtq, ##<< Quantiles of PIs for for retrospective period R and source type t. 
#   ## Output from \code{\link{GenerateSourceTypePIs}}.
#   PIs2.Rtq = NULL, ##<< Optional: Quantiles of PIs for for retrospective period R and source type t. 
#   ## Output from \code{\link{GenerateSourceTypePIs}}.
#   PIsmean.Rt = NULL, ##<< Optional: Mean of PIs for retrospective period R and source type t. 
#   ## Output from \code{\link{GenerateSourceTypePIs}}.
#   select.sourcetype = NULL, ##<< Vector of source type indices from 1 to number of source types (can be a subset).
#   labels.sourcetype, ##<< Vector of source type labels.
#   igme = 100, ##<< Simulated true value.
#   xlab = "U5MR", ##<< Label for x-axis.
#   name.pdf = NULL, ##<< File name of .pdf file to save plot as.
#   name.eps = NULL ##<< File name of .eps file to save plot as.
# ) {
#   if (is.null(select.sourcetype)) {
#     select.sourcetype <- seq(1, length(PIs.Rtq[1, ,1]))
#   } else {
#     labels.sourcetype <- labels.sourcetype[select.sourcetype]
#   }
#   PIs.temp.Rtq <- igme*PIs.Rtq[, select.sourcetype,] 
#   if (!is.null(PIs2.Rtq)) {
#     PIs2.temp.Rtq <- igme*PIs2.Rtq[, select.sourcetype, ]    
#   } else {
#     PIs2.temp.Rtq <- array(NA, dim(PIs.temp.Rtq))
#   }
#   if (!is.null(PIsmean.Rt)) {
#     PIsmean.temp.Rt <- igme*PIsmean.Rt[, select.sourcetype]    
#   } else {
#     PIsmean.temp.Rt <- NULL
#   }
#   nrecall <- dim(PIs.temp.Rtq)[1]
#   nsourcetypes <- dim(PIs.temp.Rtq)[2]
#   if (!is.null(name.pdf)) {
#     pdf(file = name.pdf, width = 7, height = nrecall*5)
#   }
#   if (!is.null(name.eps)) {
#     postscript(file = name.eps, width = 3.27, height = 6,
#                bg = "white", onefile = FALSE, horizontal = FALSE, pointsize = 8)
#   }
#   par(mar = c(5,16.5,3,1.5), mfrow = c(nrecall, 1), 
#       cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
#   for (R in 1:nrecall) {
#     xmax <- min(500, 
#                 1.1*max(PIs.temp.Rtq[R,,],
#                         PIs2.temp.Rtq[R,,], na.rm = T))
#     xmin <- max(-Inf,
#                 (0.9+0.2*(min(PIs.temp.Rtq[R,,], 
#                               PIs2.temp.Rtq[R,,], na.rm = T) < 0))
#                 *min(PIs.temp.Rtq[R, , ], PIs2.temp.Rtq[R, , ], na.rm = T))
#     seqtoplot <- seq(1, nsourcetypes)
#     plot(-seqtoplot~ PIs.temp.Rtq[R, seqtoplot, 2], 
#          main = paste("Retrospective period:", recallperiod.R[R], 
#                       "years"),
#          xlim = c(xmin, xmax),  ylim = c(-1-nsourcetypes, 0),
#          ylab = "", yaxt = "n", xlab = xlab, type = "n")
#     for (type in seq(1,nsourcetypes,2)){
#       polygon(-0.5+c(0,0,500,500,0), -type + c(-0.5, 0.5, 0.5, -0.5, -0.5),
#               col = "lightgrey", border = NA)
#     }
#     abline(h = -seqtoplot)
#     axis(2, at = -seqtoplot, label = labels.sourcetype, las = 1, cex.axis = 1.5)
#     box()
#     segments(PIs.temp.Rtq[R, seqtoplot, 1], -seqtoplot,
#              PIs.temp.Rtq[R, seqtoplot, 3], -seqtoplot, 
#              lwd = 3, col = rgb(red=135, green=206, blue=235, max=255))
#     if (!is.null(PIs2.Rtq)) {
#       add <- 0 #-0.1
#       segments(PIs2.temp.Rtq[R, seqtoplot, 1], add-seqtoplot,
#                PIs2.temp.Rtq[R, seqtoplot, 3], add-seqtoplot, 
#                lwd = 3, col = 4)
#     }
#     abline(v = igme, lty = 1)
#     points(-seqtoplot ~ PIs.temp.Rtq[R,seqtoplot,2], pch = 3, col = 4, lwd = 3)
#     if (!is.null(PIsmean.temp.Rt)) {
#       points(-seqtoplot ~ PIsmean.temp.Rt[R, seqtoplot], pch = 19, col = "darkgrey", lwd = 2)
#     }
#   }
#   if (!is.null(name.pdf) | !is.null(name.eps)) 
#     dev.off()
#   ##value<< \code{NULL}.
#   return(invisible())
# }
#----------------------------------------------------------------------
PlotSourceTypePIs <- function( # Plot prediction intervals for observations from each source type
  recallperiod.R, ##<< Vector of retrospective periods. 
  PIs.Rtq, ##<< Quantiles of PIs for for retrospective period R and source type t. 
  ## Output from \code{\link{GenerateSourceTypePIs}}.
  PIs2.Rtq = NULL, ##<< Optional: Quantiles of PIs for for retrospective period R and source type t. 
  ## Output from \code{\link{GenerateSourceTypePIs}}.
  PIsmean.Rt = NULL, ##<< Optional: Mean of PIs for retrospective period R and source type t. 
  ## Output from \code{\link{GenerateSourceTypePIs}}.
  select.sourcetype = NULL, ##<< Vector of source type indices from 1 to number of source types (can be a subset).
  labels.sourcetype, ##<< Vector of source type labels.
  igme = 100, ##<< Simulated true value.
  xlab = "U5MR", ##<< Label for x-axis.
  one.plot = FALSE, ##<< Plot PIs for all retrospective period on one plot. # change JR, 19 Aug 2013
  name.pdf = NULL, ##<< File name of .pdf file to save plot as.
  name.eps = NULL ##<< File name of .eps file to save plot as.
) {
  if (is.null(select.sourcetype)) {
    select.sourcetype <- seq(1, length(PIs.Rtq[1, ,1]))
  } else {
    labels.sourcetype <- labels.sourcetype[select.sourcetype]
  }
  PIs.temp.Rtq <- igme*PIs.Rtq[, select.sourcetype,] 
  if (!is.null(PIs2.Rtq)) {
    PIs2.temp.Rtq <- igme*PIs2.Rtq[, select.sourcetype, ]    
  } else {
    PIs2.temp.Rtq <- array(NA, dim(PIs.temp.Rtq))
  }
  if (!is.null(PIsmean.Rt)) {
    PIsmean.temp.Rt <- igme*PIsmean.Rt[, select.sourcetype]    
  } else {
    PIsmean.temp.Rt <- NULL
  }
  nrecall <- dim(PIs.temp.Rtq)[1]
  nsourcetypes <- dim(PIs.temp.Rtq)[2]
  if (one.plot) {
    cols1 <- c("#FFCCE3", "#87CEEB", "#64EB87")[1:nrecall] # light red, blue, green
    cols2 <- c("#FF0000", "#0000FF", "#00FF00")[1:nrecall] # red, blue, green
  }
  if (!is.null(name.pdf)) {
    pdf(file = name.pdf, width = ifelse(one.plot, 12, 7), height = ifelse(one.plot, 7, nrecall*5)) # change JR, 19 Aug 2013
  }
  if (!is.null(name.eps)) {
    postscript(file = name.eps, width = 3.27, height = 6,
               bg = "white", onefile = FALSE, horizontal = FALSE, pointsize = 8)
  }
  if (one.plot) { # change JR, 19 Aug 2013
    par(mar = c(5,16.5,2,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
    layout(matrix(1:2, 1, 2, byrow = T), widths = c(8.7, 3.3))
  } else {
    par(mar = c(5,16.5,1,1.5), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
    layout(matrix(1:nrecall, nrecall, 1, byrow = T))
  }
  seqtoplot <- seq(1, nsourcetypes)
  if (one.plot) {
    xmax <- min(500, 
                1.1*max(c(PIs.temp.Rtq, PIs2.temp.Rtq), na.rm = T))
    xmin <- max(-Inf,
                (0.9+0.2*(min(c(PIs.temp.Rtq, PIs2.temp.Rtq), na.rm = T) < 0))
                *min(c(PIs.temp.Rtq, PIs2.temp.Rtq), na.rm = T))
    plot(-seqtoplot ~ PIs.temp.Rtq[nrecall, seqtoplot, 2], 
         main = "", #paste("Retrospective period:", recallperiod.R[nrecall], "years"), #####
         xlim = c(xmin, xmax),  ylim = c(-1-nsourcetypes, 0),
         ylab = "", yaxt = "n", xlab = xlab, type = "n")
    for (type in seq(1,nsourcetypes,2)) {
      polygon(-0.5+c(0,0,500,500,0), -type + c(-0.5, 0.5, 0.5, -0.5, -0.5),
              col = "lightgrey", border = NA)
    }
    abline(h = -seqtoplot)
    axis(2, at = -seqtoplot, label = labels.sourcetype, las = 1, cex.axis = 1.5)
    box()
    for (R in nrecall:1) {
      segments(PIs.temp.Rtq[R, seqtoplot, 1], -seqtoplot+((R-1)*0.2),
               PIs.temp.Rtq[R, seqtoplot, 3], -seqtoplot+((R-1)*0.2), 
               lwd = 5, col = cols1[R]) # change JR, 19 Aug 2013
      if (!is.null(PIs2.Rtq)) {
        add <- 0 #-0.2
        segments(PIs2.temp.Rtq[R, seqtoplot, 1], add-seqtoplot+((R-1)*0.2),
                 PIs2.temp.Rtq[R, seqtoplot, 3], add-seqtoplot+((R-1)*0.2), 
                 lwd = 5, col = cols2[R]) # change JR, 19 Aug 2013
      }
      abline(v = igme)
      points((-seqtoplot+((R-1)*0.2)) ~ PIs.temp.Rtq[R,seqtoplot,2], pch = 3, col = cols2[R], lwd = 5)
      if (!is.null(PIsmean.temp.Rt)) {
        points((-seqtoplot+((R-1)*0.2)) ~ PIsmean.temp.Rt[R, seqtoplot], pch = 19, col = cols1[R], lwd = 4)
      }
    }
    par(mar = c(0,0,0,0))
    EmptyPlot()
    legend("center", legend = c(paste0("Retrospective period of ", recallperiod.R, " years")),
           lwd = 5, col = cols2, cex = 1)
  } else {
    for (R in 1:nrecall) {
      xmax <- min(500, 
                  1.1*max(PIs.temp.Rtq[R,,],
                          PIs2.temp.Rtq[R,,], na.rm = T))
      xmin <- max(-Inf,
                  (0.9+0.2*(min(PIs.temp.Rtq[R,,], 
                                PIs2.temp.Rtq[R,,], na.rm = T) < 0))
                  *min(PIs.temp.Rtq[R, , ], PIs2.temp.Rtq[R, , ], na.rm = T))
      plot(-seqtoplot~ PIs.temp.Rtq[R, seqtoplot, 2], 
           main = paste("Retrospective period:", recallperiod.R[R], 
                        "years"),
           xlim = c(xmin, xmax),  ylim = c(-1-nsourcetypes, 0),
           ylab = "", yaxt = "n", xlab = xlab, type = "n")
      for (type in seq(1,nsourcetypes,2)){
        polygon(-0.5+c(0,0,500,500,0), -type + c(-0.5, 0.5, 0.5, -0.5, -0.5),
                col = "lightgrey", border = NA)
      }
      abline(h = -seqtoplot)
      axis(2, at = -seqtoplot, label = labels.sourcetype, las = 1, cex.axis = 1.5)
      box()
      segments(PIs.temp.Rtq[R, seqtoplot, 1], -seqtoplot,
               PIs.temp.Rtq[R, seqtoplot, 3], -seqtoplot, 
               lwd = 3, col = rgb(red=135, green=206, blue=235, max=255))
      if (!is.null(PIs2.Rtq)) {
        add <- 0 #-0.1
        segments(PIs2.temp.Rtq[R, seqtoplot, 1], add-seqtoplot,
                 PIs2.temp.Rtq[R, seqtoplot, 3], add-seqtoplot, 
                 lwd = 3, col = 4)
      }
      abline(v = igme, lty = 1)
      points(-seqtoplot ~ PIs.temp.Rtq[R,seqtoplot,2], pch = 3, col = 4, lwd = 3)
      if (!is.null(PIsmean.temp.Rt)) {
        points(-seqtoplot ~ PIsmean.temp.Rt[R, seqtoplot], pch = 19, col = "darkgrey", lwd = 2)
      }
    }
  } # end one.plot
  if (!is.null(name.pdf) | !is.null(name.eps)) 
    dev.off()
  ##value<< \code{NULL}.
  return(invisible())
}
