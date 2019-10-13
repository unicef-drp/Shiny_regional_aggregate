#----------------------------------------------------------------------
# getjagsparameters.R
#----------------------------------------------------------------------
GetJAGSParameters <- function(
  jags.data,
  data.val,
  settings
) {
  list2env(settings, envir = environment())
  #----------------------------------------------------------------------
  # Spline parameters
  jags.par.splines.all <- jags.par.nonvr.all <- jags.par.vr.all <- jags.par.vrincomplete.all <- 
    jags.par.ypredict.all <- jags.par.predict.all <- NULL
  jags.par.splines <- jags.par.nonvr <- jags.par.vr <- NULL
  if (!use.constant.sigma.u & 
        is.null(periods.smooth.list) & 
        is.null(periods.unsmooth.list)) {
    if (run.type == "global")
      jags.par.splines.all <- c(jags.par.splines.all, "mu.a", "sigma.a")
    jags.par.splines.all <- c(jags.par.splines.all, paste0("a.c[", 1:jags.data$C, "]"))
  } else {
    jags.par.splines.all <- c(jags.par.splines.all, "a")
  }
  u.cqnames <- NULL
  for (c in 1:jags.data$C)
    u.cqnames <- c(u.cqnames, paste0("u.cq[", c, ",", 1:jags.data$q.c[c], "]"))
  jags.par.splines.all <- c(jags.par.splines.all, 
                            paste0("b.cm[", 1:jags.data$C, ",", 1, "]"),
                            paste0("b.cm[", 1:jags.data$C, ",", 2, "]"),
                            u.cqnames)
  if (indicator.type == "IMR")
    jags.par.splines.all <- c(jags.par.splines.all, paste0("p.c[", 1:jags.data$C, "]"))
  # drop a.c's for which a.c[c] = mu.a
  jags.par.splines <- jags.par.splines.all[!(jags.par.splines.all %in%
                                               paste0("a.c[", (1:jags.data$C)[
                                                 jags.data$useWorldsigmau.c[1:jags.data$C] == 1], "]"))]
  #----------------------------------------------------------------------
  # Non-VR parameters
  if (!is.null(jags.data$Cnonvr)) {
    if (jags.data$Cnonvr > 0) {
      if (run.type == "global")
        jags.par.nonvr.all <- c(jags.par.nonvr.all, 
                                paste0("sigma.ynonvr.t[", seq(1, jags.data$ntypes), "]"),
                                paste0("sigma.ynonvr.tnoSE[", seq(1, jags.data$ntypesnoSE), "]"),
                                paste0("mu.beta.tr[", seq(1, jags.data$ntypes), ",1]"),
                                paste0("mu.beta.tr[", seq(1, jags.data$ntypes), ",2]"),
                                paste0("sigma.beta.tr[", seq(1, jags.data$ntypes), ",1]"),
                                paste0("sigma.beta.tr[", seq(1, jags.data$ntypes), ",2]"),
                                "dft") 
      for (c in jags.data$getc.nonvr.d[!is.na(jags.data$getc.nonvr.d)]) {
        jags.par.nonvr.all <- c(jags.par.nonvr.all, 
                                paste0("beta.csr[", c, ",", 1:jags.data$S.c[c], ",1]"),
                                paste0("beta.csr[", c, ",", 1:jags.data$S.c[c], ",2]"))
      }
      if (use.country.variance.multipliers)
        jags.par.nonvr.all <- c(jags.par.nonvr.all, 
                                paste0("mu.v.r[", 1:2, "]"),
                                paste0("sigma.v.r[", 1:2, "]"),
                                paste0("v.cr[", jags.data$getc.nonvr.d[
                                  !is.na(jags.data$getc.nonvr.d)], ",1]"), 
                                paste0("v.cr[", jags.data$getc.nonvr.d[
                                  !is.na(jags.data$getc.nonvr.d)], ",2]"))
      jags.par.nonvr <- jags.par.nonvr.all
      if (!is.null(jags.data$has.serieslevelbiasatprior.cs)) {
        for (c in jags.data$getc.nonvr.d[!is.na(jags.data$getc.nonvr.d)])
          jags.par.nonvr.all <- c(jags.par.nonvr.all, 
                                  paste0("beta.cs1[", c, ",", 1:jags.data$S.c[c], "]"))
        getc.serieslevelbias.d <- seq(1, jags.data$C)[
          rowSums(jags.data$has.serieslevelbiasatprior.cs[1:jags.data$C, , drop = FALSE], na.rm = T) > 0]
        for (c in getc.serieslevelbias.d)
          jags.par.nonvr <- c(jags.par.nonvr, 
                              paste0("beta.cs1[", c, ",", 
                                     seq(1, jags.data$S.c[c])[
                                       jags.data$has.serieslevelbiasatprior.cs[c, 1:jags.data$S[c]] == 1], "]"))
      } # end isSeriesLevelBiasAtPrior
      if (!is.null(jags.data$is.nonvrwithbias.cs)) {
        for (c in jags.data$getc.nonvr.d[!is.na(jags.data$getc.nonvr.d)])
          jags.par.nonvr.all <- c(jags.par.nonvr.all, 
                                  paste0("bias.nonvr.cs[", c, ",", 1:jags.data$S.c[c], "]"))
        getc.nonvrbias.d <- seq(1, jags.data$C)[
          rowSums(jags.data$is.nonvrwithbias.cs[1:jags.data$C, , drop = FALSE], na.rm = T) > 0]
        for (c in getc.nonvrbias.d)
          jags.par.nonvr <- c(jags.par.nonvr, 
                              paste0("bias.nonvr.cs[", c, ",", 
                                     seq(1, jags.data$S.c[c])[
                                       jags.data$is.nonvrwithbias.cs[c, 1:jags.data$S[c]] == 1], "]"))
      } # end isNonVRWithBias
      if (add.dhsdirect.bias) {
        if (run.type == "global") {
          jags.par.nonvr.all <- c(jags.par.nonvr.all, "mu.biasatzerorecall", 
                                  "sigma.biasatzerorecall", "recallnobias")
          jags.par.nonvr <- c(jags.par.nonvr, "mu.biasatzerorecall", 
                              "sigma.biasatzerorecall", "recallnobias")
        }
        for (c in jags.data$getc.nonvr.d[!is.na(jags.data$getc.nonvr.d)])
          jags.par.nonvr.all <- c(jags.par.nonvr.all, 
                                  paste0("biasatzerorecall.cs[", c, ",", 1:jags.data$S.c[c], "]"))
        getc.dhsdirectbias.d <- seq(1, jags.data$C)[
          rowSums(jags.data$is.dhsdirect.cs[1:jags.data$C, , drop = FALSE], na.rm = T) > 0]
        for (c in getc.dhsdirectbias.d)
          jags.par.nonvr <- c(jags.par.nonvr, 
                              paste0("biasatzerorecall.cs[", c, ",", 
                                     seq(1, jags.data$S.c[c])[
                                       jags.data$is.dhsdirect.cs[c, 1:jags.data$S[c]] == 1], "]"))
      } # end add.dhsdirect.bias
    }
  }
  #----------------------------------------------------------------------
  # for VR observations
  if (!is.null(jags.data$Cvr)) {
    if (jags.data$Cvr > 0) {
      if (!input.vr.se)
        jags.par.vr.all <- c(jags.par.vr.all, 
                             paste0("sigma.yvr.c[", 
                                    jags.data$getc.vr.d[!is.na(jags.data$getc.vr.d)], "]"))
      jags.par.vr <- jags.par.vr.all
      if (!is.null(jags.data$is.vrwithbias.ci)) {
        jags.par.vr.all <- c(jags.par.vr.all, 
                             paste0("bias.vr.c[", 
                                    jags.data$getc.vr.d[!is.na(jags.data$getc.vr.d)], "]"))
        jags.par.vr <- c(jags.par.vr, paste0("bias.vr.c[", 
                                             seq(1, jags.data$C)[
                                               rowSums(jags.data$is.vrwithbias.ci[1:jags.data$C, , drop = FALSE], 
                                                       na.rm = T) > 0], "]"))
      }
      if (!is.null(jags.data$is.splinesabovevr.ci)) {
        cat("Check...\n")
        if (!is.null(jags.data$Cvrincomplete)) {
          for (c in jags.data$getc.vrincomplete.d[!is.na(jags.data$getc.vrincomplete.d)]) {
            cat(paste0("Country with index ", c, " has ",
                       jags.data$nvrincomplete.c[c], 
                       " incomplete VR observations.\n"))
            jags.par.vrincomplete.all <- c(jags.par.vrincomplete.all, 
                                       paste0("yvrincomplete.ci[" , c, ",", 
                                              jags.data$geti.vrincomplete.cj[c, 
                                                                             1:jags.data$nvrincomplete.c[c]], "]"))
          }
        } # end incomplete VR
        if (!is.null(jags.data$Cvrincompminmax)) {
          for (c in jags.data$getc.vrincompminmax.d[!is.na(jags.data$getc.vrincompminmax.d)]) {
            cat(paste0("Country with index ", c, " has ",
                       jags.data$nvrincompminmax.c[c], 
                       " incomplete VR (with min/max completeness) observations.\n"))
            jags.par.vrincomplete.all <- c(jags.par.vrincomplete.all, 
                                       paste0("yvrincomplete.ci[" , c, ",", 
                                              jags.data$geti.vrincompminmax.cj[c, 
                                                                                   1:jags.data$nvrincompminmax.c[c]], "]"))     
          }
        } # end incomplete VR minmax
      } # end incomplete VR any
    }
  }
  #----------------------------------------------------------------------
  # For posterior predictive checks
  if (is.null(data.val)) { # change JR, 20140507
    for (c in 1:jags.data$C) {
      indices.normdist <- indices.tdist <- NA
      if (c %in% jags.data$getc.normdist.d)
        indices.normdist <- jags.data$geti.normdist.cj[c, ]
      if (c %in% jags.data$getc.tdist.d)
        indices.tdist <- jags.data$geti.tdist.cj[c, ]
      jags.par.ypredict.all <- c(jags.par.ypredict.all, 
                                 paste0("ypredict.ci[", c, ",", 
                                        sort(c(indices.normdist[!is.na(indices.normdist)], 
                                               indices.tdist[!is.na(indices.tdist)])), "]"))
    }
  } else {
    for (c in 1:jags.data$C) {
      indices.trainnormdist <- indices.traintdist <- 
        indices.testnormdist <- indices.testtdist <- NA
      if (c %in% jags.data$getc.trainnormdist.d) # note: skips over loop if jags.data$getc.normdist.d is NULL
        indices.trainnormdist <- jags.data$geti.trainnormdist.cj[c, ]
      if (c %in% jags.data$getc.traintdist.d)
        indices.traintdist <- jags.data$geti.traintdist.cj[c, ]
      if (c %in% jags.data$getc.testnormdist.d)
        indices.testnormdist <- jags.data$geti.testnormdist.cj[c, ]
      if (c %in% jags.data$getc.testtdist.d)
        indices.testtdist <- jags.data$geti.testtdist.cj[c, ]
      jags.par.ypredict.all <- c(jags.par.ypredict.all, 
                                 paste0("ypredict.ci[", c, ",", 
                                        sort(c(indices.trainnormdist[!is.na(indices.trainnormdist)], 
                                               indices.traintdist[!is.na(indices.traintdist)],
                                               indices.testnormdist[!is.na(indices.testnormdist)], 
                                               indices.testtdist[!is.na(indices.testtdist)],)), "]"))
    }
  }
  #----------------------------------------------------------------------
  # For prediction
  # if (get.jags.predictions) {
  #   for (c in 1:jags.data$C) {
  #     jags.par.predict.all <- paste0("m.ct[", c, 1:jags.data$P, "]")
  #   }
  # }
  ##value<< List of JAGS parameter names.
  jags.par.all <- list(jags.par.splines = jags.par.splines.all,
                       jags.par.nonvr = jags.par.nonvr.all,
                       jags.par.vr = jags.par.vr.all,
                       jags.par.vrincomplete = jags.par.vrincomplete.all,
                       jags.par.ypredict = jags.par.ypredict.all,
                       jags.par.predict = jags.par.predict.all)
  jags.par <- list(jags.par.splines = jags.par.splines,
                   jags.par.nonvr = jags.par.nonvr,
                   jags.par.vr = jags.par.vr,
                   jags.par.vrincomplete = jags.par.vrincomplete.all,
                   jags.par.ypredict = jags.par.ypredict.all,
                   jags.par.predict = jags.par.predict.all)
  return(list(jags.par.all = jags.par.all,
              jags.par = jags.par))
}
