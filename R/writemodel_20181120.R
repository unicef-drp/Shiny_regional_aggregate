#----------------------------------------------------------------------
# writemodel.R
# Leontine Alkema & Jin Rou New, 2012-2014
#----------------------------------------------------------------------
WriteModel <- function( # Write JAGS model out as a .txt file
  mcmc.meta, ##<< \code{mcmc.meta}
  output.dir ##<< Output directory for model .txt file.
) {
  list2env(mcmc.meta$settings, envir = environment())
  Q <- mcmc.meta$jags.data$Q
  cat("
  #----------------------------------------------------------------------
  # B3 model for child mortality estimation
  # Leontine Alkema and Jin Rou New, 2013
  #----------------------------------------------------------------------
  model{",
  ifelse(indicator.type == "U5MR", "
    for (c in 1:C) {
      for (i in 1:n.c[c]) {
        yhat.ci[c,i] <- (m.ci[c,i] + logbias.ci[c,i]) # m refers to the spline f
      }
    }", "
    for (c in 1:C) {
      for (i in 1:n.c[c]) {
        yhat.ci[c,i] <- (l.ci[c,i] + logbias.ci[c,i]) # y refers to log(q1)
       # l.ci[c,i] <- log(exp(m.ci[c,i])*q5hat.ci[c,i]/(exp(m.ci[c,i])+1)) # m refers to the spline f for logit(q1/q5)
        l.ci[c,i] <- log(q5hat.ci[c,i]/(exp(-m.ci[c,i])+1))
      }
    }"), "
  #----------------------------------------------------------------------
  # Observations
  #----------------------------------------------------------------------
  ", file = file.path(output.dir, "model.txt"), fill = T, append = T)
  if (!is.validation) {
    #--------------------------------------------------
    # Full run
    #--------------------------------------------------
    # no y.ci and ypredict.ci here for incomplete VR data
    if (mcmc.meta$data$Cnormdist > 0) {
      cat("
    for (d in 1:Cnormdist) {
      for (j in 1:nnormdist.c[getc.normdist.d[d]]) {
        y.ci[getc.normdist.d[d], geti.normdist.cj[getc.normdist.d[d], j]] ~
            dnorm(yhat.ci[getc.normdist.d[d], geti.normdist.cj[getc.normdist.d[d], j]], 
                  tau.y.ci[getc.normdist.d[d], geti.normdist.cj[getc.normdist.d[d], j]])
        ypredict.ci[getc.normdist.d[d], geti.normdist.cj[getc.normdist.d[d], j]] ~ 
            dnorm(yhat.ci[getc.normdist.d[d], geti.normdist.cj[getc.normdist.d[d], j]], 
                  tau.y.ci[getc.normdist.d[d], geti.normdist.cj[getc.normdist.d[d], j]])
      }
    }
      ", file = file.path(output.dir, "model.txt"), fill = T, append = T)
    }
    if (mcmc.meta$data$Ctdist > 0) {
      cat("
    for (d in 1:Ctdist) {
      for (j in 1:ntdist.c[getc.tdist.d[d]]) {
        y.ci[getc.tdist.d[d], geti.tdist.cj[getc.tdist.d[d], j]] ~
            dt(yhat.ci[getc.tdist.d[d], geti.tdist.cj[getc.tdist.d[d], j]], 
              tau.y.ci[getc.tdist.d[d], geti.tdist.cj[getc.tdist.d[d], j]], 
              dft.ci[getc.tdist.d[d], geti.tdist.cj[getc.tdist.d[d], j]])
        ypredict.ci[getc.tdist.d[d], geti.tdist.cj[getc.tdist.d[d], j]] ~ 
            dt(yhat.ci[getc.tdist.d[d], geti.tdist.cj[getc.tdist.d[d], j]], 
              tau.y.ci[getc.tdist.d[d], geti.tdist.cj[getc.tdist.d[d], j]],
              dft.ci[getc.tdist.d[d], geti.tdist.cj[getc.tdist.d[d], j]])
      }
    }
      ", file = file.path(output.dir, "model.txt"), fill = T, append = T)
    }
  } else {
    #--------------------------------------------------
    # Validation run
    #--------------------------------------------------
    # no y.ci here for incomplete VR data
    if (mcmc.meta$data$Ctrainnormdist > 0) {
      cat(" 
    for (d in 1:Ctrainnormdist) { 
      for (j in 1:ntrainnormdist.c[getc.trainnormdist.d[d]]) {
        y.ci[getc.trainnormdist.d[d], geti.trainnormdist.cj[getc.trainnormdist.d[d], j]] ~ 
            dnorm(yhat.ci[getc.trainnormdist.d[d], geti.trainnormdist.cj[getc.trainnormdist.d[d], j]], 
                  tau.y.ci[getc.trainnormdist.d[d], geti.trainnormdist.cj[getc.trainnormdist.d[d], j]])
        ypredict.ci[getc.trainnormdist.d[d], geti.trainnormdist.cj[getc.trainnormdist.d[d], j]] ~ 
            dnorm(yhat.ci[getc.trainnormdist.d[d], geti.trainnormdist.cj[getc.trainnormdist.d[d], j]], 
                  tau.y.ci[getc.trainnormdist.d[d], geti.trainnormdist.cj[getc.trainnormdist.d[d], j]])
      }
    }
    ", file = file.path(output.dir, "model.txt"), fill = T, append = T)
    }
    if (mcmc.meta$data$Ctraintdist > 0) {
      cat(" 
    for (d in 1:Ctraintdist) {
      for (j in 1:ntraintdist.c[getc.traintdist.d[d]]) {
        y.ci[getc.traintdist.d[d], geti.traintdist.cj[getc.traintdist.d[d],j]] ~ 
            dt(yhat.ci[getc.traintdist.d[d], geti.traintdist.cj[getc.traintdist.d[d], j]], 
               tau.y.ci[getc.traintdist.d[d], geti.traintdist.cj[getc.traintdist.d[d], j]], 
               dft.ci[getc.traintdist.d[d], geti.traintdist.cj[getc.traintdist.d[d], j]])
        ypredict.ci[getc.traintdist.d[d], geti.traintdist.cj[getc.traintdist.d[d],j]] ~ 
            dt(yhat.ci[getc.traintdist.d[d], geti.traintdist.cj[getc.traintdist.d[d], j]], 
               tau.y.ci[getc.traintdist.d[d], geti.traintdist.cj[getc.traintdist.d[d], j]],
               dft.ci[getc.traintdist.d[d], geti.traintdist.cj[getc.traintdist.d[d], j]])
      }
    }
    ", file = file.path(output.dir, "model.txt"), fill = T, append = T)
    }
    # no ypredict.ci here for incomplete VR data
    if (mcmc.meta$data$Ctestnormdist > 0) {
      cat("
    for (d in 1:Ctestnormdist) {
      for (j in 1:ntestnormdist.c[getc.testnormdist.d[d]]) {
        ypredict.ci[getc.testnormdist.d[d], geti.testnormdist.cj[getc.testnormdist.d[d], j]] ~ 
            dnorm(yhat.ci[getc.testnormdist.d[d], geti.testnormdist.cj[getc.testnormdist.d[d], j]], 
                  tau.y.ci[getc.testnormdist.d[d], geti.testnormdist.cj[getc.testnormdist.d[d], j]])
      }
    }
    ", file = file.path(output.dir, "model.txt"), fill = T, append = T)
    }
    if (mcmc.meta$data$Ctesttdist > 0) {
      cat(" 
    for (d in 1:Ctesttdist) {
      for (j in 1:ntesttdist.c[getc.testtdist.d[d]]) {
        ypredict.ci[getc.testtdist.d[d], geti.testtdist.cj[getc.testtdist.d[d],j]] ~ 
            dt(yhat.ci[getc.testtdist.d[d], geti.testtdist.cj[getc.testtdist.d[d], j]], 
               tau.y.ci[getc.testtdist.d[d], geti.testtdist.cj[getc.testtdist.d[d], j]],
               dft.ci[getc.testtdist.d[d], geti.testtdist.cj[getc.testtdist.d[d], j]])
      }
    }
    ", file = file.path(output.dir, "model.txt"), fill = T, append = T)
    }
  }  
  if (!is.null(mcmc.meta$data$Cnonvr)) {
  if (mcmc.meta$data$Cnonvr > 0) {
    cat("
  #--------------------------------------------------
  # Non-VR
  #--------------------------------------------------
  for (d in 1:Cnonvr) {
    for (j in 1:nnonvr.c[getc.nonvr.d[d]]) {
      logbias.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]] <- (
        # multiple obs per series
        (beta.csr[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]], 1] +
          beta.csr[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]], 2]*
          recall.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]])*
          (1-is.singleobs.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]]) + 
        # single obs per series
        mu.beta.tr[type.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]], 1]*
          is.singleobs.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]]",
        ifelse(!is.null(mcmc.meta$jags.data$is.nonvrwithbias.cs), "+ 
        logbias.nonvr.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]]*
          is.nonvrwithbias.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]]", ""),
        ifelse(add.dhsdirect.bias, "+ 
        (biasatzerorecall.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]] - 
          biasatzerorecall.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]]/
          recallnobias*
          ifelse(recallnotcentred.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]] < recallnobias, 
            recallnotcentred.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]], recallnobias)
        )*is.dhsdirect.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]]", ""), "
      ) # end logbias.ci
      recallnotcentred.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]] <- (
        surveyyear.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]] - 
        year.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]])
      recall.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]] <- (
        recallnotcentred.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]] - recall.mid)
      tau.y.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]] <- 
        pow(var.y.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]], -1)
      var.y.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]] <- (
        var.ynonvr.t[type.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]]]*
          (1-is.typenoSE.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]])
        + var.ynonvr.tnoSE[typenoSE.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], j]]]*
          is.typenoSE.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]]", 
        ifelse(!all(is.na(mcmc.meta$jags.data$se.ynonvr.ci)), " 
        + pow(se.ynonvr.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]], 2)", ""), 
        ifelse(!is.null(mcmc.meta$jags.data$has.serieslevelbiasatprior.cs), "
        + pow(has.serieslevelbiasatprior.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]]*
          sigma.beta.tr[type.cs[getc.nonvr.d[d], series.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]]], 1], 2)", ""),    
      ") # end var.y.ci
      dft.ci[getc.nonvr.d[d], geti.nonvr.cj[getc.nonvr.d[d], j]] <- dft", "
    } # end j loop
    for (s in 1:S.c[getc.nonvr.d[d]]) {",
      ifelse(!is.null(mcmc.meta$jags.data$has.serieslevelbiasatprior.cs), "
      beta.csr[getc.nonvr.d[d], s, 1] <-
        beta.cs1[getc.nonvr.d[d], s]*(1-has.serieslevelbiasatprior.cs[getc.nonvr.d[d], s]) +
        mu.beta.tr[type.cs[getc.nonvr.d[d], s], 1]*has.serieslevelbiasatprior.cs[getc.nonvr.d[d], s]
      beta.cs1[getc.nonvr.d[d], s] ~ 
        dnorm(mu.beta.tr[type.cs[getc.nonvr.d[d], s], 1],
              tau.beta.ctr[getc.nonvr.d[d], type.cs[getc.nonvr.d[d], s], 1])", "
      beta.csr[getc.nonvr.d[d], s, 1] ~ 
        dnorm(mu.beta.tr[type.cs[getc.nonvr.d[d], s], 1],
              tau.beta.ctr[getc.nonvr.d[d], type.cs[getc.nonvr.d[d], s], 1])
      "), "
      beta.csr[getc.nonvr.d[d], s, 2] ~ 
        dnorm(mu.beta.tr[type.cs[getc.nonvr.d[d], s], 2],
              tau.beta.ctr[getc.nonvr.d[d], type.cs[getc.nonvr.d[d], s], 2])",
      ifelse(!is.null(mcmc.meta$jags.data$is.nonvrwithbias.cs), "
      logbias.nonvr.cs[getc.nonvr.d[d], s] <- log(bias.nonvr.cs[getc.nonvr.d[d], s])
      bias.nonvr.cs[getc.nonvr.d[d], s] ~ dunif(0, 1)", ""),
      ifelse(add.dhsdirect.bias, "
      biasatzerorecall.cs[getc.nonvr.d[d], s] ~ dnorm(mu.biasatzerorecall, tau.biasatzerorecall)T(,0)", ""), "
    } # end s loop
    ",  
    ifelse(use.country.variance.multipliers, "
    # subtract mean for country variance multipliers to speed up
    for (r in 1:2) {
      vraw.cr[getc.nonvr.d[d], r] ~ dlnorm(mu.v.r[r], tau.v.r[r])
      v.cr[getc.nonvr.d[d], r] <- exp(log(vraw.cr[getc.nonvr.d[d], r]) - mu.v.r[r])
      for (t in 1:ntypes) {
        tau.beta.ctr[getc.nonvr.d[d], t, r] <- tau.beta.tr[t, r]/v.cr[getc.nonvr.d[d], r]
      }
    }", "
    for (r in 1:2) {
      for (t in 1:ntypes) {    
        tau.beta.ctr[getc.nonvr.d[d], t, r] <- tau.beta.tr[t, r]
      }
    }
    "), "
  } # end country loop",
  ifelse(use.country.variance.multipliers, "
  for (r in 1:2) {
    mu.v.r[r] ~ dnorm(0,0.01)
    sigma.v.r[r] ~ dunif(0, 5)
    tau.v.r[r] <- pow(sigma.v.r[r], -2)
  }", ""), "
  for (t in 1:ntypes) {
    var.ynonvr.t[t] <- pow(sigma.ynonvr.t[t], 2)
    for (r in 1:2) {
      tau.beta.tr[t, r] <- pow(sigma.beta.tr[t, r], -2)
    }",
    ifelse(run.type == "global", "
    sigma.ynonvr.t[t] ~ dunif(0, 0.5) 
    mu.beta.tr[t, 1:2] ~ dmnorm(mu0.mubeta.tr[t, 1:2], Tau0.mubeta.trr[t, 1:2, 1:2])
    for (r in 1:2) {
      sigma.beta.tr[t, r] ~ dunif(0, 5)
    }", "
    sigma.ynonvr.t[t] <- sigma0.ynonvr.t[t]
    mu.beta.tr[t, 1:2] <- mu0.beta.tr[t, 1:2]
    for (r in 1:2) {
      sigma.beta.tr[t, r] <- sigma0.beta.tr[t, r]
    }"), "
  }
  for (tnoSE in 1:ntypesnoSE) {
    var.ynonvr.tnoSE[tnoSE] <- pow(sigma.ynonvr.tnoSE[tnoSE], 2)",
    ifelse(run.type == "global", "
    sigma.ynonvr.tnoSE[tnoSE] ~ dunif(0, 0.5)", "
    sigma.ynonvr.tnoSE[tnoSE] <- sigma0.ynonvr.tnoSE[tnoSE]"), "
  }",
  ifelse(add.dhsdirect.bias, paste("
  tau.biasatzerorecall <- pow(sigma.biasatzerorecall, -2)",
  ifelse(run.type == "global", "
  mu.biasatzerorecall ~ dunif(log(0.85), 10) # change JR, 17 May: from dunif(log(0.85), 0)
  sigma.biasatzerorecall ~ dunif(0, 1) # change JR, 17 May: from dunif(0, 0.5)
  recallnobias ~ dunif(0, 7)", "
  mu.biasatzerorecall <- mu0.biasatzerorecall
  sigma.biasatzerorecall <- sigma0.biasatzerorecall
  recallnobias <- recallnobias0"
  )), ""), 
  ifelse(run.type == "global", "
  dft ~ dunif(2, 30)", "
  dft <- dft0"), 
    file = file.path(output.dir, "model.txt"), fill = T, append = T)
  }
  } # end non-VR observations
  
  if (!is.null(mcmc.meta$data$Cvr)) {
  if (mcmc.meta$data$Cvr > 0) {
    cat("
  #--------------------------------------------------
  # VR
  #--------------------------------------------------
  # all VR
  for (d in 1:Cvr) {
    for (j in 1:nvr.c[getc.vr.d[d]]) {
      logbias.ci[getc.vr.d[d], geti.vr.cj[getc.vr.d[d], j]] <- ", ifelse(is.null(mcmc.meta$jags.data$is.vrwithbias.ci), "0", "
        logbias.vr.c[getc.vr.d[d]]*is.vrwithbias.ci[getc.vr.d[d], geti.vr.cj[getc.vr.d[d], j]]"), 
      ifelse(input.vr.se, "
      tau.y.ci[getc.vr.d[d], geti.vr.cj[getc.vr.d[d], j]] <- tau.yvr.ci[getc.vr.d[d], geti.vr.cj[getc.vr.d[d], j]]
      tau.yvr.ci[getc.vr.d[d], geti.vr.cj[getc.vr.d[d], j]] <- pow(se.yvr.ci[getc.vr.d[d], geti.vr.cj[getc.vr.d[d], j]], -2)", "
      tau.y.ci[getc.vr.d[d], geti.vr.cj[getc.vr.d[d], j]] <- tau.yvr.c[getc.vr.d[d]]"), "
    } # end j loop",
    ifelse(input.vr.se, "", "
    tau.yvr.c[getc.vr.d[d]] <- pow(sigma.yvr.c[getc.vr.d[d]], -2)
    sigma.yvr.c[getc.vr.d[d]] ~ dunif(0, 0.5)"), 
    ifelse(is.null(mcmc.meta$jags.data$is.vrwithbias.ci), "", "
    logbias.vr.c[getc.vr.d[d]] <- log(bias.vr.c[getc.vr.d[d]])
    bias.vr.c[getc.vr.d[d]] ~ dunif(0, 1)"), "    
  } # end country loop",
  ifelse(input.vr.se, "", "
  tau.yvr.c[C+1] <- 0 # dummy
  sigma.yvr.c[C+1] <- 0 # dummy"),
  ifelse(is.null(mcmc.meta$jags.data$is.vrwithbias.ci), "", "
  logbias.vr.c[C+1] <- 0 # dummy
  bias.vr.c[C+1] <- 0 # dummy")    
    , file = file.path(output.dir, "model.txt"), fill = T, append = T)
  } # end VR observations
  if (!is.null(mcmc.meta$data$Cvrincomplete)) {
    if (mcmc.meta$data$Cvrincomplete > 0) {
      cat("
  # incomplete VR
  for (d in 1:Cvrincomplete) {
    for (j in 1:nvrincomplete.c[getc.vrincomplete.d[d]]) {
      # stochastic variance var.y.ci accounted for
      yvrincomplete.ci[getc.vrincomplete.d[d], geti.vrincomplete.cj[getc.vrincomplete.d[d], j]] ~ 
        dnorm(y.ci[getc.vrincomplete.d[d], geti.vrincomplete.cj[getc.vrincomplete.d[d], j]], 
              tau.y.ci[getc.vrincomplete.d[d], geti.vrincomplete.cj[getc.vrincomplete.d[d], j]])
      # trajectories are constrained to be above yvrincomplete.ci
      is.splinesabovevr.ci[getc.vrincomplete.d[d], geti.vrincomplete.cj[getc.vrincomplete.d[d], j]] ~ 
        dinterval(yhat.ci[getc.vrincomplete.d[d], geti.vrincomplete.cj[getc.vrincomplete.d[d], j]], 
                  yvrincomplete.ci[getc.vrincomplete.d[d], geti.vrincomplete.cj[getc.vrincomplete.d[d], j]] -
                  logcompincompleteVR.ci2[getc.vrincomplete.d[d], geti.vrincomplete.cj[getc.vrincomplete.d[d], j], 1])
    }
  } # end country loop"
  , file = file.path(output.dir, "model.txt"), fill = T, append = T)
    }
  } # end incomplete VR observations
  if (!is.null(mcmc.meta$data$Cvrincompminmax)) {
    if (mcmc.meta$data$Cvrincompminmax > 0) {
      cat("
  # incomplete VR with min/max level of completeness
  for (d in 1:Cvrincompminmax) {
    for (j in 1:nvrincompminmax.c[getc.vrincompminmax.d[d]]) {
      # stochastic variance var.y.ci accounted for
      yvrincomplete.ci[getc.vrincompminmax.d[d], geti.vrincompminmax.cj[getc.vrincompminmax.d[d], j]] ~ 
        dnorm(y.ci[getc.vrincompminmax.d[d], geti.vrincompminmax.cj[getc.vrincompminmax.d[d], j]], 
              tau.y.ci[getc.vrincompminmax.d[d], geti.vrincompminmax.cj[getc.vrincompminmax.d[d], j]])
      # trajectories are constrained to be between yvrincomplete.ci/maxcomplevel to yvrincomplete.ci/mincomplevel
      is.splinesabovevr.ci[getc.vrincompminmax.d[d], geti.vrincompminmax.cj[getc.vrincompminmax.d[d], j]] ~ 
        dinterval(yhat.ci[getc.vrincompminmax.d[d], geti.vrincompminmax.cj[getc.vrincompminmax.d[d], j]], 
                  yvrincomplete.ci[getc.vrincompminmax.d[d], geti.vrincompminmax.cj[getc.vrincompminmax.d[d], j]] -
                  logcompincompleteVR.ci2[getc.vrincompminmax.d[d], geti.vrincompminmax.cj[getc.vrincompminmax.d[d], j], 1:2])
    }
  } # end country loop"
  , file = file.path(output.dir, "model.txt"), fill = T, append = T)
    }
  } # end incomplete VR min/max observations
  if (is.validation) {
    if (!is.null(mcmc.meta$data$Ctestvrincompleteany)) {
      if (mcmc.meta$data$Ctestvrincompleteany > 0) {
        cat("
  # incomplete VR in validation test set (not used)
  for (d in 1:Ctestvrincompleteany) {
    for (j in 1:ntestvrincompleteany.c[getc.vrincompleteany.d[d]]) {
      yvrincomplete.ci[getc.testvrincompleteany.d[d], geti.testvrincompleteany.cj[getc.testvrincompleteany.d[d], j]] <- 0 # dummy
    }
  } # end country loop"
      , file = file.path(output.dir, "model.txt"), fill = T, append = T)
      }
    }
  } # end incomplete VR observations in validation test set
  if (!is.null(mcmc.meta$data$Cvrincomplete) & !is.null(mcmc.meta$data$Cvrincompminmax)) {
    if (mcmc.meta$data$Cvrincomplete+mcmc.meta$data$Cvrincompminmax > 0) {
      cat("
  yvrincomplete.ci[C+1, nmax] <- 0 # dummy"
          , file = file.path(output.dir, "model.txt"), fill = T, append = T)
    }
  } else if (!is.null(mcmc.meta$data$Cvrincomplete)) {
    if (mcmc.meta$data$Cvrincomplete > 0) {
      cat("
  yvrincomplete.ci[C+1, nmax] <- 0 # dummy"
    , file = file.path(output.dir, "model.txt"), fill = T, append = T)
    } 
  } else if (!is.null(mcmc.meta$data$Cvrincompminmax)) {
    if (mcmc.meta$data$Cvrincompminmax > 0) {
      cat("
  yvrincomplete.ci[C+1, nmax] <- 0 # dummy"
          , file = file.path(output.dir, "model.txt"), fill = T, append = T)
    } 
  }
  } # end incomplete VR dummy
  cat("
  #----------------------------------------------------------------------
  # Splines
  #----------------------------------------------------------------------
  #--------------------------------------------------
  # Prior distributions of spline parameters
  #--------------------------------------------------
  for (c in 1:C) {", "
    for (q in 1:q.c[c]) {
      u.cq[c,q] ~ dnorm(0,", ifelse(run.type == "global" & !use.constant.sigma.u, 
                                    # "tau.u.c[c])", "tau.u.c[c]/r.q[q])"), "
                                    "tau.u.c[c])", "(tau.u.c[c]/r.q[q])*0.5)"), "
    }
    for (q in qplus1.c[c]:Q) {
      u.cq[c,q] <- 0 # not used anyway
    }
    ",
  ifelse(indicator.type == "U5MR", "
    b.cm[c,1] <- log(level.c[c])
    level.c[c] ~ dunif(1, 1000) # change JR, 20 Feb: from level.c ~ dunif(1, 500)
    b.cm[c,2] <- -ARR.c[c]*I
    ARR.c[c] ~ dunif(-0.2, 0.25) # change JR, 20 Feb: from ARR.c ~ dunif(-0.1, 0.15)",
  ifelse(indicator.type == "IMR", "  
    b.cm[c,1] <- logit(p.c[c])
    p.c[c] ~ dunif(0.01, 1)
    b.cm[c,2] ~ dnorm(0, 0.1)", "")), 
  ifelse(!use.constant.sigma.u & 
           is.null(periods.smooth.list) & 
           is.null(periods.unsmooth.list), "", "
    tau.u.c[c] <- tau.u"), "
  } # end country loop
  b.cm[C+1, 2] <- 0 # dummy",
  ifelse(indicator.type == "U5MR", "
  level.c[C+1] <- 0 # dummy
  ARR.c[C+1] <- 0 # dummy",
  ifelse(indicator.type == "IMR", "
  p.c[C+1] <- 0", "")), "
  #--------------------------------------------------
  # Smoothing parameters
  #--------------------------------------------------",
  ifelse(!use.constant.sigma.u & 
           is.null(periods.smooth.list) & 
           is.null(periods.unsmooth.list), paste("
  # !use.constant.sigma.u
  for (c in 1:C) {
    tau.u.c[c] <- exp(-2*a.c[c])
    a.c[c] <- (1-useWorldsigmau.c[c])*a.temp.c[c] + useWorldsigmau.c[c]*mu.a
    a.temp.c[c] ~ dnorm(mu.a, tau.a)T(-8, ) # change JR, 20140528
  }
  tau.u.c[C+1] <- 0 # dummy
  a.temp.c[C+1] <- 0 # dummy
  a.c[C+1] <- 0 # dummy
  tau.a <- pow(sigma.a, -2)", 
  ifelse(run.type == "global", "
  mu.a ~ dnorm(-3, 0.1)
  sigma.a ~ dunif(0, 5)", "
  mu.a <- mu0.a
  sigma.a <- sigma0.a")), 
  #--------------------------------------------------
  paste("
  # use.constant.sigma.u | do smoothing | do unsmoothing
  tau.u <- exp(-2*a)", 
  ifelse(run.type == "global", "
  a ~ dnorm(-2, 0.02)", "
  a <- a0")))
  , file = file.path(output.dir, "model.txt"), fill = T, append = T)
  #----------------------------------------------------------------------
  # Spline for observations
  #----------------------------------------------------------------------
  # note: start obs loop, closed at end of this part
  cat("
  for (c in 1:C) {
    for (i in 1:n.c[c]) {
    m.ci[c,i] <- (mge.ci[c,i] + mre.ci[c,i] + 
                  mre2.ci[c,i]+ mre3.ci[c,i]+ mre4.ci[c,i] )
    mge.ci[c,i] <- (b.cm[c,1]*BG.cim[c,i,1] + b.cm[c,2]*BG.cim[c,i,2])
    mre.ci[c,i] <- (u.cq[c,1]*Z.ciq[c,i,1]",
                    paste0("+ u.cq[c,", seq(2, min(20, Q)), "]", "*Z.ciq[c,i,", seq(2, min(20, Q)), "]"),
                    ")"
    , file = file.path(output.dir, "model.txt"), fill = T, append = T)

  if (Q > 20) {
  cat("
    mre2.ci[c,i] <- (0",
                    paste0("+ u.cq[c,", seq(21, min(40, Q)), "]", "*Z.ciq[c,i,", seq(21, min(40, Q)), "]"),
                    ")"
    , file = file.path(output.dir, "model.txt"), fill = T, append = T)
  } else {
  cat("
    mre2.ci[c,i] <- 0"
      , file = file.path(output.dir, "model.txt"), fill = T, append = T)
  }
  
  if (Q > 40) {
    cat("
        mre3.ci[c,i] <- (0",
        paste0("+ u.cq[c,", seq(41, min(60, Q)), "]", "*Z.ciq[c,i,", seq(41, min(60, Q)), "]"),
        ")"
        , file = file.path(output.dir, "model.txt"), fill = T, append = T)
  } else {
    cat("
        mre3.ci[c,i] <- 0"
        , file = file.path(output.dir, "model.txt"), fill = T, append = T)
  }  
  
  if (Q > 60) {
    cat("
        mre4.ci[c,i] <- (0",
        paste0("+ u.cq[c,", seq(61, min(80, Q)), "]", "*Z.ciq[c,i,", seq(61, min(80, Q)), "]"),
        ")" 
        , file = file.path(output.dir, "model.txt"), fill = T, append = T)
  } else {
    cat("
        mre4.ci[c,i] <- 0"
        , file = file.path(output.dir, "model.txt"), fill = T, append = T)
  }
  cat("
    } # end i loop
  } # end country loop"
  , file = file.path(output.dir, "model.txt"), fill = T, append = T)
  
#   #----------------------------------------------------------------------
#   # Spline for grid of years
#   #----------------------------------------------------------------------
#   if (get.jags.predictions) {
#     cat("
#     for (c in 1:C) {
#       for (t in 1:P) {    
#     m.ct[c,t] <- (mge.ct[c,t] + mre.ct[c,t] 
#                   + mre2.ct[c,t]+ mre3.ct[c,t]+ mre4.ct[c,t] )
#     mge.ct[c,t] <- (b.cm[c,1]*BG.ctm[c,t,1] + b.cm[c,2]*BG.ctm[c,t,2])
#     mre.ct[c,t] <- (u.cq[c,1]*Z.ctq[c,t,1]",
#         paste0("+u.cq[c,", seq(2,min(20, mcmc.meta$data$Qpredict)), "]", "*Z.ctq[c,t,",seq(2, min(20, mcmc.meta$data$Qpredict)), "]"),
#         ")", file = file.path(output.dir, "model.txt"), fill = T, append = T)
#     if (mcmc.meta$data$Qpredict > 20) { 
#       cat("
#       mre2.ct[c,t] <- (0",
#           paste0("+u.cq[c,", seq(21, min(40, mcmc.meta$data$Qpredict)), "]", "*Z.ctq[c,t,",seq(21, min(40, mcmc.meta$data$Qpredict)), "]"),
#           ")"
#           , file = file.path(output.dir, "model.txt"), fill = T, append = T)
#     } else {
#       cat("mre2.ct[c,t] <- 0", file = file.path(output.dir, "model.txt"), fill = TRUE, append = T)
#     }
#     if (mcmc.meta$data$Qpredict > 40) { 
#       cat("mre3.ct[c,t] <- (0",
#           paste0("+u.cq[c,", seq(41, min(60, mcmc.meta$data$Qpredict)), "]", "*Z.ctq[c,t,",seq(41, min(60, mcmc.meta$data$Qpredict)), "]"),
#           ")"
#           , file = file.path(output.dir, "model.txt"), fill = T, append = T)
#     } else {
#       cat("mre3.ct[c,t] <- 0", file = file.path(output.dir, "model.txt"), fill = TRUE, append = T)
#     }
#     if (mcmc.meta$data$Qpredict > 60) { 
#       cat("mre4.ct[c,t] <- (0",
#           paste0("+u.cq[c,", seq(61, mcmc.meta$data$Qpredict), "]", "*Z.ctq[c,t,",seq(61, mcmc.meta$data$Qpredict), "]"),
#           ")"
#           , file = file.path(output.dir, "model.txt"), fill = T, append = T)
#     } else {
#       cat("mre4.ct[c,t] <- 0", file = file.path(output.dir, "model.txt"), fill = T, append = T)
#     }
#     # close country-obs loop
#     cat("}} # end country-obs loop", file = file.path(output.dir, "model.txt"), fill = T, append = T)
#   }
  #----------------------------------------------------------------------
  # close model file
  cat("} # end model ", file = file.path(output.dir, "model.txt"), fill = T, append = T)
  #----------------------------------------------------------------------
  ##value<< NULL; prints out .txt file
  return(invisible())
}
