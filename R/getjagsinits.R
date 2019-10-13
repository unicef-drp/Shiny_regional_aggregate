#----------------------------------------------------------------------
# getjagsinits.R
#----------------------------------------------------------------------
GetJAGSInits <- function( # Get JAGS inits
  data,
  jags.data,
  jags.data.for.inits,
  settings
) {
  list2env(settings, envir = environment())
  #----------------------------------------------------------------------
  # Splines parameters
  includeIncompleteVRAny.c <- rep(NA, jags.data$C)
  u.cq.inits <- matrix(NA, jags.data$C, jags.data$Q)
  a.temp.c.inits <- log(jags.data.for.inits$sigma0.u.c[1:jags.data$C])
  b.cm <- jags.data.for.inits$b0.cm # dim = C+1 x 2
  b0.c.inits <- b.cm[1:jags.data$C, 1] 
  b1.c.inits <- b.cm[1:jags.data$C, 2]
  for (c in 1:jags.data$C) {
    u.cq.inits[c, 1:(jags.data.for.inits$k.c[c]-2)] <- 
      jags.data.for.inits$u0.cq[c, 1:(jags.data.for.inits$k.c[c]-2)]
    # fixed inits used for countries with incomplete VR, because of the difficulty in 
    # getting inits that satisfy the constraints
    includeIncompleteVRAny.c[c] <- any(data$isincompletevr.Lc.j[[c]] == 1) # change JR, 20140530
    if (!includeIncompleteVRAny.c[c]) {
      u.cq.inits[c, 1:(jags.data.for.inits$k.c[c]-2)] <- u.cq.inits[c, 1:(jags.data.for.inits$k.c[c]-2)] +
        rnorm(jags.data.for.inits$k.c[c]-2, 0, jags.data.for.inits$sigma0.u.c[c]*3)
      a.temp.c.inits[c] <- a.temp.c.inits[c] + rnorm(1, 0, 3)
      b0.c.inits[c] <- b0.c.inits[c] + rnorm(1, 0, sqrt(jags.data.for.inits$Sigma0.b.Lc[[c]][1, 1])*3)
      b1.c.inits[c] <- b1.c.inits[c] + rnorm(1, 0, sqrt(jags.data.for.inits$Sigma0.b.Lc[[c]][2, 2])*3) # change JR, 20140601
    }
  } # end country loop
  jags.inits <- list(u.cq = u.cq.inits)
  if (!use.constant.sigma.u & is.null(periods.smooth.list) & 
        is.null(periods.unsmooth.list)) {
    if (run.type == "global")
      jags.inits <- c(jags.inits, list(
        mu.a = rnorm(1, -3, sqrt(10)), # rnorm(1, 0, 0.1),
        sigma.a = runif(1, 0, 5), # runif(1, 0.01, 1),
        a.temp.c = c(ifelse(a.temp.c.inits < -8, -8, a.temp.c.inits), NA)))
  }
  if (indicator.type == "U5MR") {
    jags.inits <- c(jags.inits, list(
      level.c = c(ifelse(exp(b0.c.inits) < 3, 3,
                         ifelse(exp(b0.c.inits) > 500, 500, exp(b0.c.inits))), NA),
      ARR.c = c(ifelse(-b1.c.inits/I < -0.1, -0.1,
                       ifelse(-b1.c.inits/I > 0.15, 0.15, 
                              -b1.c.inits/I)), NA)
    ))
  } else if (indicator.type == "IMR") {
    p.c.inits <- invlogit(b0.c.inits)
    jags.inits <- c(jags.inits, list(
      b.cm = cbind(c(rep(NA, length(b0.c.inits)), NA), c(b1.c.inits, NA)),
      p.c = c(ifelse(p.c.inits < 0.01, 0.01,
                     ifelse(p.c.inits > 0.99, 0.99, p.c.inits)), NA)
    ))
  }
  #----------------------------------------------------------------------
  # Non-VR parameters
  if (!is.null(jags.data$Cnonvr)) {
    if (jags.data$Cnonvr > 0) {
      Cmax <- max(which(jags.data$S.c != 0), na.rm = T)
      Smax <- max(jags.data$S.c, na.rm = T)
      beta.csr <- array(NA, c(Cmax, Smax, 2)) 
      beta.cs1 <- matrix(NA, jags.data$C, Smax)
      for (c in jags.data$getc.nonvr.d[!is.na(jags.data$getc.nonvr.d)]) {
        beta.csr[c, 1:jags.data$S.c[c], 1] <- rnorm(jags.data$S.c[c], 0, 0.15*3)
        beta.csr[c, 1:jags.data$S.c[c], 2] <- rnorm(jags.data$S.c[c], 0, 0.02*3)
        beta.cs1[c, 1:jags.data$S.c[c]] <- rnorm(jags.data$S.c[c], 0, 0.15*3)
      }
      if (is.null(jags.data$has.serieslevelbiasatprior.cs)) {
        jags.inits <- c(jags.inits, list(beta.csr = beta.csr))
      } else {
        beta.csr[, , 1] <- NA # NA because beta.cs1 used instead
        jags.inits <- c(jags.inits, list(beta.csr = beta.csr,
                                         beta.cs1 = beta.cs1))
      }
      if (run.type == "global") { # change JR, 20140521: added/updated inits
        mu.beta.tr.inits <- matrix(NA, jags.data$ntypes, 2)
        mu.beta.tr.inits[, 1] <- rnorm(jags.data$ntypes, 0, 0.15*3)
        mu.beta.tr.inits[, 2] <- rnorm(jags.data$ntypes, 0, 0.02*3)
        jags.inits <- c(jags.inits, list(
          sigma.ynonvr.t = c(runif(jags.data$ntypes, 0, 0.5)), #0.01, 0.2)),
          sigma.ynonvr.tnoSE = c(runif(jags.data$ntypesnoSE, 0, 0.5)), #0.01, 0.2)),
          mu.beta.tr = mu.beta.tr.inits,
          sigma.beta.tr = matrix(runif(2*jags.data$ntypes, 0, 5), jags.data$ntypes, 2),
          dft = runif(1, 2, 30) 
        ))
      }
      if (add.dhsdirect.bias) {
        if (run.type == "global") {
          jags.inits <- c(jags.inits, list(
            mu.biasatzerorecall = runif(1, log(0.85), 10),
            sigma.biasatzerorecall = runif(1, 0, 1),
            recallnobias = runif(1, 0, 7)
          ))
        }        
        biasatzerorecall.cs <- matrix(NA, Cmax, Smax)
        for (c in 1:Cmax) {
          select <- jags.data$is.dhsdirect.cs[c, ] == 1 & !is.na(jags.data$is.dhsdirect.cs[c, ])
          if (any(select))
            biasatzerorecall.cs[c, select] <- rnorm(sum(select), 0, 1) # change JR, 20140521: added randomness, was set at 0.05
        }
        jags.inits <- c(jags.inits, list(biasatzerorecall.cs = biasatzerorecall.cs))
      }
      if (!is.null(jags.data$is.nonvrwithbias.cs)) {
        bias.nonvr.cs <- matrix(NA, Cmax, Smax)
        bias.nonvr.cs[jags.data$is.nonvrwithbias.cs[1:Cmax, , drop = FALSE] == 1 & 
                        !is.na(jags.data$is.nonvrwithbias.cs[1:Cmax, , drop = FALSE])] <- 
          runif(sum(c(jags.data$is.nonvrwithbias.cs[1:Cmax, , drop = FALSE]) == 1, na.rm = T), 0, 1)
        jags.inits <- c(jags.inits, list(bias.nonvr.cs = bias.nonvr.cs))
      }
    }
  }
  #----------------------------------------------------------------------
  # VR parameters
  if (!is.null(jags.data$Cvr)) {
    if (jags.data$Cvr > 0) {
      if (!input.vr.se) {
        sigma.yvr.c <- rep(NA, jags.data$C+1)
        sigma.yvr.c[jags.data$getc.vr.d[!is.na(jags.data$getc.vr.d)]] <- 
          runif(jags.data$Cvr, 0, 0.5)
        jags.inits <- c(jags.inits, list(sigma.yvr.c = sigma.yvr.c))
      }
      if (!is.null(jags.data$is.vrwithbias.ci)) {
        bias.vr.c <- rep(NA, jags.data$C+1)
        bias.vr.c[jags.data$getc.vr.d[!is.na(jags.data$getc.vr.d)]] <- 
          runif(jags.data$Cvr, 0, 1)
        jags.inits <- c(jags.inits, list(bias.vr.c = bias.vr.c))
      }
      if (!is.null(jags.data$is.splinesabovevr.ci)) {
        nmax <- max(jags.data$n.c, na.rm = T)
        u.cq.temp <- u.cq.inits
        u.cq.temp[is.na(u.cq.inits)] <- 0 # else NA values will be obtained after matrix multiplication        
        yvrincomplete.ci <- matrix(NA, jags.data$C+1, nmax)
        if (!is.null(jags.data$Cvrincomplete)) {
          for (d in 1:jags.data$Cvrincomplete) {
            c <- jags.data$getc.vrincomplete.d[d]
            indices.vrincomplete <- jags.data$geti.vrincomplete.cj[c, ]
            indices.vrincomplete <- indices.vrincomplete[!is.na(indices.vrincomplete)]
            # set to arbitrary small value so that yhat.ci > yvrincomplete.ci upon initialisation
            yvrincomplete.ci[c, indices.vrincomplete] <- -8
          } 
        }
        if (!is.null(jags.data$Cvrincompminmax)) {
          for (d in 1:jags.data$Cvrincompminmax) {
            getc.vrincompminmax <- jags.data$getc.vrincompminmax.d[d]
            indices.vrincompminmax <- jags.data$geti.vrincompminmax.cj[getc.vrincompminmax, ]
            indices.vrincompminmax <- indices.vrincompminmax[!is.na(indices.vrincompminmax)]
            # set inits of yvrincomplete just below spline fit
            yvrincomplete.ci[getc.vrincompminmax, indices.vrincompminmax] <- 0.99*
              (b.cm[getc.vrincompminmax, ] %*% 
                 t(jags.data$BG.cim[getc.vrincompminmax, , ]) +
                 u.cq.temp[getc.vrincompminmax, ] %*% 
                 t(jags.data$Z.ciq[getc.vrincompminmax, , ]))[indices.vrincompminmax]
          }
        }
        jags.inits <- c(jags.inits, list(yvrincomplete.ci = yvrincomplete.ci))
      } # end incomplete VR
    }
  }
  ##value<< List of JAGS inits.
  return(jags.inits)
}
