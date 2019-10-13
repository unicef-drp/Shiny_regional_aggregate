#----------------------------------------------------------------------
# getjagsdatasubfunctions.R
#----------------------------------------------------------------------
GetJAGSDataForSplines <- function( # Get JAGS data list for splines
  data,
  data.val,
  indicator.type,
  runname.U5MR,
  I,
  run.type,
  year.lastestimate,
  periods.unsmooth.list = NULL,
  periods.smooth.list = NULL,
  periods.constant.list = NULL,
  hiv.file, 
  adj.file,
  runname.igme, ##<< Runname of UN IGME run of the previous year, 
  ## with \code{output/runname.igme} containing U5MR estimates stored as a \code{res.U5MR.rda}.
  mean.b0 = ifelse(indicator.type == "U5MR", 4.02, 2.19), ##<< Mean of b0 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
  mean.b1 = ifelse(indicator.type == "U5MR", -0.0889, 0.0516), ##<< Mean of b1 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
  mean.u = ifelse(indicator.type == "U5MR", -0.000446, 0.0271), ##<< Mean of u estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
  mean.var.b0 = ifelse(indicator.type == "U5MR", 0.00129, 0.490), ##<< Mean variance of b0 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
  mean.var.b1 = ifelse(indicator.type == "U5MR", 0.000110, 0.0342), ##<< Mean variance of b1 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
  mean.sigma.u = ifelse(indicator.type == "U5MR", 0.0424, 0.206) ##<< Mean sd of u estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
) {
  # minimum start year is 1990.5, but 1970.5 for Russia so that regional estimates for that region begins in 1970.5 # change JR, 9 Jun
  year.t <- seq(floor(min(ifelse(is.element("RUS", data$iso.c), 1971, 1986), 
                          data$minyear.c, na.rm = T))-0.5, year.lastestimate) # last year needs to be a future projection!
  # to make sure first and last observation year is within year.t for all countries
  # use max length of observation period to find max number of splines
  maxobsperiod <- max(data$maxyear.c - min(data$minyear.c, year.t, na.rm = T), na.rm = T) 
  # find max number of splines
  # add 2 at start and 2+1 at end, so +5 in total
  # +1 so that Q > qplus1.c (for write_model.R)
  K <- ceiling(maxobsperiod/I)+5+1
  M <- 2  # number of b's (difference degree, also called d)
  Q <- K - M
  # make dimensions equal to C+1 to avoid problems when running it for one country
  year.ci <- y.ci <- q5hat.ci <- y.inits.ci <- matrix(NA, data$C+1, data$nmax)
  BG.cim <- array(NA, c(data$C+1, data$nmax, M))
  Z.ciq <- array(0, c(data$C+1, data$nmax, Q))
  q.c <- k.c <- sigma0.u.c <- rep(NA, data$C+1)
  u0.cq <- matrix(NA, data$C+1, Q)
  b0.cm <- matrix(NA, data$C+1, 2)
  Sigma0.b.Lc <- T0.b.Lc <- replicate(data$C+1, matrix(NA, 2, 2), simplify = FALSE)
  u.cq <- matrix(NA, data$C+1, Q)
  b.cm <- matrix(NA, data$C+1, 2)
  IMR_larger_than_U5MR <- vector(length = data$C) # indicating if empirical IMR is larger than estimated U5MR
  for (c in 1:data$C) {
    year.i <- c(unlist(data$year.Lcs.j[[c]]), unlist(data$yearvr.Lc.j[[c]]))
    year.min <- floor(min(ifelse(data$iso.c[c] == "RUS", 1971, 1986), 
                          data$minyear.c[c], na.rm = T))-0.5 
    year.ci[c, 1:data$n.c[c]] <- year.i
    u.i <- c(unlist(data$u.Lcs.j[[c]]), unlist(data$uvr.Lc.j[[c]]))
    includeIncompleteVRAny <- any(data$isincompletevr.Lc.j[[c]] == 1)
    if (includeIncompleteVRAny) {
      # for incomplete VR data, get inits data based on max of previous year's estimates and 1.1*obs
      u.igme.i <- Getq5Estimates(iso = data$iso.c[c], years = year.i, runname = runname.igme) 
      is.vrincompleteany.i <- c(rep(FALSE, data$nnonvr.c[c]), data$isincompletevr.Lc.j[[c]] == 1) # note: (data$isincompletevr.Lc.j[[c]] == 1) is NULL if all are non-VR data
      u.inits.i <- ifelse(is.vrincompleteany.i, mapply(max, u.igme.i, u.i*1.1), u.igme.i)
    }
    # get crisis-free data for crisisadjfordata.c countries
    if (data$crisisadjfordata.c[c]) {
      u.i <- GetCrisisSubtractedSeries(u.i = u.i, year.i = year.i, iso = data$iso.c[c],
                                       adj.file = adj.file)
      if (includeIncompleteVRAny)
        u.inits.i <- GetCrisisSubtractedSeries(u.i = u.inits.i, year.i = year.i, iso = data$iso.c[c],
                                               adj.file = adj.file)
    }
    if (any(u.i <= 0))
      stop(paste0("Zero or negative observation (u.i) found for ", data$name.c[c], 
                  " (, ", data$iso.c[c], ")"))
    # get HIV-free data for HIV countries
    if (data$hiv.c[c]) {
      y.i <- log(GetHIVSubtractedSeries(u.i = u.i, year.i = year.i, iso = data$iso.c[c],
                                        hiv.file = hiv.file))
      if (includeIncompleteVRAny)
        y.inits.i <- log(GetHIVSubtractedSeries(u.i = u.inits.i, year.i = year.i, iso = data$iso.c[c],
                                                hiv.file = hiv.file))
    } else {
      y.i <- log(u.i)
      if (includeIncompleteVRAny)
        y.inits.i <- log(u.inits.i)
    }
    y.ci[c, 1:data$n.c[c]] <- y.i
    if (includeIncompleteVRAny)
      y.inits.ci[c, 1:data$n.c[c]] <- y.inits.i
    if (indicator.type == "IMR") {
      q5hat.i <- Getq5Estimates(iso = data$iso.c[c], years = year.i, runname = runname.U5MR)
      # get HIV-free q5 estimates for HIV countries
      if (data$hiv.c[c])
        q5hat.i <- GetHIVSubtractedSeries(u.i = q5hat.i, year.i = year.i, iso = data$iso.c[c], 
                                          hiv.file = hiv.file)
      q5hat.ci[c, 1:data$n.c[c]] <- q5hat.i 
      l.i <- logit(ifelse(exp(y.i) < q5hat.i, exp(y.i)/q5hat.i, 0.9999999999)) # logit cannot take value of >= 1!
      
      IMR_larger_than_U5MR [c] = ifelse(any(exp(y.i) > q5hat.i),TRUE,FALSE) # add by YS
    } else {
      IMR_larger_than_U5MR [c] = FALSE}
    res <- GetSplines(years.t = c(year.min, year.i), I = I,
                      years.combine = periods.constant.list) 
    ktemp <- length(res$alphayears.k)
    B.ik <- res$B.tk[-1, ] # change JR, 17 Jun: remove the first (extra) column corresponding to year.min
    ## note: year.min was added to years.t vector in GetSplines() so that start year of splines would be at least
    ## 1970.5 for Russia or 1990.5 otherwise.
    D2 <- diff(diag(ktemp), diff = 2)
    G <- cbind(rep(1, ktemp), seq(1, ktemp)-ktemp/2) # centre second column of G
    Dcomb <- t(D2)%*%solve(D2%*%t(D2))
    Z.iq <- B.ik%*%Dcomb
    q.c[c] <- dim(Z.iq)[2]
    Z.ciq[c, 1:data$n.c[c], 1:q.c[c]] <- Z.iq
    BG.im <- B.ik%*%G  
    BG.cim[c, 1:data$n.c[c],] <- BG.im
    
    # fit mixed model to get u and b
    one <- rep(1, nrow(Z.iq))
    if (length(y.i) > 2) {
      if (indicator.type == "U5MR") {
        if (!includeIncompleteVRAny) {
          mod <- lme(fixed = y.i ~ -1 + BG.im, random = list(one = pdIdent(~ Z.iq - 1)),control = lmeControl(opt = "optim")) #20180214 Kai Zhong Added control since did not converge and caused error
        } else {
          mod <- lme(fixed = y.inits.i ~ -1 + BG.im, random = list(one = pdIdent(~ Z.iq - 1)))
        }
      } else if (indicator.type == "IMR") {
        mod <- lme(fixed = l.i ~ -1 + BG.im, random = list(one = pdIdent(~ Z.iq - 1)))
      }
      u.q <- summary(mod)$coefficients$random$one
      b.m <- summary(mod)$coefficients$fixed  
      u.cq[c, 1:length(u.q)] <- u.q
      b.cm[c, ] <- b.m
    } else {
      # change JR, 3 Apr 2013: error with lme if less than 3 observations available,
      # so set as mean of values from all other countries
      u.q <- rep(mean.u, q.c[c]) # -0.001080 
      b.m <- c(mean.b0, mean.b1) # c(3.939, -0.09051)
    }
    # for checking:
    # recover the alphas
    # if(indicator.type == "U5MR") {
    #   yhat <- fitted(mod)
    #   lines(yhat ~ x.i, col = 2)
    # } else {
    #   lhat <- fitted(mod)
    #   lines(lhat ~ x.i, col = 2)
    # }
    # 
    # W <- diag(ktemp) - (t(D2)%*%solve(D2%*%t(D2))%*%D2)
    # s <- svd(W, nu = 2, nv = 2)
    # L <- s$u
    # QR decomposition, find A with L*A = G
    # A <- qr.solve(L, G)
    # alpha.ktemp <- qr.solve(a = rbind(solve(A)%*%t(L), D2), b = c(b.m, u.q))
    # if(indicator.type == "U5MR") {
    #   yhat2 <- B.ik%*%alpha.ktemp
    # } else {
    #   lhat2 <- B.ik%*%alpha.ktemp
    # }
    # summary(yhat2 - yhat)
    # check u's and b's
    # b.m
    # hist(c(u.q))
    # sd(c(u.q))
    # summary(mod)
    
    # save for inits
    k.c[c] <- ktemp
    u0.cq[c, 1:(ktemp-2)] <- u.q
    b0.cm[c, ] <- b.m
    # change LA: remove inflation here, inflate a bit less in inits
    if (length(y.i) > 2) { # because error with lme if if less than 3 observations available
      Sigma0.b.Lc[[c]] <- summary(mod)$varFix
      T0.b.Lc[[c]] <- solve(Sigma0.b.Lc[[c]])
      sigma0.u.c[c] <- as.double(nlme::VarCorr(mod)[1, 2]) # 
    }
    
    if (run.type == "country") { # get r.q's, multipliers of sigma.u's # 
      r.q <- rep(1, q.c[c]) # default value of r.q without tweaks is 1
      # get q's corresponding to the u's
      if (!is.null(periods.smooth.list)) {
        q.select.L <- GetSplines(years.t = c(year.min, year.i), I = I,
                                 years.combine = periods.constant.list,
                                 years.smoothing.L = periods.smooth.list)$q.select
        for (period in 1:length(q.select.L)) {
          if (length(q.select.L[[period]]) < 2)
            cat(paste0("Warning: Smoothing period", period, " is too short; at least 2 u's are required.\n"))
          r.q[q.select.L[[period]]] <- 1/sum((1:length(q.select.L[[period]]))^2)
        }
      }
      if (!is.null(periods.unsmooth.list)) {
        q.select.L <- GetSplines(years.t = c(year.min, year.i), I = I,
                                 years.combine = periods.constant.list,
                                 years.unsmoothing.L = periods.unsmooth.list)$q.select # 
        for (period in 1:length(q.select.L)) { # 
          if (length(q.select.L[[period]]) < 3)
            cat(paste0("Warning: Unsmoothing period", period, " is too short; at least 3 u's are required.\n"))
        }
        q.select <- unique(unlist(q.select.L))
        r.q[q.select] <- 1000
      }
    } # end run.type country
  } # end country loop
  
  nnormdist.c <- ntdist.c <- rep(NA, data$C+1)
  geti.normdist.cj <- geti.tdist.cj <- matrix(NA, data$C+1, data$nmax)
  for (c in 1:data$C) {
    is.dhsdirectany.i <- is.element(c(unlist(data$sourceid.Lcs.j[[c]]), 
                                      rep("VR", data$nvr.c[c])),
                                    c(data$sourceid.Lc.s[[c]][data$isDHSdirectany.Lc.s[[c]] == 1], "VR"))
    if (data$nvr.c[c] == 0) {
      is.incompletevrany.i <-  rep(FALSE, data$nnonvr.c[c]) # change JR, 20140512
    } else {
      is.incompletevrany.i <-  c(rep(FALSE, data$nnonvr.c[c]), data$isincompletevr.Lc.j[[c]] == 1)
    }
    is.normdist.i <- is.dhsdirectany.i & !is.incompletevrany.i # incomplete VR any dropped
    is.tdist.i <- !is.dhsdirectany.i
    nnormdist.c[c] <- sum(is.normdist.i)
    ntdist.c[c] <- sum(is.tdist.i)
    if (nnormdist.c[c] > 0)
      geti.normdist.cj[c, 1:nnormdist.c[c]] <- seq(1, data$n.c[c])[is.normdist.i]
    if (ntdist.c[c] > 0)
      geti.tdist.cj[c, 1:ntdist.c[c]] <- seq(1, data$n.c[c])[is.tdist.i]
  } # end country loop
  
  #inits.list <- list(u.cq = u.cq, b.cm = b.cm,
  #                   Sigma0.b.Lc = Sigma0.b.Lc,
  #                   sigma0.u.c = sigma0.u.c)
  #save(inits.list, file = "inits.list.rda")

  # input mean variance values for countries where lmer estimates were not available (corr values are not used)
  # mean.var.b0 <- 0.00129 # mean(sapply(Sigma0.b.Lc, function(x) x[1, 1]), na.rm = T)
  # mean.var.b1 <- 0.000110 # mean(sapply(Sigma0.b.Lc, function(x) x[2, 2]), na.rm = T)
  for (c in 1:data$C) {
    if (is.na(Sigma0.b.Lc[[c]][1, 1])) {
      Sigma0.b.Lc[[c]][1, 1] <- mean.var.b0
      Sigma0.b.Lc[[c]][2, 2] <- mean.var.b1
      T0.b.Lc[[c]][1, 1] <- 1/Sigma0.b.Lc[[c]][1, 1]
      T0.b.Lc[[c]][2, 2] <- 1/Sigma0.b.Lc[[c]][2, 2]
    }
  }
  # mean.sigma.u <- 0.0424 # mean(sigma0.u.c, na.rm = T)
  sigma0.u.c <- ifelse(is.na(sigma0.u.c), mean.sigma.u, sigma0.u.c)
  
  getc.normdist.d <- c(seq(1, data$C)[nnormdist.c[1:data$C] > 0], NA)
  getc.tdist.d <- c(seq(1, data$C)[ntdist.c[1:data$C] > 0], NA)
  Cnormdist <- sum(!is.na(getc.normdist.d))
  Ctdist <- sum(!is.na(getc.tdist.d))
  
  
  useWorldsigmau.c = c(ifelse(data$useglobalsmoothing.c | IMR_larger_than_U5MR, 1, 0), NA) #by YS
  print (IMR_larger_than_U5MR)
  
  jags.data.splines <- list(
    n.c = data$n.c,
    y.ci = y.ci,
    C = data$C,
    Q = Q,
    BG.cim = BG.cim,
    Z.ciq = Z.ciq,
    q.c = q.c,
    qplus1.c = q.c + 1,
    useWorldsigmau.c = useWorldsigmau.c # change JR, 20140502 # change JR, 20140516
  )
  if (indicator.type == "U5MR")
    jags.data.splines <- c(jags.data.splines, list(I = I))
  if (indicator.type == "IMR") 
    jags.data.splines <- c(jags.data.splines, list(q5hat.ci = q5hat.ci))
  if (run.type == "country")
    jags.data.splines <- c(jags.data.splines, list(r.q = r.q))
  if (is.null(data.val)) {
    if (Cnormdist > 0)
      jags.data.splines <- c(jags.data.splines, 
                             list(Cnormdist = Cnormdist, nnormdist.c  = nnormdist.c,
                                  getc.normdist.d = getc.normdist.d,
                                  geti.normdist.cj = geti.normdist.cj))
    if (Ctdist > 0)
      jags.data.splines <- c(jags.data.splines, 
                             list(Ctdist = Ctdist, ntdist.c = ntdist.c,
                                  getc.tdist.d = getc.tdist.d,
                                  geti.tdist.cj = geti.tdist.cj))
    data <- c(data, list(year.ci = year.ci, 
                         y.ci = y.ci,
                         Cnormdist = Cnormdist, Ctdist = Ctdist))
  } else {
    ywithNAs.ci <- y.ci # for validation: not needed, but just to make sure test data is not included
    for (c in 1:data$C)
      ywithNAs.ci[c, data.val$geti.test.cj[c, 1:data.val$ntest.c[c]]] <- NA
    jags.data.splines$y.ci <- ywithNAs.ci    
    # only select variables in data.val need to be used in model
    names.data.val.select <- NULL
    if (data.val$Ctrainnormdist > 0)
      names.data.val.select <- c(names.data.val.select, 
                                 "Ctrainnormdist", "ntrainnormdist.c",
                                 "getc.trainnormdist.d", "geti.trainnormdist.cj")  
    if (data.val$Ctraintdist > 0)
      names.data.val.select <- c(names.data.val.select, 
                                 "Ctraintdist", "ntraintdist.c",
                                 "getc.traintdist.d", "geti.traintdist.cj")
    if (data.val$Ctestnormdist > 0)
      names.data.val.select <- c(names.data.val.select,
                                 "Ctestnormdist", "ntestnormdist.c",
                                 "getc.testnormdist.d", "geti.testnormdist.cj")
    if (data.val$Ctesttdist > 0)
      names.data.val.select <- c(names.data.val.select, 
                                 "Ctesttdist", "ntesttdist.c",
                                 "getc.testtdist.d", "geti.testtdist.cj")
    # get final jags.data.splines
    jags.data.splines <- c(jags.data.splines, 
                           data.val[names(data.val) %in% names.data.val.select])
    data <- c(data, list(year.ci = year.ci, y.ci = y.ci))
  } # end is.null(data.val)
  jags.data.for.inits <- list(k.c = k.c,
                              u0.cq = u0.cq,
                              sigma0.u.c = sigma0.u.c, # change JR, 20140530
                              b0.cm = b0.cm,
                              Sigma0.b.Lc = Sigma0.b.Lc, # change JR, 20140530
                              T0.b.Lc = T0.b.Lc)
  ##value<<
  return(list(jags.data.splines = jags.data.splines, ##<< JAGS data for all observations/splines
              jags.data.for.inits = jags.data.for.inits, ##<< Data used for JAGS inits
              data = data, ##<< Data to add
              IMR_larger_than_U5MR =IMR_larger_than_U5MR ##<< indicating if IMR is larger than U5MR and wheter global smoothing should be applied
              
  )) 
}
#----------------------------------------------------------------------
GetJAGSDataForSplinesCountrySpecificRun <- function(# Get posterior medians of spline smoothing parameters from global run
  iso,
  data.global,
  use.constant.sigma.u,
  periods.unsmooth.list = NULL,
  periods.smooth.list = NULL
) {
  # change JR, 20140501
  if (!use.constant.sigma.u & 
        is.null(periods.smooth.list) & 
        is.null(periods.unsmooth.list)) {
    mu0.a <- unlist(data.global$mcmc.post$mu.a)
    sigma0.a <- unlist(data.global$mcmc.post$sigma.a)
    jags.data.splines.sigmau0 <- list(mu0.a = mu0.a, sigma0.a = sigma0.a)
  } else {
    if (use.constant.sigma.u) {
      a0 <- unlist(data.global$mcmc.post$a)
    } else { # smooth | unsmooth
      if (iso %in% data.global$iso.c) { # change JR, 21040522
        a0 <- unlist(data.global$mcmc.post[paste0("a.c[", which(data.global$iso.c == iso), "]")])
      } else {
        stop(paste0("Posterior median of a.c is not available for ", iso, " from the global run!"))
      }
    }
    jags.data.splines.sigmau0 <- list(a0 = a0)
  }
  ##value<<
  return(jags.data.splines.sigmau0 = jags.data.splines.sigmau0)
}
#----------------------------------------------------------------------
GetJAGSDataForNonVRObservations <- function(
  data,
  data.val,
  add.dhsdirect.bias,
  set.dhsdirect.prior,
  indicator.type,
  run.type,
  se.censusindirect.missing,
  se.othernonvr.missing,
  dhsdirect.prior.mu.mubeta1,
  dhsdirect.prior.sigma.mubeta1
) {
  year.ci <- se.ynonvr.ci <- series.ci <- matrix(NA, data$C+1, data$nmax)
  surveyyear.cs <- typename.cs <- typenoSEname.cs <- matrix(NA, data$C+1, data$Smax)
  geti.nonvr.cj <- matrix(NA, data$C+1, data$nnonvrmax)
  typename.Lc.s <- typenoSEname.Lc.s <- list()
  typenames <- typenoSEnames <- NULL
  setSeriesLevelBiasAtPrior <- any(unlist(data$isserieslevelbiasatprior.Lc.s) == 1, na.rm = T) # change JR, 20140430
  if (setSeriesLevelBiasAtPrior) # change JR, 20140430
    has.serieslevelbiasatprior.cs <- matrix(NA, data$C+1, data$Smax)
  # change JR, 20140407 # change JR, 20140429
  hasNonVRBias <- any(unlist(data$hasbias.Lc.s) == 1, na.rm = T)
  if (hasNonVRBias)
    is.nonvrwithbias.cs <- matrix(NA, data$C+1, data$Smax)
  for (c in 1:data$C) {
    year.i <- c(unlist(data$year.Lcs.j[[c]]), unlist(data$yearvr.Lc.j[[c]]))
    year.ci[c, 1:data$n.c[c]] <- year.i
    if (data$nnonvr.c[c] > 0) {
      geti.nonvr.cj[c, 1:data$nnonvr.c[c]] <- 1:data$nnonvr.c[c]
      # se(log(u)) = se(u)/u by delta method
      # for observations without SEs, assume se.censusindirect.missing for Census Indirect observations 
      # and se.othernonvr.missing otherwise
      se.ynonvr.ci[c, 1:data$nnonvr.c[c]] <- ifelse(!is.na(unlist(data$se.Lcs.j[[c]])),
                                                    unlist(data$senonNA.Lcs.j[[c]])/(unlist(data$u.Lcs.j[[c]])),
                                                    ifelse(paste(unlist(data$sourcetype.Lcs.j[[c]]), 
                                                                 unlist(data$method.Lcs.j[[c]])) == "Census Indirect", 
                                                           se.censusindirect.missing, 
                                                           se.othernonvr.missing))
      # number non-VR series
      i <- 0
      for (s in 1:data$nseriesnonvr.c[c]) {
        ns <- length(data$year.Lcs.j[[c]][[s]])
        series.ci[c ,(i+1):(i+ns)] <- rep(s, ns) 
        i <- i + ns
      } # end s loop       
      surveyyear.cs[c, 1:data$nseriesnonvr.c[c]] <- unlist(data$surveyyear.Lc.s[[c]])
      typename.cs[c, 1:data$nseriesnonvr.c[c]] <- paste(unlist(data$sourcetype.Lc.s[[c]]),
                                                        unlist(data$method.Lc.s[[c]]))
      # if SE is missing for DHS/Other DHS Direct/MICS Indirect, add to typenoSEname.cs
      # (to avoid error variance that's too small, e.g. for Sri Lanka)
      typenoSEname.cs[c, 1:data$nseriesnonvr.c[c]] <- "Others"
      for (s in 1:data$nseriesnonvr.c[c]) {
        isWithoutSE <- length(unique(data$se.Lcs.j[[c]][[s]])) == 1 & is.na(unique(data$se.Lcs.j[[c]][[s]])[1])
        if (isWithoutSE & is.element(typename.cs[c, s], c("DHS Direct", "Other DHS Direct", "MICS Indirect")))
          typenoSEname.cs[c, s] <- paste(typename.cs[c, s], "no SE")
      } # end s loop for typenoSEname.cs
      if (setSeriesLevelBiasAtPrior)
        has.serieslevelbiasatprior.cs[c, 1:data$nseriesnonvr.c[c]] <- data$isserieslevelbiasatprior.Lc.s[[c]]
      if (hasNonVRBias) # change JR, 20140407 # change JR, 20140429
        is.nonvrwithbias.cs[c, 1:data$nseriesnonvr.c[c]] <- data$hasbias.Lc.s[[c]]
    } # end non-VR obs    
  } # end country loop
  getc.nonvr.d <- c(seq(1, data$C)[data$nnonvr.c[1:data$C] > 0], NA)
  Cnonvr <- sum(!is.na(getc.nonvr.d))
  if (Cnonvr > 0) {
    # table(typename.cs)
    # merge types with few series
    typenamemerged.cs <- typename.cs
    typenamemerged.cs[is.element(typenamemerged.cs, c("MICS Direct", "Census Direct"))] <- "Others Direct"
    typenamemerged.cs[is.element(typenamemerged.cs, c("DHS Indirect", "Other DHS Indirect"))] <- "Others Indirect"
    typename.cs <- typenamemerged.cs
    typename.Lc.s[[c]] <- typename.cs[c, ][!is.na(typename.cs[c, ])]
    typenoSEname.Lc.s[[c]] <- typenoSEname.cs[c, ][!is.na(typenoSEname.cs[c, ])]
    # table(typename.cs)
    type.cs <- matrix(as.numeric(as.factor(typename.cs)), data$C+1, data$Smax)
    typenames <- levels(as.factor(typename.cs))
    ntypes <- length(unique(typenames))
    is.singleobs.cs <- ifelse(typename.cs == "Others Household Deaths" | # is.element turns this into a vector
                               typename.cs == "Others Life Table", 1, 0)
    is.dhsdirect.cs <- ifelse(typename.cs == "DHS Direct", 1, 0)
    typenoSE.cs <- matrix(as.numeric(as.factor(typenoSEname.cs)), data$C+1, data$Smax)
    is.typenoSE.cs <- matrix(ifelse(c(typenoSEname.cs) == "Others", 0, 1), data$C+1, data$Smax)
    typenoSEnames <- levels(as.factor(typenoSEname.cs))
    ntypesnoSE <- length(unique(typenoSEnames))
    mu0.mubeta.tr <- matrix(0, ntypes, 2)
    Sigma0.mubeta.trr <- Tau0.mubeta.trr <- array(NA, c(ntypes, 2, 2))
    if (set.dhsdirect.prior) {
      if (is.element("DHS Direct", typenames)) { # posterior median from multilevel model
        mu0.mubeta.tr[which(typenames == "DHS Direct"), 1] <- dhsdirect.prior.mu.mubeta1
      }
    }
    for (t in 1:ntypes) {
      Sigma0.mubeta.trr[t, 1:2, 1:2] <- cbind(c(0.15^2,0), c(0, 0.02^2))
      if (set.dhsdirect.prior & is.element("DHS Direct", typenames)) {
        if (t == which(typenames == "DHS Direct")) { # (post sd)^2 from multilevel model
          Sigma0.mubeta.trr[t, 1, 1] <- dhsdirect.prior.sigma.mubeta1^2
        }
      }
      Tau0.mubeta.trr[t, 1:2, 1:2] <- solve(Sigma0.mubeta.trr[t, 1:2, 1:2])
    }
  } # end non-VR loop  
  jags.data.nonvr <- NULL
  if (Cnonvr > 0) {
    jags.data.nonvr <- c(jags.data.nonvr,
                         list(Cnonvr = Cnonvr, 
                              nnonvr.c = data$nnonvr.c, 
                              getc.nonvr.d = getc.nonvr.d, 
                              geti.nonvr.cj = geti.nonvr.cj,
                              S.c = data$nseriesnonvr.c,
                              year.ci = year.ci,
                              se.ynonvr.ci = se.ynonvr.ci,
                              series.ci = series.ci,
                              surveyyear.cs = surveyyear.cs,
                              recall.mid = data$recall.mid,
                              ntypes = ntypes,
                              ntypesnoSE = ntypesnoSE,
                              type.cs = type.cs, 
                              typenoSE.cs = typenoSE.cs,
                              is.singleobs.cs = is.singleobs.cs,
                              is.typenoSE.cs = is.typenoSE.cs))
    if (run.type == "global")
      jags.data.nonvr <- c(jags.data.nonvr, list(mu0.mubeta.tr = mu0.mubeta.tr,
                                                 Tau0.mubeta.trr = Tau0.mubeta.trr))
    if (setSeriesLevelBiasAtPrior) # change JR, 20140430
      jags.data.nonvr <- c(jags.data.nonvr, 
                           list(has.serieslevelbiasatprior.cs = has.serieslevelbiasatprior.cs))
    if (hasNonVRBias) # change JR, 20140407 # change JR, 20140429
      jags.data.nonvr <- c(jags.data.nonvr,
                           list(is.nonvrwithbias.cs = is.nonvrwithbias.cs))
    if (add.dhsdirect.bias)
      jags.data.nonvr <- c(jags.data.nonvr,
                           list(is.dhsdirect.cs = is.dhsdirect.cs))
    if (all(is.na(jags.data.nonvr$se.ynonvr.ci))) # remove se.ynonvr.ci if all NA otherwise BUGS error
      jags.data.nonvr <- jags.data.nonvr[names(jags.data.nonvr) != "se.ynonvr.ci"]
    data <- c(data, list(Cnonvr = Cnonvr,
                         typename.Lc.s = typename.Lc.s,
                         typenoSEname.Lc.s = typenoSEname.Lc.s,
                         typenames = typenames,
                         typename.cs = typename.cs,
                         typenoSEnames = typenoSEnames,
                         typenoSEname.cs = typenoSEname.cs))
  } # end non-vr loop
  ##value<<
  return(list(jags.data.nonvr = jags.data.nonvr, 
              data = data))
}
#----------------------------------------------------------------------
GetJAGSDataForNonVRObservationsCountrySpecificRun <- function( # Input posterior median estimates using mcmc.array.global
  data,
  data.global,
  add.dhsdirect.bias  
) {
  ntypes <- length(data$typenames)
  ntypesnoSE <- length(data$typenoSEnames)
  sigma0.ynonvr.t <- rep(NA, ntypes+1) # to avoid BUGS error "expected collection operator c" when ntypes = 1
  sigma0.ynonvr.tnoSE <- rep(NA, ntypesnoSE+1)
  mu0.beta.tr <- matrix(NA, ntypes+1, 2)
  sigma0.beta.tr <- matrix(NA, ntypes+1, 2)
  for (t in 1:ntypes) {
    t.global <- match(data$typenames[t], data.global$typenames)
    sigma0.ynonvr.t[t] <- unlist(data.global$mcmc.post[paste0("sigma.ynonvr.t[", t.global, "]")])
    mu0.beta.tr[t, 1] <- unlist(data.global$mcmc.post[paste0("mu.beta.tr[", t.global, ",1]")])
    mu0.beta.tr[t, 2] <- unlist(data.global$mcmc.post[paste0("mu.beta.tr[", t.global, ",2]")])
    sigma0.beta.tr[t, 1] <- unlist(data.global$mcmc.post[paste0("sigma.beta.tr[", t.global, ",1]")])
    sigma0.beta.tr[t, 2] <- unlist(data.global$mcmc.post[paste0("sigma.beta.tr[", t.global, ",2]")])
  }
  for (tnoSE in 1:ntypesnoSE) {
    tnoSE.global <- match(data$typenoSEnames[tnoSE], data.global$typenoSEnames)
    sigma0.ynonvr.tnoSE[tnoSE] <- unlist(data.global$mcmc.post[paste0("sigma.ynonvr.tnoSE[", tnoSE.global, "]")])
  }  
  jags.data.nonvr.countryspecific <- list(
    sigma0.ynonvr.tnoSE = sigma0.ynonvr.tnoSE,
    sigma0.ynonvr.t = sigma0.ynonvr.t,
    mu0.beta.tr = mu0.beta.tr,
    sigma0.beta.tr = sigma0.beta.tr,
    dft0 = unlist(data.global$mcmc.post$dft))
  if (add.dhsdirect.bias) {
    mu0.biasatzerorecall <- unlist(data.global$mcmc.post$mu.biasatzerorecall)
    sigma0.biasatzerorecall <- unlist(data.global$mcmc.post$sigma.biasatzerorecall) # change JR, 20140505
    recallnobias0 <- unlist(data.global$mcmc.post$recallnobias) # change JR, 20140505
    jags.data.nonvr.countryspecific <- c(jags.data.nonvr.countryspecific, list(
      mu0.biasatzerorecall = mu0.biasatzerorecall,
      sigma0.biasatzerorecall = sigma0.biasatzerorecall,
      recallnobias0 = recallnobias0))
  }
  ##value<<
  return(jags.data.nonvr.countryspecific = jags.data.nonvr.countryspecific)
}
#----------------------------------------------------------------------
GetJAGSDataForVRObservations <- function(
  data,
  data.val,
  input.vr.se,
  se.vr.min,
  se.vr.missing
) {
  se.yvr.ci <- matrix(NA, data$C+1, data$nmax)
  geti.vr.cj <- matrix(NA, data$C+1, data$nvrmax)
  hasVRBias <- any(unlist(data$hasbiasvr.Lc.j) == 1, na.rm = T)
  if (hasVRBias)
    is.vrwithbias.ci <- matrix(NA, data$C+1, data$nmax)
  for (c in 1:data$C) {
    if (data$nvr.c[c] > 0) {
      select.vr.i <- (data$nnonvr.c[c]+1):data$n.c[c]
      geti.vr.cj[c, 1:data$nvr.c[c]] <- seq(1, data$n.c[c])[select.vr.i]
      if (input.vr.se) {
        # include VR SEs, with lower bound of se.vr.min, and assume se.vr.missing SE for VR observations without SEs
        se.yvr.ci[c, select.vr.i] <- ifelse(!is.na(unlist(data$sevr.Lc.j[[c]])),
                                            ifelse(unlist(data$senonNAvr.Lc.j[[c]])/unlist(data$uvr.Lc.j[[c]]) < se.vr.min, 
                                                   se.vr.min, unlist(data$senonNAvr.Lc.j[[c]])/unlist(data$uvr.Lc.j[[c]])),
                                            se.vr.missing)
      } # end input.vr.se loop
      if (hasVRBias) # change JR, 20140429
        is.vrwithbias.ci[c, select.vr.i] <- data$hasbiasvr.Lc.j[[c]]
    }
  } # end country loop
  getc.vr.d <- c(seq(1, data$C)[data$nvr.c[1:data$C] > 0], NA)
  Cvr <- sum(!is.na(getc.vr.d))
  data <- c(data, list(Cvr = Cvr))
  ##value<<
  if (Cvr > 0) {
    jags.data.vr <- list(Cvr = Cvr,
                         nvr.c = data$nvr.c,
                         getc.vr.d = getc.vr.d,
                         geti.vr.cj = geti.vr.cj)
    if (input.vr.se & !all(is.na(se.yvr.ci))) # don't add se.yvr.ci if all NA otherwise BUGS error
      jags.data.vr <- c(jags.data.vr, list(se.yvr.ci = se.yvr.ci))
    if (hasVRBias) # change JR, 20140429
      jags.data.vr <- c(jags.data.vr, list(is.vrwithbias.ci = is.vrwithbias.ci))
    if (any(unlist(data$isincompletevr.Lc.j) == 1, na.rm = T)) { # change JR, 20140506: incomplete VR and incomplete VR minmax are now distinct sets
      jags.data.vrincomplete.temp <- GetJAGSDataForIncompleteVRObservations(data = data, data.val = data.val)
      jags.data.vrincomplete <- jags.data.vrincomplete.temp$jags.data.vrincomplete
      jags.data.vr <- c(jags.data.vr, jags.data.vrincomplete)
      data <- c(data, jags.data.vrincomplete.temp$data)
    }
  } else {
    jags.data.vr <- NULL
  }
  return(list(jags.data.vr = jags.data.vr,
              data = data))
}
#----------------------------------------------------------------------
GetJAGSDataForIncompleteVRObservations <- function(
  data,
  data.val
) {
  # change JR, 20140505
  nvrincomplete.c <- nvrincompminmax.c <- rep(NA, data$C+1)
  geti.vrincomplete.cj <- geti.vrincompminmax.cj <- matrix(NA, data$C+1, data$nvrmax)
  is.incompletevrany.ci <- mincompincompleteVR.ci <- maxcompincompleteVR.ci <-
    matrix(NA, data$C+1, data$nmax)
  compincompleteVR.ci2 <- array(NA, dim = c(data$C+1, data$nmax, 2))
  # incomplete VR in validation test set are dropped
  if (is.null(data.val)) {
    is.train.ci <- matrix(TRUE, data$C+1, data$nmax)
  } else {
    is.train.ci <- data.val$is.train.ci
  }
  for (c in 1:data$C) {
    if (data$nvr.c[c] > 0) {
      select.vr.i <- (data$nnonvr.c[c]+1):data$n.c[c]
      is.incompletevrany.ci[c, select.vr.i] <- data$isincompletevr.Lc.j[[c]]
      is.vrincomplete.i <- is.train.ci[c, 1:data$n.c[c]] & c(rep(FALSE, data$nnonvr.c[c]), 
                                                             data$isincompletevr.Lc.j[[c]] == 1 & is.na(data$mincompincompletevr.Lc.j[[c]]))
      nvrincomplete.c[c] <- sum(is.vrincomplete.i)
      if (nvrincomplete.c[c] > 0) {
        geti.vrincomplete.cj[c, 1:nvrincomplete.c[c]] <- seq(1, data$n.c[c])[is.vrincomplete.i]
        compincompleteVR.ci2[c, , 1] <- 1 # change JR, 20140502
      }
      is.vrincompminmax.i <- is.train.ci[c, 1:data$n.c[c]] & c(rep(FALSE, data$nnonvr.c[c]), 
                                                                   data$isincompletevr.Lc.j[[c]] == 1 & !is.na(data$mincompincompletevr.Lc.j[[c]]))
      nvrincompminmax.c[c] <- sum(is.vrincompminmax.i) 
      if (nvrincompminmax.c[c] > 0) { # change JR, 20140505
        geti.vrincompminmax.cj[c, 1:nvrincompminmax.c[c]] <- seq(1, data$n.c[c])[is.vrincompminmax.i]
        mincompincompleteVR.ci[c, select.vr.i] <- data$mincompincompletevr.Lc.j[[c]]
        maxcompincompleteVR.ci[c, select.vr.i] <- data$maxcompincompletevr.Lc.j[[c]]
        compincompleteVR.ci2[c, , 2] <- mincompincompleteVR.ci[c, ]
        compincompleteVR.ci2[c, , 1] <- ifelse(is.na(maxcompincompleteVR.ci[c, ]), 
                                               1, maxcompincompleteVR.ci[c, ])
      }
    }
  } # end country loop
  getc.vrincomplete.d <- c(seq(1, data$C)[!is.na(nvrincomplete.c[1:data$C]) & 
                                            nvrincomplete.c[1:data$C] > 0], NA)
  Cvrincomplete <- sum(!is.na(getc.vrincomplete.d))
  getc.vrincompminmax.d <- c(seq(1, data$C)[!is.na(nvrincompminmax.c[1:data$C]) & 
                                                  nvrincompminmax.c[1:data$C] > 0], NA)
  Cvrincompminmax <- sum(!is.na(getc.vrincompminmax.d))
  #----------------------------------------------------------------------
  if (!is.null(data.val)) { # for incomplete VR obs in validation test set
    ntestvrincompleteany.c <- rep(NA, data$C+1)
    geti.testvrincompleteany.cj <- matrix(NA, data$C+1, data$nvrmax)
    for (c in 1:data$C) {
      if (nvrincomplete.c[c] > 0 & data.val$ntest.c[c] > 0) {
        indices.testvrincompleteany.i <- intersect(c(geti.vrincomplete.cj[c, ], geti.vrincompminmax.cj[c, ]), 
                                                   geti.test.cj[c, ])
        indices.testvrincompleteany.i <- indices.testvrincompleteany.i[!is.na(indices.testvrincompleteany.i)]
        ntestvrincompleteany.c[c] <- length(indices.testvrincompleteany.i)
        geti.testvrincompleteany.cj[c, 1:length(indices.testvrincompleteany.i)] <- indices.testvrincompleteany.i
      }
    } # end country loop
    getc.testvrincompleteany.d <- c(seq(1, data$C)[ntestvrincompleteany.c[1:data$C] > 0], NA)
    Ctestvrincompleteany <- sum(!is.na(getc.testvrincompleteany.d))
  }
  #----------------------------------------------------------------------
  jags.data.vrincomplete <- list(
    nmax = data$nmax,
    is.splinesabovevr.ci = matrix(1, data$C+1, data$nmax),
    logcompincompleteVR.ci2 = log(compincompleteVR.ci2)) # 1 such that yhat.ci (fit) > y.ci (logVRobs) and y.ci < yhat.ci (fit) < y.ci - log(mincompVR)
  if (Cvrincomplete > 0)
    jags.data.vrincomplete <- c(jags.data.vrincomplete, list(
      Cvrincomplete = Cvrincomplete,
      nvrincomplete.c = nvrincomplete.c,
      getc.vrincomplete.d = getc.vrincomplete.d,
      geti.vrincomplete.cj = geti.vrincomplete.cj))
  if (Cvrincompminmax > 0)
    jags.data.vrincomplete <- c(jags.data.vrincomplete, list(
      Cvrincompminmax = Cvrincompminmax,
      nvrincompminmax.c = nvrincompminmax.c,
      getc.vrincompminmax.d = getc.vrincompminmax.d,
      geti.vrincompminmax.cj = geti.vrincompminmax.cj))
  if (!is.null(data.val)) { # for incomplete VR obs in validation test set
    if (Ctestvrincompleteany > 0) 
      jags.data.vrincomplete <- c(jags.data.vrincomplete, 
                                  list(Ctestvrincompleteany = Ctestvrincompleteany,
                                       ntestvrincompleteany.c = ntestvrincompleteany.c,
                                       getc.testvrincompleteany.d = getc.testvrincompleteany.d,
                                       geti.testvrincompleteany.cj = geti.testvrincompleteany.cj))
  } 
  data.output <- list(Cvrincomplete = Cvrincomplete,
                      Cvrincompminmax = Cvrincompminmax,
                      is.incompletevrany.ci = is.incompletevrany.ci, 
                      mincompincompleteVR.ci = mincompincompleteVR.ci,
                      maxcompincompleteVR.ci = maxcompincompleteVR.ci)
  if (!is.null(data.val))
    data.output <- c(data.output, list(Ctestvrincompleteany = Ctestvrincompleteany))
  ##value<<
  return(list(jags.data.vrincomplete = jags.data.vrincomplete,
              data = data.output ##<< Not used in JAGS
  ))
}
#----------------------------------------------------------------------
# GetJAGSDataForPrediction <- function(
#   data,
#   I,
#   year.lastestimate,
#   periods.constant.list
# ) {
#   # set estimation years # minimum start year is 1990.5 # 
#   year.t <- seq(floor(min(ifelse(is.element("RUS", data$iso.c), 1971, 1986), 
#                           data$minyear.c, na.rm = T))-0.5, year.lastestimate)  # last year needs to be a future projection!
#   # to make sure first and last observation year is within year.t for all countries
#   n.years <- length(year.t)
#   P <- ceiling(length(year.t)/I)+5+1 # change JR, 25 Mar
#   M <- 2
#   Qpredict <- P - M
#   Q <- Qpredict
#   B.ctp <- array(NA, c(data$C+1, n.years, P))
#   BG.ctm <- array(NA, c(data$C+1, n.years, M))
#   Z.ctq <- array(0, c(data$C+1, n.years, Q))
#   for (c in 1:data$C) {
#     year.i <- c(unlist(data$year.Lcs.j[[c]]), unlist(data$yearvr.Lc.j[[c]]))
#     year.min <- floor(min(ifelse(data$iso.c[c] == "RUS", 1971, 1986), 
#                           data$minyear.c[c], na.rm = T))-0.5 
#     resobs <- GetSplines(years.t = c(year.min, year.i), I = I, 
#                          years.combine = periods.constant.list)
#     resproject <- GetSplines(years.t = year.t, I = I,
#                              years.combine = periods.constant.list,
#                              year0 = resobs$alphayears.k[1]) # such that knot placement is the same
#     Kpredict <- length(resproject$alphayears.k)
#     B.ctp[c, , 1:Kpredict] <- resproject$B.tk
#     d <- 2
#     D2predict <- diff(diag(Kpredict), diff = d) # difference matrix
#     Dcombpredict <- t(D2predict)%*%solve(D2predict%*%t(D2predict))
#     Gpredict <- cbind(rep(1, Kpredict), seq(1, Kpredict)-Kpredict/2) 
#     BG.ctm[c, , ] <- B.ctp[c, , 1:Kpredict]%*%Gpredict
#     Z.tq <- B.ctp[c, , 1:Kpredict]%*%Dcombpredict  
#     Z.ctq[c, , 1:dim(Z.tq)[2]] <- Z.tq
#   }
#   ##value<<
#   jags.data.predict <- list(P = P, 
#                             BG.ctm = BG.ctm, 
#                             Z.ctq = Z.ctq, 
#                             Qpredict = Qpredict)
#   return(jags.data.predict = jags.data.predict)
# }
