#----------------------------------------------------------------------
# constructoutput.R
#----------------------------------------------------------------------
ConstructOutput <- function( # Construct output for MCMC run
  ## Construct output for MCMC run: Country trajectories and estimates.
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored and new objects will be added.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  is.validation.up.to.2000 = FALSE, ##<< Logical value indicating whether or not to construct output for new validation exercise.
  load.alpha.cpj = FALSE, ##<< Logical value indicating whether or not alpha.cpj has already been constructed (to skip steps).
  year.start = NULL, ##<< Start year of estimates to output in .txt file. If \code{NULL}, defaults to first observation year for each country.
  year.end = NULL, ##<< End year of estimates to output in .txt file. If \code{NULL}, defaults to \code{mcmc.meta$settings$year.lastestimate}.
  year1 = 1990.5, ##<< First year used for ARR calculation.
  year2 = 2000.5, ##<< Second year used for ARR calculation.
  year3 = 2005.5, ##<< Third year used for ARR calculation, usually last year of estimation for validation exercise.
  year4 = NULL, ##<< Last year used for ARR calculation. If \code{NULL}, defaults to \code{mcmc.meta$settings$year.lastestimatepublished} or \code{NULL} if validation run and
  ## if \code{mcmc.meta$settings$year.lastestimatepublished} > \code{mcmc.meta$settings$year.cutoff}.
  percentiles = c(0.05, 0.5, 0.95), ##<< Percentiles for 90% UIs.
  global.alpha.diffs.median = NULL, ##<< Bayesian melding global gamma median parameter. # UN IGME 2013: -0.08
  global.alpha.diffs.sd = NULL, ##<< Bayesian melding global gamma sd parameter. # UN IGME 2013: 0.1
  weights.alpha = seq(0.1, 0.6, 0.1), ##<< Vector of weights assigned to global gamma distribution.
  weight.alpha.publish = 0.5, ##<< \code{weight.alpha} to use for Results.csv.
  quick.plot.check = TRUE ##<< Logical value to indicate if a quick plot should be made for checking
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  load(file.path(output.dir, "mcmc.meta.rda"))
  load(file.path(output.dir, "mcmc.array.rda"))
  list2env(mcmc.meta$settings, envir = environment())
  if (is.null(global.alpha.diffs.median)) {
    if (is.null(mcmc.meta$settings$global.gamma.median)) {
      stop(paste0("global.alpha.diffs.median cannot be found in mcmc.meta$settings. ",
                  "Please assign a value to global.alpha.diffs.median in the ConstructOutput function."))
    } else {
      global.alpha.diffs.median <- mcmc.meta$settings$global.gamma.median
    }
  }  
  if (is.null(global.alpha.diffs.sd)) {
    if (is.null(mcmc.meta$settings$global.gamma.sd)) {
      stop(paste0("global.alpha.diffs.sd cannot be found in mcmc.meta$settings. ",
                  "Please assign a value to global.alpha.diffs.median in the ConstructOutput function."))
    } else {
      global.alpha.diffs.sd <- mcmc.meta$settings$global.gamma.sd
    }
  } 
  hiv.file <- mcmc.meta$files$hiv.file
  adj.file <- mcmc.meta$files$adj.file
  if (is.null(year.end))
    year.end <- year.lastestimate
  if (is.null(year4)) {
    if (is.validation & year.lastestimatepublished > year.cutoff) {
      year4 <- NULL # validation
    } else {
      year4 <- year.lastestimatepublished
    }
  }
  if (is.null(mcmc.meta$settings$periods.constant.list)) # for back compatibility with older runs
    periods.constant.list <- NULL
  if (load.alpha.cpj) {
    load(file = file.path(output.dir, "alphatemp.cpj.rda"))
    alpha.cpj <- alphatemp.cpj
  }
  
  if (run.type == "global")
    SummariseGlobalRun(runname.global = runname, output.dir = output.dir)
  #----------------------------------------------------------------------
  # Steps:
  # 1. Set estimation years and declare components 
  ## for each country:
  # 2. Get B.tp where t refers to year.t and p to the p splines that are needed
  # and check which alphas (and sigma.u's) are jags output and recover those
  # 3. Simulate additional alphas to get alpha.p
  # 4. Construct estimates and UIs
  #----------------------------------------------------------------------
  # 1. Set estimation years 
  #----------------------------------------------------------------------
  C <- mcmc.meta$data$C
  # minimum start year is 1990.5, 1970.5 for Russia
  year.t <- seq(floor(min(ifelse(is.element("RUS", mcmc.meta$data$iso.c), 1971, 1991), 
                          mcmc.meta$data$minyear.c, na.rm = T))-0.5, year.lastestimate)
  # last year needs to be a future projection!
  # to make sure first and last observation year is within year.t for all countries
  nyears <- length(year.t)
  ##details<< Outputs lists of results for weights w (and year combinations y for ARR).
  nweights <- length(weights.alpha)
  weights.alpha.plusdefault <- c(0, weights.alpha)
  nweightsplus1 <- length(weights.alpha.plusdefault)
  res.cqt.Lw <- resall.cqt.Lw <- resmean.ct.Lw <- resARR.cq.Lwy <- list()
  for (w in 1:nweightsplus1) {
    res.cqt.Lw[[w]] <- resall.cqt.Lw[[w]] <- array(NA, c(C, length(percentiles), nyears))
    resmean.ct.Lw[[w]] <- array(NA, c(C, nyears))
    dimnames(res.cqt.Lw[[w]]) <- dimnames(resall.cqt.Lw[[w]]) <- 
      list(mcmc.meta$data$iso.c, percentiles, year.t)
    dimnames(resmean.ct.Lw[[w]]) <- list(mcmc.meta$data$iso.c, year.t)
    resARR.cq.Lwy[[w]] <- list()  
    for (y in 1:4) {
      if (!is.validation | is.element(y, c(1,2))) { 
        resARR.cq.Lwy[[w]][[y]] <- array(NA, c(C, length(percentiles)))
        dimnames(resARR.cq.Lwy[[w]][[y]]) <- list(mcmc.meta$data$iso.c, percentiles)
      }
    }
    if (!is.validation) {
      names(resARR.cq.Lwy[[w]]) <- c(paste0(year1, "-", year3), paste0(year1, "-", year2), 
                                     paste0(year2, "-", year4), paste0(year1, "-", year4))
    } else {
      names(resARR.cq.Lwy[[w]]) <- c(paste0(year1, "-", year3), paste0(year1, "-", year2))
    }
  }
  names(res.cqt.Lw) <- names(resall.cqt.Lw) <- names(resmean.ct.Lw) <- names(resARR.cq.Lwy) <- 
    weights.alpha.plusdefault
  P <- ceiling(length(year.t)/I)+5+1
  B.ctp <- array(NA, c(C, nyears, P)) 
  nsim <- prod(dim(mcmc.array)[1:2])
  a.cj <- array(NA, c(C, nsim))
  u5.ctj <- array(NA, c(C, nyears, nsim))
  dimnames(u5.ctj) <- list(mcmc.meta$data$iso.c, year.t, NULL)
  if (!load.alpha.cpj) 
    alpha.cpj <- alphatemp.cpj <- array(NA, c(C, P, nsim)) # j refers to post sample
  if (is.validation) {
    u5full.ctj <- array(NA, c(C, nyears, nsim))
    dimnames(u5full.ctj) <- list(mcmc.meta$data$iso.c, year.t, NULL)
    alphafull.cpj <- array(NA, c(C, P, nsim))
  }
  resproject.list.c <- list()
  res.alpha.cp <- array(NA, c(C, P))
  # for other weights.alpha
  for (w in 1:nweights) {
    eval(parse(text = paste0("u5new", w, ".ctj", " <- array(NA, c(C, nyears, nsim))")))
    eval(parse(text = paste0("dimnames(u5new", w, ".ctj)", " <- list(mcmc.meta$data$iso.c, year.t, NULL)")))
    eval(parse(text = paste0("alphanew", w, ".cpj", " <- array(NA, c(C, P, nsim))")))
    eval(parse(text = paste0("res.alphanew", w, ".cp", " <- array(NA, c(C, P))")))
  }
  #pdf(file = paste("results ", Sys.Date(), ".pdf", sep = ""), width = 7, height = 7)
  for (c in 1:C) {
    cat(paste("Constructing output for country", c, "out of", C, ifelse(C == 1, "country", "countries"), "\n"))
    seed.output.country <- mcmc.meta$general$seed.MCMC*as.numeric(mcmc.meta$data$uncode.c[c]) # change JR, 20140508
    #----------------------------------------------------------------------
    # 2. Get B.tp where t refers to year.t and p to the p splines that are needed
    # and check which alphas (and sigma.u's) are jags output and recover those
    #----------------------------------------------------------------------
    year.i <- c(unlist(mcmc.meta$data$year.Lcs.j[[c]]), unlist(mcmc.meta$data$yearvr.Lc.j[[c]]))
    year.min <- floor(min(ifelse(mcmc.meta$data$iso.c[c] == "RUS", 1971, 1991), 
                          mcmc.meta$data$minyear.c[c], na.rm = T))-0.5
    resobs <- GetSplines(years.t = c(year.min, year.i), I = I,
                         years.combine = periods.constant.list)
    # for later, to recover the alphas:
    B.ik <- resobs$B.tk[-1, ] # change JR, 17 Jun: remove the first column corresponding to year.min
    Ktemp <- length(resobs$alphayears.k)
    if (!load.alpha.cpj) {
      D2 <- diff(diag(Ktemp), diff = 2)
      W <- diag(Ktemp) - (t(D2)%*%solve(D2%*%t(D2))%*%D2)
      stemp <- svd(W, nu = 2, nv = 2)
      L <- stemp$u
      G <- cbind(rep(1, Ktemp), seq(1, Ktemp)-Ktemp/2) # center second column of G
      A <- qr.solve(L, G)
    }
    resproject <- GetSplines(years.t = year.t, I = I,
                             years.combine = periods.constant.list,
                             year0 = resobs$alphayears.k[1]) # such that knot placement is the same
    Kproject <- length(resproject$alphayears.k)
    B.ctp[c, , 1:Kproject] <- resproject$B.tk
    # change to character because comparison of numerics is tricky!
    resobs$alphayears.k <- as.numeric(as.character(resobs$alphayears.k))
    resproject$alphayears.k <- as.numeric(as.character(resproject$alphayears.k)) 
    
    if (!load.alpha.cpj) {
      # recover the alphas
      j = 0 # refers to posterior sample
      for (chain in 1:dim(mcmc.array)[2]) {
        for (s in 1:dim(mcmc.array)[1]) {        
          j <- j + 1
          u.q <- mcmc.array[s, chain, paste0("u.cq[", c, ",", 1:mcmc.meta$jags.data$q.c[c], "]")]
          b.m <- mcmc.array[s, chain, paste0("b.cm[", c, ",", 1:2,"]")]
          # alphas need to be sorted (which they are!)
          alpha.cpj[c, seq(1, Kproject)[is.element(resproject$alphayears.k, resobs$alphayears.k)], j] <- 
            alphatemp.cpj[c, seq(1, Kproject)[is.element(resproject$alphayears.k, resobs$alphayears.k)], j] <-
            qr.solve(a = rbind(solve(A)%*%t(L), D2), b = c(b.m, u.q))
        }
      }
    }
    if (is.validation)
      alphafull.cpj[c, ,] <- alpha.cpj[c, , ]
    #----------------------------------------------------------------------
    # 3. Simulate the other alphas (forward projections) 
    #----------------------------------------------------------------------
    # get a.j
    if (!use.constant.sigma.u & 
          is.null(periods.smooth.list) & 
          is.null(periods.unsmooth.list)) {
      a.j <- a.cj[c, ] <- c(mcmc.array[, , paste0("a.c", "[", c,"]")])  
    } else {
      a.j <- a.cj[c, ] <- c(mcmc.array[, , "a"])
    }
    # get kstart
    if (is.validation.up.to.2000) {
      kstart <- max(which(resproject$alphayears.k < 2000))
    } else if (!is.validation) {
      kstart <- which(resproject$alphayears.k==resobs$alphayears.k[Ktemp-1])
    } else {
      kstart.full <- which(resproject$alphayears.k==resobs$alphayears.k[Ktemp-1])
      yeartrain.j <- year.i[mcmc.meta$data.val$geti.training.cj[c, !is.na(mcmc.meta$data.val$geti.training.cj[c, ])]]
      resobstrain <- GetSplines(years.t = c(year.min, yeartrain.j), I = I,
                                years.combine = periods.constant.list,
                                year0 = max(year.i)-0.5*I)
      Ktemptrain <- length(resobstrain$alphayears.k) 
      resobstrain$alphayears.k <- as.numeric(as.character(resobstrain$alphayears.k))
      kstart <- which(resproject$alphayears.k == resobstrain$alphayears.k[Ktemptrain-1])
    }
    # get results without Bayesian melding
    sigma.j <- exp(a.j)
    if (is.validation) { 
      set.seed(seed.output.country*100) # change JR, 20140508
      # change JR, 14 Aug 2013: to get results with alphas sampled after last reference date in full data set 
      # (instead of last reference date in train set)
      for (k in (kstart.full+2):Kproject) { 
        # change JR, 14 Aug 2013: we do not want to resample alpha[Ktemp] for results without Bayesian melding,
        # so we start in Ktemp+1 (note difference of kstart+2 instead of kstart+1 here)
        u.extra.j <- rnorm(nsim, 0, sd = sigma.j)
        alphafull.cpj[c, k, ] <- (u.extra.j - alphafull.cpj[c, k-2, ] + 2*alphafull.cpj[c, k-1, ])
      }
    }
    set.seed(seed.output.country*200) # change JR, 20140508
    for (k in (kstart+2):Kproject) { 
      # change JR, 14 Aug 2013: we do not want to resample alpha[Ktemp] for results without Bayesian melding,
      # so we start in Ktemp+1 (note difference of kstart+2 instead of kstart+1 here)
      u.extra.j <- rnorm(nsim, 0, sd = sigma.j)
      alpha.cpj[c, k, ] <- (u.extra.j - alpha.cpj[c, k-2, ] + 2*alpha.cpj[c, k-1, ])
    }
    
    #Kproject <- length(resproject$alphayears.k)
    
    
    #  add by YS and DJS, to keep the rate constant (control alpha)
    if (!is.null(mcmc.meta$settings$special.constant.list)){
    #if (mcmc.meta$data$iso.c=="YEM"|mcmc.meta$data$iso.c=="SYR"){
      cat(paste('Special Constant Period',special.constant.list[[1]],'\n'))
      k.constant=which(resobs$alphayears.k >= special.constant.list[[1]][1] & resobs$alphayears.k <= special.constant.list[[1]][2])
      k.constant.start = min(k.constant) ## same as k.start?
      alpha.cpj.flat <- alpha.cpj.adj <- alpha.cpj
      #for (k in c(k.constant,k.constant.end+1)){
      for (k in k.constant.start:Kproject){  ## edit by DJS 20170525 to make alpha constant through end of projection period
        alpha.cpj.flat[c, k, ] <- alpha.cpj[c, k.constant.start-1, ]
      }
      diff.alpha.cpj <- apply(alpha.cpj.flat, 2, median)-apply(alpha.cpj, 2, median)
      alpha.cpj.adj <- alpha.cpj+diff.alpha.cpj
    }
    ##### end of adding by YS and DJS
    
    ## added by DJS 20170525 to make alpha constant through end of projection period
    if (!is.null(mcmc.meta$settings$special.constant.list)){
      # if(mcmc.meta$data$iso.c=="YEM"|mcmc.meta$data$iso.c=="SYR"){
      alphanew0.pj <- DoBayesianMelding(weight.alpha = 0, 
                                        m = global.alpha.diffs.median, sd = global.alpha.diffs.sd, 
                                        a.j = a.j, 
                                        kstart = kstart, Kproject = Kproject, 
                                        alpha.pj = alpha.cpj[c, , ])
      years.adjust <- (k.constant.start):Kproject
      alphanew0.pj.adj <- alphanew0.pj
      for(years.adjust.k in 1:length(years.adjust)){
        alphanew0.pj.adj[years.adjust[years.adjust.k],] <- alphanew0.pj[years.adjust[years.adjust.k],] - median(apply(alphanew0.pj.adj[(years.adjust[years.adjust.k]-1):years.adjust[years.adjust.k],], 2, diff))
      }# j loop
    } # if special constant
    ## end of added by DJS 20170525
    
    # save years for plotting 
    if (!is.validation & !is.validation.up.to.2000) {
      resproject$alphayears.k.plot <- resproject$alphayears.k[(kstart-Ktemp+1):Kproject]
      resproject$uyears.q.plot <- resproject$alphayears.k.plot[-c(1, length(resproject$alphayears.k.plot))]
      resproject.list.c[[c]] <- resproject
    }
    # get results with Bayesian melding
    for (w in 1:nweights) {
      weight.alpha <- weights.alpha[w]
      set.seed(seed.output.country*300*weight.alpha*10) # change JR, 20140508
      alphanew.pj <- DoBayesianMelding(weight.alpha = weight.alpha, 
                                       m = global.alpha.diffs.median, sd = global.alpha.diffs.sd, 
                                       a.j = a.j, 
                                       kstart = kstart, Kproject = Kproject, 
                                       alpha.pj = alpha.cpj[c, , ])
      eval(parse(text = paste0("alphanew", w, ".cpj[c, , ]", " <- alphanew.pj")))
    }
    
    #----------------------------------------------------------------------
    # 4. Construct estimates and UIs
    #----------------------------------------------------------------------
    # alphas are NA before first observation year
    # first set these alphas to zero to get U, and then set U to NA for missing years
    alpha.cpj[c, is.na(alpha.cpj[c, , 1]), ] <- 0
    if (is.validation)
      alphafull.cpj[c, is.na(alphafull.cpj[c, , 1]), ] <- 0
    if (indicator.type == "U5MR") {
      u5temp.tj <- exp(apply(t(alpha.cpj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject])))
      if (is.validation)
        u5fulltemp.tj <- exp(apply(t(alphafull.cpj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject])))
    } else if (indicator.type == "IMR") {
      logittemp.tj <- apply(t(alpha.cpj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject]))
      q5hat.t <- Getq5Estimates(iso = mcmc.meta$data$iso.c[c], years = year.t, runname = runname.U5MR,
                                printWarnings = T)
      u5temp.tj <- exp(logittemp.tj)*q5hat.t/(exp(logittemp.tj)+1)
      if (is.validation) {
        logitfulltemp.tj <- apply(t(alphafull.cpj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject]))
        u5fulltemp.tj <- exp(logitfulltemp.tj)*q5hat.t/(exp(logitfulltemp.tj)+1)
      }
    }
    
    ## added by DJS 20170525 to make alpha constant through end of projection period
    if(!is.null(mcmc.meta$settings$special.constant.list)){
    # if(mcmc.meta$data$iso.c=="YEM"|mcmc.meta$data$iso.c=="SYR"){
    alpha.cpj.flat[c, is.na(alpha.cpj.flat[c, , 1]), ] <- 0
    # if (is.validation)
    #   alphafull.cpj[c, is.na(alphafull.cpj[c, , 1]), ] <- 0
    if (indicator.type == "U5MR") {
      u5temp.tj.flat <- exp(apply(t(alpha.cpj.flat[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject])))
      # if (is.validation)
      #   u5fulltemp.tj <- exp(apply(t(alphafull.cpj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject])))
    } else if (indicator.type == "IMR") {
      logittemp.tj.flat <- apply(t(alpha.cpj.flat[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject]))
      q5hat.t.flat <- Getq5Estimates(iso = mcmc.meta$data$iso.c[c], years = year.t, runname = runname.U5MR,
                                printWarnings = T)
      u5temp.tj.flat <- exp(logittemp.tj.flat)*q5hat.t.flat/(exp(logittemp.tj.flat)+1)
      # if (is.validation) {
      #   logitfulltemp.tj <- apply(t(alphafull.cpj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject]))
      #   u5fulltemp.tj <- exp(logitfulltemp.tj)*q5hat.t.flat/(exp(logitfulltemp.tj)+1)
      # }
    }
    }# if
  
    # if(!is.null(mcmc.meta$settings$special.constant.list)){
    # # if(mcmc.meta$data$iso.c=="YEM"|mcmc.meta$data$iso.c=="SYR"){
    #   alpha.cpj.adj[c, is.na(alpha.cpj.adj[c, , 1]), ] <- 0
    #   # if (is.validation)
    #   #   alphafull.cpj[c, is.na(alphafull.cpj[c, , 1]), ] <- 0
    #   if (indicator.type == "U5MR") {
    #     u5temp.tj.adj <- exp(apply(t(alpha.cpj.adj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject])))
    #     # if (is.validation)
    #     #   u5fulltemp.tj <- exp(apply(t(alphafull.cpj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject])))
    #   } else if (indicator.type == "IMR") {
    #     logittemp.tj.adj <- apply(t(alpha.cpj.adj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject]))
    #     q5hat.t.adj <- Getq5Estimates(iso = mcmc.meta$data$iso.c[c], years = year.t, runname = runname.U5MR,
    #                                    printWarnings = T)
    #     u5temp.tj.adj <- exp(logittemp.tj.adj)*q5hat.t.adj/(exp(logittemp.tj.adj)+1)
    #     # if (is.validation) {
    #     #   logitfulltemp.tj <- apply(t(alphafull.cpj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject]))
    #     #   u5fulltemp.tj <- exp(logitfulltemp.tj)*q5hat.t.adj/(exp(logitfulltemp.tj)+1)
    #     # }
    #   } 
    # }# if
      
    if(!is.null(mcmc.meta$settings$special.constant.list)){
    # if(mcmc.meta$data$iso.c=="YEM"|mcmc.meta$data$iso.c=="SYR"){
      alphanew0.pj.adj[is.na(alphanew0.pj.adj[,1]),] <- 0
      # if (is.validation)
      #   alphafull.cpj[c, is.na(alphafull.cpj[c, , 1]), ] <- 0
      if (indicator.type == "U5MR") {
        u5tempnew0.tj.adj <- exp(apply(t(alphanew0.pj.adj[1:Kproject,]), 1, "%*%", t(B.ctp[c, , 1:Kproject])))
        # if (is.validation)
        #   u5fulltemp.tj <- exp(apply(t(alphafull.cpj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject])))
      } else if (indicator.type == "IMR") {
        logittempnew0.tj.adj <- apply(t(alphanew0.pj.adj[1:Kproject,]), 1, "%*%", t(B.ctp[c, , 1:Kproject]))
        q5hat.t.adj <- Getq5Estimates(iso = mcmc.meta$data$iso.c[c], years = year.t, runname = runname.U5MR,
                                      printWarnings = T)
        u5tempnew0.tj.adj <- exp(logittempnew0.tj.adj)*q5hat.t.adj/(exp(logittempnew0.tj.adj)+1)
        # if (is.validation) {
        #   logitfulltemp.tj <- apply(t(alphafull.cpj[c, 1:Kproject , ]), 1, "%*%", t(B.ctp[c, , 1:Kproject]))
        #   u5fulltemp.tj <- exp(logitfulltemp.tj)*q5hat.t.adj/(exp(logitfulltemp.tj)+1)
        # }
      } 
    }# if  
    ## end of added by DJS 20170525
    
    # add back HIV q's
    year4.input <- year4
    if (is.validation) year4.input <- NULL 
    res <- CalculateQuantities(u5temp.tj = u5temp.tj,
                               alpha.pj = alpha.cpj[c, , ],
                               B.tp = B.ctp[c, , ],
                               Kproject = Kproject,
                               iso = mcmc.meta$data$iso.c[c],
                               indicator.type = indicator.type, runname.U5MR = runname.U5MR,
                               hiv = mcmc.meta$data$hiv.c[c], hiv.file = hiv.file,
                               crisisadj = mcmc.meta$data$crisisadj.c[c], adj.file = adj.file,
                               year.t = year.t, year.i = year.i,
                               estyear.min = ifelse(mcmc.meta$data$iso.c[c] == "RUS", 1970.5, 1990.5),
                               year1 = year1, year2 = year2, year3 = year3, 
                               year4 = year4.input,
                               percentiles = percentiles)
    res.alpha.cp[c, ] <- res$res.alpha.p
    u5.ctj[c, , ] <- res$u5.tj
    resall.cqt.Lw[['0']][c, , ] <- res$resall.qt
    res.cqt.Lw[['0']][c, , ] <- res$res.qt
    resmean.ct.Lw[['0']][c, ] <- res$resmean.t
    for (y in 1:4) {
      if (!is.validation | is.element(y, c(1,2))) {
        resARR.cq.Lwy[['0']][[y]][c, ] <- unlist(res$resARR.q.Ly[[y]])
      }
    }
    
    ## added by DJS 2017-05-26
    if(!is.null(mcmc.meta$settings$special.constant.list)){
    # if(mcmc.meta$data$iso.c=="YEM"|mcmc.meta$data$iso.c=="SYR"){
    res.alpha.cp.flat <- res.alpha.cp
    u5.ctj.flat <- u5.ctj
    resall.cqt.Lw.flat <- resall.cqt.Lw$`0`
    res.cqt.Lw.flat <- res.cqt.Lw$`0`
    resmean.ct.Lw.flat <- resmean.ct.Lw$`0`
    resARR.cq.Lwy.flat <- resARR.cq.Lwy$`0`
    res.flat <- CalculateQuantities(u5temp.tj = u5temp.tj.flat,
                               alpha.pj = alpha.cpj.flat[c, , ],
                               B.tp = B.ctp[c, , ],
                               Kproject = Kproject,
                               iso = mcmc.meta$data$iso.c[c],
                               indicator.type = indicator.type, runname.U5MR = runname.U5MR,
                               hiv = mcmc.meta$data$hiv.c[c], hiv.file = hiv.file,
                               crisisadj = mcmc.meta$data$crisisadj.c[c], adj.file = adj.file,
                               year.t = year.t, year.i = year.i,
                               estyear.min = ifelse(mcmc.meta$data$iso.c[c] == "RUS", 1970.5, 1990.5),
                               year1 = year1, year2 = year2, year3 = year3, 
                               year4 = year4.input,
                               percentiles = percentiles)
    res.alpha.cp.flat[c, ] <- res.flat$res.alpha.p
    u5.ctj.flat[c, , ] <- res.flat$u5.tj
    resall.cqt.Lw.flat[c, , ] <- res.flat$resall.qt
    res.cqt.Lw.flat[c, , ] <- res.flat$res.qt
    resmean.ct.Lw.flat[c, ] <- res.flat$resmean.t
    for (y in 1:4) {
      if (!is.validation | is.element(y, c(1,2))) {
        resARR.cq.Lwy.flat[[y]][c, ] <- unlist(res.flat$resARR.q.Ly[[y]])
      }
    }
    
    # res.alpha.cp.adj <- res.alpha.cp
    # u5.ctj.adj <- u5.ctj
    # resall.cqt.Lw.adj <- resall.cqt.Lw$`0`
    # res.cqt.Lw.adj <- res.cqt.Lw$`0`
    # resmean.ct.Lw.adj <- resmean.ct.Lw$`0`
    # resARR.cq.Lwy.adj <-  resARR.cq.Lwy$`0`
    # res.adj <- CalculateQuantities(u5temp.tj = u5temp.tj.adj,
    #                                 alpha.pj = alpha.cpj.adj[c, , ],
    #                                 B.tp = B.ctp[c, , ],
    #                                 Kproject = Kproject,
    #                                 iso = mcmc.meta$data$iso.c[c],
    #                                 indicator.type = indicator.type, runname.U5MR = runname.U5MR,
    #                                 hiv = mcmc.meta$data$hiv.c[c], hiv.file = hiv.file,
    #                                 crisisadj = mcmc.meta$data$crisisadj.c[c], adj.file = adj.file,
    #                                 year.t = year.t, year.i = year.i,
    #                                 estyear.min = ifelse(mcmc.meta$data$iso.c[c] == "RUS", 1970.5, 1990.5),
    #                                 year1 = year1, year2 = year2, year3 = year3, 
    #                                 year4 = year4.input,
    #                                 percentiles = percentiles)
    # res.alpha.cp.adj[c, ] <- res.adj$res.alpha.p
    # u5.ctj.adj[c, , ] <- res.adj$u5.tj
    # resall.cqt.Lw.adj[c, , ] <- res.adj$resall.qt ## change these to keep 
    # res.cqt.Lw.adj[c, , ] <- res.adj$res.qt
    # resmean.ct.Lw.adj[c, ] <- res.adj$resmean.t
    # for (y in 1:4) {
    #   if (!is.validation | is.element(y, c(1,2))) {
    #     resARR.cq.Lwy.adj[[y]][c, ] <- unlist(res.adj$resARR.q.Ly[[y]])
    #   }
    # }
    
    resnew0.alpha.cp.adj <- res.alpha.cp
    u5new0.ctj.adj <- u5.ctj
    resallnew0.cqt.Lw.adj <- resall.cqt.Lw$`0`
    resnew0.cqt.Lw.adj <- res.cqt.Lw$`0`
    resmeannew0.ct.Lw.adj <- resmean.ct.Lw$`0`
    resARRnew0.cq.Lwy.adj <-  resARR.cq.Lwy$`0`
    resnew0.adj <- CalculateQuantities(u5temp.tj = u5tempnew0.tj.adj,
                                   alpha.pj = alphanew0.pj.adj,
                                   B.tp = B.ctp[c, , ],
                                   Kproject = Kproject,
                                   iso = mcmc.meta$data$iso.c[c],
                                   indicator.type = indicator.type, runname.U5MR = runname.U5MR,
                                   hiv = mcmc.meta$data$hiv.c[c], hiv.file = hiv.file,
                                   crisisadj = mcmc.meta$data$crisisadj.c[c], adj.file = adj.file,
                                   year.t = year.t, year.i = year.i,
                                   estyear.min = ifelse(mcmc.meta$data$iso.c[c] == "RUS", 1970.5, 1990.5),
                                   year1 = year1, year2 = year2, year3 = year3, 
                                   year4 = year4.input,
                                   percentiles = percentiles)
    resnew0.alpha.cp.adj[c, ] <- resnew0.adj$res.alpha.p
    u5new0.ctj.adj[c, , ] <- resnew0.adj$u5.tj
    resallnew0.cqt.Lw.adj[c, , ] <- resnew0.adj$resall.qt ## change these to keep 
    resnew0.cqt.Lw.adj[c, , ] <- resnew0.adj$res.qt
    resmeannew0.ct.Lw.adj[c, ] <- resnew0.adj$resmean.t
    for (y in 1:4) {
      if (!is.validation | is.element(y, c(1,2))) {
        resARRnew0.cq.Lwy.adj[[y]][c, ] <- unlist(resnew0.adj$resARR.q.Ly[[y]])
      }
    }
  } # if special constant
    ## end of add by DJS 2017-05-26
    
    # for results with full data set
    if (is.validation) {
      resfull <- CalculateQuantities(u5temp.tj = u5fulltemp.tj,
                                     alpha.pj = alphafull.cpj[c, , ],
                                     B.tp = B.ctp[c, , ],
                                     Kproject = Kproject,
                                     iso = mcmc.meta$data$iso.c[c],
                                     indicator.type = indicator.type, runname.U5MR = runname.U5MR, # change JR, 21 Jun
                                     hiv = mcmc.meta$data$hiv.c[c], hiv.file = hiv.file, # change JR, 30 May
                                     crisisadj = mcmc.meta$data$crisisadj.c[c], adj.file = adj.file,
                                     year.t = year.t, year.i = year.i,
                                     estyear.min = ifelse(mcmc.meta$data$iso.c[c] == "RUS", 1970.5, 1990.5), # change JR, 9 Jun
                                     year1 = year1, year2 = year2, year3 = year3, 
                                     year4 = year4.input,
                                     percentiles = percentiles)
      u5full.ctj[c, , ] <- resfull$u5.tj
    }
    # for other weights.alpha
    for (w in 1:nweights) {
      resnew <- CalculateQuantities(alpha.pj = eval(parse(text = paste0("alphanew", w, ".cpj[c, , ]"))),
                                    B.tp = B.ctp[c, , ],
                                    Kproject = Kproject,
                                    iso = mcmc.meta$data$iso.c[c],
                                    indicator.type = indicator.type, runname.U5MR = runname.U5MR,
                                    hiv = mcmc.meta$data$hiv.c[c], hiv.file = hiv.file,
                                    crisisadj = mcmc.meta$data$crisisadj.c[c], adj.file = adj.file,
                                    year.t = year.t, year.i = year.i,
                                    estyear.min = ifelse(mcmc.meta$data$iso.c[c] == "RUS", 1970.5, 1990.5),
                                    year1 = year1, year2 = year2, year3 = year3, 
                                    year4 = year4.input,
                                    percentiles = percentiles)
      eval(parse(text = paste0("res.alphanew", w, ".cp[c, ] <- resnew$res.alpha.p")))
      eval(parse(text = paste0("u5new", w, ".ctj[c, , ] <- resnew$u5.tj")))
      resall.cqt.Lw[[paste(weights.alpha[w])]][c, , ] <- resnew$resall.qt
      res.cqt.Lw[[paste(weights.alpha[w])]][c, , ] <- resnew$res.qt
      resmean.ct.Lw[[paste(weights.alpha[w])]][c, ] <- resnew$resmean.t
      for (y in 1:4) {
        if(!is.validation | is.element(y, c(1,2)))
          resARR.cq.Lwy[[paste(weights.alpha[w])]][[y]][c, ] <- unlist(resnew$resARR.q.Ly[[y]])
      }
    } # end w loop
    
    ## edit DJS 2017-06-07 to keep U5 flat for use in IMR in Syria and Yemen
    if(!is.null(mcmc.meta$settings$special.constant.list)){
      num.u5new <- which(weights.alpha==weight.alpha.publish)
      eval(parse(text = paste0("u5new", num.u5new, ".ctj[c, , ] <- resnew0.adj$u5.tj")))
    }
    
    if (quick.plot.check) {
      u.i <- c(unlist(mcmc.meta$data$u.Lcs.j[[c]]), unlist(mcmc.meta$data$uvr.Lc.j[[c]]))
      plot(u.i ~ year.i, main = mcmc.meta$data$name.c[c], 
           ylim = c(0, min(max(res.cqt.Lw[[paste0(weights.alpha[1])]][c, , ], u.i, na.rm = T))), 
           xlim = c(min(year.i, na.rm = T), max(year.i, year.t, na.rm = T)))
      for (q in 1:length(percentiles))
        lines(res.cqt.Lw[['0']][c, q, ] ~ year.t)
      for (w in 1:nweights) {
        for (q in 1:length(percentiles))
          lines(res.cqt.Lw[[paste0(weights.alpha[w])]][c, q, ] ~ year.t, col = w+1)
      }
    } # end quick.plot.check
  } # end country loop
  #dev.off()
  
  # output results.csv file
  if(!is.null(mcmc.meta$settings$special.constant.list)){ ## edit DJS 20170526
    res.cqt.Lw.unadj <- res.cqt.Lw
    res.cqt.Lw[[paste0(weight.alpha.publish)]] <- resnew0.cqt.Lw.adj # get UI from resnew0.cqt.Lw.adj
    res.cqt.Lw[[paste0(weight.alpha.publish)]][,percentiles==0.5,] <- res.cqt.Lw.flat[,percentiles==0.5,] # get flat median from res.cqt.Lw.flat[,2,]
    OutputResultsWide(res.cqt = res.cqt.Lw[[paste0(weight.alpha.publish)]],
                      name.c = mcmc.meta$data$name.c,
                      iso.c = mcmc.meta$data$iso.c,
                      year.t = year.t,
                      indicator.type = indicator.type,
                      year.start = 1931.5,
                      year.end = year.end,
                      output.dir = output.dir,
                      file.name = "Results")
  } else {
  OutputResultsWide(res.cqt = res.cqt.Lw[[paste0(weight.alpha.publish)]],
                    name.c = mcmc.meta$data$name.c,
                    iso.c = mcmc.meta$data$iso.c,
                    year.t = year.t,
                    indicator.type = indicator.type,
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results")
  
  }
  # output results.txt file for CME Info
  if (run.type == "country") {
    results <- OutputResultsLong(res.qt = res.cqt.Lw[[paste0(weight.alpha.publish)]][1, , ], 
                                 year.t = year.t, year.start = year.start, year.end = year.end,
                                 output.dir = output.dir)
  } else {
    results <- NULL
  }
  # save all quantities
  name.append <- ifelse(is.validation.up.to.2000, "(upto2000).rda", ".rda")
  if (!load.alpha.cpj) {
    iso.c <- mcmc.meta$data$iso.c
    name.c <- mcmc.meta$data$name.c
    save(iso.c, file = file.path(output.dir, "iso.c.rda"))
    save(name.c, file = file.path(output.dir, "name.c.rda"))
    rm(iso.c); rm(name.c)
    save(year.t, file = file.path(output.dir, "year.t.rda"))
    save(resproject.list.c, file = file.path(output.dir, "resproject.list.c.rda"))
    save(res.alpha.cp, file = file.path(output.dir, "res.alpha.cp.rda"))
    save(alpha.cpj, file = file.path(output.dir, "alpha.cpj.rda"))
    save(alphatemp.cpj, file = file.path(output.dir, "alphatemp.cpj.rda"))
    if(!is.null(mcmc.meta$settings$special.constant.list)){ ## edit DJS 20170526
      save(alphanew0.pj.adj, file = file.path(output.dir, "alphanew0.pj.adj.rda"))
      } 
  }
  save(u5.ctj, file = file.path(output.dir, paste0("u5.ctj", name.append)))
  save(res.cqt.Lw, file = file.path(output.dir, paste0("res.cqt.Lw", name.append)))
  save(resall.cqt.Lw, file = file.path(output.dir, paste0("resall.cqt.Lw", name.append)))
  save(resmean.ct.Lw, file = file.path(output.dir, paste0("resmean.ct.Lw", name.append)))
  save(resARR.cq.Lwy, file = file.path(output.dir, paste0("resARR.cq.Lwy", name.append)))
  if (is.validation)
    save(u5full.ctj, file = file.path(output.dir, paste0("u5full.ctj", name.append)))
  for (w in 1:nweights) {
    eval(parse(text = paste0("save(alphanew", w, ".cpj, file = file.path(output.dir, \"alphanew", 
                             w, ".cpj", name.append, "\"))")))
    eval(parse(text = paste0("save(u5new", w, ".ctj, file = file.path(output.dir, \"u5new", 
                             w, ".ctj", name.append, "\"))")))
  }
 
  if(!is.null(mcmc.meta$settings$special.constant.list)){ ## edit DJS 20170526
    save(res.cqt.Lw.unadj, file = file.path(output.dir, "res.cqt.Lw.unadj.rda"))
    save(resARRnew0.cq.Lwy.adj, file = file.path(output.dir, "resARRnew0.cq.Lwy.adj.rda"))
    save(u5.ctj.flat, file = file.path(output.dir, "u5.ctj.flat.rda"))
  } 
  
  # get HIV-removed and HIV-removed & log scale results
  # change JR, 20150602: get crisis-removed and crisis-and-HIV-removed results
  # start with final results
  res.hivremoved.cqt.Lw <- res.crisisremoved.cqt.Lw <- res.crisisandhivremoved.cqt.Lw <- 
    res.logscale.hivremoved.cqt.Lw <- res.cqt.Lw
  for (w in 1:nweightsplus1) {
    weight.alpha <- weights.alpha.plusdefault[w]
    # undo crisis post-adjustment for crisisadj countries
    for (c in (1:mcmc.meta$data$C)[mcmc.meta$data$crisisadj.c]) {
      u.median.t <- res.cqt.Lw[[paste0(weight.alpha)]][c, percentiles == 0.5, ]
      # get crisis-free results that include only HIV adjustments
      propadj.t <- GetCrisisAdjustedEstimates(u.t = u.median.t, year.t = year.t, 
                                              iso = mcmc.meta$data$iso.c[c],
                                              operation = "-",
                                              adj.file = adj.file)$propadj.t
      res.crisisandhivremoved.cqt.Lw[[paste0(weight.alpha)]][c, , ] <- 
        res.crisisremoved.cqt.Lw[[paste0(weight.alpha)]][c, , ] <- 
        t(apply(res.cqt.Lw[[paste0(weight.alpha)]][c, , ], 1, "*", propadj.t))
    }
    # undo HIV post-adjustment for countries with high HIV prevalence  
    for (c in (1:mcmc.meta$data$C)[mcmc.meta$data$hiv.c]) {
      # change JR, 20150602: fixed bug: assume relative uncertainty in 
      # unadjusted U5MR equal to relative uncertainty in adjusted U5MR.
      # relative adjustment was not correctly carried out before this date for res files.
      # get HIV-free results that include only crisis adjustments
      u.median.t <- res.cqt.Lw[[paste0(weight.alpha)]][c, percentiles == 0.5, ]
      propadjhiv.t <- GetHIVAdjustedEstimates(u.t = u.median.t, year.t = year.t, 
                                              iso = mcmc.meta$data$iso.c[c],
                                              operation = "-",
                                              hiv.file = hiv.file)$propadjhiv.t
      res.hivremoved.cqt.Lw[[paste0(weight.alpha)]][c, , ] <- 
        t(apply(res.cqt.Lw[[paste0(weight.alpha)]][c, , ], 1, "*", propadjhiv.t))
      # get HIV and crisis-free results
      u.median2.t <- res.crisisremoved.cqt.Lw[[paste0(weight.alpha)]][c, percentiles == 0.5, ]
      propadjhiv2.t <- GetHIVAdjustedEstimates(u.t = u.median2.t, year.t = year.t, 
                                               iso = mcmc.meta$data$iso.c[c],
                                               operation = "-",
                                               hiv.file = hiv.file)$propadjhiv.t
      res.crisisandhivremoved.cqt.Lw[[paste0(weight.alpha)]][c, , ] <-
        t(apply(res.crisisremoved.cqt.Lw[[paste0(weight.alpha)]][c, , ], 1, "*", propadjhiv2.t))
    }
    res.logscale.hivremoved.cqt.Lw[[paste0(weight.alpha)]] <- 
      log(res.hivremoved.cqt.Lw[[paste0(weight.alpha)]])
  }
  save(res.hivremoved.cqt.Lw, 
       file = file.path(output.dir, paste0("res.hivremoved.cqt.Lw", name.append)))
  save(res.crisisremoved.cqt.Lw, 
       file = file.path(output.dir, paste0("res.crisisremoved.cqt.Lw", name.append)))
  save(res.crisisandhivremoved.cqt.Lw, 
       file = file.path(output.dir, paste0("res.crisisandhivremoved.cqt.Lw", name.append)))
  save(res.logscale.hivremoved.cqt.Lw, 
       file = file.path(output.dir, paste0("res.logscale.hivremoved.cqt.Lw", name.append)))
  OutputResultsWide(res.cqt = res.hivremoved.cqt.Lw[[paste0(weight.alpha.publish)]],
                    name.c = mcmc.meta$data$name.c,
                    iso.c = mcmc.meta$data$iso.c,
                    year.t = year.t,
                    indicator.type = indicator.type,
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results (HIV-free)")
  OutputResultsWide(res.cqt = res.crisisremoved.cqt.Lw[[paste0(weight.alpha.publish)]],
                    name.c = mcmc.meta$data$name.c,
                    iso.c = mcmc.meta$data$iso.c,
                    year.t = year.t,
                    indicator.type = indicator.type,
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results (crisis-free)")
  OutputResultsWide(res.cqt = res.crisisandhivremoved.cqt.Lw[[paste0(weight.alpha.publish)]],
                    name.c = mcmc.meta$data$name.c,
                    iso.c = mcmc.meta$data$iso.c,
                    year.t = year.t,
                    indicator.type = indicator.type,
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results (crisis-and-HIV-free)")
  # new obs PIs
  if (!load.alpha.cpj & !is.validation.up.to.2000) {
    GetResiduals(runname = runname, output.dir = output.dir, 
                 percentiles = percentiles)
    # change JR, 20131126: added crisis adjustment
    GetNewObservationPIs(runname = runname, output.dir = output.dir) # get expypredict.ciq 
  } # end new obs PIs loop
  ##value<< \code{NULL}; Saves all results to \code{output.dir}.
  return(results)
  
  if(!is.null(mcmc.meta$settings$special.constant.list)){ ## edit DJS 20170526
  res.hivremoved.cqt.Lw.unadj <- res.crisisremoved.cqt.Lw.unadj <- res.crisisandhivremoved.cqt.Lw.unadj <- 
      res.logscale.hivremoved.cqt.Lw.unadj <- res.cqt.Lw.unadj
    for (w in 1:nweightsplus1) {
      weight.alpha <- weights.alpha.plusdefault[w]
      # undo crisis post-adjustment for crisisadj countries
      for (c in (1:mcmc.meta$data$C)[mcmc.meta$data$crisisadj.c]) {
        u.median.t <- res.cqt.Lw.unadj[[paste0(weight.alpha)]][c, percentiles == 0.5, ]
        # get crisis-free results that include only HIV adjustments
        propadj.t <- GetCrisisAdjustedEstimates(u.t = u.median.t, year.t = year.t, 
                                                iso = mcmc.meta$data$iso.c[c],
                                                operation = "-",
                                                adj.file = adj.file)$propadj.t
        res.crisisandhivremoved.cqt.Lw.unadj[[paste0(weight.alpha)]][c, , ] <- 
          res.crisisremoved.cqt.Lw.unadj[[paste0(weight.alpha)]][c, , ] <- 
          t(apply(res.cqt.Lw.unadj[[paste0(weight.alpha)]][c, , ], 1, "*", propadj.t))
      }
      # undo HIV post-adjustment for countries with high HIV prevalence  
      for (c in (1:mcmc.meta$data$C)[mcmc.meta$data$hiv.c]) {
        # change JR, 20150602: fixed bug: assume relative uncertainty in 
        # unadjusted U5MR equal to relative uncertainty in adjusted U5MR.
        # relative adjustment was not correctly carried out before this date for res files.
        # get HIV-free results that include only crisis adjustments
        u.median.t <- res.cqt.Lw.unadj[[paste0(weight.alpha)]][c, percentiles == 0.5, ]
        propadjhiv.t <- GetHIVAdjustedEstimates(u.t = u.median.t, year.t = year.t, 
                                                iso = mcmc.meta$data$iso.c[c],
                                                operation = "-",
                                                hiv.file = hiv.file)$propadjhiv.t
        res.hivremoved.cqt.Lw.unadj[[paste0(weight.alpha)]][c, , ] <- 
          t(apply(res.cqt.Lw.unadj[[paste0(weight.alpha)]][c, , ], 1, "*", propadjhiv.t))
        # get HIV and crisis-free results
        u.median2.t <- res.crisisremoved.cqt.Lw.unadj[[paste0(weight.alpha)]][c, percentiles == 0.5, ]
        propadjhiv2.t <- GetHIVAdjustedEstimates(u.t = u.median2.t, year.t = year.t, 
                                                 iso = mcmc.meta$data$iso.c[c],
                                                 operation = "-",
                                                 hiv.file = hiv.file)$propadjhiv.t
        res.crisisandhivremoved.cqt.Lw.unadj[[paste0(weight.alpha)]][c, , ] <-
          t(apply(res.crisisremoved.cqt.Lw.unadj[[paste0(weight.alpha)]][c, , ], 1, "*", propadjhiv2.t))
      }
      res.logscale.hivremoved.cqt.Lw.unadj[[paste0(weight.alpha)]] <- 
        log(res.hivremoved.cqt.Lw.unadj[[paste0(weight.alpha)]])
    }
    save(res.hivremoved.cqt.Lw.unadj, 
         file = file.path(output.dir, paste0("res.hivremoved.cqt.Lw.unadj", name.append)))
    save(res.crisisremoved.cqt.Lw.unadj, 
         file = file.path(output.dir, paste0("res.crisisremoved.cqt.Lw.unadj", name.append)))
    save(res.crisisandhivremoved.cqt.Lw.unadj, 
         file = file.path(output.dir, paste0("res.crisisandhivremoved.cqt.Lw.unadj", name.append)))
    save(res.logscale.hivremoved.cqt.Lw.unadj, 
         file = file.path(output.dir, paste0("res.logscale.hivremoved.cqt.Lw.unadj", name.append)))
  } # if special constant
}
#----------------------------------------------------------------------
CalculateARR <- function( # Calculate ARR, the annual rate of reduction
  u5mr, ##<< U5MR estimates, can be in the form of a vector or a matrix/data frame
  years, ##<< Vector of years.
  year.start, ##<< Start year for period
  year.end ##<< End year for period
) {
  if (is.vector(u5mr)) {
    arr <- 1/(year.end-year.start)*log(u5mr[years == year.end]/u5mr[years == year.start])*-100
  } else if (is.matrix(u5mr) | is.data.frame(u5mr)) {
    if (dim(u5mr)[1] == length(years)) {
      arr <- 1/(year.end-year.start)*log(u5mr[years == year.end, ]/u5mr[years == year.start, ])*-100
    } else if (dim(u5mr)[2] == length(years)) {
      arr <- 1/(year.end-year.start)*log(u5mr[, years == year.end]/u5mr[, years == year.start])*-100
    } else {
      arr <- NULL
    }
  } else {
    arr <- NULL
  }
  return(arr)
}
#----------------------------------------------------------------------
CalculateDecline <- function( # Calculate percentage decline.
  u5mr, ##<< U5MR estimates, can be in the form of a vector or a matrix/data frame
  years, ##<< Vector of years.
  year.start, ##<< Start year for period
  year.end ##<< End year for period
) {
  if (is.vector(u5mr)) {
    decline <- (u5mr[years == year.end] - u5mr[years == year.start])/u5mr[years == year.start]*-100
  } else if (is.matrix(u5mr) | is.data.frame(u5mr)) {
    if (dim(u5mr)[1] == length(years)) {
      decline <- (u5mr[years == year.end, ] - u5mr[years == year.start, ])/u5mr[years == year.start, ]*-100
    } else if (dim(u5mr)[2] == length(years)) {
      decline <- (u5mr[, years == year.end] - u5mr[, years == year.start])/u5mr[, years == year.start]*-100
    } else {
      decline <- NULL
    }
  } else {
    decline <- NULL
  }
  return(decline)
}
#----------------------------------------------------------------------
DoBayesianMelding <- function(# Do Bayesian melding for global and country alphas.
  weight.alpha, 
  m, ##<< Bayesian melding mean parameter.
  sd, ##<< Bayesian melding sd parameter.
  a.j, ##<< Posterior samples of a.
  kstart, ##<< k index just before the one to start melding from.
  Kproject, ##<< Kproject.
  alpha.pj #<< Posterior (unmelded) samples of alpha.
) {
  alphanew.pj <- alpha.pj
  sigma.j <- exp(a.j)
  sigmanew.j <- sigma.j
  for (k in (kstart+1):Kproject) {
    dnew.j <- (-alphanew.pj[k-2, ] + alphanew.pj[k-1, ])
    sigmanew.j <- sqrt((1-weight.alpha)*sigmanew.j^2 + weight.alpha*sd^2)
    alphanew.pj[k, ] <- alphanew.pj[k-1, ] + rnorm(length(a.j), 
                                                   (1-weight.alpha)*dnew.j + weight.alpha*m,
                                                   sigmanew.j)
  }  
  ##value<<
  return(alphanew.pj) ##<< Posterior (melded) samples of alpha.
}
#----------------------------------------------------------------------
CalculateQuantities <- function( # Calculate relevant quantities such as quantiles of estimates and ARR.
  u5temp.tj = NULL, ##<< Posterior samples of U5MR/IMR.
  alpha.pj = NULL, ##<< Posterior samples of alpha, required if \code{u5temp.tj} is \code {NULL}.
  B.tp = NULL, ##<< Matrix of B-splines, required if \code{u5temp.tj} is \code {NULL}.
  Kproject = NULL, ##<< Kproject, required if \code{u5temp.tj} is \code {NULL}.
  iso, ##<< ISO country code.
  indicator.type, ##<< Indicator type. # change JR, 21 Jun
  runname.U5MR = NULL, ##<< Required if \code{indicator.type} is \code{U5MR}. # change JR, 21 Jun
  hiv, ##<< HIV adjustment required? # change JR, 30 May
  hiv.file, ##<< File path to HIV adjustment file. # change JR, 30 May
  crisisadj, ##<< Crisis adjustment required?
  adj.file, ##<< File path to crisis adjustment file.
  year.t, ##<< Years of estimation.
  year.i, ##<< Observation years.
  estyear.min = 1990.5, ##<< Minimum year for which estimates are required.
  year1 = 1990.5, ##<< First year used for ARR calculation.
  year2 = 2000.5, ##<< Second year used for ARR calculation.
  year3 = 2005.5, ##<< Third year used for ARR calculation, usually last year of estimation for validation exercise.
  year4 = 2013.5, ##<< Last year used for ARR calculation, usually last year of estimation.
  percentiles = c(0.05, 0.5, 0.95) ##<< Percentiles for 90% UIs.
) {
  if (is.null(u5temp.tj)) {
    # alphas are NA before first observation year
    # first set these alphas to zero to get U, and then set U to NA for missing years
    alpha.pj[is.na(alpha.pj[, 1]), ] <- 0
    if (indicator.type == "U5MR") {
      u5temp.tj <- exp(apply(t(alpha.pj[1:Kproject , ]), 1, "%*%", t(B.tp[, 1:Kproject])))
    } else {
      logittemp.tj <- apply(t(alpha.pj[1:Kproject , ]), 1, "%*%", t(B.tp[, 1:Kproject]))
      q5hat.t <- Getq5Estimates(iso = iso, years = year.t, runname = runname.U5MR,
                                printWarnings = F)
      u5temp.tj <- exp(logittemp.tj)*q5hat.t/(exp(logittemp.tj)+1)
    }
  }
  # U5MR cannot exceed 1000
  u5temp.tj[u5temp.tj > 1000] <- 1000
  u.median.t <- apply(u5temp.tj, 1, quantile, 0.5) #, na.rm = T)
  # HIV post-adjustment for countries with high HIV prevalence
  if (hiv) {
    propadjhiv.t <- GetHIVAdjustedEstimates(u.t = u.median.t, 
                                            year.t = year.t, iso = iso,
                                            hiv.file = hiv.file)$propadjhiv.t
    u5temp2.tj <- apply(u5temp.tj, 2, "*", propadjhiv.t) # change to multiply the matrixes
  } else {
    u5temp2.tj <- u5temp.tj
  }
  u.median2.t <- apply(u5temp2.tj, 1, quantile, 0.5)#, na.rm = T)
  # crisis post-adjustment for crisisadj countries
  if (crisisadj) {
    propadj.t <- GetCrisisAdjustedEstimates(u.t = u.median2.t, 
                                            year.t = year.t, iso = iso,
                                            adj.file = adj.file)$propadj.t
    u5.tj <- apply(u5temp2.tj, 2, "*", propadj.t)
  } else {
    u5.tj <- u5temp2.tj
  }
  if (!is.null(alpha.pj)) {
    res.alpha.p <- apply(alpha.pj, 1, median)
  } else {
    res.alpha.p <- NULL
  }
  resall.qt <- apply(u5.tj, 1, quantile, percentiles, na.rm = T)  
  u5.tj[year.t < min(estyear.min, floor(year.i)-0.5, na.rm = T),] <- NA
  res.qt <- apply(u5.tj, 1, quantile, percentiles, na.rm = T)
  resmean.t <- apply(u5.tj, 1, mean, na.rm = T)
  # correct for mean(vector of NA's, na.rm = T) = NaN
  resmean.t <- ifelse(rowSums(!is.na(u5.tj)) == 0, NA, resmean.t)
  #  calculate ARRs
  ARR.year1.year3.j <- CalculateARR(u5.tj, year.t, year1, year3)
  ARR.year1.year2.j <- CalculateARR(u5.tj, year.t, year1, year2)
  resARR.year1.year3.q <- quantile(ARR.year1.year3.j, percentiles, na.rm = T)
  resARR.year1.year2.q <- quantile(ARR.year1.year2.j, percentiles, na.rm = T)
  if (!is.null(year4)) {
    ARR.year2.year4.j <- CalculateARR(u5.tj, year.t, year2, year4)
    ARR.year1.year4.j <- CalculateARR(u5.tj, year.t, year1, year4)
    resARR.year2.year4.q <- quantile(ARR.year2.year4.j, percentiles, na.rm = T)
    resARR.year1.year4.q <- quantile(ARR.year1.year4.j, percentiles, na.rm = T)
  } else {
    resARR.year2.year4.q <- resARR.year1.year4.q <- NULL
  }
  resARR.q.Ly <- list(resARR.year1.year3.q = resARR.year1.year3.q,
                      resARR.year1.year2.q = resARR.year1.year2.q,
                      resARR.year2.year4.q = resARR.year2.year4.q,
                      resARR.year1.year4.q = resARR.year1.year4.q)
  ##value<< List containing:
  return(list(res.alpha.p = res.alpha.p, ##<< Estimated values of alpha.
              u5.tj = u5.tj, ##<< Posterior samples of U5MR/IMR for year t with adjustments if needed.
              resall.qt = resall.qt, ##<< Estimated quantiles (full).
              res.qt = res.qt, ##<< Estimated quantiles (from first observation year).
              resmean.t = resmean.t, ##<< Estimated mean fit.
              resARR.q.Ly = resARR.q.Ly ##<< List of quantiles of ARR.
  ))
}
#----------------------------------------------------------------------
OutputResultsLong <- function(# Output results in long format
  res.qt, ##<< Results.
  year.t, ##<< Years of estimation.
  year.start = NULL, ##<< Earliest year of estimation to publish.
  year.end = NULL, ##<< Latest year of estimation to publish.
  output.dir = NULL ##<< Optional: File directory to output results.txt.
) {
  res.qt.for.output <- res.qt
  if (!is.null(year.start)) {
    if (year.start > min(year.t)) {
      year.t.output <- year.t[year.t >= year.start]
      res.qt.for.output[, year.t < year.start] <- NA
    } else {
      year.t.output <- year.t
    }
  } else {
    year.t.output <- year.t
  }
  if (!is.null(year.end)) {
    if (year.end < max(year.t)) {
      year.t.output <- year.t.output[year.t.output <= year.end]
      res.qt.for.output[, year.t < year.start] <- NA
    } else {
      year.t.output <- year.t.output
    }
  } else {
    year.t.output <- year.t.output
  }
  res.qt.for.output.final <- res.qt.for.output[, is.element(year.t, year.t.output)]  
  results <- data.frame(Year = year.t.output, 
                        Estimate = roundoff(res.qt.for.output.final[2,  ], digits = 1), # change JR, 22 Aug 2013: round to roundoff
                        Lower.bound = roundoff(res.qt.for.output.final[1, ], digits = 1), # change JR, 22 Aug 2013: round to roundoff
                        Upper.bound = roundoff(res.qt.for.output.final[3, ], digits = 1)) # change JR, 22 Aug 2013: round to roundoff
  results <- results[!is.na(results$Estimate), ]
  # if (!is.null(output.dir))
  # write.table(results, file = file.path(output.dir, "results.txt"), row.names = F)
  ##value<< Results in long format
  return(results)
}
#----------------------------------------------------------------------
OutputResultsWide <- function(# Output results in wide format
  res.cqt, ##<< Results.
  name.c, ##<< Country name
  iso.c, ##<< Country ISO code
  year.t, ##<< Years of estimation.
  indicator.type, ##<< Indicator type
  year.start = NULL, ##<< Earliest year of estimation to publish.
  year.end = NULL, ##<< Latest year of estimation to publish.
  output.dir = NULL, ##<< File directory to output CSV file.
  file.name = "Results" ##<< File name of CSV file.
) {
  results.output <- NULL
  for (c in 1:length(iso.c)) {
    results <- OutputResultsLong(res.qt = res.cqt[c, , ], 
                                 year.t = year.t, year.start = year.start, year.end = year.end,
                                 output.dir = output.dir)
    est.years.output <- year.start:year.end
    est <- UI.lower <- UI.upper <- rep(NA, length(est.years.output))
    est[is.element(est.years.output, results$Year)] <- results$Estimate
    UI.lower[is.element(est.years.output, results$Year)] <- results$Lower.bound
    UI.upper[is.element(est.years.output, results$Year)] <- results$Upper.bound
    results.output <- rbind(results.output, UI.lower, est, UI.upper)
  }
  Q <- dim(res.cqt)[2]
  rownames(results.output) <- NULL
  results.final <- cbind(data.frame(rep(name.c, each = Q), 
                                    rep(iso.c, each = Q),
                                    rep(c("Lower", "Median", "Upper"), length(iso.c)),
                                    rep(ifelse(indicator.type == "U5MR", "Under-five Mortality Rate", 
                                               "Infant Mortality Rate"), Q*length(iso.c)),
                                    rep("Total", Q*length(iso.c))),
                          results.output)
  colnames(results.final) <- c("Country Name", "ISO Code", "Quantile", "Indicator", "Subgroup", 
                                est.years.output)
  if (!is.null(output.dir))
    write.csv(results.final, file = file.path(output.dir, paste0(file.name, ".csv")), 
              row.names = F, na = "")
  ##value<< Results in wide format
  return(results.final)
}
#----------------------------------------------------------------------
GetNewObservationPIs <- function( # Get new observation PIs for plotting
  runname = "test", ##<< Run name.
  output.dir = NULL ##<< Output directory to save files to. If \code{NULL}, {output/runname} is used.
) {
  if (is.null(output.dir))
    output.dir <- file.path("output", runname)
  load(file.path(output.dir, "mcmc.meta.rda"))
  load(file.path(output.dir, "ypredict.hivremoved.ciq.rda"))
  list2env(mcmc.meta$settings, envir = environment())
  hiv.file <- mcmc.meta$files$hiv.file
  adj.file <- mcmc.meta$files$adj.file
  if (is.null(mcmc.meta$settings$periods.constant.list)) # for back compatibility with older runs
    periods.constant.list <- NULL
  expypredict.ciq <- exp(ypredict.hivremoved.ciq)
  C <- mcmc.meta$data$C
  Q <- dim(ypredict.hivremoved.ciq)[3]
  
  for (c in 1:C) {
    year.i <- c(unlist(mcmc.meta$data$year.Lcs.j[[c]]), unlist(mcmc.meta$data$yearvr.Lc.j[[c]]))
    # HIV post-adjustment for countries with high HIV prevalence
    if (mcmc.meta$data$hiv.c[c]) {
      # change JR, 20150602: fixed bug: assume relative uncertainty in 
      # unadjusted U5MR equal to relative uncertainty in adjusted U5MR.
      # relative adjustment was not correctly carried out before this date for expypredict.ciq files.
      u.median.t <- expypredict.ciq[c, 1:mcmc.meta$data$n.c[c], 2]
      for (q in 1:Q) {
        expypredict.ciq[c, 1:mcmc.meta$data$n.c[c], q] <- 
          expypredict.ciq[c, 1:mcmc.meta$data$n.c[c], q]*
          GetHIVAdjustedEstimates(
            u.t = u.median.t,
            year.t = year.i, 
            iso = mcmc.meta$data$iso.c[c],
            hiv.file = hiv.file)$propadjhiv.t
      }
    }
    # crisis post-adjustment for crisisadjfordata.c countries (because crisis q was subtracted off data earlier)
    if (mcmc.meta$data$crisisadjfordata.c[c]) {
      # change JR, 20150602: fixed bug: assume relative uncertainty in 
      # unadjusted U5MR equal to relative uncertainty in adjusted U5MR.
      # relative adjustment was not correctly carried out before this date for expypredict.ciq files.
      u.median2.t <- expypredict.ciq[c, 1:mcmc.meta$data$n.c[c], 2]
      for (q in 1:Q) {
        expypredict.ciq[c, 1:mcmc.meta$data$n.c[c], q] <- 
          expypredict.ciq[c, 1:mcmc.meta$data$n.c[c], q]*
          GetCrisisAdjustedEstimates(
            u.t = u.median2.t,
            year.t = year.i, 
            iso = mcmc.meta$data$iso.c[c],
            adj.file = adj.file)$propadj.t
      }
    }
  } # end country loop  
  save(expypredict.ciq, file = file.path(output.dir, "expypredict.ciq.rda"))
  ##value<< \code{NULL}.
  return(invisible())
}
