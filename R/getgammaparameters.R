#----------------------------------------------------------------------
# getgammaparameters.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
GetGammaParameters <- function( # Get median and sd of global gamma (differences of adjacent alphas) distribution.
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored and new objects will be added.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  is.validation.up.to.2000 = FALSE ##<< Logical value indicating whether or not to use only alphas before year 2000.
) {
  # Inputs
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  load(file.path(output.dir, "mcmc.meta.rda"))
  load(file.path(output.dir, "mcmc.array.rda"))
  list2env(mcmc.meta$settings, envir = environment())
  
  if (!file.exists(file.path(output.dir, "alphatemp.cpj.rda")))
    GetAlphas(runname = runname, output.dir = output.dir)
  load(file.path(output.dir, "alphatemp.cpj.rda"))  
  
  name.append <- ifelse(is.validation.up.to.2000, "(upto2000).rda", ".rda")
  if (is.validation.up.to.2000) {
    # minimum start year is 1990.5, 1970.5 for Russia
    year.t <- seq(floor(min(ifelse(is.element("RUS", mcmc.meta$data$iso.c), 1971, 1986), 
                            mcmc.meta$data$minyear.c, na.rm = T))-0.5, year.lastestimate)
    for (c in 1:mcmc.meta$data$C) {
      year.i <- c(unlist(mcmc.meta$data$year.Lcs.j[[c]]), unlist(mcmc.meta$data$yearvr.Lc.j[[c]]))
      year.min <- floor(min(ifelse(mcmc.meta$data$iso.c[c] == "RUS", 1971, 1986), 
                            mcmc.meta$data$minyear.c[c], na.rm = T))-0.5
      resobs <- GetSplines(years.t = c(year.min, year.i), I = I,
                           years.combine = periods.constant.list)
      resproject <- GetSplines(years.t = year.t, I = I,
                               years.combine = periods.constant.list,
                               year0 = resobs$alphayears.k[1]) # such that knot placement is the same
      Kproject <- length(resproject$alphayears.k)
      # set alpha to NA for alphas corresponding to years in and after the year 2000 for validation
      kstart <- max(which(resproject$alphayears.k < 2000))
      alphatemp.cpj[c, (kstart+1):Kproject, ] <- NA
    } # end country loop
    save(alphatemp.cpj, file = file.path(output.dir, paste0("alphatemp.cpj", name.append)))  
  } # end is.validation.up.to.2000 loop
  # get gammas (differences between adjacent alphas)
  gamma.ckj <- (alphatemp.cpj[, -1, ] - alphatemp.cpj[, -(dim(alphatemp.cpj)[2]), ])
  gamma.ck <- apply(gamma.ckj, c(1, 2), median, na.rm = T)
  # save
  save(gamma.ckj, file = file.path(output.dir, paste0("gamma.ckj", name.append)))
  save(gamma.ck, file = file.path(output.dir, paste0("gamma.ck", name.append)))
    
  # get parameters using posterior medians
  m <- median(c(gamma.ck), na.rm = T)
  sd <- sd(c(gamma.ck), na.rm = T)
  cat(paste0("Global distribution of changes in alpha has median m = ", 
             m, " and sd = ", sd, "\n"))
  
  # save to mcmc.meta
  mcmc.meta$settings$global.gamma.median <- m
  mcmc.meta$settings$global.gamma.sd <- sd 
  save(mcmc.meta, file = file.path(output.dir, "mcmc.meta.rda"))
  cat(paste0("The global median and sd have been saved to ", output.dir, "mcmc.meta$settings"))
  ##value<< List with median and sd of global gamma distribution
  return(list(m = m, sd = sd))
}
#----------------------------------------------------------------------
GetAlphas <- function( # Get alphas.
  runname = "test", ##<< Run name.
  output.dir = NULL ##<< Directory where mcmc.meta and raw MCMC output are stored and new objects will be added.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
) {
  # Inputs
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  load(file.path(output.dir, "mcmc.meta.rda"))
  load(file.path(output.dir, "mcmc.array.rda"))
  list2env(mcmc.meta$settings, envir = environment())
  
  C <- mcmc.meta$data$C
  # minimum start year is 1990.5, 1970.5 for Russia
  year.t <- seq(floor(min(ifelse(is.element("RUS", mcmc.meta$data$iso.c), 1971, 1986), 
                          mcmc.meta$data$minyear.c, na.rm = T))-0.5, year.lastestimate)
  # to make sure first and last observation year is within year.t for all countries
  nyears <- length(year.t)
  P <- ceiling(length(year.t)/I)+5+1
  B.ctp <- array(NA, c(C, nyears, P)) 
  nsim <- prod(dim(mcmc.array)[1:2])
  a.cj <- array(NA, c(C, nsim))
  u5.ctj <- array(NA, c(C, nyears, nsim))
  alpha.cpj <- alphatemp.cpj <- array(NA, c(C, P, nsim)) # j refers to post sample
  
  for (c in 1:C) {
    cat(paste("Getting alphas for country", c, "out of", C, ifelse(C == 1, "country", "countries"), "\n"))
    year.i <- c(unlist(mcmc.meta$data$year.Lcs.j[[c]]), unlist(mcmc.meta$data$yearvr.Lc.j[[c]]))
    year.min <- floor(min(ifelse(mcmc.meta$data$iso.c[c] == "RUS", 1971, 1986), 
                          mcmc.meta$data$minyear.c[c], na.rm = T))-0.5
    resobs <- GetSplines(years.t = c(year.min, year.i), I = I,
                         years.combine = periods.constant.list)
    # for later, to recover the alphas:
    B.ik <- resobs$B.tk[-1, ] # change JR, 20131202: remove the first column corresponding to year.min
    Ktemp <- length(resobs$alphayears.k)
    D2 <- diff(diag(Ktemp), diff = 2)
    W <- diag(Ktemp) - (t(D2)%*%solve(D2%*%t(D2))%*%D2)
    stemp <- svd(W, nu = 2, nv = 2)
    L <- stemp$u
    G <- cbind(rep(1, Ktemp), seq(1, Ktemp)-Ktemp/2) # center second column of G
    A <- qr.solve(L, G)
    resproject <- GetSplines(years.t = year.t, I = I,
                             years.combine = periods.constant.list,
                             year0 = resobs$alphayears.k[1]) # such that knot placement is the same
    Kproject <- length(resproject$alphayears.k)
    B.ctp[c, , 1:Kproject] <- resproject$B.tk
    # change to character because comparison of numerics is tricky!
    resobs$alphayears.k <- as.numeric(as.character(resobs$alphayears.k))
    resproject$alphayears.k <- as.numeric(as.character(resproject$alphayears.k)) 
    
    # recover the alphas
    j = 0 # refers to posterior sample
    for (chain in 1:dim(mcmc.array)[2]) { # change JR, 12 Aug 2013: order of j (by column instead of row!)
      for (s in 1:dim(mcmc.array)[1]) {
        j <- j + 1
        u.q <- mcmc.array[s, chain, paste0("u.cq[", c, ",", 1:mcmc.meta$jags.data$q.c[c], "]")]
        b.m <- mcmc.array[s, chain, paste0("b.cm[", c, ",", 1:2,"]")]
        # alphas need to be sorted (which they are!)
        alphatemp.cpj[c, seq(1, Kproject)[is.element(resproject$alphayears.k, resobs$alphayears.k)], j] <-
          qr.solve(a = rbind(solve(A)%*%t(L), D2), b = c(b.m, u.q))
      }
    }
  } # end country loop
  save(alphatemp.cpj, file = file.path(output.dir, "alphatemp.cpj.rda"))  
  ##value<< \code{NULL}
  return(invisible())
}
