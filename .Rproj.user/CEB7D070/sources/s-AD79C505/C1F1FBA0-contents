
output.dir <-  here::here("Aggregate results (median) 2019-08-15")
output.dir.samples <- file.path(output.dir, "samples")
output.dir.samplescombined <- file.path(output.dir, "samples_combined")

myregion <- sample(paste0("R",1:3), 196, replace = TRUE)

GetRegionalResultsBWC(regiontypes = SDGRegionAll,
                      regions = myregion,
                      filename = "myregion",
                      output.dir = output.dir, output.dir.samples = output.dir.samples,
                      output.dir.samplescombined = output.dir.samplescombined,
                      run.on.server = run.on.server,
                      percentiles = c(0.05, 0.5, 0.95), ndigits = 1, 
                      replace.rates.reg = "M49Region",                          
                      round.output = FALSE)



GetRegionalResultsBWC <- function(
  output.dir,
  output.dir.samples,
  output.dir.samplescombined,
  regions, 
  regiontypes,
  filename,
  run.on.server,
  percentiles,
  ndigits,
  replace.rates.reg,
  round.output# DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
) {
  
  cat(paste0("Generating output for ", filename, "...\n"))
  nregs <- length(regiontypes)
  regions[is.na(regions)] <- 0 # to remove NAs
  
  if(is.null(replace.rates.reg)){ # DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
    load(file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  } else {
    load(file = file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda"))) 
  }
  nsim <- dim(deathu5.ctj)[3] #change back 20170818 # 1 for median
  # ## load dx and lx arrays once so not loaded at every j
  # load(file.path(output.dir.samplescombined, paste0("dx.array.ctj.rda")))
  # load(file.path(output.dir.samplescombined, paste0("lx.array.ctj.rda")))
  # nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.rda")))
  # if(nn.exists){
  #   load(file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.rda")))
  #   load(file.path(output.dir.samplescombined, paste0("lx.nn.array.ctj.rda")))
  # }
  
  if (run.on.server) {
    # calculate once to get population arrays, because multiple chains will be running at once for parallel computing
    CalculateRegionalDeathsBWC(j = 1, output.dir.samples = output.dir.samples,
                               output.dir.samplescombined = output.dir.samplescombined,
                               regions = regions, regiontypes = regiontypes, filename = filename,
                               replace.rates.reg = replace.rates.reg)
    cat(paste0("Output generated for trajectory ", 1, " out of ", nsim,
               ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
    if (nsim > 1) {
      registerDoMC(cores = 6)
      #registerDoParallel(cores=6)
      foreach (j=1:nsim) %dopar% {
        CalculateRegionalDeathsBWC(j = j, output.dir.samples = output.dir.samples,
                                   output.dir.samplescombined = output.dir.samplescombined,
                                   regions = regions, regiontypes = regiontypes, filename = filename,
                                   replace.rates.reg = replace.rates.reg)
        cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                   ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
      }
    }
  } else {
    for (j in 1:nsim) {
      CalculateRegionalDeathsBWC(j = j, output.dir.samples = output.dir.samples,
                                 output.dir.samplescombined = output.dir.samplescombined,
                                 regions = regions, regiontypes = regiontypes, filename = filename,
                                 replace.rates.reg = replace.rates.reg)
      cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                 ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
    }
  }
  cat(paste0("Combining and outputting regional results...\n"))
  CombineAndOutputRegionalResults(output.dir = output.dir,
                                  output.dir.samples = output.dir.samples,
                                  output.dir.samplescombined = output.dir.samplescombined,
                                  regiontypes = regiontypes,
                                  filename = filename,
                                  percentiles = percentiles,
                                  ndigits = ndigits,
                                  replace.rates.reg = replace.rates.reg,
                                  round.output = round.output)
}


# sub functions -----------------------------------------------------------

CalculateRegionalDeathsBWC <- function(
  j,
  output.dir.samples,
  output.dir.samplescombined,
  regions,
  regiontypes,
  filename,
  replace.rates.reg # DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
) {

  if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
    load(file.path(output.dir.samplescombined, "info.rda"))
    list2env(info, envir = environment())
    load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
    load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
    load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
    
    nn.exists <- file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
    if(nn.exists){
      load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
    }
  } else {
    load(file.path(output.dir.samplescombined, "info.rda"))
    list2env(info, envir = environment())
    load(file.path(output.dir.samplescombined, paste0("death0.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined, paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
    
    nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    if(nn.exists){
      load(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    } 
  } # if/else  replace.rates.reg
  
  # edit DJS 2018-07-27 round deaths at country level before summing to region and world
  deathu5.ctj <- roundoff(deathu5.ctj, digits = 0)
  death0.ctj <- roundoff(death0.ctj, digits = 0)
  death1to4.ctj <- roundoff(death1to4.ctj, digits = 0)
  if(nn.exists) deathnn.ctj <- roundoff(deathnn.ctj, digits = 0)
  # edit DJS 2018-07-27
  
  nregs <- length(regiontypes)
  
  # infant and u5 deaths BWC method
  ## load dx and lx arrays once so not loaded at every j
  # load(file.path(output.dir.samplescombined, paste0("dx.array.ctj.rda")))
  # load(file.path(output.dir.samplescombined, paste0("lx.array.ctj.rda")))
  # nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.rda")))
  # if(nn.exists){
  #   load(file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.rda")))
  #   load(file.path(output.dir.samplescombined, paste0("lx.nn.array.ctj.rda")))
  # }
  # 
  # dx.array.by.c <- dx.array.ctj[,,,j]
  # lx.array.by.c <- lx.array.ctj[,,,j]
  # if(nn.exists){
  #   dx.nn.array.by.c <- dx.nn.array.ctj[,,,j]
  #   lx.nn.array.by.c <- lx.nn.array.ctj[,,,j]
  # }
  
  # nn.exists <- file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  # load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
  # load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
  # if(nn.exists){
  # load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
  # load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
  # }
  
  if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
    load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
    if(nn.exists){
      load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
      load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
    }
  } else {
    load(file.path(output.dir.samples, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
    load(file.path(output.dir.samples, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    if(nn.exists){
      load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
      load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
    }
  } # if/else is.null(replace.rates.reg)
  
  ## load country mortality rates for later death calc
  load(file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
  load(file.path(output.dir.samplescombined, "imr.ctj.rda"))
  if(nn.exists) load(file.path(output.dir.samplescombined, "nmr.ctj.rda"))
  
  nsim <- dim(u5mr.ctj)[3]
  
  # edit DJS 2018-03-09 remove rounding for median calculation
  # if (nsim == 1) {
  #   u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
  #   imr.ctj <- roundoff(imr.ctj, digits = ndigits)
  #   if(nn.exists) nmr.ctj <- roundoff(nmr.ctj, digits = ndigits)
  # }
  # end edit DJS 2018-03-09
  
  ## wgt and year matrixes for later death calc
  # wgt.mat
  weight.j.1 <- (53-(1:52))/52 #weight for mortality rate in year 1
  wgt.mat <- t(matrix(cbind(weight.j.1, 1-weight.j.1), ncol=10, nrow=52))
  if(nn.exists){
    # wgt.nmr.mat
    weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4)) #weight for mortality rate in year 1
    wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1-weight.nmr.j.1), ncol=2, nrow=52))
  }
  # years.mat
  years <- years.k <- seq(1950,1950+ncol(u5mr.ctj)-1,1)
  for(yr in years){
    ifelse(yr==1950, years.mat <- seq(yr+.5,yr+5,.5), years.mat <- cbind(years.mat, seq(yr+.5,yr+5,.5)))
  }
  
  getBWC <- function(bwc=NULL, year){
    if(is.null(bwc)){
      bwc <- 1:52
    }
    return(((year-1950)*52)+bwc)
  }
  
  # create separate directory for each region type
  output.dir.samples.region <- file.path(output.dir.samples, filename)
  dir.create(file.path(getwd(), output.dir.samples.region), showWarnings = FALSE)
  
  if (j == 1) { # calculate once
    pop0.rt <- pop1to4.rt <- pop0.orig.rt <- pop1to4.orig.rt <- popu5.orig.rt <-
      coverage0.rt <- coverageu5.rt <- matrix(NA, nregs, nyears)
  } else {
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop0.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.orig.rt.rda")))
  }
  M0.rt <- M1to4.rt <- q0.rt <- q1to4.rt <- q5.rt <- qnn.rt <- 
    death0.rt <- death1to4.rt <- deathu5.rt <- deathnn.rt <- death0.all.rt <- death1to4.all.rt <- deathu5.all.rt <- deathnn.all.rt <- matrix(NA, nregs, nyears)
  
  removeNA <- T
  
  which.no.rates <- which(apply(!is.na(u5mr.ctj),1,sum)<1)
  
  for (r in 1:nregs) {
    if (filename %in% c("UNICEFProgRegion", "UNICEFReportRegion", "MDGRegion", "SDGRegion", "SDGSimpleRegion", "WBRegion", "UNPDRegion", "OICRegion", "M49Region", "Wealthall", "Wealthdata")) 
      {
      reg.num <- ChooseRegion(region = regiontypes[r], regiontype = filename)
      select.reg <- (1:nrow(regions))[regions[, is.element(colnames(regions),
                                                           paste0(filename, reg.num))] == regiontypes[r]]
      # to remove LIE from regional aggregate calcualtions -- to implement in 2018
      select.reg.og <- select.reg
      ifelse(is.na(match(which.no.rates,select.reg.og)), select.reg <- select.reg.og, select.reg <- select.reg.og[-match(which.no.rates,select.reg.og)])
      } else if (filename %in% c("WHORegion", "CountdownCountries", "ECAAfricaRegion","GlobalStrategyCountries",
                               "AURegion", "FragileCountries2013", "FragileCountries2014", "FragileCountries2015", 
                               "FragileCountries2017", "FragileCountries2018", "FragileCountries2018OECD1", "FragileCountries2018OECD2", "WealthallGlobal", "WealthdataGlobal", "WorldBankReg2", "NewWorldBank", "USAIDCountries", "AfricanEconomicCommunityRegion", "ECACountries", "GAVICountries")) {
      select.reg <- (1:length(regions))[regions == regiontypes[r]]
      # to remove LIE from regional aggregate calcualtions -- to implement in 2018
      select.reg.og <- (1:length(regions))[regions == regiontypes[r]]
      ifelse(is.na(match(which.no.rates,select.reg.og)), select.reg <- select.reg.og, 
             select.reg <- select.reg.og[-match(which.no.rates, select.reg.og)])
    }
    if (j == 1) { # calculate the first time
      for (i in 1:nyears) {
        # check that population coverage > 50% per region
        pop0.rt[r, i] <- sum(pop0.ct[select.reg,i])
        pop1to4.rt[r, i] <- sum(pop1to4.ct[select.reg,i])
        pop0.orig.rt[r, i] <- sum(pop0.orig.ct[select.reg,i])
        pop1to4.orig.rt[r, i] <- sum(pop1to4.orig.ct[select.reg,i])
        popu5.orig.rt[r, i] <- pop0.orig.rt[r, i] + pop1to4.orig.rt[r, i]
        coverage0.rt[r, i] <- pop0.rt[r, i]/pop0.orig.rt[r, i]
        coverageu5.rt[r, i] <- (pop0.rt[r, i] + pop1to4.rt[r, i])/(popu5.orig.rt[r, i])
      }
      # save the first time
      save(pop0.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.rt.rda")))
      save(pop1to4.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.rt.rda")))
      save(popu5.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
      save(pop0.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
      save(pop1to4.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.orig.rt.rda")))
      save(coverageu5.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_coverageu5.rt.rda")))
      save(coverage0.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_coverage0.rt.rda")))
    }
    for (i in 1:nyears) {
      # calculate deaths
      death0.rt[r, i] <- sum(death0.ctj[select.reg, i, j], na.rm = T)
      death1to4.rt[r, i] <- sum(death1to4.ctj[select.reg, i, j], na.rm = T)
      deathu5.rt[r, i] <- sum(deathu5.ctj[select.reg, i, j], na.rm = T)
      if(nn.exists) deathnn.rt[r, i] <- sum(deathnn.ctj[select.reg, i, j], na.rm = T)
      
      # calculate rates
      q0.rt[r, i] <- sum(dx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)/sum(lx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)
      
      if(i>1){
        q1to4.rt[r, i] <- 1-((1-(sum(dx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),select.reg], na.rm=removeNA)/sum(lx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),select.reg], na.rm=removeNA)))^4) 
      }
      
      if(nn.exists){
        qnn.rt[r, i] <- sum(dx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)/sum(lx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA) 
      }
      
      q5.rt[r, i] <- 1-(1-q0.rt[r, i])*(1-q1to4.rt[r, i])
    } # i loop for years
    
    if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
      ## do BWC method again for all countries in region replacing missing rates with regional rates, then sum deaths at world level for deathXX.all.wtj
      u5mr.temp.rt <- u5mr.ctj[select.reg,,j]
      imr.temp.rt <- imr.ctj[select.reg,,j]
      if(nn.exists) nmr.temp.rt <- nmr.ctj[select.reg,,j]
      livebirths.rt <- livebirths.ct[select.reg,]
      
      deathu5.temp.rt <- death0.temp.rt <- deathnn.temp.rt <- matrix(NA, nrow(u5mr.temp.rt), nyears)
      
      for(k in 1:dim(u5mr.temp.rt)[1]){
        ## get live births for years.k
        wpp.livebirths.k <- as.numeric(livebirths.rt[k,match(years.k,years)])
        #if(nrow(wpp.livebirths.k)<1) next
        bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
        
        for(ik in 1:length(years.k)){
          if(years.k[ik]+5>max(years.k)){
            # u1 mortality rates; has IMR for year[i] and year[i+1] 
            nmx.u1.i <- imr.temp.rt[k,match(years.k[ik:length(years.k)],years)]
            nmx.u1.i[is.na(nmx.u1.i)] <- q0.rt[r,match(years.k[ik:length(years.k)],years)[is.na(nmx.u1.i)]]*1000
            # u5 mortality rates; need same length as u1 to convert to 4q1
            nmx.u5.i <- u5mr.temp.rt[k,match(years.k[ik:length(years.k)],years)]
            nmx.u5.i[is.na(nmx.u5.i)] <- q5.rt[r,match(years.k[ik:length(years.k)],years)[is.na(nmx.u5.i)]]*1000
            # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
            nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
            nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
            nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
            # combine appropriate rates in mortality rate vector
            nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
          } else {
            # u1 mortality rates; has IMR for year[i] and year[i+1] 
            nmx.u1.i <- imr.temp.rt[k,match(years.k[ik]:(years.k[ik]+5),years)]
            nmx.u1.i[is.na(nmx.u1.i)] <- q0.rt[r,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u1.i)]]*1000
            # u5 mortality rates; need same length as u1 to convert to 4q1
            nmx.u5.i <- u5mr.temp.rt[k,match(years.k[ik]:(years.k[ik]+5),years)]
            nmx.u5.i[is.na(nmx.u5.i)] <- q5.rt[r,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u5.i)]]*1000
            # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
            nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
            nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
            nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
            # combine in mortality rate vector for lifetable function
            nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
          } #if/else 
          
          ## turn nmx.i into matrix
          ifelse(ik==1, nmx.mat.k <- matrix(nmx.i, nrow=10, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=10, ncol=52)))
          #if(nsim==1) nmx.mat.k <- roundoff(nmx.mat.k, digits=1)
          # nmr
          if(nn.exists){
            if(years.k[ik]+1>max(years.k)){
              nmr.i <- c(nmr.temp.rt[k,match(years.k[ik],years)], NA)
              nmr.i[is.na(nmr.i)] <- qnn.rt[r,match(years.k[ik:length(years.k)],years)[is.na(nmr.i)]]*1000
            } else {
              nmr.i <- nmr.temp.rt[k,match(years.k[ik]:years.k[ik+1],years)]
              nmr.i[is.na(nmr.i)] <- qnn.rt[r,match(years.k[ik]:years.k[ik+1],years)[is.na(nmr.i)]]*1000
            } # if/else
            
            ## turn nmr.i into matrix
            ifelse(ik==1, nmr.mat.k <- matrix(nmr.i, nrow=2, ncol=52), nmr.mat.k <- cbind(nmr.mat.k ,matrix(nmr.i, nrow=2, ncol=52)))
            # if(nsim==1) nmr.mat.k <- roundoff(nmr.mat.k,digits=1)
          }
        } # ik loop for nmx matrix
        
        ## get infant and u5 deaths
        wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=10, ncol=length(years.k)*52)
        nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
        npx.mat.k <- 1-nqx.mat.k
        lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
        dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
        
        ## get nn deaths
        if(nn.exists){
          wgt.nn.mat.k <- matrix(rep(wgt.nmr.mat,length(years.k)),nrow=2, ncol=length(years.k)*52)
          nqx.nn.mat.k <- wgt.nn.mat.k*(nmr.mat.k/1000)
          npx.nn.mat.k <- 1-nqx.nn.mat.k
          lx.nn.mat.k <- apply(rbind(bwc.vec.k, npx.nn.mat.k),2,cumprod)
          dx.nn.mat.k <- -apply(lx.nn.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
        }
        
        years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=10, ncol=length(years.k)*52)
        years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
        
        for(yrk in min(years.k):max(years.k)){
          deathu5.temp.rt[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T) 
          death0.temp.rt[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
          if(nn.exists) deathnn.temp.rt[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
        } # for loop for summing deaths
        # edit DJS 2018-07-27 round deaths at country level before summing to region and world
        deathu5.temp.rt <- roundoff(deathu5.temp.rt, digits = 0)
        death0.temp.rt <- roundoff(death0.temp.rt, digits = 0)
        if(nn.exists) deathnn.temp.rt <- roundoff(deathnn.temp.rt, digits = 0)
        # edit DJS 2018-07-27
      } # k loop for countries in the region
      
      
      death0.all.rt[r,] <- apply(death0.temp.rt, 2, sum, na.rm=F)
      deathu5.all.rt[r,] <- apply(deathu5.temp.rt, 2, sum, na.rm=F)
      if(nn.exists) deathnn.all.rt[r,] <- apply(deathnn.temp.rt, 2, sum, na.rm=F)
      
    } else {# if replace.rates.reg
      death0.all.rt <- death0.rt
      deathu5.all.rt <- deathu5.rt
      if(nn.exists) deathnn.all.rt <- deathnn.rt
    } # else 
    
    
    
    save(q0.rt, file = file.path(output.dir.samples.region, paste0("q0.rt_", j, ".rda")))
    save(q1to4.rt, file = file.path(output.dir.samples.region, paste0("q1to4.rt_", j, ".rda")))
    save(q5.rt, file = file.path(output.dir.samples.region, paste0("q5.rt_", j, ".rda")))
    save(death0.all.rt, file = file.path(output.dir.samples.region, paste0("death0.all.rt_", j, ".rda")))
    save(death1to4.all.rt, file = file.path(output.dir.samples.region, paste0("death1to4.all.rt_", j, ".rda")))
    save(deathu5.all.rt, file = file.path(output.dir.samples.region, paste0("deathu5.all.rt_", j, ".rda")))
    if(nn.exists){
      save(qnn.rt, file = file.path(output.dir.samples.region, paste0("qnn.rt_", j, ".rda")))
      save(deathnn.all.rt, file = file.path(output.dir.samples.region, paste0("deathnn.all.rt_", j, ".rda")))
    } # if
  } # r loop regions
}



#----------------------------------------------------------------------
CombineAndOutputRegionalResults <- function(
  output.dir,
  output.dir.samples,
  output.dir.samplescombined,
  regiontypes,
  filename,
  percentiles,
  ndigits,
  replace.rates.reg,
  round.output# DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
) {
  # load one file first to get dimensions
  if(is.null(replace.rates.reg)){ # DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
    load(file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  } else {
    load(file = file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
  }
  
  nsim <- dim(deathu5.ctj)[3]
  nregs <- length(regiontypes)
  load(file.path(output.dir.samplescombined, "info.rda"))
  list2env(info, envir = environment())
  
  # load world results
  world.results.exist <- file.exists(file.path(output.dir.samplescombined, "res.world.rda"))
  if(world.results.exist){
    load(file = file.path(output.dir.samplescombined, "res.world.rda"))
    load(file = file.path(output.dir.samplescombined, "global.RoDs.ui.rda"))
  }
  
  # create separate directory for each region type
  output.dir.samples.region <- file.path(output.dir.samples, filename)
  
  death0.rtj <- death1to4.rtj <- deathu5.rtj <- deathnn.rtj <- death0.all.rtj <- death1to4.all.rtj <- deathu5.all.rtj <- deathnn.all.rtj <- 
    M0.rtj <- M1to4.rtj <- q0.rtj <- q1to4.rtj <- q5.rtj <- qnn.rtj <- u5mr.rtj <- imr.rtj <- nmr.rtj <- array(NA, c(nregs, nyears, nsim))
  
  pop0.rt <- pop1to4.rt <- pop0.orig.rt <- pop1to4.orig.rt <- popu5.orig.rt <-
    coverage0.rt <- coverageu5.rt <- matrix(NA, nregs, nyears)
  
  for (j in 1:nsim) {
    load(file = file.path(output.dir.samples.region, paste0("q0.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("q1to4.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("q5.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("death0.all.rt_", j, ".rda")))
    # load(file = file.path(output.dir.samples.region, paste0("death1to4.all.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("deathu5.all.rt_", j, ".rda")))
    
    nn.exists <- file.exists(file.path(output.dir.samples.region, paste0("qnn.rt_", j, ".rda")))
    if(nn.exists){
      load(file = file.path(output.dir.samples.region, paste0("qnn.rt_", j, ".rda")))
    }
    if(nn.exists){
      load(file = file.path(output.dir.samples.region, paste0("deathnn.all.rt_", j, ".rda")))
    }
    
    q0.rtj[ , , j] <- q0.rt
    q1to4.rtj[ , , j] <- q1to4.rt
    q5.rtj[ , , j] <- q5.rt
    if(nn.exists) qnn.rtj[ , , j] <- qnn.rt
    death0.all.rtj[ , , j] <- death0.all.rt
    # death1to4.all.rtj[ , , j] <- death1to4.all.rt
    deathu5.all.rtj[ , , j] <- deathu5.all.rt
    if(nn.exists) deathnn.all.rtj[ , , j] <- deathnn.all.rt
  }
  u5mr.rtj <- q5.rtj*1000
  imr.rtj <- q0.rtj*1000
  if(nn.exists) nmr.rtj <- qnn.rtj*1000
  
  
  # save the samples
  dimnames(u5mr.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(u5mr.rtj)[2]-1)))
  save(u5mr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_u5mr.rtj.rda")))
  dimnames(imr.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(imr.rtj)[2]-1)))
  save(imr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_imr.rtj.rda")))
  dimnames(deathu5.all.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(deathu5.all.rtj)[2]-1)))
  save(deathu5.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_deathu5.all.rtj.rda")))
  dimnames(death0.all.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(death0.all.rtj)[2]-1)))
  save(death0.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_death0.all.rtj.rda")))
  if(nn.exists){
    dimnames(nmr.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(nmr.rtj)[2]-1)))
    save(nmr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_nmr.rtj.rda")))
    dimnames(deathnn.all.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(deathnn.all.rtj)[2]-1)))
    save(deathnn.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_deathnn.all.rtj.rda")))
  }
  
  # delete samples
  unlink(file.path(output.dir.samples.region, paste0("q0.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("q1to4.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("q5.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("death0.all.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("death1to4.all.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("deathu5.all.rt_", 1:nsim, ".rda")))
  if(nn.exists){
    unlink(file.path(output.dir.samples.region, paste0("qnn.rt_", 1:nsim, ".rda")))
    unlink(file.path(output.dir.samples.region, paste0("deathnn.all.rt_", 1:nsim, ".rda")))
  }
  
  # load population and coverage info
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_coverageu5.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_coverage0.rt.rda")))
  
  # regional summaries
  u5mr.qrt <- apply(u5mr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qrt <- apply(imr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.all.qrt <- apply(deathu5.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.all.qrt <- apply(death0.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  if(nn.exists){
    nmr.qrt <- apply(nmr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
    deathnn.all.qrt <- apply(deathnn.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  }
  
  # NA if coverage < 0.5
  for (q in 1:length(percentiles)) {
    u5mr.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    imr.qrt[q, , ][coverage0.rt < 0.5] <- NA
    if(nn.exists) nmr.qrt[q, , ][coverage0.rt < 0.5] <- NA
    # deathu5.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1):(max(which(coverageu5.rt < 0.5))+5))] <- NA
    # death0.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1))] <- NA
    # if(nn.exists) deathnn.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1))] <- NA
    deathu5.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    death0.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    if(nn.exists) deathnn.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    for(regy in 1:dim(u5mr.qrt)[2]){
      # use presence of rate to put NA for deaths
      ifelse(length(which(is.na(u5mr.qrt[q,regy,])))>0,
             deathu5.all.qrt[q,regy,(max(which(is.na(u5mr.qrt[q,regy,])))):(max(which(is.na(u5mr.qrt[q,regy,])))+5)] <- NA,
             deathu5.all.qrt[q,regy,1:5] <- NA)
      ifelse(length(which(is.na(imr.qrt[q,regy,])))>0,
             death0.all.qrt[q,regy,(max(which(is.na(imr.qrt[q,regy,])))):(max(which(is.na(imr.qrt[q,regy,])))+1)] <- NA,
             death0.all.qrt[q,regy,1] <- NA)
      if(nn.exists){
        ifelse(length(which(is.na(nmr.qrt[q,regy,])))>0,
               deathnn.all.qrt[q,regy,(max(which(is.na(nmr.qrt[q,regy,])))):(max(which(is.na(nmr.qrt[q,regy,])))+1)] <- NA,
               deathnn.all.qrt[q,regy,1] <- NA)
      }
      
      # use coverage to put NAs for death 
      # if(length(which(coverageu5.rt[regy,]<0.5))>0){
      # deathu5.all.qrt[q,regy,(max(which(coverageu5.rt[regy,] < 0.5))+1):(max(which(coverageu5.rt[regy,] < 0.5))+5)] <- NA
      # death0.all.qrt[q,regy,(max(which(coverageu5.rt[regy,] < 0.5))+1)] <- NA
      # if(nn.exists) deathnn.all.qrt[q,regy,(max(which(coverageu5.rt[regy,] < 0.5))+1)] <- NA
      # } else {
      # deathu5.all.qrt[q,regy,1:5] <- NA
      # death0.all.qrt[q,regy,1] <- NA
      # if(nn.exists) deathnn.all.qrt[q,regy,1] <- NA
      # }
    } # regy loop
  }
  
  # regional summary
  res.year <- NULL
  for (i in 1:nyears) {
    if(nn.exists){
      ifelse(round.output, t.u5mr.qrt <- roundoff(t(u5mr.qrt[,,i]), digits = ndigits), t.u5mr.qrt <- t(u5mr.qrt[,,i]))
      ifelse(round.output, t.imr.qrt <- roundoff(t(imr.qrt[,,i]), digits = ndigits), t.imr.qrt <- t(imr.qrt[,,i]))
      ifelse(round.output, t.nmr.qrt <- roundoff(t(nmr.qrt[,,i]), digits = ndigits), t.nmr.qrt <- t(nmr.qrt[,,i]))
      
      if(world.results.exist){
        res.year <- rbind(res.year,
                          rbind(
                            cbind(est.years.floor[i],
                                  roundoff(popu5.orig.rt[,i], digits = 0),
                                  roundoff(pop0.orig.rt[,i], digits = 0),
                                  roundoff(coverageu5.rt[,i]*100, digits = 2),
                                  roundoff(coverage0.rt[,i]*100, digits = 2),
                                  t.u5mr.qrt,
                                  t.imr.qrt,
                                  t.nmr.qrt,
                                  roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                  roundoff(t(death0.all.qrt[,,i]), digits = 0),
                                  roundoff(t(deathnn.all.qrt[,,i]), digits = 0)),
                            res.world[res.world[, 1] == est.years.floor[i], ]
                          )) 
      } else {
        res.year <- rbind(res.year,
                          rbind(
                            cbind(est.years.floor[i],
                                  roundoff(popu5.orig.rt[,i], digits = 0),
                                  roundoff(pop0.orig.rt[,i], digits = 0),
                                  roundoff(coverageu5.rt[,i]*100, digits = 2),
                                  roundoff(coverage0.rt[,i]*100, digits = 2),
                                  t.u5mr.qrt,
                                  t.imr.qrt,
                                  t.nmr.qrt,
                                  roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                  roundoff(t(death0.all.qrt[,,i]), digits = 0),
                                  roundoff(t(deathnn.all.qrt[,,i]), digits = 0))
                          ))         
      } # if(world.results.exist)
    } else {
      ifelse(round.output, t.u5mr.qrt <- roundoff(t(u5mr.qrt[,,i]), digits = ndigits), t.u5mr.qrt <- t(u5mr.qrt[,,i]))
      ifelse(round.output, t.imr.qrt <- roundoff(t(imr.qrt[,,i]), digits = ndigits), t.imr.qrt <- t(imr.qrt[,,i]))
      
      if(world.results.exist){
        res.year <- rbind(res.year,
                          rbind(
                            cbind(est.years.floor[i],
                                  roundoff(popu5.orig.rt[,i], digits = 0),
                                  roundoff(pop0.orig.rt[,i], digits = 0),
                                  roundoff(coverageu5.rt[,i]*100, digits = 2),
                                  roundoff(coverage0.rt[,i]*100, digits = 2),
                                  t.u5mr.qrt,
                                  t.imr.qrt,
                                  roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                  roundoff(t(death0.all.qrt[,,i]), digits = 0)),
                            res.world[res.world[, 1] == est.years.floor[i], ]
                          ))
      } else {
        res.year <- rbind(res.year,
                          rbind(
                            cbind(est.years.floor[i],
                                  roundoff(popu5.orig.rt[,i], digits = 0),
                                  roundoff(pop0.orig.rt[,i], digits = 0),
                                  roundoff(coverageu5.rt[,i]*100, digits = 2),
                                  roundoff(coverage0.rt[,i]*100, digits = 2),
                                  t.u5mr.qrt,
                                  t.imr.qrt,
                                  roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                  roundoff(t(death0.all.qrt[,,i]), digits = 0))
                          ))        
      } # if(world.results.exist)
    } # if/else
  }
  ifelse(world.results.exist, 
         res.region <- cbind(rep(c(regiontypes, "World"), nyears), res.year), 
         res.region <- cbind(rep(c(regiontypes), nyears), res.year)
  )
  
  # output to .csv
  ui.colnames <- c(" lower bound", " median", " upper bound")
  if(nn.exists){
    colnames(res.region) <- c("Region", "Year",
                              "Under-five population", "Infant population",
                              "Population coverage (under 5)", "Population coverage (age 0)",
                              paste0("U5MR", ui.colnames),
                              paste0("IMR", ui.colnames),
                              paste0("NMR", ui.colnames),
                              paste0("Under-five deaths", ui.colnames),
                              paste0("Infant deaths", ui.colnames),
                              paste0("Neonatal deaths", ui.colnames))
  } else {
    colnames(res.region) <- c("Region", "Year",
                              "Under-five population", "Infant population",
                              "Population coverage (under 5)", "Population coverage (age 0)",
                              paste0("U5MR", ui.colnames),
                              paste0("IMR", ui.colnames),
                              paste0("Under-five deaths", ui.colnames),
                              paste0("Infant deaths", ui.colnames))
  }
  
  if (nsim == 1) res.region <- res.region[, !grepl("bound", colnames(res.region))]
  write.csv(res.region, file = file.path(output.dir, paste0("Rates & Deaths_", filename, ".csv")),
            row.names = F, na = "")
  
  # round off to 1 d.p. before calculation (for median only)
  if (dim(u5mr.rtj)[3] == 1) { # change JR, 26 Aug 2013
    u5mr.rtj <- roundoff(u5mr.rtj, digits = ndigits)
    imr.rtj <- roundoff(imr.rtj, digits = ndigits)
  }
  
  # regional summary - rates of decline
  region.RoDs.ui <- NULL
  for (r in 1:nregs) {
    ARR.year1.year4.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
                                      year.start = year1, year.end = year4)
    ARR.year1.year2.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
                                      year.start = year1, year.end = year2)
    ARR.year2.year4.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
                                      year.start = year2, year.end = year4)
    required.ARR.j <- ifelse(year4 < year.target,
                             1/(year.target-year4)*
                               log(roundoff(u5mr.rtj[r, est.years == year1, ]*factor.target, digits = ndigits)/
                                     u5mr.rtj[1, est.years == year4, ])*-100, NA)
    changeinARR.j <- ARR.year2.year4.j - ARR.year1.year2.j
    decline.year1.year4.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
                                              year.start = year1, year.end = year4)
    decline.year1.year2.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
                                              year.start = year1, year.end = year2)
    decline.year2.year4.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
                                              year.start = year2, year.end = year4)
    ARR.year1.year4.ui <- quantile(ARR.year1.year4.j, probs = percentiles)
    ARR.year1.year2.ui <- quantile(ARR.year1.year2.j, probs = percentiles)
    ARR.year2.year4.ui <- quantile(ARR.year2.year4.j, probs = percentiles)
    # change JR, 20150605: set na.rm = TRUE if all required.ARR are NAs,
    # indicating that year4 = year.target
    required.ARR.ui <- quantile(required.ARR.j, probs = percentiles,
                                na.rm = all(is.na(required.ARR.j)))
    changeinARR.ui <- quantile(changeinARR.j, probs = percentiles)
    decline.year1.year4.ui <- quantile(decline.year1.year4.j, probs = percentiles)
    decline.year1.year2.ui <- quantile(decline.year1.year2.j, probs = percentiles)
    decline.year2.year4.ui <- quantile(decline.year2.year4.j, probs = percentiles)
    region.RoDs.ui <- rbind(region.RoDs.ui,
                            c(ARR.year1.year4.ui, ARR.year1.year2.ui, ARR.year2.year4.ui,
                              required.ARR.ui, changeinARR.ui, decline.year1.year4.ui,
                              decline.year1.year2.ui, decline.year2.year4.ui))
    
  }
  colnames(region.RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5, ui.colnames),
                                paste0("ARR ", year1-0.5, "-", year2-0.5, ui.colnames),
                                paste0("ARR ", year2-0.5, "-", year4-0.5, ui.colnames),
                                paste0("Required ARR", ui.colnames),
                                paste0("Change in ARR", ui.colnames),
                                paste0("Percentage decline ", year1-0.5, "-", year4-0.5, ui.colnames),
                                paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
                                paste0("Percentage decline ", year2-0.5, "-", year4-0.5, ui.colnames))
  ifelse(world.results.exist,
         region.RoDs <- data.frame(Region = c(regiontypes, "World"), rbind(region.RoDs.ui, global.RoDs.ui)),
         region.RoDs <- data.frame(Region = c(regiontypes), region.RoDs.ui)
  )
  if (nsim == 1)
    region.RoDs <- region.RoDs[, !grepl("bound", colnames(region.RoDs))]
  write.csv(region.RoDs, file = file.path(output.dir, paste0("Rates of Decline_", filename, ".csv")),
            row.names = F, na = "")
  cat(paste0("Output generated for ", filename, ".\n"))
}

