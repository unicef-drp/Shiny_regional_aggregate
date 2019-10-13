#----------------------------------------------------------------------
# getjagsdata.R
# Leontine Alkema & Jin Rou New, 2012-2013
#----------------------------------------------------------------------
GetJAGSDataAll <- function( # Get all JAGS data
  data,
  data.val,
  data.global,
  hiv.file,
  adj.file,
  settings
) {
  list2env(settings, envir = environment())
  # get JAGS data for all observations/splines
  jags.data.splines.temp <- GetJAGSDataForSplines(data = data,
                                                  data.val = data.val,
                                                  indicator.type = indicator.type,
                                                  runname.U5MR = runname.U5MR,
                                                  I = I,
                                                  run.type = run.type,
                                                  year.lastestimate = year.lastestimate,
                                                  periods.unsmooth.list = periods.unsmooth.list,
                                                  periods.smooth.list = periods.smooth.list,
                                                  periods.constant.list = periods.constant.list,
                                                  hiv.file = hiv.file,
                                                  adj.file = adj.file,
                                                  runname.igme = runname.igme)
  data <- jags.data.splines.temp$data
  jags.data.splines <- jags.data.splines.temp$jags.data.splines
  if (run.type == "country") {
    jags.data.splines.sigmau0 <- GetJAGSDataForSplinesCountrySpecificRun(
      iso = data$iso.c[1],
      data.global = data.global,
      use.constant.sigma.u = use.constant.sigma.u,
      periods.unsmooth.list = periods.unsmooth.list,
      periods.smooth.list = periods.smooth.list)
    jags.data.splines <- c(jags.data.splines, jags.data.splines.sigmau0)
  } # end run.type country
  jags.data <- jags.data.splines
  jags.data.for.inits <- jags.data.splines.temp$jags.data.for.inits # required for later steps
  #----------------------------------------------------------------------
  # get JAGS data for non-VR observations
  jags.data.nonvr.temp <- GetJAGSDataForNonVRObservations(data = data,
                                                          data.val = data.val,
                                                          add.dhsdirect.bias = add.dhsdirect.bias,
                                                          set.dhsdirect.prior = set.dhsdirect.prior,
                                                          indicator.type = indicator.type,
                                                          run.type = run.type,
                                                          se.censusindirect.missing = se.censusindirect.missing,
                                                          se.othernonvr.missing = se.othernonvr.missing,
                                                          dhsdirect.prior.mu.mubeta1 = dhsdirect.prior.mu.mubeta1,
                                                          dhsdirect.prior.sigma.mubeta1 = dhsdirect.prior.sigma.mubeta1)
  data <- jags.data.nonvr.temp$data
  jags.data.nonvr <- jags.data.nonvr.temp$jags.data.nonvr
  if (run.type == "country" & !is.null(data$Cnonvr)) {
    if (data$Cnonvr > 0) {
      jags.data.nonvr.countryspecific <- GetJAGSDataForNonVRObservationsCountrySpecificRun(
        data = data, data.global = data.global, add.dhsdirect.bias = add.dhsdirect.bias)
      jags.data.nonvr <- c(jags.data.nonvr, jags.data.nonvr.countryspecific)
    }
  } # end run.type country
  jags.data <- c(jags.data, jags.data.nonvr) # note: even if Cnonvr = 0, we still need Cnormdist and Ctdist
  #----------------------------------------------------------------------
  # JAGS data for VR observations
  jags.data.vr.temp <- GetJAGSDataForVRObservations(data = data,
                                                    data.val = data.val,
                                                    input.vr.se = input.vr.se,
                                                    se.vr.min = se.vr.min,
                                                    se.vr.missing = se.vr.missing)
  data <- jags.data.vr.temp$data
  jags.data.vr <- jags.data.vr.temp$jags.data.vr
  jags.data <- c(jags.data, jags.data.vr)
  #----------------------------------------------------------------------
  # # JAGS data for prediction
  # if (get.jags.predictions) {
  #   jags.data.predict <- GetJAGSDataForPrediction(jags.data = jags.data, 
  #                                                 I = I,
  #                                                 year.lastestimate = year.lastestimate,
  #                                                 periods.constant.list = periods.constant.list)
  #   jags.data <- c(jags.data, jags.data.predict)
  # }
  ##value<< List containing:
  return(list(jags.data = jags.data, ##<< List of JAGS data
              data = data, ##<< \code{data} with additional elements
              jags.data.for.inits = jags.data.for.inits ##<< List of JAGS data required for \code{GetJAGSInits} function
  ))
}
