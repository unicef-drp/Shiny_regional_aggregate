#----------------------------------------------------------------------
# getloessestimates.R
# Based on loess functions from CME Info
# Leontine Alkema & Jin Rou New, 2012-2013
#----------------------------------------------------------------------
GetLoessEstimates <- function(
  iso.c, ##<< Country vector of ISO country codes.
  method.c, ##<< Country vector denoting "Loess" for countries for which loess estimates are desired.
  hiv.c, ##<< Country vector of logical values denoting if country is HIV country.
  crisisadj.c, ##<< Country vector of logical values denoting if country requires crisis adjustment.
  smallcountry.c, ##<< Country vector of logical values denoting if country is a small country. 
  notes.c = NULL, ##<< Country vectory denoting ""VR trend used" if applicable and "" otherwise.
  indicator.type, ##<< "U5MR" or "IMR"?
  est.years, ##<< Vector of estimate years.
  year.cutoff = 2015, ##<< Cut-off year, set to earlier year like 2006 for validation.
  alpha.c = NULL, ##<< Country vector of smoothness parameter alpha values. 
  ## If \code{NULL}, default alpha is calculated and used.
  changedalpha.c = NULL, ##<< Country vector of description of changes to default alpha values.
  ## If \code{NULL}, default alpha is calculated and used, i.e. not additional changes are made to alpha values.
  getUIs = FALSE, ##<< Get UIs for loess fit?
  year1 = 1990.5, ##<< First year used for ARR calculation.
  year2 = 2000.5, ##<< Second year used for ARR calculation.
  year3 = 2005.5, ##<< Third year used for ARR calculation, usually last year of estimation for validation exercise.
  year4 = 2011.5, ##<< Last year used for ARR calculation, usually last year of estimation.
  percentiles = c(0.05, 0.5, 0.95), ##<< Percentiles for 90% UIs.
  est.year.getcoeffs = NULL, ##<< Optional: Year to get coefficients of loess fit for, for use in further analysis.
  log.base = exp(1), ##<< Base of logarithm to use for data transformation.
  data.file = NULL, ##<< File path to data file. If \code{NULL}, default database is used.
  hiv.file = NULL, ##<< File path to HIV adjustment file. If \code{NULL}, default HIV adjustment file is used.
  adj.file = NULL ##<< File path to crisis adjustment file. If \code{NULL}, default crisis adjustment file is used.
) {
  if (is.null(data.file))
    data.file <- file.path("input", paste0("data", indicator.type, "_clean_inclHIV.csv"))
  if (is.null(hiv.file))
    hiv.file <- file.path("input", paste0("dataUNAIDS_", indicator.type, ".csv"))
  if (is.null(adj.file))
    adj.file <- file.path("input", paste0("dataPostAdj_", indicator.type, ".csv"))

  # read in data  and exclude data in/after year.cutoff
  # find alpha and get estimates
  data <- read.csv(file = data.file, header = T, stringsAsFactors = F, encoding = "latin1")
  C <- length(iso.c)
  nsurveys.new.c <- nvrs.c <- rep(NA, C)
  if (is.null(notes.c))
    notes.c <- rep("", C)
  if (is.null(alpha.c) | max(data$surveyyear.i) >= year.cutoff) {
    # note problem when finding unique surveys:
    # source i is sometimes inconsistent (e.g. Albania source for direct DHS contains "calendar year" 
    # and differs from source.i for indirect estimates)
    # quick fix: use sourceType for DHS instead
    surveyidunique.i <- paste(
      ifelse(data$sourcetype.i=="DHS", "DHS",
             ifelse(ifelse(data$sourcetype.i=="Other DHS", "Other DHS", 
                           ifelse(data$sourcetype.i=="MICS", "MICS", 
                                  paste(data$source.i))))), data$seriesyear.i)
    alpha.c <- rep(NA, C)
    for (c in 1:C) {
      if (method.c[c] == "Loess") {
        select <- data$countrycode.i == as.character(iso.c[c]) & (data$surveyyear.i < year.cutoff)
        nsurveys.new.c[c] <- length(unique(surveyidunique.i[select & data$sourcetype.i != "VR"]))
        nvrs.c[c] <- sum(data$sourcetype.i[select] == "VR")
        alpha.c[c] <- FindAlphaDefault(nuniquesurveys = nsurveys.new.c[c],
                                       nvr = nvrs.c[c],
                                       iso = iso.c[c],
                                       smallcountry = smallcountry.c[c],
                                       notes = notes.c[c])
        if (!is.null(changedalpha.c)) {
          changedalpha.c <- ifelse(is.na(changedalpha.c), "", changedalpha.c)
          alpha.c[c] <- ifelse(changedalpha.c[c] == "Half alpha", alpha.c[c]/2,
                               ifelse(changedalpha.c[c] == "Quarter alpha", alpha.c[c]/4,
                                      ifelse(changedalpha.c[c] == "", alpha.c[c], 
                                             as.numeric(changedalpha.c[c]))))
        }
      } else {
        alpha.c[c] <- NA
      }
    }
  } # end ifnull loop
  # fit loess with alpha.c 
  if (getUIs) {
    CIs.iid.cqt <- array(NA, c(C, 3, length(est.years)))
    resARR.cq.Ly <- list()
    for (y in 1:4) {
      resARR.cq.Ly[[y]] <- array(NA, c(C, length(percentiles)))
    }
  }
  igme2 = list(u.ct = matrix(NA, C, length(est.years)), t = est.years, iso.c = iso.c,
               propadj.ct = matrix(NA, C, length(est.years)), 
               propadjhiv.ct = matrix(NA, C, length(est.years)),
               coeffs.est.c2 = matrix(NA, C, 2)) # change JR, 24 Jan # intercept+slope for loglinear fit for estimate in 2011.5
  for (c in 1:C) {
    cat(paste0("Fitting loess to country ", c, " of ", C, " countries.\n"))
    if (method.c[c] == "Loess") {
      select <- data$countrycode.i == paste(iso.c[c]) & (data$surveyyear.i < year.cutoff)
      u.i <- data$u.i[select]
      year.i <- data$year.i[select]
      method.i <- data$method.i[select]
      seriesyear.i <- data$seriesyear.i[select]
      surveyyear.i <- data$surveyyear.i[select]
      sourceID.i <- data$sourceID.i[select]
      #period.i <- data$period.i[select]
      #included.i <- data$included.i[select]
      country.i <- data$country.i[select]
      countrycode.i <- data$countrycode.i[select]
      se.i <- data$se.i[select]
      source.i <- data$source.i[select]
      recallperiod.i <- data$recallperiod.i[select]
      sourcetype.i <- data$sourcetype.i[select]
      
      if (notes.c[c] == "VR trend used") {
        z.i <- ifelse(data$sourcetype.i[select] == "VR", 1, 0)
      } else {
        z.i <- NULL
      }
      # get estimates
      res <- GetIIDUIs(u.i = u.i,
                       year.i = year.i, 
                       iso = iso.c[c], 
                       alpha = alpha.c[c],
                       hiv = hiv.c[c],
                       crisisadj = crisisadj.c[c],
                       z.i = z.i,
                       method.i = method.i, surveyyear.i = surveyyear.i, 
                       sourceID.i = sourceID.i, source.i = source.i, sourcetype.i = sourcetype.i,
                       #period.i = period.i, #included.i = included.i,
                       est.years = est.years,
                       est.year.getcoeffs = est.year.getcoeffs,
                       getUIs = getUIs, 
                       year.cutoff = year.cutoff,
                       year1 = year1,
                       year2 = year2,
                       year3 = year3,
                       year4 = year4,
                       percentiles = percentiles,
                       log.base = log.base,
                       hiv.file = hiv.file,
                       adj.file = adj.file)
      if (getUIs) {
        CIs.iid.cqt[c, , ] <- res$CIs.iid.qt
        resARR.q.Ly <- list(resARR.year1.year3.q = res$ARR.year1.year3.iid.q,
                            resARR.year1.year2.q = res$ARR.year1.year2.iid.q,
                            resARR.year2.year4.q = res$ARR.year2.year4.iid.q,
                            resARR.year1.year4.q = res$ARR.year1.year4.iid.q)
        for (y in 1:4) {
          if (year.cutoff > year4 | is.element(y, c(1,2))) {
            resARR.cq.Ly[[y]][c, ] <- unlist(resARR.q.Ly[[y]])
          }
        }
      }
      igme2$u.ct[c,] <- res$igmeu.t
      if (!is.null(res$propadjhiv.t))
        igme2$propadjhiv.ct[c,] <- res$propadjhiv.t
      if (!is.null(res$propadj.t))
        igme2$propadj.ct[c,] <- res$propadj.t
      if (!is.null(res$coeffs.est))
        igme2$coeffs.est.c2[c, ] <- res$coeffs.est
    }
  } # end country loop
  if (!getUIs) {
    return(list(alpha.c = alpha.c, nseriesnonvr.c = nsurveys.new.c, nvrs.c = nvrs.c, igme = igme2, 
                est.years = est.years))  
  } else {
    if(year.cutoff > year4) {
      names(resARR.cq.Ly) <- c(paste0(year1, "-", year3), paste0(year1, "-", year2), 
                               paste0(year2, "-", year4), paste0(year1, "-", year4))
    } else {
      names(resARR.cq.Ly) <- c(paste0(year1, "-", year3), paste0(year1, "-", year2))
    }
    return(list(alpha.c = alpha.c, nseriesnonvr.c = nsurveys.new.c, nvrs.c = nvrs.c, igme = igme2, 
                est.years = est.years, # note: est.years can be found in igme$t
                CIs.iid.cqt = CIs.iid.cqt, 
                resARR.cq.Ly = resARR.cq.Ly))      
  }
}
#---------------------------------------------------------------
GetLoessFit <- function(
  est.years, 
  iso,
  alpha, 
  hiv,
  z.i,
  year.i.fit,
  u.i.fit,
  est.year.getcoeffs,
  log.base = exp(1),
  hiv.file,
  nMVN = 10000 ##<< Number of samples for uncertainty bounds
) {
  w.it <- GetWeightsforRowObsperColumnEstyear(est.years = est.years, 
                                              alpha = alpha, obs.years = year.i.fit)
  igmeu.t <- rep(NA, length(est.years))
  u.est.nt <- matrix(NA, nMVN, length(est.years))
  for (t in 1:length(est.years)) {
    if (is.null(z.i)) {
      if (!hiv) {
        loess.predict <- lm(log(u.i.fit, base = log.base) ~ year.i.fit, weights = w.it[,t]) 
      } else {
        loess.predict <- lm(log(GetHIVSubtractedSeries(year.i = year.i.fit, u.i = u.i.fit, iso = iso, hiv.file = hiv.file), base = log.base) ~ year.i.fit, weights = w.it[,t])
      }
    } else {
      if (!hiv) {
        loess.predict <- lm(log(u.i.fit, base = log.base) ~ year.i.fit + z.i, weights = w.it[,t]) 
      } else {
        loess.predict <-  lm(log(GetHIVSubtractedSeries(year.i = year.i.fit, u.i = u.i.fit, iso = iso, hiv.file = hiv.file), base = log.base) ~ year.i.fit + z.i, weights = w.it[,t]) 
      }
    }
    coeffs <- summary(loess.predict)$coefficients[1:2, 1]
    if (!is.null(est.year.getcoeffs) & est.years[t] == 2011.5) {
      coeffs.est <- coeffs
    } else {
      coeffs.est <- NULL
    }
    igmeu.t[t] <- exp(coeffs[1]+est.years[t]*coeffs[2])
    # old method: just MVN for loess betas
    varcov <- vcov(loess.predict)[1:2,1:2]
    if (!is.na(varcov[1,1])) {
      # note: error for some countries with very few obs: df = 0!
      random.draws.b2 <- rmvnorm(nMVN, coeffs, varcov)
      u.est.nt[,t] <- exp(random.draws.b2[,1] + est.years[t] * random.draws.b2[,2] )
    }
  } # end t-loop
  return(list(igmeu.t = igmeu.t, u.est.nt = u.est.nt, coeffs.est = coeffs.est))
}
#---------------------------------------------------------------
GetIIDUIs <- function(
  u.i, 
  year.i, 
  iso, 
  hiv,
  crisisadj,
  z.i = NULL, # z.i = 1 for VR obs for Kaz and Kyr
  method.i, 
  surveyyear.i, 
  sourceID.i, 
  source.i, 
  sourcetype.i,
  #period.i, 
  #included.i,
  est.years,
  alpha, 
  est.year.getcoeffs,
  getUIs = FALSE, 
  year.cutoff,
  year1,
  year2,
  year3,
  year4,
  percentiles = c(0.05, 0.5, 0.95),
  log.base = exp(1),
  hiv.file,
  adj.file
) {
  # fit loess
  res.loess <- GetLoessFit(est.years = est.years, alpha = alpha, iso = iso, hiv = hiv,
                           z.i = z.i, year.i.fit = year.i, u.i.fit = u.i, 
                           est.year.getcoeffs = est.year.getcoeffs, log.base = log.base, hiv.file = hiv.file)
  igmeu.t <- res.loess$igmeu.t
  if (getUIs) {
    u.est.nt <- res.loess$u.est.nt
  }
  propadjhiv.t <- NULL
  if (hiv) { # HIV countries: add back UNAIDS estimates  
    temp <- GetHIVAdjustedEstimates(year.t = est.years, u.t = igmeu.t, iso = iso,
                                    hiv.file = hiv.file)
    igmeu.t <- temp$u.t
    propadjhiv.t <- temp$propadjhiv.t
    if (getUIs) {
      u.est.nt <- t(t(u.est.nt)*propadjhiv.t)
    }
  }
  propadj.t <- NULL
  if (crisisadj) { # countries with crises/conflicts
    temp2 <- GetCrisisAdjustedEstimates(year.t = est.years, u.t = igmeu.t, iso = iso, 
                                        adj.file = adj.file)
    igmeu.t <- temp2$u.t
    propadj.t <- temp2$propadj.t
    if (getUIs){
      u.est.nt <- t(t(u.est.nt)*propadj.t)
    }
  }  
  if (!getUIs) {
    return(list(igmeu.t  = igmeu.t, 
                #included.i = included.i, 
                propadj.t = propadj.t, 
                propadjhiv.t = propadjhiv.t,
                coeffs.est = res.loess$coeffs.est))
  } else {
    CIs.iid.qt <- apply(u.est.nt, 2, quantile, percentiles, na.rm = T)
    ARR.year1.year3.n <- CalculateARR(u5mr = u.est.nt, years = est.years, year.start = year1, year.end = year3)
    ARR.year1.year2.n <- CalculateARR(u5mr = u.est.nt, years = est.years, year.start = year1, year.end = year2)
    ARR.year1.year3.iid.q <- quantile(ARR.year1.year3.n, percentiles, na.rm = T)
    ARR.year1.year2.iid.q <- quantile(ARR.year1.year2.n, percentiles, na.rm = T)
    if (year.cutoff > year4) {
      ARR.year2.year4.n <- CalculateARR(u5mr = u.est.nt, years = est.years, year.start = year2, year.end = year4)
      ARR.year1.year4.n <- CalculateARR(u5mr = u.est.nt, years = est.years, year.start = year1, year.end = year4)
      ARR.year2.year4.iid.q <- quantile(ARR.year2.year4.n, percentiles, na.rm = T)
      ARR.year1.year4.iid.q <- quantile(ARR.year1.year4.n, percentiles, na.rm = T)
    } else {
      ARR.year2.year4.iid.q <- ARR.year1.year4.iid.q <- NULL
    }
    ##value<< List containing:
    return(list(CIs.iid.qt  = CIs.iid.qt, 
                ARR.year1.year3.iid.q = ARR.year1.year3.iid.q, 
                ARR.year1.year2.iid.q = ARR.year1.year2.iid.q,
                ARR.year2.year4.iid.q = ARR.year2.year4.iid.q,
                ARR.year1.year4.iid.q = ARR.year1.year4.iid.q,
                igmeu.t = igmeu.t, 
                #included.i = included.i, 
                propadjhiv.t = propadjhiv.t, 
                propadj.t = propadj.t, 
                coeffs.est = res.loess$coeffs.est))
  }
}
#---------------------------------------------------------------
FindAlphaDefault <- function(# Calculate default loess alpha based.
  nuniquesurveys, ##<< Number of unique non-VR series.
  nvr, ##<< Number of VR observations.
  iso, ##<< ISO country code.
  smallcountry, ## Small country?
  notes ##<< Any notes like "VR trend used"/"VR counted as 1 survey"
) {
  # for countries where VR considered as 0 or 1 survey
  nvr <- ifelse(notes == "VR trend used", 0, 
                ifelse(notes == "VR counted as 1 survey", 5, nvr))
  alpha.default <- ifelse(nuniquesurveys != 0, 5/(nuniquesurveys + nvr/5),
                          ifelse(smallcountry, 25/nvr, 12.5/nvr))
  return(alpha.default)
}
#---------------------------------------------------------------
find.w <- function(# Find weights
  alpha, ##<< Loess alpha value.
  x, ##<< Vector of observation years.
  x0 ##<< Year where we want the estimate
) {
  dist.x0 <- sqrt((x - x0)^2)
  if (alpha < 1) {
    # why floor!? then prop less than alpha...
    include.x <- rank(dist.x0, ties.method = 'min') <= floor(alpha * length(x))
    w.alpha.x0 <- rep(0, times = length(x))
    w.alpha.x0[include.x == 1] <- (1 - (dist.x0[include.x == 1] / max(dist.x0[include.x == 1]))^3)^3
  } else {
    # p = 1 not 2??? number of expl. vars or number of parameters?
    w.alpha.x0 <- (1 - (dist.x0/(max(dist.x0)*alpha^(1/2)))^3)^3
  }
  w.alpha.x0
}
#---------------------------------------------------------------
GetWeightsforRowObsperColumnEstyear <- function(est.years, alpha, obs.years){
  # construct matrix with each colum the weights for that est year
  w.it <- matrix(NA, length(obs.years), length(est.years))
  for (t in 1:length(est.years)){
    if (est.years[t] <= max(obs.years)) {
      w.it[,t] <- find.w(alpha, x = obs.years, x0 = est.years[t])
    } else {
      w.it[,t] <- find.w(alpha, x = obs.years, x0 = max(obs.years))
      # shouldn't it be min max!?
    }
  }
  return(w.it)
}
