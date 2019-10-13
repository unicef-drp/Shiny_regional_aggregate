#----------------------------------------------------------------------
# getsplines.R
#----------------------------------------------------------------------
GetSplines <- function( # Get B-splines
  years.t, ##<< Vector of years (without NAs) for which splines need to be calculated (determines the number of rows of B.tk)
  year0 = NULL, ##<< Year which determines knot placement. By default, knot is placed half-interval before last observation
  years.combine.L = NULL, ##<< List of vectors of length 2 of start and end years of periods for which splines will be combined
  I = 2.5, ##<< Interval length between two knots during observation period
  years.smoothing.L = NULL, ##<< List of vectors of length 2 of start and end years (without NAs) of periods 
  ## for which smoothness of spline fit needs to be increased.
  years.unsmoothing.L = NULL ##<< List of vectors of length 2 of start and end years (without NAs) of periods 
  ## for which smoothness of spline fit needs to be decreased. The u at the midpoint of the period and the two 
  ## adjacent u's will be adjusted.
  ##details<< \code{year0} and \code{I} need to be chosen such that
  ## years.t are contained in interval (year0-1000*I, year0+1000*I).
  ## This should be fine as long as \code{I} is not smaller than 0.1 year or so.
) {
  if (is.null(year0)) {
    year0 <- max(years.t)-0.5*I # change JR, 18 Feb
  } 
  # get knots, given that one knot needs to be in year0
  # do NOT make any assumptions about where year0 is compared to years.t!  
  knots <- seq(year0-1000*I, year0+1000*I, I) # change JR, 3 May
  while (min(years.t) < knots[1]) knots <- c(seq(knots[1]-1000*I, knots[1]-I,I), knots)
  while (max(years.t) > knots[length(knots)]) knots <- c(knots, seq(knots[length(knots)]+I, 
                                                                    knots[length(knots)]+1000*I, I)) # change JR, 3 May
  Btemp.tk <- bs(years.t, knots = knots[-c(1, length(knots))],  
                 Boundary.knots = knots[c(1, length(knots))])
  indicesofcolswithoutzeroes <- which(apply(Btemp.tk, 2, sum) > 0)
  # only remove columns with zeroes at start and end
  startnonzerocol <- indicesofcolswithoutzeroes[1]
  endnonzerocol <- indicesofcolswithoutzeroes[length(indicesofcolswithoutzeroes)]
  B.tk <- Btemp.tk[,startnonzerocol:endnonzerocol]
  alphayears.k <- knots[startnonzerocol:endnonzerocol]
  uyears.q <- alphayears.k[-c(1, length(alphayears.k))] # denotes midpoint of period that u has an effect on
  
  # combine splines during period indicated by years.combine # change JR, 21 Jun
  if (!is.null(years.combine.L)) {
    for (p in length(years.combine.L)) {
      k.select <- which(alphayears.k >= years.combine.L[[p]][1] & alphayears.k <= years.combine.L[[p]][2])
      grouping <- 1:length(alphayears.k)
      grouping[k.select] <- min(k.select)
      # get B.tk with combined splines
      B.tk.comb <- t(rowsum(t(B.tk), group = grouping, reorder = F))
      # get knot years for B.tk with combined splines
      alphayears.k.comb <- NULL # change JR, 2 Jul
      if (min(k.select) != 1)
        alphayears.k.comb <- c(alphayears.k.comb, alphayears.k[1:(min(k.select)-1)])
      alphayears.k.comb <- c(alphayears.k.comb, mean(alphayears.k[k.select]))
      if (max(k.select) != length(alphayears.k))
        alphayears.k.comb <- c(alphayears.k.comb, alphayears.k[(max(k.select)+1):length(alphayears.k)])
      # get uyears for B.tk with combined splines
      uyears.q.comb <- alphayears.k.comb[-c(1, length(alphayears.k.comb))]
      B.tk <- B.tk.comb
      alphayears.k <- alphayears.k.comb
      uyears.q <- uyears.q.comb
    }
  }
  
  # get spline indices for periods where spline fit smoothness is to be adjusted # change JR, 29 May
  if (!is.null(years.smoothing.L)) {
    years.tweak <- q.select <- list()
    for (p in 1:length(years.smoothing.L)) {
      years.tweak[[p]] <- c(seq(years.smoothing.L[[p]][1], years.smoothing.L[[p]][2], 0.5*I), 
                            years.smoothing.L[[p]][2])
      years.tweak[[p]] <- unique(years.tweak[[p]])
      q.select.temp <- NULL
      for (t in 1:length(years.tweak[[p]])) {
        compare <- (uyears.q-0.5*I) > years.tweak[[p]][t]
        q.select.temp <- c(q.select.temp, which(diff(compare) == 1))
      }
      q.select[[p]] <- unique(q.select.temp)
    }
  } else if (!is.null(years.unsmoothing.L)) {
    years.tweak <- q.select <- list()
    for (p in 1:length(years.unsmoothing.L)) {
      years.tweak[[p]] <- (years.unsmoothing.L[[p]][1] + years.unsmoothing.L[[p]][2])/2
      compare <- (uyears.q-0.5*I) > years.tweak[[p]]
      q.temp <- which(diff(compare) == 1)
      q.select[[p]] <- (q.temp-1):(q.temp+1)
    }
  } else {
    q.select <- NULL
  }
  ##value<< List of B-splines containing:
  return(list(B.tk = B.tk, ##<< Matrix of B-splines.
              alphayears.k = alphayears.k, ##<< Vector of knots.
              uyears.q = uyears.q, ##<< Vector of years corresponding to u's.
              q.select = q.select ##<< List of q's corresponding to u's which will be adjusted, if applicable.
              ))
}
