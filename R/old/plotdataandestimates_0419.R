#----------------------------------------------------------------------
# plotdataandestimates.R
# Leontine Alkema & Jin Rou New, 2012-2013
#----------------------------------------------------------------------
PlotDataAndEstimates <- function(# Plot data, estimated fits and uncertainty intervals.
  ### Plot data, estimated fits and uncertainty intervals.
  data, ##<< \code{data} list from \code{\link{ReadData}}.
  data.all = NULL, ##<< \code{data.all} list from \code{\link{ReadData}}. 
  ## If \code{NULL}, excluded series/observations are not plotted.
  c, ##<< Index of country to plot.
  est.years = NULL,##<< Estimation years for CIs.cqt.
  CIs.cqt = NULL,##<< Optional: Show CIs for country c (percentiles lower-median-upper and estimation year t).
  CIs.tr.cqt = NULL, ##<< Optional: Show training CIs?
  CIs.iid.cqt = NULL, ##<< Optional: Show old CIs?.
  CIs2.cqt = NULL, ##<< Optional: Show another set of CIs?
  CIs3.cqt = NULL, ##<< Optional: Show yet another set of CIs?
  CIs4.cqt = NULL, ##<< Optional: Show yet one more set of CIs?
  legendfull  = NULL, ##<< Optional: Name in legend for CIs.
  legendtr = NULL, ##<< Optional: Name in legend for CIs.tr.cqt.
  legendiid = NULL, ##<< Optional: Name in legend for CIs.iid.cqt.
  legend2 = NULL, ##<< Optional: Name in legend for CIs2.cqt.
  legend3 = NULL, ##<< Optional: Name in legend for CIs3.cqt.
  legend4 = NULL, ##<< Optional: Name in legend for CIs4.cqt.
  igme = NULL,##<< Optional: Include IGME estimates? Output from \code{\link{GetIGME}}.
  igme2 = NULL,##<< Optional: Include alternative IGME estimates? Output from \code{\link{GetIGME}}.
  legendigme2 = NULL,##<< Optional: Legend for alternative IGME estimates.
  legendigmemethod = NULL, ##<< Optional: Indicate IGME method used in legend.
  legend.bty = "o",
  plot.se = TRUE, ##<< Show SEs?
  newobsPIs.ciq = NULL, ##<< Optional: Show PIs for new observations? Array of quantiles for exp(ypredicts) 
  ## for country c and observation i. Output from \code{\link{ConstructOutput}}.
  PIexcl.iq = NULL, ##<< Optional. Show PIs for excluded observations?
  excluded.surveys.Lc.s = NULL, ##<< Optional: Show excluded surveys in grey.
  excludedobsandyears.Lc.i2 = NULL, ##<< Optional: Show excluded observations in grey.
  Ytr.c = NULL, ##<< Optional: Show last observation year as a vertical line in training set?
  plot.biasadjobs = FALSE, ##<< Show bias-adjusted observations?
  plot.b1adjobs = FALSE, ##<< Show bias-adjusted (for level only) observations?
  data.hivremoved.biasadjusted = NULL, ##<< Optional: Bias-adjusted data, required if \code{plot.biasadjobs}
  ## or \code{plot.b1adjobs} is \code{TRUE}.
  B.tk = NULL, ##<< Optional: Show B-splines (multiplied by constant)? Use resproject.list.c[[c]]$B.tk 
  ## from \code{\link{ConstructOutput}}.
  alphayears.k = NULL, ##<< Optional: Years corresponding to B-splines and alphas, needed to show B-splines 
  ## and exp(alphas) as points. Use resproject.list.c[[c]]$alphayears.k from \code{\link{ConstructOutput}}. # change JR, 20140423
  alpha.cp = NULL, ##<< Optional: Show exp(alphas) as points? Output from \code{\link{ConstructOutput}}
  knots = NULL, ##<< Optional: Show knots as dotted vertical lines? Use 
  ## resproject.list.c[[c]]$uyears.q.plot from \code{\link{ConstructOutput}}.  
  u5.tj = NULL, ##<< Optional: Show a sample of trajectories for a particular country for estimation year t 
  ## and trajectory j.  
  col.CI = "#FF000020", ##<< Optional: Colour for CIs, default is red. # change JR, 24 Sep 2013: from FF000032
  col.val = "#00FF0032", ##<< Optional: Colour for CIs.tr, default is green. # change JR, 24 Sep 2013: from #00FF0050
  col.un = "#0000FF32", ##<< Optional: Colour for CIs.iid.cqt, default is blue.
  col.CI2 = "#FFA50032", ##<< Optional: Colour for another set of CIs?
  col.CI3 = "#A020F032", ##<< Optional: Colour for yet another set of CIs?
  col.CI4 = "#FFC0CB32", ##<< Optional: Colour for yet one more set of CIs?
  col.igme = "black", ##<< Optional: Colour for IGME estimates, default black.
  col.igme2 = "darkgreen", ##<< Optional: Colour for alternative IGME estimates, default dark green.
  col.data = NULL, ##<< Optional: Colour (same for all data points) for included data
  col.data.all = NULL, ##<< Optional: Colour (same for all data points) for excluded data
  col.newobsPIs = "#FF000050", ##<< Optional: Colour for PIs for new obs, default red.
  col.biasadjobs = "#0000FF50", ##<< Optional: Colour for bias-adjusted observations, default blue.
  col.b1adjobs = "#60331150", ##<< Optional: Colour for bias-adjusted (for level only) 
  ## observations, default brown.
  ylab = "U5MR", ##<< y-axis label for both plots.
  ymax = NULL, ##<< Optional: User-defined ymax for first plot.
  title1 = NULL, ##<< Optional: Title for first plot.
  title2 = NULL, ##<< Optional: Title for second plot.
  main.plot = TRUE, ##<< Include main plot? # change JR, 26 Aug 2013
  zoom = TRUE, ##<< Add zoom plot?
  zoom.year.start = 1990, ##<< First year of zoom range for zoom plot.
  zoom.year.end = 2013, ##<< Last year of zoom range for zoom plot.
  add.legend = TRUE, ##<< Add legend plot?
  seriesnames.in.full = TRUE, ##<< Display series names in full? # change JR, 3 Sep 2013 
  mfrow.suppress = FALSE, ##<< Change layout of plots with layout() argument instead of mfrow argument?
  suppress.legend.plot1 = FALSE, ##<< Suppress legend in plot 1?
  suppress.legend.plot2 = FALSE, ##<< Suppress legend in plot 2?
  cex.adj.factor = 1, ##<< Optional: Factor to adjust plot text size by
  cex.legend = 1.5 ##<< cex for the legend
) {
  if (main.plot + zoom == 2) {
    i.seq <- 1:2
  } else if (main.plot + zoom == 1) {
    if (main.plot)
      i.seq <- 1
    if (zoom)
      i.seq <- 2
  } else {
    cat("Error: Either main.plot or zoom must be TRUE.")
    return()
  }
  # change JR, 3 Sep 2013
  if ((main.plot + zoom + add.legend) == 3 & !seriesnames.in.full) {
    plot.widths <- c(1.1, 1.1, 0.8)
  } else {
    plot.widths <- rep(1, main.plot + zoom + add.legend)
  }
  if (!mfrow.suppress) 
    layout(matrix(1:(main.plot + zoom + add.legend), 1, main.plot + zoom + add.legend), widths = plot.widths)
  par(mar = c(5.5, 5.5, 5, 1), mgp = c(3.5, 1.5, 0), # change JR, 20140422
      cex.main = 3*cex.adj.factor, cex.axis = 2*cex.adj.factor, cex.lab = 2*cex.adj.factor)
  for (i in i.seq) { # second plot is zoomed
    if (i == 1) {
      main <- ifelse(!is.null(title1), title1, ifelse(!is.null(data$name.c[c]), data$name.c[c], data.all$name.c[c]))
      if (is.null(data.all)) {
        xmin <- round(-2+min(data$yearvr.Lc.j[[c]], unlist(data$year.Lcs.j[[c]])))
        ymin <- 0  
        ymax <- ifelse(is.null(ymax), min(500, 1.1*max(data$uvr.Lc.j[[c]], unlist(data$u.Lcs.j[[c]]))), ymax)
        xmax <- round(0+max(est.years, data$yearvr.Lc.j[[c]], unlist(data$year.Lcs.j[[c]])))
      } else {
        xmin <- round(-2+min(data.all$yearvr.Lc.j[[c]], unlist(data.all$year.Lcs.j[[c]])))
        ymin <- 0
        ymax <- ifelse(is.null(ymax), min(500, 1.1*max(data.all$uvr.Lc.j[[c]], unlist(data.all$u.Lcs.j[[c]]))), 
                       ymax)
        xmax <- round(0+max(est.years, data.all$yearvr.Lc.j[[c]], unlist(data.all$year.Lcs.j[[c]])))
      }
    } else {
      main <- ifelse(!is.null(title2), title2, 
                     ifelse(main.plot, "Zoomed in", 
                            ifelse(!is.null(data$name.c[c]), data$name.c[c], data.all$name.c[c])))
      xmin <- zoom.year.start
      xmax <- zoom.year.end
      if (!is.null(CIs.cqt)) {
        ymin <- max(0, 0.9*min(CIs.cqt[c, c(1,2), is.element(floor(est.years), seq(floor(xmin), floor(xmax)))],
                                  na.rm = T), na.rm = T)
        ymax <- min(500, 1.1*max(CIs.cqt[c, c(1,2), is.element(floor(est.years), seq(floor(xmin), floor(xmax)))],
                                  na.rm = T), na.rm = T)        
      } else {
        ymin <- ymax <- NULL
      }
      if (!is.null(CIs.tr.cqt)) {        
        ymin <- min(ymin, 0.9*min(CIs.tr.cqt[c, c(1,2), is.element(floor(est.years), seq(floor(xmin), floor(xmax)))],
                                  na.rm = T), na.rm = T)
        ymax <- max(ymax, 1.1*max(CIs.tr.cqt[c, c(1,2), is.element(floor(est.years), seq(floor(xmin), floor(xmax)))],
                                  na.rm = T), na.rm = T)
      }
      if (!is.null(data)) {
        ymin <- min(ymin, 0.9*min(data$uvr.Lc.j[[c]][data$yearvr.Lc.j[[c]] <= xmax 
                                                     & data$yearvr.Lc.j[[c]] >= xmin], 
                                  unlist(data$u.Lcs.j[[c]])[unlist(data$year.Lcs.j[[c]]) <= xmax & 
                                                              unlist(data$year.Lcs.j[[c]]) >= xmin],
                                  na.rm = T), na.rm = T)
        ymax <- max(ymax, 1.1*max(data$uvr.Lc.j[[c]][data$yearvr.Lc.j[[c]] <= xmax & 
                                                       data$yearvr.Lc.j[[c]] >= xmin], 
                                  unlist(data$u.Lcs.j[[c]])[unlist(data$year.Lcs.j[[c]]) <= xmax & 
                                                              unlist(data$year.Lcs.j[[c]]) >= xmin],
                                  na.rm = T), na.rm = T)
      }
      if (!is.null(data.all)) {
        ymin <- min(ymin, 0.9*min(data.all$uvr.Lc.j[[c]][data.all$yearvr.Lc.j[[c]] <= xmax 
                                                     & data.all$yearvr.Lc.j[[c]] >= xmin], 
                                  unlist(data.all$u.Lcs.j[[c]])[unlist(data.all$year.Lcs.j[[c]]) <= xmax & 
                                                              unlist(data.all$year.Lcs.j[[c]]) >= xmin],
                                  na.rm = T), na.rm = T)
        ymax <- max(ymax, 1.1*max(data.all$uvr.Lc.j[[c]][data.all$yearvr.Lc.j[[c]] <= xmax & 
                                                       data.all$yearvr.Lc.j[[c]] >= xmin], 
                                  unlist(data.all$u.Lcs.j[[c]])[unlist(data.all$year.Lcs.j[[c]]) <= xmax & 
                                                              unlist(data.all$year.Lcs.j[[c]]) >= xmin],
                                  na.rm = T), na.rm = T)
      }
      # ymin <- 0 # adhoc change JR, 20140323
    } # end i loop
    plot(1, ylab = ylab, main = main, xlab = "Year", xlim = c(xmin, xmax), ylim = c(ymin, ymax), type = "n")
    # plot estimated fits and CIs
    if (!is.null(CIs4.cqt))
      PlotCIs(c = c, CIs.cqt = CIs4.cqt, year.t = est.years, col.est = "pink", col.CI = col.CI4)
    if (!is.null(CIs3.cqt))
      PlotCIs(c = c, CIs.cqt = CIs3.cqt, year.t = est.years, col.est = "purple", col.CI = col.CI3)
    if (!is.null(CIs2.cqt))
      PlotCIs(c = c, CIs.cqt = CIs2.cqt, year.t = est.years, col.est = "orange", col.CI = col.CI2)
    if (!is.null(CIs.iid.cqt))
      PlotCIs(c = c, CIs.cqt = CIs.iid.cqt, year.t = est.years, col.est = "blue", col.CI = col.un)
    if (!is.null(CIs.tr.cqt))
      PlotCIs(c = c, CIs.cqt = CIs.tr.cqt, year.t = est.years, col.est = "green", col.CI = col.val)
    if (!is.null(CIs.cqt))
      PlotCIs(c = c, CIs.cqt = CIs.cqt, year.t = est.years, col.est = "red", col.CI = col.CI)
    if (plot.se) {
      # add survey and VR SEs # change JR, 1 Jun
      if (!is.null(data.all)) {
        AddSurveyData(u.Ls.i = data.all$u.Lcs.j[[c]], 
                      year.Ls.i = data.all$year.Lcs.j[[c]],
                      se.Ls.i = data.all$se.Lcs.j[[c]], 
                      included.Ls.i = data.all$included.Lcs.j[[c]],
                      plot.se = TRUE, plot.data.excl = TRUE,
                      seriesnames.in.full = seriesnames.in.full,
                      lwd = 2*cex.adj.factor)
        AddVRData(u.Ls.i = data.all$uvr.Lcs.j[[c]], 
                  year.Ls.i = data.all$yearvr.Lcs.j[[c]],
                  se.Ls.i = data.all$sevr.Lcs.j[[c]], 
                  included.Ls.i = data.all$includedvr.Lcs.j[[c]],
                  source.s = data.all$sourcevr.Lc.s[[c]],
                  plot.se = TRUE, plot.data.excl = TRUE,
                  seriesnames.in.full = seriesnames.in.full,
                  lwd = 2*cex.adj.factor)
      } else {
        AddSurveyData(u.Ls.i = data$u.Lcs.j[[c]], 
                      year.Ls.i = data$year.Lcs.j[[c]],
                      se.Ls.i = data$se.Lcs.j[[c]], 
                      included.Ls.i = data$included.Lcs.j[[c]],
                      plot.se = TRUE, plot.data.excl = TRUE,
                      seriesnames.in.full = seriesnames.in.full,
                      lwd = 2*cex.adj.factor)
        AddVRData(u.Ls.i = data$uvr.Lcs.j[[c]], 
                  year.Ls.i = data$yearvr.Lcs.j[[c]],
                  se.Ls.i = data$sevr.Lcs.j[[c]], 
                  included.Ls.i = data$includedvr.Lcs.j[[c]],
                  source.s = data$sourcevr.Lc.s[[c]],
                  plot.se = TRUE, plot.data.excl = TRUE,
                  seriesnames.in.full = seriesnames.in.full,
                  lwd = 2*cex.adj.factor)
      }
    }
    if (!is.null(data.all)) {
      for (plot.data.excl in c(TRUE, FALSE)) {
        res <- AddSurveyData(u.Ls.i = data.all$u.Lcs.j[[c]], 
                             year.Ls.i = data.all$year.Lcs.j[[c]],
                             included.Ls.i = data.all$included.Lcs.j[[c]],
                             col.s = rep(col.data.all, data.all$nseriesnonvr.c[c]), # change JR, 20140527
                             sourcetype.s = data.all$sourcetype.Lc.s[[c]], 
                             method.s = data.all$method.Lc.s[[c]],
                             surveyyear.s = data.all$seriesyear.Lc.s[[c]],
                             source.s = data.all$source.Lc.s[[c]],
                             hasbias.s = data.all$hasbias.Lc.s[[c]], # change JR, 20140429
                             plot.data.excl = plot.data.excl,
                             seriesnames.in.full = seriesnames.in.full,
                             lwd = 2*cex.adj.factor)
      }
#       # different sets of data for data.all and data (e.g. comparing database of current year to previous year)
#       res <- AddSurveyData(u.Ls.i = data.all$u.Lcs.j[[c]], 
#                            year.Ls.i = data.all$year.Lcs.j[[c]],
#                            included.Ls.i = data.all$included.Lcs.j[[c]],
#                            col.s = rep(col.data.all, data.all$nseriesnonvr.c[c]), # change JR, 20140527
#                            sourcetype.s = data.all$sourcetype.Lc.s[[c]], 
#                            method.s = data.all$method.Lc.s[[c]],
#                            surveyyear.s = data.all$seriesyear.Lc.s[[c]],
#                            source.s = data.all$source.Lc.s[[c]],
#                            hasbias.s = data.all$hasbias.Lc.s[[c]], # change JR, 20140429
#                            plot.data.excl = FALSE,
#                            seriesnames.in.full = seriesnames.in.full,
#                            lwd = 2*cex.adj.factor)
#       res <- AddSurveyData(u.Ls.i = data$u.Lcs.j[[c]], 
#                            year.Ls.i = data$year.Lcs.j[[c]],
#                            included.Ls.i = data$included.Lcs.j[[c]],
#                            col.s = rep(col.data, data$nseriesnonvr.c[c]), # change JR, 20140527
#                            sourcetype.s = data$sourcetype.Lc.s[[c]], 
#                            method.s = data$method.Lc.s[[c]],
#                            surveyyear.s = data$seriesyear.Lc.s[[c]],
#                            source.s = data$source.Lc.s[[c]],
#                            hasbias.s = data$hasbias.Lc.s[[c]], # change JR, 20140429
#                            seriesnames.in.full = seriesnames.in.full,
#                            lwd = 2*cex.adj.factor)
    } else {
      res <- AddSurveyData(u.Ls.i = data$u.Lcs.j[[c]], 
                           year.Ls.i = data$year.Lcs.j[[c]],
                           included.Ls.i = data$included.Lcs.j[[c]],
                           col.s = rep(col.data, data$nseriesnonvr.c[c]), # change JR, 20140527
                           sourcetype.s = data$sourcetype.Lc.s[[c]], 
                           method.s = data$method.Lc.s[[c]],
                           surveyyear.s = data$seriesyear.Lc.s[[c]],
                           source.s = data$source.Lc.s[[c]],
                           hasbias.s = data$hasbias.Lc.s[[c]], # change JR, 20140429
                           seriesnames.in.full = seriesnames.in.full,
                           lwd = 2*cex.adj.factor)
    }
    legendtext.s <- res$legendtext.s
    legendcol.s <- res$legendcol.s
    legendbg.s <- res$legendbg.s
    legendpch.s <- res$legendpch.s
    if (plot.b1adjobs & !is.null(data.hivremoved.biasadjusted)) { 
      # add bias-adjusted (for level only) survey data series
      AddSurveyData(u.Ls.i = data.hivremoved.biasadjusted$ub1adj.Lcs.j[[c]], 
                    year.Ls.i = data$year.Lcs.j[[c]],
                    sourcetype.s = data$sourcetype.Lc.s[[c]], 
                    method.s = data$method.Lc.s[[c]], 
                    surveyyear.s = data$seriesyear.Lc.s[[c]], 
                    hasbias.s = data$hasbias.Lc.s[[c]], # change JR, 20140429
                    col.s = rep(col.b1adjobs, data$nseriesnonvr.c[c]),
                    plot.se = FALSE,
                    seriesnames.in.full = seriesnames.in.full,
                    lwd = 2*cex.adj.factor)
    }
    if (plot.biasadjobs & !is.null(data.hivremoved.biasadjusted)) { 
      # add bias-adjusted survey data series
      AddSurveyData(u.Ls.i = data.hivremoved.biasadjusted$ubiasadj.Lcs.j[[c]], 
                    year.Ls.i = data$year.Lcs.j[[c]],
                    sourcetype.s = data$sourcetype.Lc.s[[c]], 
                    method.s = data$method.Lc.s[[c]], 
                    surveyyear.s = data$seriesyear.Lc.s[[c]], 
                    hasbias.s = data$hasbias.Lc.s[[c]], # change JR, 20140429
                    col.s = rep(col.biasadjobs, data$nseriesnonvr.c[c]),
                    plot.se = FALSE,
                    seriesnames.in.full = seriesnames.in.full,
                    lwd = 2*cex.adj.factor)
    }
    if (!is.null(newobsPIs.ciq)) { # add PIs for new observations
      year.i <- c(unlist(data$year.Lcs.j[[c]]), unlist(data$yearvr.Lc.j[[c]]))
      for(y in 1:length(year.i)) {
        segments(year.i[y], newobsPIs.ciq[c, y, 1], year.i[y], newobsPIs.ciq[c, y, 3], 
                 col = col.newobsPIs)
      }
    }
    if (!is.null(PIexcl.iq)) { # add PIs for excluded observations
      segments(excludedobsandyears.Lc.i2[[c]][1:length(PIexcl.iq[,1]), 2], PIexcl.iq[, 1],
               excludedobsandyears.Lc.i2[[c]][1:length(PIexcl.iq[,1]), 2], PIexcl.iq[, 3], 
               col = "darkgrey", lwd = 3*cex.adj.factor)
    }  
    # plot a sample of trajectories
    if (!is.null(u5.tj)) {
      traj.colors <- rainbow(20)
      for (traj in 1:20) {
        j <- sample(1:ncol(u5.tj), 20)[traj]
        lines(u5.tj[, j] ~ est.years, col = traj.colors[traj], lwd = 1*cex.adj.factor)
      }
    }
    if (!is.null(B.tk) & !is.null(alphayears.k)) { # add B-splines
      # scaling.bsplines <- min(50, round(min(CIs.cqt[c, 2, ], na.rm = T), digits = -1))
      # for (k in 1:length(alphayears.k)) {
      #   lines(scaling.bsplines*B.tk[,k] ~ est.years, type= "l", col = k, lwd = 2*cex.adj.factor)
      # }
      # change JR, 20140424: splines for which coeffs are pooled are dashed
      maxyear <- unique(c(data$maxyear.c[c], data.all$maxyear.c[c]))
      K <- which(alphayears.k == maxyear + 1.5*unique(diff(alphayears.k)))
      lty.k <- c(rep(1, K-1), rep(2, length(alphayears.k)-K+1))
      scaling.bsplines <- min(50, round(min(CIs.cqt[c, 2, ], na.rm = T), digits = -1))
      for (k in 1:length(alphayears.k)) {
        lines(scaling.bsplines*B.tk[,k] ~ est.years, type= "l", col = k, lwd = 2*cex.adj.factor, lty = lty.k[k])
      }
    }
    if (!is.null(knots)) { # add knots
      for(k in 1:length(knots)) {
        abline(v = knots[k], col = "grey", lty = 2, lwd = 1*cex.adj.factor)
      }
      #text(x = knots, y = 0, labels = 1:length(knots))
    }
    # plot estimated fits and CIs
    if (!is.null(igme)) # add IGME fit # added before CIs
      lines(igme$u.ct[c,] ~ igme$t, col = col.igme, lty = 1, lwd = 5*cex.adj.factor) # change JR, 20140423
    if (!is.null(igme2)) # add alternative IGME fit # change JR, 20140423: order of igme and igme2 plotting swapped
      lines(igme2$u.ct[c,] ~ igme2$t, col = col.igme2, lty = 2, lwd = 5*cex.adj.factor) # change JR, 20140423
    if (!is.null(CIs4.cqt))
      lines(CIs4.cqt[c, 2, ] ~ est.years, col = "pink", lwd = 5*cex.adj.factor)
    if (!is.null(CIs3.cqt))
      lines(CIs3.cqt[c, 2, ] ~ est.years, col = "purple", lwd = 5*cex.adj.factor)
    if (!is.null(CIs2.cqt))
      lines(CIs2.cqt[c, 2, ] ~ est.years, col = "orange", lwd = 5*cex.adj.factor)
    if (!is.null(CIs.iid.cqt))
      lines(CIs.iid.cqt[c, 2, ] ~ est.years, col = "blue", lwd = 5*cex.adj.factor)
    if (!is.null(CIs.tr.cqt))
      lines(CIs.tr.cqt[c, 2, ] ~ est.years, col = "green", lwd = 5*cex.adj.factor)
    if (!is.null(CIs.cqt))
      lines(CIs.cqt[c, 2, ] ~ est.years, col = "red", lwd = 5*cex.adj.factor)
    if (!is.null(Ytr.c)) # add vertical line for last observation year
      abline(v = Ytr.c[c]) 
    if (!is.null(alpha.cp) & !is.null(alphayears.k)) # add exp(alphas) # change JR, 20140423: change alphayears.k.plot to alphayears.k
      points(exp(alpha.cp[c, alpha.cp[c, ] != 0]) ~ alphayears.k, type = "p", pch = 1, cex = 1.5*cex.adj.factor) # change JR, 20140423: change alphayears.k.plot to alphayears.k
    # add VR observations
    if (!is.null(data.all)) {
      for (plot.data.excl in c(TRUE, FALSE)) {
        resvr <- AddVRData(u.Ls.i = data.all$uvr.Lcs.j[[c]], 
                           year.Ls.i = data.all$yearvr.Lcs.j[[c]],
                           included.Ls.i = data.all$includedvr.Lcs.j[[c]],
                           col.s = rep(col.data.all, data.all$nseriesvr.c[c]), # change JR, 20140527
                           hasbias.Ls.i = data.all$hasbiasvr.Lcs.j[[c]], # change JR, 20140429
                           isincomplete.Ls.i = data.all$isincompletevr.Lcs.j[[c]], # change JR, 20140429
                           source.s = data.all$sourcevr.Lc.s[[c]],
                           plot.data.excl = plot.data.excl,
                           seriesnames.in.full = seriesnames.in.full,
                           lwd = 2*cex.adj.factor)
      }
#       resvr <- AddVRData(u.Ls.i = data.all$uvr.Lcs.j[[c]], 
#                          year.Ls.i = data.all$yearvr.Lcs.j[[c]],
#                          included.Ls.i = data.all$includedvr.Lcs.j[[c]],
#                          col.s = rep(col.data.all, data.all$nseriesvr.c[c]), # change JR, 20140527
#                          hasbias.Ls.i = data.all$hasbiasvr.Lcs.j[[c]], # change JR, 20140429
#                          isincomplete.Ls.i = data.all$isincompletevr.Lcs.j[[c]], # change JR, 20140429
#                          source.s = data.all$sourcevr.Lc.s[[c]],
#                          plot.data.excl = F,
#                          seriesnames.in.full = seriesnames.in.full,
#                          lwd = 2*cex.adj.factor)
#       resvr <- AddVRData(u.Ls.i = data$uvr.Lcs.j[[c]], 
#                          year.Ls.i = data$yearvr.Lcs.j[[c]],
#                          included.Ls.i = data$includedvr.Lcs.j[[c]],
#                          col.s = rep(col.data, data$nseriesvr.c[c]), # change JR, 20140527
#                          hasbias.Ls.i = data$hasbiasvr.Lcs.j[[c]], # change JR, 20140429
#                          isincomplete.Ls.i = data$isincompletevr.Lcs.j[[c]], # change JR, 20140429
#                          source.s = data$sourcevr.Lc.s[[c]],
#                          seriesnames.in.full = seriesnames.in.full,
#                          lwd = 2*cex.adj.factor)
    } else {
      resvr <- AddVRData(u.Ls.i = data$uvr.Lcs.j[[c]],
                         year.Ls.i = data$yearvr.Lcs.j[[c]],
                         included.Ls.i = data$includedvr.Lcs.j[[c]],
                         col.s = rep(col.data.all, data$nseriesvr.c[c]), # change JR, 20140527
                         hasbias.Ls.i = data$hasbiasvr.Lcs.j[[c]], # change JR, 20140429
                         isincomplete.Ls.i = data$isincompletevr.Lcs.j[[c]], # change JR, 20140429
                         source.s = data$sourcevr.Lc.s[[c]],
                         seriesnames.in.full = seriesnames.in.full,
                         lwd = 2*cex.adj.factor)
    }
    legendtext.s <- c(resvr$legendtext.s, legendtext.s)
    legendcol.s <- c(resvr$legendcol.s, legendcol.s)
    legendbg.s <- c(resvr$legendbg.s, legendbg.s)
    legendpch.s <- c(resvr$legendpch.s, legendpch.s)
    if (!is.null(excludedobsandyears.Lc.i2[[c]])) { # add excluded observations and years
      points(excludedobsandyears.Lc.i2[[c]][,1] ~ excludedobsandyears.Lc.i2[[c]][,2], 
             col = "white", bg = "black", pch = 21, lwd = 3*cex.adj.factor, cex = 0.8)
    }
    if (!is.null(excluded.surveys.Lc.s)) { # add excluded surveys
      if (!is.null(excluded.surveys.Lc.s[[paste(name.c[c])]])) {
        for (s in excluded.surveys.Lc.s[[paste(name.c[c])]]) {
          u <- data$u.Lcs.j[[c]][[s]]
          year <- data$year.Lcs.j[[c]][[s]]
          nobs <- length(u)
          points(u ~ year, pch = 19, col = "darkgrey", lwd = 5*cex.adj.factor)
          lines(u ~ year, col = darkgrey, lwd = 3*cex.adj.factor)  
        }
      }
    }
    # add legends    
    if (!( (i == 1 & suppress.legend.plot1) | (i == 2 & suppress.legend.plot2) )) {
      if (is.null(legendigmemethod))
        legendigmemethod <- "UN IGME 2012"
      if (is.null(legendigme2))
        legendigme2 <- "Default Loess"
      if (!is.null(igme) & !is.null(igme2)) {
        legend("bottomleft", legend = c(legendigmemethod, legendigme2), col = c(col.igme, col.igme2), # change JR, 20140423
               lty = c(1,2), lwd = 3, bty = legend.bty, cex = cex.legend*cex.adj.factor) # change JR, 20140423
      } else if (!is.null(igme)) {
        legend("bottomleft", legend = legendigmemethod, col = col.igme, lty = 1, lwd = 3, bty = legend.bty, # change JR, 20140423
               cex = cex.legend*cex.adj.factor)
      } else if (!is.null(igme2)) {
        legend("bottomleft", legend = legendigme2, col = col.igme2, lty = 2, lwd = 3, bty = legend.bty, # change JR, 20140423
               cex = cex.legend*cex.adj.factor)
      }
      if (!is.null(CIs.cqt) | !is.null(legendfull)) {
        if ((!is.null(CIs.tr.cqt) | !is.null(legendtr)) + (!is.null(CIs.iid.cqt) | !is.null(legendiid)) == 2) {
          legend.text <- c(ifelse(is.null(legendfull), "B3", legendfull), 
                           ifelse(is.null(legendtr), "Training", legendtr), 
                           ifelse(is.null(legendiid), "Old UIs", legendiid))
          if (!is.null(legend2)) 
            legend.text <- c(legend.text, legend2)
          if (!is.null(legend3)) 
            legend.text <- c(legend.text, legend3)
          if (!is.null(legend4)) 
            legend.text <- c(legend.text, legend4)
          legend.col <- c("red", "green", "blue")
          if (!is.null(legend2)) 
            legend.col <- c(legend.col, "orange")
          if (!is.null(legend3)) 
            legend.col <- c(legend.col, "purple")  
          if (!is.null(legend4)) 
            legend.col <- c(legend.col, "pink") 
          legend("topright", legend = legend.text, col = legend.col, lty = 1, lwd = 3, bty = legend.bty, 
                 cex = cex.legend*cex.adj.factor)        
        } else if ((!is.null(CIs.tr.cqt) | !is.null(legendtr)) + (!is.null(CIs.iid.cqt) | !is.null(legendiid)) == 1) {
          if (!is.null(CIs.tr.cqt) | !is.null(legendtr)) 
            legend("topright", legend = c(ifelse(is.null(legendfull), "B3", legendfull),
                                            ifelse(is.null(legendtr), "Training", legendtr)),
                   bty = legend.bty,
                   col = c("red", "green"), lty = 1, lwd = 3, cex = cex.legend*cex.adj.factor)
          if (!is.null(CIs.iid.cqt) | !is.null(legendiid)) 
            legend("topright", legend = c(ifelse(is.null(legendfull), "B3", legendfull), 
                                            ifelse(is.null(legendiid), "Old UIs", legendiid)),
                   bty = legend.bty,
                   col = c("red", "blue"), lty = 1, lwd = 3, cex = cex.legend*cex.adj.factor)
        } else {
          legend("topright", legend = c(ifelse(is.null(legendfull), "B3", legendfull)),
                 bty = legend.bty,
                 col = c("red"), lty = 1, lwd = 3, cex = cex.legend*cex.adj.factor)
        }
      } 
    } # end if(!(i = 1 & suppress.legend.plot1)) | !(i = 2 & suppress.legend.plot2)) loop  
  } # end i plots loop  
  if (add.legend) 
    PlotLegend(legendtext.s = legendtext.s, legendcol.s = legendcol.s, 
               legendbg.s = legendbg.s, legendpch.s = legendpch.s,
               cex = cex.legend*cex.adj.factor*ifelse(seriesnames.in.full, 1, 1.3), 
               seriesnames.in.full = seriesnames.in.full)
}
#----------------------------------------------------------------
AddSurveyData <- function(# Add survey data and/or sampling errors to plot
  u.Ls.i, 
  year.Ls.i, 
  se.Ls.i = NULL,
  included.Ls.i = NULL, # change JR, 3 Jun
  source.s = NULL, # change JR, 3 Jun
  sourcetype.s = NULL, 
  method.s = NULL,
  surveyyear.s = NULL,
  hasbias.s = NULL, # change JR, 20140429
  col.s = NULL, 
  plot.se = FALSE,
  plot.data.excl = FALSE,
  plot.newobsPIs = FALSE,
  plot.points = TRUE,
  lwd = 2,
  seriesnames.in.full = TRUE # change JR, 3 Sep 2013
) {
  # change JR, 10 May 2013: added select.surveys  
  # nsurveys <- length(year.Ls.i)
  select.surveys <- !sapply(year.Ls.i, is.null)
  nsurveys <- sum(select.surveys)
  if (nsurveys == 0 ) return()
  if (!is.null(surveyyear.s)) {
    # re-order if survey dates are not chronological nicer for plotting)
    surveys <- rev(order(as.numeric(substring(surveyyear.s[select.surveys], first = 1, last = 4))))
  } else {
    surveys <- seq(1, nsurveys)
  }
  if (is.null(col.s)) {
    col.palette <- c(brewer.pal(12, "Paired")[c(2, 4, 10, 6, 8)], 
                     brewer.pal(8, "Dark2")[4],
                     brewer.pal(12, "Paired")[c(1, 3, 5, 7, 9)]) # change JR, 24 Sep 2013
    col.s <- rep(col.palette, ceiling(nsurveys/length(col.palette)))[1:nsurveys]
    
  } 
  legendpch.si <- rep(NA, nsurveys)
  legendtext.si <- rep("", nsurveys)
  if (is.null(source.s) | !seriesnames.in.full) { # change JR, 4 Jun
    egendtext.si <- paste0(sourcetype.s, " ", method.s, " ", 
                            surveyyear.s)[select.surveys][surveys] # change JR, 3 Sep 2013
  } else {
    llegendtext.si <- paste0(source.s, " ", surveyyear.s, " (",  sourcetype.s, " ", 
                            method.s, ")")[select.surveys][surveys]
  }
  if (!is.null(hasbias.s)) {
    hasbias.si <- hasbias.s[select.surveys][surveys]
  } else {
    hasbias.si <- rep(0, nsurveys)
  }
  
  # note: s in data and legend/col now refer to different ordering
  si <- 0
  for (s in surveys) {
    si <- si+1
    u <- u.Ls.i[select.surveys][[s]]
    nobs <- length(u)
    year <- year.Ls.i[select.surveys][[s]]
    if (!is.null(se.Ls.i)) {
      se <- se.Ls.i[select.surveys][[s]]
    } else {
      se <- rep(NA, nobs)
    }
    if (!is.null(included.Ls.i)) {
      included <- included.Ls.i[select.surveys][[s]] # change JR, 3 Jun
    } else {
      included <- rep(1, nobs)
    }
    legendpch.si[si] <- ifelse(hasbias.si[si] == 1, # change JR, 20140429
                               ifelse(sum(included == 1) > 0, 18, 5),
                               ifelse(sum(included == 1) > 0, 19, 1))
    if (!plot.data.excl) {
      # u <- u[included == 1]
      # year <- year[included == 1]
      # se <- se[included == 1]
      # included <- included[included == 1]
      ### if there's no excluded in the series
      if(prod(unlist(included))!=0){ # if there's no exclusion in the series
        if (nobs == 1) {
          if (plot.se) {
            segments(year, u-2*se, year, u+2*se, col = col.s[si])
          } else {
            points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429 
                   bg = ifelse(included == 1, col.s[si], "white"), 
                   col = col.s[si], lwd = lwd, cex = 1.8)
          }   
        } else {
          if (plot.se){
            for (obs in 2:nobs){
              polygon(c(year[obs-1],year[obs-1],year[obs],year[obs],year[obs-1]),
                      c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                        u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]), 
                      col = adjustcolor("grey", alpha.f = 0.2), border = NA)
            }
          } else {
            lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))  
            if (plot.points) 
              points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429
                     bg = ifelse(included == 1, col.s[si], "white"), 
                     col = col.s[si], lwd = lwd, cex = 1.8)
          }
        }} else{ #if there's exclusion in the series
          sub=splitseries(u=u,inclusion = included,year=year)
          sub_u=sub$sub_u
          sub_year=sub$sub_year
          
          for (i in 1:length(sub_u)){
            year=sub_year[[i]]
            u=sub_u[[i]]
            
            
            if (nobs == 1) {
              if (plot.se) {
                segments(year, u-2*se, year, u+2*se, col = col.s[si])
              } else {
                points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429 
                       #bg = ifelse(included == 1, col.s[si], "white"), 
                       bg = col.s[si],
                       col = col.s[si], lwd = lwd, cex = 1.8)
              }   
            } else {
              if (plot.se){
                for (obs in 2:nobs){
                  polygon(c(year[obs-1],year[obs-1],year[obs],year[obs],year[obs-1]),
                          c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                            u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]), 
                          col = adjustcolor("grey", alpha.f = 0.2), border = NA)
                }
              } else {
                lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))  
                if (plot.points) 
                  points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429
                         #bg = ifelse(included == 1, col.s[si], "white"), 
                         bg = col.s[si],
                         col = col.s[si], lwd = lwd, cex = 1.8)
              }
            }}
          
        } # end of exlusion in the series
    } else{
      if (nobs == 1) {
        if (plot.se) {
          segments(year, u-2*se, year, u+2*se, col = col.s[si])
        } else {
          points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429 
                 bg = ifelse(included == 1, col.s[si], "white"), 
                 col = col.s[si], lwd = lwd, cex = 1.8)
        }   
      } else {
        if (plot.se){
          for (obs in 2:nobs){
            polygon(c(year[obs-1],year[obs-1],year[obs],year[obs],year[obs-1]),
                    c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                      u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]), 
                    col = adjustcolor("grey", alpha.f = 0.2), border = NA)
          }
        } else {
          lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))  
          if (plot.points) 
            points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429
                   bg = ifelse(included == 1, col.s[si], "white"), 
                   col = col.s[si], lwd = lwd, cex = 1.8)
        }
      }
    }
    
    
    
  }
  ##value<< List containing:
  return(list(legendcol.s = col.s, ##<< Vector of colours used to plot series.
              legendbg.s = col.s, ##<< Vector of background colours used to plot series (not used).
              legendtext.s = legendtext.si, ##<< Vector of series names.
              legendpch.s = legendpch.si ##<< Vector of legend plotting characters.
  ))
}
#----------------------------------------------------------------------
AddVRData <- function(# Add VR data and/or sampling errors to the plot
  u.Ls.i, 
  year.Ls.i, 
  se.Ls.i = NULL, 
  included.Ls.i = NULL, # change JR, 3 Jun 
  hasbias.Ls.i = NULL, # change JR, 20140429
  isincomplete.Ls.i = NULL, # change JR, 20140429
  source.s = NULL,
  col.s = NULL, 
  plot.se = FALSE, 
  plot.data.excl = FALSE,
  plot.points = TRUE,
  plot.lines = TRUE,
  plot.newobsPIs = FALSE, 
  lwd = 2,
  seriesnames.in.full = TRUE # change JR, 3 Sep 2013
) {
  # use for plot se or obs!
  nseries <- length(year.Ls.i)
  if (nseries == 0) return()
  series <- seq(1, nseries)
  if (is.null(col.s)) {
    col.s <- rep(c("black", brewer.pal(11, "BrBG")[1:4]), ceiling(nseries/4))[1:nseries]
    #col.s <- rep(c("black", "black", "black", "black", "black"), ceiling(nseries/4))[1:nseries]
  }
  legendtext.si <- paste0(ifelse(grepl("SVR", source.s[series]), "", "VR "), 
                          ifelse(grepl("WHO", source.s[series]), 
                                 ifelse(grepl("Recalculated", source.s[series]), "WHO (Recalculated) ", "WHO "),
                                 source.s[series]))
  # ad-hoc change JR, 3 Sep 2013
  legendtext.si <- gsub("Population Growth Estimation Experiment", "Pop Growth Est Expmt", legendtext.si)
  # note: s in data and legend/col now refer to different ordering
  si <- 0
  legendpch.si <- rep(NA, nseries)
  for (s in series) {
    si <- si+1
    u <- u.Ls.i[[s]]
    nobs <- length(u)
    year <- year.Ls.i[[s]]
    if (!is.null(se.Ls.i)) {
      se <- se.Ls.i[[s]]
    } else {
      se <- rep(NA, nobs)
    }
    
    if (!is.null(included.Ls.i)) {
      included <- included.Ls.i[[s]] # change JR, 3 Jun
    } else {
      included <- rep(1, nobs)
    }
    
    if (!is.null(hasbias.Ls.i)) { # change JR, 20140429
      hasbias <- hasbias.Ls.i[[s]]
    } else {
      hasbias <- rep(0, nobs)
    }
    
    if (!is.null(isincomplete.Ls.i)) { # change JR, 20140429
      isincomplete <- isincomplete.Ls.i[[s]]
    } else {
      isincomplete <- rep(0, nobs)
    }
    
    legendpch.si[si] <- ifelse(sum(included == 1) > 0, 15, 0)# 19, 1), # change JR, 20140423
    
    #start to revise
    
    
    if (!plot.data.excl) {
      # u <- u[included == 1]
      # year <- year[included == 1]
      # se <- se[included == 1]
      # included <- included[included == 1]
      if(prod(unlist(included))!=0){ # if there's no exclusion in the series
        if (nobs == 1) {
          if (plot.se) {
            segments(year, u-2*se, year, u+2*se, col = col.s[si])
          } else {
            points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                   bg = ifelse(included == 1, col.s[si], "white"),
                   col = col.s[si], lwd = lwd, 
                   cex = ifelse(hasbias | isincomplete, 2.5, 2)) # change JR, 20140429
          }   
        } else {
          if (plot.se) {
            for (obs in 2:nobs) {
              polygon(c(year[obs-1], year[obs-1], year[obs], year[obs], year[obs-1]),
                      c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                        u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]), 
                      col = adjustcolor("grey", alpha.f = 0.2), border = NA)
            }
          } else {
            if (plot.lines & !(all(as.logical(included) )& any(hasbias | isincomplete))) # change JR, 20140429
              lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))  
            if (plot.points) 
              points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                     bg = ifelse(included == 1, col.s[si], "white"), # change JR, 11 Jul
                     col = col.s[si], lwd = lwd, 
                     cex = ifelse(hasbias | isincomplete, 2.5, 2)) # change JR, 20140429
      }}
      }else{ # there is exclusion in the series
        sub=splitseries(u=u,inclusion = included,year=year)
        sub_u=sub$sub_u
        sub_year=sub$sub_year  
        
        for (i in 1:length(sub_u)){
          u=sub_u[[i]]
          year=sub_year[[i]]
          
          if (nobs == 1) {
            if (plot.se) {
              segments(year, u-2*se, year, u+2*se, col = col.s[si])
            } else {
              points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                     #bg = ifelse(included == 1, col.s[si], "white"),
                     bg= col.s[si],
                     col = col.s[si], lwd = lwd, 
                     cex = ifelse(hasbias | isincomplete, 2.5, 2)) # change JR, 20140429
            }   
          } else {
            if (plot.se) {
              for (obs in 2:nobs) {
                polygon(c(year[obs-1], year[obs-1], year[obs], year[obs], year[obs-1]),
                        c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                          u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]), 
                        col = adjustcolor("grey", alpha.f = 0.2), border = NA)
              }
            } else {
              if (plot.lines & !(all(as.logical(included) )& any(hasbias | isincomplete))) # change JR, 20140429
                lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))  
              if (plot.points) 
                points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                       #bg = ifelse(included == 1, col.s[si], "white"), # change JR, 11 Jul
                       bg = col.s[si],
                       col = col.s[si], lwd = lwd, 
                       cex = ifelse(hasbias | isincomplete, 2.5, 2)) # change JR, 20140429
            }} 
          
        }# end of subseries
    }} else { # end of point excl
      if (nobs == 1) {
        if (plot.se) {
          segments(year, u-2*se, year, u+2*se, col = col.s[si])
        } else {
          points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                 bg = ifelse(included == 1, col.s[si], "white"),
                 col = col.s[si], lwd = lwd, 
                 cex = ifelse(hasbias | isincomplete, 2.5, 2)) # change JR, 20140429
        }   
      } else {
        if (plot.se) {
          for (obs in 2:nobs) {
            polygon(c(year[obs-1], year[obs-1], year[obs], year[obs], year[obs-1]),
                    c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                      u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]), 
                    col = adjustcolor("grey", alpha.f = 0.2), border = NA)
          }
        } else {
          if (plot.lines & !(all(as.logical(included) )& any(hasbias | isincomplete))) # change JR, 20140429
            lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))  
          if (plot.points) 
            points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                   bg = ifelse(included == 1, col.s[si], "white"), # change JR, 11 Jul
                   col = col.s[si], lwd = lwd, 
                   cex = ifelse(hasbias | isincomplete, 2.5, 2)) # change JR, 20140429
        }}
      
      
    }
  }
  ##value<< List containing:
  return(list(legendcol.s = col.s, ##<< Vector of colours used to plot series.
              legendbg.s = col.s, ##<< Vector of background colours used to plot series.
              legendtext.s = legendtext.si, ##<< Vector of series names.
              legendpch.s = legendpch.si ##<< Vector of legend plotting characters.
  ))
}
#----------------------------------------------------------------------
PlotCIs <- function(# Add confidence intervals to plot
  c,
  CIs.cqt,
  year.t,
  col.est,
  col.CI,
  lwd = 5
) {
  for (t in 2:length(year.t)) {
    polygon(c(year.t[t-1], year.t[t-1], year.t[t], year.t[t], year.t[t-1]),
            c(CIs.cqt[c,1,t-1], CIs.cqt[c,3,t-1], CIs.cqt[c,3,t], CIs.cqt[c,1,t], CIs.cqt[c,1,t-1]), 
            col = col.CI,  border = NA)
  }
  lines(CIs.cqt[c, 2, ] ~ year.t, col = col.est, lwd = lwd)
  ##value<< \code{NULL}.
  return(invisible())
}
#----------------------------------------------------------------
PlotLegend <- function(# Plot legend.
  legendtext.s, ##<< Vector of legend text labels.
  legendcol.s, ##<< Vector of legend colours.
  legendbg.s = NULL, ##<< Vector of legend background colours for plotting characters.
  legendpch.s, ##<< Vector of legend plotting characters.
  seriesnames.in.full = TRUE, ##<< Display series names in full? If \code{FALSE}, only display series names
  ## for series with year 1990 or later if there are more than 20 series. # change JR, 3 Sep 2013 
  cex = 1.5 ##<< Magnification size of legend
) {
  par(mar = c(1,0,1,1))
  if (!seriesnames.in.full & length(legendtext.s) > 20) { # change JR, 3 Sep 2013
    select <- !grepl("190|191|192|193|194|195|196|197|198", legendtext.s)
    legendtext.s <- legendtext.s[select]
    legendcol.s <- legendcol.s[select]
    legendpch.s <- legendpch.s[select]
    if (!is.null(legendbg.s))
      legendbg.s <- legendbg.s[select]
  }
  plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  if (is.null(legendbg.s)) {
    legend("left", legend = c(rev(legendtext.s)) ,
           col = c(rev(legendcol.s)), pch = c(rev(legendpch.s)),
           cex = cex, lwd = 0.5*cex, lty = 1) 
  } else {
    legend("left", legend = c(rev(legendtext.s)) ,
           col = c(rev(legendcol.s)), pch = c(rev(legendpch.s)), pt.bg = c(rev(legendbg.s)),
           cex = cex, lwd = 0.5*cex, lty = 1) 
  }
  ##value<< \code{NULL}.
  return(invisible())
}
#----------------------------------------------------------------
EmptyPlot <- function() {
  plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}
#----------------------------------------------------------------
splitseries=function(
  inclusion,
  u,
  year
){
  ind_pos=which(inclusion==0)
  sub_inclusion=splitAt(inclusion,ind_pos)
  sub_u=splitAt(u,ind_pos)
  sub_year=splitAt(year,ind_pos)
  
  for(i in 1:length(sub_u)){
    sub_u[[i]]=sub_u[[i]][sub_inclusion[[i]]==1]
    sub_year[[i]]=sub_year[[i]][sub_inclusion[[i]]==1]
  }
  
  return(list(
    sub_u=sub_u,
    sub_inclusion=sub_inclusion,
    sub_year=sub_year
  ))
  
}

splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

