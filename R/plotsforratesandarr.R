#----------------------------------------------------------------------
# plotsforratesandarr.R
#----------------------------------------------------------------------
PlotARRCategories <- function(
  RoD.L, ##<< List containing name.c (vector of country names), iso.c (vector of ISO country codes),
  ## CIs.ARR.cq (matrix of UIs for ARR), select.c (logical vector indicating whether to plot country).
  RoD.region.L = NULL, ##<< List containing name.r (vector of region names) and
  ## CIs.changeinARR.rq (matrix of UIs for ARR) if plots of region quantities are desired.
  goal = 4.4, ##<< Target ARR (used as benchmark for categorisation).
  fig.dir = NULL,
  name.pdf = NULL, ##<< File name of pdf file if pdf output is desired. Either \code{name.pdf} or
  ## \code{name.tif} should be non-null.
  name.tif = NULL ##<< File name of tiff file if tiff output is desired. Either \code{name.pdf} or
  ## \code{name.tif} should be non-null.
) {
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig")
  dir.create(fig.dir, showWarnings = F)
  list2env(RoD.L, envir = environment())
  C <- nrow(CIs.ARR.cq)
  igme.c <- CIs.ARR.cq[, 2]
  low.c <- CIs.ARR.cq[, 1]
  up.c <- CIs.ARR.cq[, 3]
  regions <- unique(region.c)
  nregions <- length(regions)
  # categorize countries
  categories.all <- c("Unclear", "Unclear/Unlikely", "Likely/Unlikely", "Likely/Unclear", "Likely")
  cat.c <- ifelse(low.c <= 0 & up.c >= goal, 1,
                  ifelse(low.c <= 0 & up.c < goal, 2,
                         ifelse(low.c < goal & up.c < goal, 3,
                                ifelse(low.c < goal & up.c >= goal, 4,
                                       ifelse(low.c >= goal & up.c >= goal, 5, NA)))))
  cats <- sort(unique(cat.c[select.c]))
  categories <- categories.all[match(as.numeric(cats), 1:5)]
  # number of countries in each category
  cat(paste0("Number of countries per category for evidence of progress in reducing U5MR/progress at ARR of ",
             goal, "% and above:\n"))
  table.output <- table(cat.c[select.c]) 
  names(table.output) <- categories
  print(table.output)
  # percentage of countries in each category
  cat(paste0("Percentage of countries per category for evidence of progress in reducing U5MR/progress at ARR of ",
             goal, "% and above:\n"))
  table.output <- round(100*table(cat.c[select.c])/sum(select.c))
  names(table.output) <- categories
  print(table.output)
  # categorize regions
  if (!is.null(RoD.region.L)) {
    list2env(RoD.region.L, envir = environment())
    igme.r <- CIs.ARR.rq[, 2]
    low.r <- CIs.ARR.rq[, 1]
    up.r <- CIs.ARR.rq[, 3]
    cat.r <- ifelse(low.r <= 0 & up.r >= goal, 1,
                    ifelse(low.r <= 0 & up.r < goal, 2,
                           ifelse(low.r < goal & up.r < goal, 3,
                                  ifelse(low.r < goal & up.r >= goal, 4,
                                         ifelse(low.r >= goal & up.r >= goal, 5, NA)))))
    # cat.r[name.r == "World"] <-  1 # plot first
    select.r <- is.element(name.r, c("World", region.c[select.c]))
  }
  # plot
  if (!is.null(RoD.region.L)) {
    nlines1 <- sum(c(select.r & is.element(cat.r, 1:3)))*1.5 + sum(select.c & is.element(cat.c, 1:3))
    nlines2 <- sum(c(select.r & !is.element(cat.r, 1:3)))*1.5 + sum(select.c & !is.element(cat.c, 1:3))
    maxnlines <- max(nlines1, nlines2)
  } else {
    nlines1 <- sum(select.c & is.element(cat.c, 1:3))
    nlines2 <- sum(select.c & !is.element(cat.c, 1:3))
    maxnlines <- max(nlines1, nlines2)
  }
  ymin <- -floor(maxnlines)-2
  if (!is.null(name.pdf))
    pdf(file = file.path(fig.dir, name.pdf), width = 10, height = ceiling(0.2*maxnlines))
  if (!is.null(name.tif))
    tiff(file = file.path(fig.dir, name.tif), width = 10, height = ceiling(0.2*maxnlines), 
         units="cm", pointsize=8,res=300, bg="white",compression="lzw")
  par(mar = c(5,9,1,1), cex.axis = 1, cex.lab = 1.1, mfrow = c(1,2))
  if (!is.null(RoD.region.L)) {
    xmax <- 1.1*max(up.c[select.c], up.r[select.r])
    xmin <- min(low.c[select.c], low.r[select.r])
  } else {
    xmax <- 1.1*max(up.c[select.c])
    xmin <- min(low.c[select.c])
  }
  xmin <- ifelse(xmin < 0, 1.1*xmin, 0.9*xmin)
  totseqtoplot <- 0
  colors <- c("darkgrey", 2, 1, "blue", "darkgreen")
  for (cato in cats) {
    c.select.all <- seq(1, C)[select.c & is.element(cat.c, cato)]
    order <- order.country <- order(igme.c[c.select.all])#name.c
    if (!is.null(RoD.region.L)) {
      select.region <- select.r & is.element(cat.r, cato)
      if (sum(select.region) > 0) {
        r.select.all <- seq(1:length(name.r))[select.region]
        igmetemp.r <- igme.r
        igmetemp.r[name.r == "World"] <- -Inf
        order.region <- order(igmetemp.r[r.select.all])
        order <- c(order.region, order.country)
      } else {
        r.select.all <- NULL
      }
    } else {
      r.select.all <- NULL
    }
    if (!is.null(r.select.all)) {
      seqtoplot <- c(seq(totseqtoplot+1, totseqtoplot+length(order.region)*1.5, 1.5), 
                     seq(totseqtoplot+length(order.region)*1.5+1, totseqtoplot+length(order.region)*1.5+1+length(order.country)-1))
    } else {
      seqtoplot <- seq(totseqtoplot+1, totseqtoplot+length(order))
    }
    totseqtoplot <- max(seqtoplot)
    if (cato==cats[1] | cato==4) {
      totseqtoplot <- 0
      if (!is.null(r.select.all)) {
        seqtoplot <- c(seq(totseqtoplot+1, totseqtoplot+length(order.region)*1.5, 1.5), 
                       seq(totseqtoplot+length(order.region)*1.5+1, totseqtoplot+length(order.region)*1.5+1+length(order.country)-1))
      } else {
        seqtoplot <- seq(totseqtoplot+1, totseqtoplot+length(order))
      }
      totseqtoplot <- max(seqtoplot)
      plot(1, type = "n",
           main = "", xlim = c(xmin,xmax), ylim = c(ymin, 0),
           ylab = "", yaxt = "n", xlab = paste0("Annual rate of reduction 1990-", year.current, " (%)"),
           cex.lab = 0.8, cex.axis = 0.7, mgp =c(1.5, 0.5, 0)) # change JR, 27 Sep 2013
      abline(v = c(0, goal))
      polygon(c(-100,-100,0,0,-10), c(ymin-5,100,100,ymin-5,ymin-5), col = "#FF000032")
      polygon(c(goal, goal,100,100,goal), c(ymin-5,100,100,ymin-5,ymin-5), col = "lightgreen")
    } 
    totseqtoplot <- totseqtoplot + 1
    #   for (i in -seqtoplot[seq(1, length(-seqtoplot),2)]){
    #     polygon(-0.5+c(xmin,xmin,500,500,xmin),
    #             i + c(-0.5, 0.5, 0.5, -0.5, -0.5),
    #             col = "lightgrey", border = NA)
    #   }
    abline(h = -seqtoplot)
    if (!is.null(r.select.all)) {
      eval(parse(text = paste0("axis(2, at = -seqtoplot,
                               label = c(", 
                               paste(paste0("expression(bold(paste(\"",
                                            name.r[r.select.all][order.region], 
                                            "\")))"), collapse = ", "),
                               ", name.c[c.select.all][order.country]), las = 1, cex.axis = 0.7)")))
      # axis(2, at = -seqtoplot, 
      #      label = c(name.r[r.select.all][order.region], 
      #                name.c[c.select.all][order.country]),
      #      las = 1, cex.axis = 0.7)
    } else {
      axis(2, at = -seqtoplot, label = name.c[c.select.all][order.country], las = 1, cex.axis = 0.7)
    }
    box()
    if (!is.null(r.select.all)) {
      segments(c(low.r[r.select.all][order.region], low.c[c.select.all][order.country]), -seqtoplot,
               c(up.r[r.select.all][order.region], up.c[c.select.all][order.country]), -seqtoplot, 
               lwd = c(rep(6, length(r.select.all)), rep(4, length(c.select.all))), 
               col = colors[cato])    
      points(-seqtoplot~c(igme.r[r.select.all][order.region], igme.c[c.select.all][order.country]), pch = 19,          
             lwd = c(rep(5, length(r.select.all)), rep(3, length(c.select.all))),
             col = colors[cato])
    } else {
      segments(low.c[c.select.all][order.country], -seqtoplot,
               up.c[c.select.all][order.country], -seqtoplot, lwd = 4, 
               col = colors[cato])
      points(-seqtoplot~igme.c[c.select.all][order.country], pch = 19, 
             col = colors[cato], lwd = 3)
    }
  }
  dev.off()
}
#----------------------------------------------------------------------
PlotChangeInARRCategories <- function(
  RoD.L, ##<< List containing name.c (vector of country names), iso.c (vector of ISO country codes),
  ## CIs.changeinARR.cq (matrix of UIs for ARR), select.c (logical vector indicating whether to plot country),
  ## region.c (vector with region that country belongs to).
  RoD.region.L = NULL, ##<< List containing name.r (vector of region names) and
  ## CIs.changeinARR.rq (matrix of UIs for ARR) if plots of region quantities are desired.
  year1,
  year2,
  year4,
  fig.dir = NULL,
  name.pdf = NULL, ##<< File name of pdf file if pdf output is desired. Either \code{name.pdf} or
  ## \code{name.tif} should be non-null.
  name.tif = NULL ##<< File name of tiff file if tiff output is desired. Either \code{name.pdf} or
  ## \code{name.tif} should be non-null.
) {
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig")
  dir.create(fig.dir, showWarnings = F)
  list2env(RoD.L, envir = environment())
  C <- nrow(CIs.changeinARR.cq)
  igme.c <- CIs.changeinARR.cq[, 2]
  low.c <- CIs.changeinARR.cq[, 1]
  up.c <- CIs.changeinARR.cq[, 3]
  regions <- unique(region.c)
  nregions <- length(regions)
  # categorize countries
  categories.all <- c("Positive change", "Negative change", "Unclear")
  cat.c <- ifelse(low.c <= 0 & up.c >= 0, 3,
                  ifelse(low.c < 0 & up.c < 0, 2,
                         ifelse(low.c > 0 & up.c > 0, 1, NA)))
  cats <- sort(unique(cat.c[select.c]))
  categories <- categories.all[match(as.numeric(cats), 1:3)]
  # number of countries in each category
  cat(paste0("Number of countries per category for evidence of change in ARR from ", 
             year1, "-", year2, " to ", year2, "-", year4, ":\n"))
  table.output <- table(cat.c[select.c]) 
  names(table.output) <- categories
  print(table.output)
  # percentage of countries in each category
  cat(paste0("Percentage of countries per category for evidence of change in ARR from ",
             year1, "-", year2, " to ", year2, "-", year4, ":\n"))
  table.output <- round(100*table(cat.c[select.c])/sum(select.c))
  names(table.output) <- categories
  print(table.output)
  
  # categorize regions
  if (!is.null(RoD.region.L)) {
    list2env(RoD.region.L, envir = environment())
    igme.r <- CIs.changeinARR.rq[, 2]
    low.r <- CIs.changeinARR.rq[, 1]
    up.r <- CIs.changeinARR.rq[, 3]
    cat.r <- ifelse(low.r <= 0 & up.r >= 0, 3,
                    ifelse(low.r < 0 & up.r < 0, 2,
                           ifelse(low.r > 0 & up.r > 0, 1, NA)))
    cat.r[name.r == "World"] <-  1 # plot first
    select.r <- is.element(name.r, c("World", region.c[select.c]))
  }
  # plot
  if (!is.null(RoD.region.L)) {
    nlines1 <- sum(c(select.r & is.element(cat.r, 1:2)))*1.5 + sum(select.c & is.element(cat.c, 1:2))
    nlines2 <- sum(c(select.r & !is.element(cat.r, 1:2)))*1.5 + sum(select.c & !is.element(cat.c, 1:2))
    maxnlines <- max(nlines1, nlines2)
  } else {
    nlines1 <- sum(select.c & is.element(cat.c, 1:2))
    nlines2 <- sum(select.c & !is.element(cat.c, 1:2))
    maxnlines <- max(nlines1, nlines2)
  }
  ymin <- -floor(maxnlines)-1
  ymin1 <- -floor(nlines1)-1
  ymin2 <- -floor(nlines2)-1
  if (!is.null(name.pdf))
    pdf(file = file.path(fig.dir, name.pdf), width = 10, height = ceiling(0.2*maxnlines))
  if (!is.null(name.tif))
    tiff(file = file.path(fig.dir, name.tif), width = 18, height = 18, 
         units="cm", pointsize=8,res=300, bg="white",compression="lzw")
  par(mar = c(6,10,1,1), mgp = c(4,1,0), cex.axis = 1, cex.lab = 1, mfrow = c(1,2))
  if (!is.null(RoD.region.L)) {
    xmax <- 1.1*max(up.c[select.c], up.r[select.r])
    xmin <- min(low.c[select.c], low.r[select.r])
  } else {
    xmax <- 1.1*max(up.c[select.c])
    xmin <- min(low.c[select.c])
  }
  xmin <- ifelse(xmin < 0, 1.1*xmin, 0.9*xmin)
  totseqtoplot <- 0
  # color line by region
  colors <- brewer.pal(nregions, "Paired")
  color.c <- colors[match(region.c, regions)]
  if (!is.null(RoD.region.L))
    color.r <- colors[match(name.r, regions)]
  for (cato in cats) {
    c.select.all <- seq(1, C)[select.c & is.element(cat.c, cato)]
    order <- order.country <- order(-igme.c[c.select.all])#name.c
    if (!is.null(RoD.region.L)) {
      select.region <- select.r & is.element(cat.r, cato)
      if (sum(select.region) > 0) {
        r.select.all <- seq(1:length(name.r))[select.region]
        igmetemp.r <- igme.r
        igmetemp.r[name.r == "World"] <- Inf
        order.region <- order(-igmetemp.r[r.select.all])
        order <- c(order.region, order.country)
      } else {
        r.select.all <- NULL
      }
    } else {
      r.select.all <- NULL
    }
    if (!is.null(r.select.all)) {
      seqtoplot <- c(seq(totseqtoplot+1, totseqtoplot+length(order.region)*1.5, 1.5), 
                     seq(totseqtoplot+length(order.region)*1.5+1, totseqtoplot+length(order.region)*1.5+1+length(order.country)-1))
    } else {
      seqtoplot <- seq(totseqtoplot+1, totseqtoplot+length(order))
    }
    totseqtoplot <- max(seqtoplot)
    if (cato==1 | cato==3) {
      totseqtoplot <- 0
      if (!is.null(r.select.all)) {
        seqtoplot <- c(seq(totseqtoplot+1, totseqtoplot+length(order.region)*1.5, 1.5), 
                       seq(totseqtoplot+length(order.region)*1.5+1, totseqtoplot+length(order.region)*1.5+1+length(order.country)-1))
      } else {
        seqtoplot <- seq(totseqtoplot+1, totseqtoplot+length(order))
      }
      totseqtoplot <- max(seqtoplot)
      plot(1, type = "n",
           main = "", xlim = c(xmin,xmax), ylim = c(ymin, -0.5),
           ylab = "", yaxt = "n", 
           xlab = paste0("Change in ARR from period \n", year1, "-", year2, " to ", year2, "-", year4, " (%)"))
      abline(v = 0)
      polygon(c(-100,-100,0,0,-10), 
              c(ymin-5,100,100,ymin-5,ymin-5), 
              col = "white") ##FF000032") 
      polygon(c(0,0, 100,100,0), 
              c(ymin-5,100,100,ymin-5,ymin-5), 
              col = "lightgreen")
    } 
    totseqtoplot <- totseqtoplot + 1
    abline(h = -seqtoplot)
    if (!is.null(r.select.all)) {
      eval(parse(text = paste0("axis(2, at = -seqtoplot,
                               label = c(", 
                               paste(paste0("expression(bold(paste(\"",
                                            name.r[r.select.all][order.region], 
                                            "\")))"), collapse = ", "),
                               ", name.c[c.select.all][order.country]), las = 1, cex.axis = 0.7)")))
      # axis(2, at = -seqtoplot, 
      #      label = c(name.r[r.select.all][order.region], 
      #                name.c[c.select.all][order.country]),
      #      las = 1, cex.axis = 0.7)
    } else {
      axis(2, at = -seqtoplot, label = name.c[c.select.all][order.country], las = 1, cex.axis = 0.7)
    }
    box()
    if (!is.null(r.select.all)) {
      colors.plot <- c(color.r[r.select.all][order.region], color.c[c.select.all][order.country])
      colors.plot[is.na(colors.plot)] <- "royalblue"
      segments(c(low.r[r.select.all][order.region], low.c[c.select.all][order.country]), -seqtoplot,
               c(up.r[r.select.all][order.region], up.c[c.select.all][order.country]), -seqtoplot, 
               lwd = c(rep(6, length(r.select.all)), rep(4, length(c.select.all))), 
               col = colors.plot)    
      points(-seqtoplot~c(igme.r[r.select.all][order.region], igme.c[c.select.all][order.country]), pch = 19,          
             lwd = c(rep(5, length(r.select.all)), rep(3, length(c.select.all))),
             col = colors.plot)
    } else {
      segments(low.c[c.select.all][order.country], -seqtoplot,
               up.c[c.select.all][order.country], -seqtoplot, lwd = 4, 
               col = color.c[c.select.all][order.country])
      points(-seqtoplot~igme.c[c.select.all][order.country], pch = 19, 
             col = color.c[c.select.all][order.country], lwd = 3)
    }
    if (cato == 1)
      # legend
      legend("bottom", legend = c("World", unique(region.c[select.c])), 
             col = c("royalblue", unique(color.c[select.c])), 
             lwd = 4, cex = 0.8)
  }
  dev.off()
}
#----------------------------------------------------------------------
PlotDecompositionOfDifferences <-  function(
  res, ##<< List of differences containing d.ct, m.ct, e.ct, iso.c, name.c and years.
  iso.select = NULL, ##<< ISO country codes to plot. If \code{NULL}, all ISOs in res$iso.c are used.
  indicator = "U5MR",
  errorlabel = "B3 - Default Loess",
  xaxislabel = "splines model", ##<< Corresponds to res$m.ct
  yaxislabel = "data model", ##<< Corresponds to res$d.ct
  cutoff.totalerror = NULL, ##<< Cut-off below which points are not displayed on plot.
  cutoff = 10, ##<< Cut-off above which points are displayed as red on plot.
  cutofft = 10, ##<< Cut-off above which text labels are displayed on plot.
  display.labels.as.text = TRUE, ##<< Display labels as text? If \code{FALSE}, display as numbers and plot legend.
  pch.cex = 1,
  fig.dir = NULL,
  fig.type = "pdf"
) {
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig")
  dir.create(fig.dir, showWarnings = F)
  if (is.null(iso.select))
    iso.select <- res$iso.c  
  
  if (fig.type == "pdf") {
    pdf(file.path(fig.dir, paste0("decompoU_", gsub("-", "vs", errorlabel), ".pdf")), 
        width = ifelse(display.labels.as.text, length(res$years)*7, length(res$years)*10), height = 7.5) # change JR, 20 Aug 2013
  } else if (fig.type == "eps") {
    postscript(file.path(fig.dir, paste0("decompoU_", gsub("-", "vs", errorlabel), ".eps")),
               width = ifelse(display.labels.as.text, length(res$years)*7, length(res$years)*10), height = 7.5
               #  , pointsize = 8
    )
  }
  # tiff(file = paste0(fig.dir, "decompoU_", year, ".tif"), width = 3.27, height = 3.27, units = "in",
  #      pointsize = 6, res = 300, bg = "white", compression = "lzw") 
  # tiff(file = paste0(fig.dir, "/tifs/", "fig10.tif"), width = 6.83, height = 3.27,
  #      units = "in", pointsize = 8, res = 300, bg = "white", compression = "lzw")
  if (display.labels.as.text) {
    layout(matrix(1:length(res$years), 1, length(res$years)))
  } else {
    layout(matrix(1:(length(res$years)*2), 1:(length(res$years)*2)), 
           width = c(rep(c(3.45, 1.55), length(res$years))))
  }
  xlim.factor <- ifelse(display.labels.as.text, 30, 2)
  ylim.factor <- 2
  for (year in res$years) {
    if (!is.null(cutoff.totalerror)) {
      select.countries <- is.element(res$iso.c, iso.select) &
        abs(res$e.ct[, res$years == year]) > cutoff.totalerror
    } else {
      select.countries <- is.element(res$iso.c, iso.select)
    } 
    cat(paste0(sum(select.countries), " countries in total are plotted for ", year, ".\n"))
    #pdf(paste0(fig.dir, "decompoU_", year, ".pdf")); par(mfrow = c(1, length(year)))
    #   xlim = c(min(res$m.ct[select.countries, res$years == year], 
    #                res$d.ct[select.countries, res$years == year], na.rm = T)-30, 
    #            30+max(res$m.ct[select.countries, res$years == year], 
    #                   res$d.ct[select.countries, res$years == year], na.rm = T))
    xlim = c(min(-10.5, res$m.ct[select.countries, res$years == year], na.rm = T)-xlim.factor, 
             xlim.factor+max(10.5, res$m.ct[select.countries, res$years == year], na.rm = T))
    ylim = c(min(-10.5, res$d.ct[select.countries, res$years == year], na.rm = T)-ylim.factor, 
             ylim.factor+max(10.5, res$d.ct[select.countries, res$years == year], na.rm = T))
    par(mar = c(4.5,4.5,4,0), cex = 1.3)
    plot(res$d.ct[select.countries, res$years == year] ~ 
           res$m.ct[select.countries, res$years == year],
         xlim = xlim, ylim = ylim,
         type = "p", pch = 19, col = "grey", 
         ylab = paste0("Difference due to ", yaxislabel),
         xlab = paste0("Difference due to ", xaxislabel), 
         main = paste0(indicator, " ", year, "\n(", errorlabel, ")"))
    #polygon(c(-100,-100,100,100,-100), c(-10,10,10,-10,-10), col = "grey", border = NA)
    #polygon(c(-10,10,10,-10,-10),c(-100,-100,100,100,-100),  col = "grey", border = NA)
    polygon(cutoff*c(-1,1,1,-1,-1),cutoff*c(-1,-1,1,1,-1),  col = "grey", border = NA)
    points(res$d.ct[select.countries, res$years == year] ~ 
             res$m.ct[select.countries, res$years == year], pch = 19, col = 1,
           cex = pch.cex)
    select <- select.countries &
      (abs(res$m.ct[, res$years == year]) > cutoff | (abs(res$d.ct[, res$years == year]) > cutoff))
    selectleft <- select & (res$m.ct[, res$years == year]) < 0
    selectright <- select & (res$m.ct[, res$years == year]) >= 0
    if (sum(selectleft, na.rm = T) > 0) 
      points(res$d.ct[selectleft, res$years == year] ~ res$m.ct[selectleft, res$years == year], pch = 19, col = 2, 
             cex = pch.cex)
    if (sum(selectright, na.rm = T) > 0) 
      points(res$d.ct[selectright, res$years == year] ~ res$m.ct[selectright, res$years == year], pch = 19, col = 2, 
             cex = pch.cex)
    # labels
    selectt <- select.countries &
      (abs(res$m.ct[, res$years == year]) > cutofft | (abs(res$d.ct[, res$years == year]) > cutofft))
    selecttleft <- selectt & (res$m.ct[, res$years == year]) < 0
    selecttright <- selectt & (res$m.ct[, res$years == year]) >= 0
    if (display.labels.as.text) {
      labels <- res$name.c
    } else {
      names.select <- sort(res$name.c[selecttleft | selecttright])
      labels.select <- 1:length(names.select)
      labels <- rep(NA, length(res$name.c))
      labels[match(names.select, res$name.c)] <- labels.select
    }
    if (sum(selecttleft, na.rm = T) > 0)  
      text(res$d.ct[selecttleft, res$years == year] ~ res$ m.ct[selecttleft, res$years == year], 
           label = labels[selecttleft], pos = 2, cex = 0.8, offset = 0.2)
    if (sum(selecttright, na.rm = T) > 0)  
      text(res$d.ct[selecttright, res$years == year] ~ res$m.ct[selecttright, res$years == year], 
           label = labels[selecttright], pos = 4, cex = 0.8, offset = 0.2)
    abline(0, 1)
    abline(h = 0)
    abline(v = 0)
    par(cex = 1/1.3) # reset
    if (!display.labels.as.text) {
      par(mar = c(0,0.5,0,0), cex = 1.1)
      EmptyPlot()
      text(x = 0.6, y = 1, label = paste(paste(labels.select, names.select), collapse = "\n"), adj = c(0, 0.5))
      #       legend("center", legend = paste(labels.select, names.select), col = "white", bty = "n")
      par(cex = 1/1.1) # reset
    }
    # dev.off()
  }
  dev.off()
}
#----------------------------------------------------------------------
OutputDecompositionResults <- function(
  res.select, ##<< 
  iso.select, ##<<
  indicator = "U5MR",
  filename,
  output.dir
) {
  if (is.null(iso.select))
    iso.select <- res.select$iso.c
  select.c <- is.element(res.select$iso.c, iso.select)
  cat(paste0("Results are for ", sum(select.c), " selected countries/regions.\n")) # 114
  table <- NULL
  for (year in res.select$years) {
    table <- rbind(table,
                   round(c(
                     mean(res.select$d.ct[select.c, res.select$years==year], na.rm = T),
                     sd(res.select$d.ct[select.c, res.select$years==year], na.rm = T),
                     mean(res.select$m.ct[select.c, res.select$years==year], na.rm = T),
                     sd(res.select$m.ct[select.c, res.select$years==year], na.rm = T),
                     mean(abs(res.select$d.ct[select.c, res.select$years==year]), na.rm = T),
                     mean(abs(res.select$m.ct[select.c, res.select$years==year]), na.rm = T),
                     sum(res.select$d.ct[select.c, res.select$years==year] > 10, na.rm = T),
                     sum(res.select$d.ct[select.c, res.select$years==year] < -10, na.rm = T),
                     sum(res.select$m.ct[select.c, res.select$years==year] > 10, na.rm = T),
                     sum(res.select$m.ct[select.c, res.select$years==year] < -10, na.rm = T)
                   ), 1))
  }
  rownames(table) <- paste(indicator, res.select$years)
  colnames(table) <- c("Mean(d.ct)", "Sd(d.ct)", "Mean(m.ct)", "Sd(m.ct)",
                       "Mean(abs(d.ct))", "Mean(abs(m.ct))",
                       "Count(d.ct>10)", "Count(d.ct<10)",
                       "Count(m.ct>10)", "Count(m.ct<10)")
  print(table)
  write.csv(table, file = file.path(output.dir, paste0("decomposummary_", filename, ".csv")))
}
#----------------------------------------------------------------------
PlotRateComparison <- function(
  res,
  cutoff = 10,
  filename = paste0("IGME 2013 vs 2012 - ", indicator),
  indicator = "U5MR",
  xlab = "IGME 2012 estimate",
  ylab = "IGME 2013 estimate",
  plot_nameT = TRUE,
  plot.regions = TRUE,
  display.labels.as.text = TRUE, ##<< Display labels as text? If \code{FALSE}, display as numbers and plot legend.
  fig.dir = NULL,
  cols.region = c("#A6CEE3", "#1F78B4", # blue
                  "#B2DF8A", "#33A02C", # green
                  "#FB9A99", "#E7298A", # pink
                  "#E31A1C", # red 
                  "#FDBF6F", "#FF7F00", # orange
                  "#CAB2D6", "#6A3D9A", # purple
                  "#FFFF99",
                  "#B15928", # brown
                  "#B3B3B3", "#666666")[-c(3, 4, 6, 7)] # grey # remove greens, pink and red
) {
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig")
  pdf(file.path(fig.dir, paste0(filename, ".pdf")), width = 7*length(res$years), height = 1*7)
  # tiff(file=paste0(figdir_comp, "/tifs/", "fig2_revised2.tif"),
  #      width = 6.83, height = 9.19,units = "in", pointsize = 8, res = 300, 
  #      bg = "white", compression = "lzw")     
  # postscript(file = paste0(figdir_comp, "/tifs/", "fig2_revised2.eps"), 
  #            paper="special", pointsize = 8,      
  #            width = 6.83, height = 9.19, #unit = "in",       
  #            horizontal = FALSE)
  par(mfrow = c(1, length(res$years)))
  name.plot.c <- res$name.c
  iso.plot.c <- res$iso.c
  select.regions <- nchar(iso.plot.c) != 3
  for (j in 1:length(res$years)) {
    year <- res$years[j]
    u.igme <- res$igme$u5mr.Lt.c[[paste0(year)]]
    u.igmeold <- res$igmeold$u5mr.Lt.c[[paste0(year)]]
    main <- paste(indicator, year)
    xlow <- 1
    xup  <- 400 
    xlim <- c(-10, max(c(u.igme, u.igmeold), na.rm = T)+ifelse(display.labels.as.text, 60, 10))
    plot(u.igme ~ u.igmeold, type = "n", xlab = xlab, ylab = ylab,
         xlim = xlim, ylim = xlim, main = main)
    polygon((c(xlow, xlow, xup, xup, xlow)),
            ((c(xlow*0.7, xlow*1.3, xup * 1.3, xup*0.7, xlow*0.7))),
            col = "lightgrey", border = NA) 
    polygon((c(xlow, xlow, xup, xup, xlow)),
            ((c(xlow*0.8, xlow*1.2, xup * 1.2, xup*0.8, xlow*0.8))),
            col = "grey", border = NA) 
    polygon((c(xlow, xlow, xup, xup, xlow)),
            ((c(xlow*0.9, xlow*1.1, xup * 1.1, xup*0.9, xlow*0.9))),
            col = "darkgrey", border = NA)
    points(u.igme[!select.regions & abs(u.igme - u.igmeold) < cutoff] ~ 
             u.igmeold[!select.regions & abs(u.igme - u.igmeold) < cutoff], col = 3,
           pch = 19, lwd = 1, cex = 1)
    points(u.igme[!select.regions & abs(u.igme - u.igmeold) > cutoff] ~ 
             u.igmeold[!select.regions & abs(u.igme - u.igmeold) > cutoff], 
           col = 2, pch = 19, lwd = 1, cex = 1)
    select <- plot_nameT & !select.regions
    selectleft <- select & u.igme < (-cutoff+u.igmeold)
    selectright <- select & u.igme > (cutoff+u.igmeold)
    if (display.labels.as.text) {
      labels <- name.plot.c
    } else {
      names.select <- sort(name.plot.c[selectleft | selectright])
      labels.select <- 1:length(names.select)
      labels <- rep(NA, length(name.plot.c))
      labels[match(names.select, name.plot.c)] <- labels.select
      # legend
      text(x = xlim[1]-5, y = xlim[2]+5, 
           label = paste(paste(labels.select, names.select), collapse = "\n"), 
           adj = c(0, 1), cex = 0.7)
    }
    if (sum(selectleft, na.rm = T) > 0)
      text(u.igme[selectleft] ~ u.igmeold[selectleft],
           pos = 4, label = labels[selectleft], cex = 0.85, offset = 0.25)
    if (sum(selectright, na.rm = T) > 0)
      text(u.igme[selectright] ~ u.igmeold[selectright],
           pos = 2, label = labels[selectright], cex = 0.85, offset = 0.25)
    # plot separately for regions
    if (plot.regions) {
      if (sum(select.regions, na.rm = T) > 0) {
        points(u.igme[select.regions] ~ u.igmeold[select.regions], 
               col = 1, bg = c(cols.region[1:(sum(select.regions, na.rm = T)-1)], "black"), # change JR, 20140421 #c(brewer.pal(sum(select.regions, na.rm = T)+1, "Paired")[-c(3, 4)], "black"), 
               pch = 23, lwd = 2, cex = c(rep(1.2, sum(select.regions, na.rm = T)-1), 1.5))
        legend("bottomright", legend = name.plot.c[select.regions], 
               # title = "Regions", title.adj = 0,
               pt.bg = c(cols.region[1:(sum(select.regions, na.rm = T)-1)], "black"), # change JR, 20140421 # c(brewer.pal(sum(select.regions, na.rm = T)+1, "Paired")[-c(3, 4)], "black"),
               pch = 23, pt.lwd = 1.5, pt.cex = c(rep(1, sum(select.regions, na.rm = T)-1), 1.2), 
               cex = 0.8, bty = "n")
      }
    }
    abline(0,1)
    box()
  } # end years loop
  dev.off()
}
#----------------------------------------------------------------------
PlotARRComparison <- function(
  res,
  iso.select = NULL, ##<< ISO country codes to plot. If \code{NULL}, all ISOs in res$iso.c are used.
  filename = paste0("IGME 2013 vs 2012 - ARR"),
  indicator = "Annual rate of reduction 1990-2011 (%)",
  xlab = "IGME 2012 estimate",
  ylab = "IGME 2013 estimate",
  goal = 4.4,
  cutoff = 2,
  cutofft = 3,
  plot_nameT = TRUE,
  plot.regions = TRUE,
  display.labels.as.text = TRUE, ##<< Display labels as text? If \code{FALSE}, display as numbers and plot legend.
  fig.dir = NULL,
  cols.region = c("#A6CEE3", "#1F78B4", # blue
                  "#B2DF8A", "#33A02C", # green
                  "#FB9A99", "#E7298A", # pink
                  "#E31A1C", # red 
                  "#FDBF6F", "#FF7F00", # orange
                  "#CAB2D6", "#6A3D9A", # purple
                  "#FFFF99",
                  "#B15928", # brown
                  "#B3B3B3", "#666666")[-c(3, 4, 6, 7)] # grey # remove greens, pink and red
) {
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig")
  pdf(file.path(fig.dir, paste0(filename, ".pdf")))
  #tiff(file=paste(figdir_comp, "/fig3.tif", sep = ""),width=3.27,height=3.27,units="in",pointsize=8,res=300,
  #     bg="white",compression="lzw") 
  # postscript(file=paste(figdir_comp, "/fig3.eps", sep = ""),
  #            width=3.27,height=3.27,pointsize=8)
  par(mfrow = c(1,1), mar = c(5,5,5,1), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.2)
  if (is.null(iso.select)) {
    select.countries <- rep(T, length(res$iso.c))
  } else {
    select.countries <- is.element(res$iso.c, iso.select) | nchar(res$iso.c) != 3 # do not exclude regions
  }
  ARRnew.c <- res$igme$ARR.c[select.countries]
  ARRold.c <- res$igmeold$ARR.c[select.countries]
  name.plot.c <- res$name.c[select.countries]
  iso.plot.c <- res$iso.c[select.countries]
  select.regions <- nchar(iso.plot.c) != 3
  xlim <- c(-1+min(ARRnew.c, ARRold.c, na.rm = T), 0+max(ARRnew.c, ARRold.c, na.rm = T))
  plot(ARRnew.c ~ ARRold.c, type = "n", xlab = xlab, ylab = ylab, 
       main = indicator,
       xlim = xlim, ylim = xlim)
  polygon(100*c(-0.1, -0.1, 0.2, 0.2, -0.1), 
          100*c(-0.1-0.03,-0.1+0.03,0.2+0.03, 0.2-0.03, -0.1-0.03), col = "lightgrey", border = NA) 
  polygon(100*c(-0.1, -0.1, 0.2, 0.2, -0.1), 
          100*c(-0.1-0.02,-0.1+0.02,0.2+0.02, 0.2-0.02, -0.1-0.02), col = "grey", border = NA) 
  polygon(100*c(-0.1, -0.1, 0.2, 0.2, -0.1), 
          100*c(-0.1-0.01,-0.1+0.01,0.2+0.01, 0.2-0.01, -0.1-0.01), col = "darkgrey", border = NA) 
  points(ARRnew.c ~ ARRold.c, type = "p", pch = 19, col = 3, cex = 0.8)
  select <- !select.regions & abs(ARRold.c - ARRnew.c) > cutoff
  selecttopleft <- select & ARRold.c < goal & ARRnew.c > goal
  selectbottomright <- select & ARRold.c > goal & ARRnew.c < goal
  if (sum(selecttopleft, na.rm = T) > 0) 
    points(ARRnew.c[selecttopleft] ~ ARRold.c[selecttopleft], type = "p", pch = 19, col = 2, cex = 0.8)
  if (sum(selectbottomright, na.rm = T) > 0) 
    points(ARRnew.c[selectbottomright] ~ ARRold.c[selectbottomright], type = "p", pch = 19, col = 2, cex = 0.8)
  # labels
  selectt <- !select.regions & abs(ARRold.c - ARRnew.c) > cutofft & plot_nameT
  selectttopleft <- selectt & ARRold.c < goal & ARRnew.c > goal
  selecttbottomright <- selectt & ARRold.c > goal & ARRnew.c < goal
  if (display.labels.as.text) {
    labels <- name.plot.c
  } else {
    names.select <- sort(name.plot.c[selectttopleft | selecttbottomright])
    labels.select <- 1:length(names.select)
    labels <- rep(NA, length(name.plot.c))
    labels[match(names.select, name.plot.c)] <- labels.select
    # legend
    text(x = xlim[1]-0.2, y = xlim[2]+0.2, 
         label = paste(paste(labels.select, names.select), collapse = "\n"), 
         adj = c(0, 1), cex = 0.8)
  }
  if (sum(selectttopleft, na.rm = T) > 0)  
    text(ARRnew.c[selectttopleft] ~ ARRold.c[selectttopleft], pos = 2, 
         label = labels[selectttopleft], cex = 0.8, offset = 0.3)
  if (sum(selecttbottomright, na.rm = T) > 0)  
    text(ARRnew.c[selecttbottomright] ~ ARRold.c[selecttbottomright], pos = 4, 
         label = labels[selecttbottomright], cex = 0.8, offset = 0.3)
  # plot separately for regions
  if (plot.regions) {
    if (sum(select.regions, na.rm = T) > 0) {
      points(ARRnew.c[select.regions] ~ ARRold.c[select.regions], 
             col = 1, bg = c(cols.region[1:(sum(select.regions, na.rm = T)-1)], "black"), # change JR, 20140421 # c(brewer.pal(sum(select.regions, na.rm = T)+1, "Paired")[-c(3, 4)], "black"), 
             pch = 23, lwd = 1.5, cex = c(rep(1.2, sum(select.regions, na.rm = T)-1), 1.5))
      legend("bottomright", legend = name.plot.c[select.regions], 
             pt.bg = c(cols.region[1:(sum(select.regions, na.rm = T)-1)], "black"), # change JR, 20140421 # c(brewer.pal(sum(select.regions, na.rm = T)+1, "Paired")[-c(3, 4)], "black"),
             pch = 23, pt.lwd = 1.5, pt.cex = c(rep(1, sum(select.regions, na.rm = T)-1), 1.2), 
             cex = 0.8, bty = "n")
    }
  }
  abline(0, 1); abline(h = goal); abline(v = goal)
  box()
  dev.off()
}
