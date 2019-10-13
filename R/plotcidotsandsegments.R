#----------------------------------------------------------------------
# plotcidotsandsegments.R
#----------------------------------------------------------------------
PlotCIDotsAndSegments <- function(# Plot a CI plot with dots for median/mean and segments for PIs.
  CIs.cq, ##<< Matrix/array of CIs for country c and quantile q.
  CIs2.cq = NULL, ##<< Optional: Additional matrix/array of CIs for country c and quantile q.
  name.c, ##<< Vector of country names.
  select.c  = NULL, ##<< Optional: Vector of indices if CIs are required for only a subset of countries. 
  order = "igme", # How to order plot: "igme", "lower", "median" or "other" (requires other.c)
  other.c = NULL, ##<< Optional: Other vector to order by.
  logscale = FALSE, ##<< Plot on log scale?
  igme.c = NULL, ##<< Optional: Vector of points to plot.
  igme2.c = NULL, ##<< Optional: Additional vector of points to plot.
  main = NULL, ##<< Optional: Main title of plot.
  xlab = NULL, ##<< Optional; Label of x-axis.
  vabline = NULL, ##<< Optional: Vertical line to plot.
  cex.axis = 1.2, ##<< Optional: Plot magnification.
  name.tif = NULL ##<< Optional: File name of .tif file to save plot to.
) {
  if (is.null(select.c)){
    select.c <- seq(1, length(CIs.cq[,1]))
  }
  if (order=="igme") {
    order <- order(igme.c[select.c])
  } else if (order=="lower") {
    order <- order(CIs.cq[select.c,1])
  } else if (order=="median") {
    order <- order(CIs.cq[select.c,2])
  } else if(order=="other") {
    order <- rev(order(other.c[select.c])) # large to small
  } else {
    order <- order(name.c[select.c])
  }
  select.c <- select.c[order]
  C <- length(name.c)
  seqtoplot <- seq(1,length(select.c))
  if (is.null(CIs2.cq)){
    CIs2.cq <- matrix(NA, dim(CIs.cq)[1], dim(CIs.cq)[2])
  }
  if (logscale){
    plotlog <- "x"
  }  else{
    plotlog <- ""
  }
  if (!is.null(name.tif)) {
    tiff(file = name.tif, width = 10, height = 5+length(seqtoplot)/2, 
         units = "cm", pointsize = 8, res = 300, bg = "white", compression = "lzw")
  }
  par(mar = c(5,12,3,1), cex.axis = cex.axis)#, mfrow = c(1,1))
  xmax <- min(500, 1.1*max(CIs.cq[select.c,], CIs2.cq[select.c,], na.rm = T))
  #  xmin <- max(ifelse(logscale, 1, -Inf),
  #               (0.9+0.1*(min(CIs.cq[select.c,], 
  #                             CIs2.cq[select.c,], na.rm = T)<0))
  #               *min(CIs.cq[select.c,],
  #                    CIs2.cq[select.c,], na.rm = T))
  # x-lim mess with pos and neg values
  xmin <- min(c(CIs.cq[select.c,], CIs2.cq[select.c,]), na.rm = T)
  xmin <- ifelse(xmin<0, 1.2*xmin, ifelse(xmin <20, 0, 0.8*xmin))
  
  plot(rep(seqtoplot,6) ~ c(CIs.cq[select.c,], CIs2.cq[select.c,]), 
       xlim = c(xmin,xmax), main = main, #xaxt = "n",
       ylim = c(-max(seqtoplot)-1, -min(seqtoplot)+1),
       ylab = "", yaxt = "n", xlab = xlab, 
       log = plotlog, type = "n")
  for (i in -seqtoplot[seq(1, length(-seqtoplot),2)]){
    polygon(-0.5+c(xmin,xmin,500,500,xmin),
            i + c(-0.5, 0.5, 0.5, -0.5, -0.5),
            col = "lightgrey", border = NA)
  }
  abline(h = -seqtoplot)
  axis(2, at = -seqtoplot, label = name.c[select.c], las = 1, 
       cex.axis = cex.axis)
  box()
  if (!is.null(CIs2.cq)) {
    add <- -0.1
    segments(CIs2.cq[select.c,1],add-seqtoplot,
             CIs2.cq[select.c,3],add-seqtoplot, lwd = 4, col = 2)
    points(add-seqtoplot ~ CIs2.cq[select.c,2], pch = 3, col = 2, lwd = 3)
  }
  if (!is.null(igme2.c)) {
    points(add+-seqtoplot~igme2.c[select.c], pch = 3, col = 2, lwd = 3)
  } 
  segments(CIs.cq[select.c,1],-seqtoplot,
           CIs.cq[select.c,3],-seqtoplot, lwd = 4)
  if (!is.null(igme.c))  {
    points(-seqtoplot~igme.c[select.c], pch = 3, col = 1, lwd = 3)
  } 
  points(-seqtoplot ~ CIs.cq[select.c,2], pch = 3, col = 1, lwd = 3)
  abline(v = vabline)
  if (!is.null(name.tif)) dev.off()
  ##value<< \code{NULL}.
  return(invisible())
}
