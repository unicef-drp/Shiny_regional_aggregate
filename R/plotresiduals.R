#----------------------------------------------------------------------
# plotresiduals.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
PlotResiduals <- function(
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where the residuals file is stored.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  fig.dir = NULL, ##<< Directory to store plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
  sample.one.obs.per.series = FALSE, ##<< Sample one observation per series to plot?
  exclude.training.data = TRUE, ##<< Exclude training data (for validation exercise)?
  exclude.most.recent.observation = FALSE, ##<< Exclude most recent observation in each country?
  exclude.vr = FALSE ##<< Exclude VR observations?
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  # read in residuals file
  residuals <- read.csv(file = file.path(output.dir, "residuals.csv"), 
                        header = T, stringsAsFactors = F, strip.white = T)
  if (exclude.training.data & !all(residuals$leftoutobs.i == 0)) {
    residuals <- residuals[residuals$leftoutobs.i == 1, ]
    filename.append <- "(Left-out obs only) "
  } else {
    filename.append <- ""
  }
  if (sample.one.obs.per.series) {
    residuals.processed <- data.frame(residuals, index.i = 1:nrow(residuals))
    residuals.processed$surveyyear.i <- ifelse(is.element(residuals.processed$sourcetype.i, 
                                                          c("VR", "Others Life Table", "Others Household Deaths")),
                                               0, residuals.processed$surveyyear.i)
    sourceids <- paste(residuals.processed$country.i, residuals.processed$source.i,
                       residuals.processed$surveyyear.i, residuals.processed$sourcetype.i)
    sourceids.unique <- unique(sourceids)
    indices.sample <- NULL
    for (s in 1:length(sourceids.unique)) {
      indices.sample <- c(indices.sample, sample(residuals.processed$index.i[sourceids == sourceids.unique[s]], 1))
    }
    residuals <- residuals[indices.sample, ]
    filename.append <- paste0(filename.append, "(One obs per series) ")
  } else {
    filename.append <- paste0(filename.append, "")
  }
  if (exclude.vr) {
    residuals <- residuals[residuals$sourcetype.i != "VR", ]
    filename.append <- paste0(filename.append, "(Non-VR only) ")
  } else {
    filename.append <- paste0(filename.append, "")
  }
  #----------------------------------------------------------------------
  # Process residuals file for plotting
  #----------------------------------------------------------------------
  # order region factors
  mdgregs.ordered <- c("Sub-Saharan Africa", "Northern Africa", "Latin America", "Caribbean", 
                       "Caucasus and Central Asia", "Eastern Asia", "Southern Asia",
                       "South-eastern Asia", "Western Asia", "Oceania", "Developed regions")
  regs.ordered <- c("SSAfr", "NAfr", "LAm", "Carib", "CauCAsia", "EAsia", "SAsia", "SEAsia", 
                    "WAsia", "Oc", "Dev")
  mdgafrregs.ordered <- c("North Africa", "Eastern Africa", "South Africa", "West Africa", 
                          "Central Africa")
  mdgregs <- mdgregs.ordered[is.element(mdgregs.ordered, residuals$region.i)]
  regs <- regs.ordered[is.element(mdgregs.ordered, residuals$region.i)]
  mdgafrregs <- mdgafrregs.ordered[is.element(mdgafrregs.ordered, residuals$afrregion.i)]
  residuals$region.i <- factor(residuals$region.i, levels = mdgregs)
  residuals$reg.i <- factor(residuals$region.i, labels = regs)
  residuals$afrregion.i <- factor(residuals$afrregion.i, levels = mdgafrregs)
  residuals$afrreg.i <- factor(residuals$afrregion.i, labels = mdgafrregs)
  # order sourcetype factors
  sourcetypes.ordered <- c("DHS Direct", "Other DHS Direct", "Others Direct", 
                           "MICS Indirect", "Census Indirect", "Others Indirect", 
                           "Others Life Table", "Others Household Deaths", "VR")
  sourcetypes <- sourcetypes.ordered[is.element(sourcetypes.ordered, residuals$sourcetype.i)]
  residuals$sourcetype.i <- factor(residuals$sourcetype.i, levels = sourcetypes)
  #----------------------------------------------------------------------
  # Exclude most recent observation in each country?
  #----------------------------------------------------------------------
  if (exclude.most.recent.observation) {
    lastObservationYearByCountry <- aggregate(year.i ~ iso.i, 
                                              data = residuals, FUN = max)
    residuals.excludinglastobs <- 
      ddply(residuals, .(iso.i), 
            function(dataSubsetByCountry) {
              return(dataSubsetByCountry[dataSubsetByCountry$year.i != max(dataSubsetByCountry$year.i), ])
            }
      )
    dim(residuals)[1]-dim(residuals.excludinglastobs)[1] == length(unique(residuals$iso.i)) # should be equal
    residuals <- residuals.excludinglastobs
    filename.append <- paste0(filename.append, "(Most recent observation excluded) ")
  } else {
    filename.append <- paste0(filename.append, "")
  }
  #----------------------------------------------------------------------
  # Plots of standardised biases (stbias.ci's)
  #----------------------------------------------------------------------
  filename <- gsub(" .pdf", ".pdf", file.path(fig.dir, paste0(runname, " Standardised biases ", 
                                                              filename.append, ".pdf")))
  pdf(file = filename, width = 21, height = 12)
  # residuals vs region
  theme_set(theme_bw(base_size = 16) + 
    theme(
	   # plot.margin = unit(c(2,1,1.5,1), "lines"), 
          plot.title = element_text(vjust = 2),
          axis.title.x = element_text(vjust = -0.5),
          axis.title.y = element_text(vjust = 0.2)))
  for(plot.by.sourcetype in c(F, T)) {
    for(geom.select in c("boxplot", "point")) {
      p <- ggplot(residuals, aes(factor(reg.i), stbias.i)) + 
        xlab("Region") + ylab("Standardised biases") + 
        labs(title = "Standardised biases")
      if(geom.select == "boxplot") {
        if(plot.by.sourcetype) {
          p <- p + geom_boxplot(aes(fill = factor(region.i))) + scale_fill_brewer(name = "MDG Region", 
                                                                                  #guide = guide_legend(ncol = 2), 
                                                                                  palette = "Paired")
        } else {
          p <- p + geom_boxplot(aes(fill = factor(region.i))) + scale_fill_brewer(name = "MDG Region", palette = "Paired")
        }
      } else {
        if(plot.by.sourcetype) {
          p <- p + geom_point(aes(colour = factor(region.i))) + scale_colour_brewer(name = "MDG Region", 
                                                                                    #guide = guide_legend(ncol = 2), 
                                                                                    palette = "Paired")
        } else {
          p <- p + geom_point(aes(colour = factor(sourcetype.i))) + scale_colour_brewer(name = "Source type", palette = "Paired")
        }
      }
      p <- p + geom_hline(yintercept = 0)
      if(plot.by.sourcetype) {
        p <- p + facet_wrap(~sourcetype.i, scales = "free") 
        #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
      }
      print(p)
    }
  }
  
  # stbiases vs quantitative variables (q5, surveyyear, recallnotcentred, tfrinsurveyyear, decreaseinTFRin15years)
  xlabels <- c("Level of q5", "Series year", "Observation year", "Retrospective period",
               "TFR in series year", "Decrease in TFR in last 15 years before series")
  collabels <- c("q5.i", "surveyyear.i", "year.i", "recallnotcentred.i", 
                 "tfrinsurveyyear.i", "decreaseinTFRin15years.i")
  for(l in 1:length(xlabels)) {
    for(plot.by.sourcetype in c(F, T)) {
      p <- ggplot(residuals, aes_string(x = collabels[l], y = "stbias.i")) +
        xlab(xlabels[l]) + ylab("Standardised biases") + labs(title = "Standardised biases")
      p <- p + geom_hline(yintercept = 0)
      if(plot.by.sourcetype) {
        p <- p + geom_point(aes(colour = factor(region.i))) + geom_smooth(se = F, method = "loess", weight = 1.5) +
          scale_colour_discrete(name = "MDG Region"
                                #, guide = guide_legend(ncol = 2)
                                ) +
          facet_wrap(~sourcetype.i, scales = "free") 
        #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
      } else {
        p <- p + geom_point(aes(colour = factor(sourcetype.i))) + geom_smooth(se = F, method = "loess", weight = 1.5) +
          scale_colour_discrete(name = "Source type")
      }
      print(p)
    }
  }
  dev.off()
  cat(paste0("Plots of standardised biases saved to ", fig.dir, "\n"))
  #----------------------------------------------------------------------
  # Plots of absolute standardised biases (abs(stbias.ci)'s)
  #----------------------------------------------------------------------
  filename <- gsub(" .pdf", ".pdf", file.path(fig.dir, paste0(runname, " Absolute standardised biases ", 
                                                              filename.append, ".pdf")))
  pdf(file = filename, width = 21, height = 12)
  # abs stbiases vs region
  theme_set(theme_bw(base_size = 16) + 
    theme(
	   # plot.margin = unit(c(2,1,1.5,1), "lines"), 
          plot.title = element_text(vjust = 2),
          axis.title.x = element_text(vjust = -0.5),
          axis.title.y = element_text(vjust = 0.2)))
  for(plot.by.sourcetype in c(F, T)) {
    for(geom.select in c("boxplot", "point")) {
      p <- ggplot(residuals, aes(factor(reg.i), absstbias.i)) + 
        xlab("Region") + ylab("Absolute standardised biases") + 
        labs(title = "Absolute standardised biases")
      if(geom.select == "boxplot") {
        if(plot.by.sourcetype) {
          p <- p + geom_boxplot(aes(fill = factor(region.i))) + scale_fill_brewer(name = "MDG Region", 
                                                                                  #guide = guide_legend(ncol = 2), 
                                                                                  palette = "Paired")
        } else {
          p <- p + geom_boxplot(aes(fill = factor(region.i))) + scale_fill_brewer(name = "MDG Region", palette = "Paired")
        }
      } else {
        if(plot.by.sourcetype) {
          p <- p + geom_point(aes(colour = factor(region.i))) + scale_colour_brewer(name = "MDG Region", 
                                                                                    #guide = guide_legend(ncol = 2), 
                                                                                    palette = "Paired")
        } else {
          p <- p + geom_point(aes(colour = factor(sourcetype.i))) + scale_colour_brewer(name = "Source type", palette = "Paired")
        }
      }
      p <- p + geom_hline(yintercept = 0)
      if(plot.by.sourcetype) {
        p <- p + facet_wrap(~sourcetype.i, scales = "free") 
        #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
      }
      print(p)
    }
  }
  
  # abs stbiases biases vs quantitative variables (q5, surveyyear, recallnotcentred, tfrinsurveyyear, decreaseinTFRin15years)
  xlabels <- c("Level of q5", "Series year", "Observation year", "Retrospective period",
               "TFR in series year", "Decrease in TFR in last 15 years before series")
  collabels <- c("q5.i", "surveyyear.i", "year.i", "recallnotcentred.i", 
                 "tfrinsurveyyear.i", "decreaseinTFRin15years.i")
  for(l in 1:length(xlabels)) {
    for(plot.by.sourcetype in c(F, T)) {
      p <- ggplot(residuals, aes_string(x = collabels[l], y = "absstbias.i")) +
        xlab(xlabels[l]) + ylab("Absolute standardised biases") + 
        labs(title = "Absolute standardised biases")
      p <- p + geom_hline(yintercept = 0)
      if(plot.by.sourcetype) {
        p <- p + geom_point(aes(colour = factor(region.i))) + geom_smooth(se = F, method = "loess", weight = 1.5) +
          scale_colour_discrete(name = "MDG Region"
                                #, guide = guide_legend(ncol = 2)
                                ) +
          facet_wrap(~sourcetype.i, scales = "free") 
        #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
      } else {
        p <- p + geom_point(aes(colour = factor(sourcetype.i))) + geom_smooth(se = F, method = "loess", weight = 1.5) +
          scale_colour_discrete(name = "Source type")
      }
      print(p)
    }
  }
  dev.off()
  cat(paste0("Plots of absolute standardised biases saved to ", fig.dir, "\n"))
  #----------------------------------------------------------------------
  # Plots of standardised biases (stbias.ci's) for DHS Direct only by interval 
  #----------------------------------------------------------------------
  filename <- gsub(" .pdf", ".pdf", file.path(fig.dir, paste0(runname, " Standardised biases_DHS Direct by interval ", 
                                                              filename.append, ".pdf")))
  pdf(file = filename, width = 21, height = 12)
  # subset for DHS Direct
  residuals.dhsdirect <- residuals[residuals$sourcetype.i == "DHS Direct", ]
  
  theme_set(theme_bw(base_size = 16) + 
    theme(
	   # plot.margin = unit(c(2,1,1.5,1), "lines"), 
          plot.title = element_text(vjust = 2),
          axis.title.x = element_text(vjust = -0.5),
          axis.title.y = element_text(vjust = 0.2)))
  # residuals vs quantitative variables (q5, surveyyear, recallnotcentred, tfrinsurveyyear, decreaseinTFRin15years)
  plots <- c("Overview", "Plot by interval", "Plot by region")
  xlabels <- c("Retrospective period")
  collabels <- c("recallnotcentred.i")
  for (l in 1:length(xlabels)) {
    for (plot in plots) {
      p <- ggplot(residuals.dhsdirect, aes_string(x = collabels[l], y = "stbias.i")) +
        xlab(xlabels[l]) + ylab("Standardised biases") + 
        labs(title = "Standardised biases")
      p <- p + geom_hline(yintercept = 0)
      if (plot == "Overview") {
        p <- p + geom_point(aes(colour = factor(interval.i))) + geom_smooth(se = F, method = "loess", weight = 1.5) +
          scale_colour_discrete(name = "Interval")
      } else if (plot == "Plot by interval") {
        p <- p + geom_point(aes(colour = factor(region.i))) + geom_smooth(se = F, method = "loess", weight = 1.5) +
          scale_colour_discrete(name = "MDG Region"
                                #, guide = guide_legend(ncol = 2)
                                ) +
          facet_wrap(~interval.i, scales = "free") 
        #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
      } else if (plot == "Plot by region") {
        p <- p + geom_point(aes(colour = factor(interval.i))) + geom_smooth(se = F, method = "loess", weight = 1.5) +
          scale_colour_discrete(name = "Interval"
                                #, guide = guide_legend(ncol = 2)
                                ) +
          facet_wrap(~region.i, scales = "free") 
        #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
      }
      print(p)
    }
  }
  dev.off()
  cat(paste0("Plots of standardised biases (DHS Direct only) by interval saved to ", fig.dir, "\n"))
  #----------------------------------------------------------------------
  # Histograms of percentiles (q.ci's)
  #----------------------------------------------------------------------
  filename <- gsub(" .pdf", ".pdf", file.path(fig.dir, paste0(runname, " Histograms of percentiles ", 
                                                              filename.append, ".pdf")))
  pdf(file = filename, width = 21, height = 12)
  # residuals vs region
  theme_set(theme_bw(base_size = 16) + 
    theme(
	   # plot.margin = unit(c(2,1,1.5,1), "lines"), 
          plot.title = element_text(vjust = 2),
          axis.title.x = element_text(vjust = -0.5),
          axis.title.y = element_text(vjust = 0.2)))
  xlabels <- c("Region", "Source type")
  collabels <- c("region.i", "sourcetype.i")
  for (plot.by.var in c(F, T)) {
    p <- ggplot(residuals, aes(q.i)) + 
      xlab("Percentiles") + ylab("Density") + labs(title = "Percentiles")
    p <- p + geom_histogram(aes(y = ..density..), binwidth = 0.05)
    p <- p + geom_hline(yintercept = 1)
    if (plot.by.var) {
      for(l in 1:length(xlabels)) {
        p <- p + facet_wrap(as.formula(paste0("~", collabels[l])), scales = "free") + 
          theme(legend.justification=c(1,0), legend.position=c(1,0))
        print(p)
      }
    } else {
      print(p)
    }
  }
  # one region per page
  for (region.select  in mdgregs) {
    p <- ggplot(residuals[residuals$region.i == region.select, ], aes(q.i)) + 
      xlab("Percentiles") + ylab("Density") + labs(title = region.select)
    p <- p + geom_histogram(aes(y = ..density..), binwidth= 0.05)
    p <- p + geom_hline(yintercept = 1)
    # plot by source type
    p <- p + facet_wrap(~sourcetype.i, scales = "free")
      # + theme(legend.justification=c(1,0), legend.position=c(1,0))
    print(p)
  }
  # one source type per page
  for (sourcetype.select  in sourcetypes) {
    p <- ggplot(residuals[residuals$sourcetype.i == sourcetype.select, ], aes(q.i)) + 
      xlab("Percentiles") + ylab("Density") + labs(title = sourcetype.select)
    p <- p + geom_histogram(aes(y = ..density..), binwidth= 0.05)
    p <- p + geom_hline(yintercept = 1)
    # plot by source type
    p <- p + facet_wrap(~region.i, scales = "free")
      #+theme(legend.justification=c(1,0), legend.position=c(1,0))
    print(p)
  }
  dev.off()
  cat(paste0("Histograms of percentiles saved to ", fig.dir, "\n"))
  ##value<< \code{NULL}.
}
#----------------------------------------------------------------------
# plots
#----------------------------------------------------------------------
# # retrospective period
# filename <- gsub(" .pdf", ".pdf", file.path(fig.dir, paste0(runname, "Retrospective period ", 
#                                                             filename.append, ".pdf")))
# pdf(file = filename, width = 21, height = 12)
# width = 21, height = 12)
# theme_set(theme_bw(base_size = 16) + 
#   theme(
	   # plot.margin = unit(c(2,1,1.5,1), "lines"), 
#         plot.title = element_text(vjust = 2),
#         axis.title.x = element_text(vjust = -0.5),
#         axis.title.y = element_text(vjust = 0.2)))
# for(plot.by.sourcetype in c(F, T)) {
#   for(geom.select in c("boxplot", "point")) {
#     p <- ggplot(residuals, aes(factor(reg.i), recallnotcentred.i)) + 
#       xlab("Region") + ylab("Retrospective period") + labs(title = "Retrospective period")
#     if(geom.select == "boxplot") {
#       if(plot.by.sourcetype) {
#         p <- p + geom_boxplot(aes(fill = factor(region.i))) + scale_fill_brewer(name = "MDG Region", guide = guide_legend(ncol = 2), palette = "Paired")
#       } else {
#         p <- p + geom_boxplot(aes(fill = factor(region.i))) + scale_fill_brewer(name = "MDG Region", palette = "Paired")
#       }
#     } else {
#       if(plot.by.sourcetype) {
#         p <- p + geom_point(aes(colour = factor(region.i))) + scale_colour_brewer(name = "MDG Region", guide = guide_legend(ncol = 2), palette = "Paired")
#       } else {
#         p <- p + geom_point(aes(colour = factor(sourcetype.i))) + scale_colour_brewer(name = "Source type", palette = "Paired")
#       }
#     }
#     p <- p + geom_hline(yintercept = 0)
#     if(plot.by.sourcetype) {
#       p <- p + facet_wrap(~sourcetype.i, scales = "free") + theme(legend.justification=c(1,0), legend.position=c(1,0))
#     }
#     print(p)
#   }
# }
# dev.off()
