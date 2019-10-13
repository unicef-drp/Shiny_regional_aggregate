#----------------------------------------------------------------------
# plotresultsforimr.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
PlotResultsForIMR <- function(# Plot results for IMR
  ## Differs from \code{PlotResults} in that \code{mcmc.meta$data} is \code{NULL} and \code{mcmc.array}
  ## is not available.
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  fig.dir = NULL, ##<< Directory to store plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
  estimates.file = NULL, ##<< UN IGME estimates file (median estimates only). If \code{NULL}, not plotted.
  weight.alpha.select = 0.5, ##<< Result type: Value of \code{weight.alpha} for Bayesian melding.
  year.start = NULL, ##<< Start year of estimates to plot. If \code{NULL}, earliest year of estimates available is used.
  year.end = NULL, ##<< End year of estimates to plot. If \code{NULL}, latest year of estimates available is used.
  percentiles = c(0.05,0.5,0.95), ##<< Percentiles.
  fig.dir.alt = NULL, ##<< Alternative directory for plots with different types plots in different folders.
  officialname = T, ###kai added this parameter to import full country name on 04/16/2018
  ## (Used for combined country-specific runs.)
  separate.plots.by.country = TRUE, ##<< Separate plots produced by country?
  file.format="pdf",
  legendtext=NULL,
  legend3=NULL,
  legend4=NULL,
  main.plot = TRUE, ##<< Include main plot?
  zoom = TRUE, ##<< Add zoomed plot?
  hiv.removed = F,   #### show results with HIV deaths removed    Kai 09/26/2018
  add.legend = TRUE, ##<< Add legend plot for data series?
  indirect_series_visibility=T, #####kai added an option whether to include indirect series   05/22/2018
  zoom.year.start = 1990,   ##<< Start year of estimates in zoom in plot. If \code{NULL}, latest year of estimates available is used.    Kai Zhong add 05/24/2018
  zoom.year.end = 2017.5   ##<< End year of estimates in zoom in plot. If \code{NULL}, latest year of estimates available is used.      Kai Zhong add 05/24/2018
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname)
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig")
  dir.create(fig.dir, showWarnings = FALSE)
  if (!is.null(fig.dir.alt)) {
    dir.create(fig.dir.alt, showWarnings = FALSE)
    dir.create(file.path(fig.dir.alt, "Results"), showWarnings = FALSE)
    dir.create(file.path(fig.dir.alt, "Results (HIV-removed)"), showWarnings = FALSE)
  }
  
  load(file.path(output.dir, "mcmc.meta.rda"))
  
  
  if(officialname==T){
    info <- read.csv(file.path(getwd(),"input/infoUNinclHIV.csv",sep=""), header = T, stringsAsFactors = F)
    officialname=as.data.frame(info)                    ###read in official country name    kai add 05/07/2018
  wuwu=data.frame(mcmc.meta$data.all$iso.c,stringsAsFactors=FALSE)                         #######read in official country name    kai add 05/07/2018   
  colnames(wuwu)="iso.c"                                                                     ####kai added 04/19/2018
  mcmc.meta$data.all$name.c=dplyr::left_join(wuwu,officialname,by="iso.c")$officialname.c           ###replace the old country name with the official country name    kai add 05/07/2018
  mcmc.meta$data$name.c=dplyr::left_join(wuwu,officialname,by="iso.c")$officialname.c
  }
  list2env(mcmc.meta$settings, envir = environment())
  hiv.file <- mcmc.meta$settings$hiv.file
  # if (is.null(hiv.file))
  #   hiv.file <- file.path("input", paste0("dataUNAIDS_", indicator.type, ".csv"))
  # if (is.null(estimates.file))
  #   estimates.file <- file.path("input", paste0(indicator.type, "_un.csv"))
  
  load(file.path(output.dir, "iso.c.rda"))
  load(file.path(output.dir, "year.t.rda"))
  year.t.temp <- year.t
  
  if(hiv.removed == F){
    load(file.path(output.dir, "res.cqt.Lw.rda"))
    res.cqt <- res.cqt.Lw[[paste0(weight.alpha.select)]]
  } else {  
    load(file.path(output.dir, "res.hivremoved.cqt.Lw.rda")) 
    res.cqt <- res.hivremoved.cqt.Lw[[paste0(weight.alpha.select)]]      
  }                       ####kai added 09/26/2018
  
  if (!is.null(year.start)) {
    if (year.start > min(year.t)) {
      res.cqt[, , year.t < year.start] <- NA
    }
  }
  if (!is.null(year.end)) {
    if (year.end < max(year.t)) {
      res.cqt[, , year.t > year.end] <- NA
    }
  }
  
  if (!is.null(estimates.file)) {
    igme <- GetIGME(country.codes = mcmc.meta$data.all$iso.c,
                    estimates.file = estimates.file,
                    hiv.file = hiv.file)
    save(igme, file = file.path(output.dir, "igme.rda"))
    cat("IGME estimates read in.\n")
  } else {
    igme <- NULL
  }
  
  
  
  plot.width <- (main.plot + zoom + add.legend)*7      ####plot width is determined by whether three parts are included or not    kai added 05/13/2018
  if(file.format=="png"){
    separate.plots.by.country=T             ######png file could only plot single country result
    print("file.format='png' will produce separate plots by country.")
  }
  
  #####add plot data function    Kai  05/24/2018
  if (!separate.plots.by.country) {
    pdf(file = file.path(fig.dir, paste0(runname, " ", "Data",ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""), ".pdf")), width = plot.width, height = 7)
  }
  for (c in 1:mcmc.meta$data.all$C){
  if (separate.plots.by.country){
    if(file.format=="png"){
      if((main.plot + zoom + add.legend)==3){
        png(file = file.path(fig.dir, paste0(runname, " Data ",  mcmc.meta$data.all$iso.c[c],ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""), ".png")), width=24.75,height=8.25,units="in", res=250)   ##when there are three parts in the plot   kai made changes 04/09/2018
      } else if((main.plot + zoom + add.legend)==2){png(file = file.path(fig.dir, paste0(runname, " Data ",  mcmc.meta$data.all$iso.c[c],ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""), ".png")), width=16.5,height=8.25,units="in", res=250)   ##when there are two random parts in the plot   kai made changes 04/09/2018
      } else {png(file = file.path(fig.dir, paste0(runname, " Data ",  mcmc.meta$data.all$iso.c[c],ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""), ".png")), width=8.25,height=8.25,units="in", res=250)}  ##when there is one random part in the plot   kai made changes 04/09/2018
    } else if(file.format=="pdf"){
      pdf(file = file.path(fig.dir, paste0(runname, " Data ",  mcmc.meta$data.all$iso.c[c],ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""), ".pdf")), width = plot.width , height = 7)   ##pdf width is determined by pdf.width which is given above     kai made changes 04/09/2018
    }
  }
  PlotDataAndEstimates(data = NULL,
                       data.all = mcmc.meta$data.all,
                       add.legend=add.legend,
                       main.plot=main.plot,
                       zoom=zoom,
                       c = c,
                       ylab = mcmc.meta$settings$indicator.type,
                       legendfull = mcmc.meta$data.all$imrmethod.c[c],
                       indirect_series_visibility=indirect_series_visibility,    #####kai added an option whether to include indirect series   05/22/2018
                       plot.se = TRUE,
                       year.end=year.end,
                       zoom.year.start = zoom.year.start ,
                       zoom.year.end = zoom.year.end)
  if (separate.plots.by.country)
    dev.off()
}
if (!separate.plots.by.country)
  dev.off()
 cat("Country data plotted.\n")
  
  
  
  if (!separate.plots.by.country) {
    if (is.null(fig.dir.alt)) {
      pdf(file = file.path(fig.dir, paste0(runname, " Results",ifelse(hiv.removed==T," (HIV removed)",""),ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""),".pdf")), width = plot.width, height = 7)     ##pdf width is determined by pdf.width which is given above    kai made changes 04/09/2018
    } else {
      pdf(file = file.path(fig.dir.alt, paste0(runname, " Results",ifelse(hiv.removed==T," (HIV removed)",""),ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""),".pdf")), width = plot.width, height = 7)     ##pdf width is determined by pdf.width which is given above    kai made changes 04/09/2018
    }
  }
  for (c in 1:mcmc.meta$data.all$C) {
    if (separate.plots.by.country){
      if(file.format=="png"){
        if((main.plot + zoom + add.legend)==3){
          png(file = file.path(fig.dir, paste0(runname, "_Results_",  mcmc.meta$data.all$iso.c[c],ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""), ".png")), width=24.75,height=8.25,units="in", res=250)   ##when there are three parts in the plot   kai made changes 04/09/2018
        } else if((main.plot + zoom + add.legend)==2){png(file = file.path(fig.dir, paste0(runname, "_Results_",  mcmc.meta$data.all$iso.c[c],ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""), ".png")), width=16.5,height=8.25,units="in", res=250)    ##when there are two random parts in the plot   kai made changes 04/09/2018
        } else {png(file = file.path(fig.dir, paste0(runname, "_Results_",  mcmc.meta$data.all$iso.c[c],ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""), ".png")), width=8.25,height=8.25,units="in", res=250)}  ##when there is one random part in the plot   kai made changes 04/09/2018
      } else if(file.format=="pdf"){
        if(all(main.plot,zoom)==TRUE){
          pdf(file = file.path(fig.dir, paste0(runname, "_Results_", mcmc.meta$data.all$iso.c[c],ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""), ".pdf")), width = plot.width , height = 7)   ##pdf width is determined by pdf.width which is given above     kai made changes 04/09/2018
        } else {pdf(file = file.path(fig.dir, paste0(runname, "_Results_", mcmc.meta$data.all$iso.c[c],ifelse(indirect_series_visibility==F," (excl. SBH if FBH is available)",""), ".pdf")), width = plot.width , height = 7)}   ##pdf width is determined by pdf.width which is given above     kai made changes 04/09/2018
      }
    }
    PlotDataAndEstimates(data = NULL,
                         data.all = mcmc.meta$data.all,
                         c = c,
                         est.years = year.t,
                         ylab = indicator.type,
                         plot.se = TRUE,
                         zoom=zoom,               ###show "zoom in" or not      kai added 05/14/2018
                         main.plot = main.plot,   ###show main plot or not     kai added 05/14/2018
                         add.legend = add.legend, ###legend included or not     kai added 05/14/2018
                         legendfull = mcmc.meta$data.all$imrmethod.c[c],
                         CIs.cqt = res.cqt, 
                         igme = igme,
                         zoom.year.start = zoom.year.start ,
                         zoom.year.end = zoom.year.end,
                         year.end=year.end,
                         indirect_series_visibility=indirect_series_visibility,
                         legendtext=legendtext,
                         legend3=legend3,
                         legend4=legend4)
   if (separate.plots.by.country)
      dev.off()
  }
  if (!separate.plots.by.country)
    dev.off()
  cat("Country results plotted.\n")
  #----------------------------------------------------------------------
  ##details<< Plot country HIV-removed data and fits using \code{\link{PlotDataAndEstimates}}.
  # get results
}
