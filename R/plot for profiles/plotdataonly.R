PlotData=function(runname,
                  indicator.type,
                  plot.se=TRUE,
                  add.legend=T,
                  main.plot=T,
                  zoom=T,
                  zoom.year.start = 1990,   ##<< Start year of estimates in zoom in plot. If \code{NULL}, latest year of estimates available is used.    Kai Zhong add 05/24/2018
                  zoom.year.end = 2017.5,   ##<< End year of estimates in zoom in plot. If \code{NULL}, latest year of estimates available is used.      Kai Zhong add 05/24/2018
                  officialname=F,
                  fig.dir=fig.dir,
                  separate.plots.by.country=F,
                  year.end=NULL,
                  indirect_series_visibility=T,  #####kai added an option whether to include indirect series   05/22/2018
                  file.format="pdf"){
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
load(file.path("output", runname, "mcmc.meta.rda"))
if(officialname==T){
  info <- read.csv(file.path(getwd(),"input/infoUNinclHIV.csv",sep=""), header = T, stringsAsFactors = F)
  officialname=as.data.frame(info)                    ###read in official country name    kai add 05/07/2018
  wuwu=data.frame(mcmc.meta$data.all$iso.c,stringsAsFactors=FALSE)
  colnames(wuwu)="iso.c"
  mcmc.meta$data.all$name.c=dplyr::left_join(wuwu,officialname,by="iso.c")$officialname.c     ###replace the old country name with the official country name    kai add 05/07/2018
  #mcmc.meta$data$name.c=dplyr::left_join(wawa,officialname,by="iso.c")$officialname.c    
}
plot.width=(main.plot + zoom + add.legend) * 7
if(file.format=="png"){
  separate.plots.by.country=T             ######png file could only plot single country result
  print("file.format='png' will produce separate plots by country.")
}
if (!separate.plots.by.country) {
pdf(file = file.path(fig.dir, paste0(runname, " ", "Data", ".pdf")), width = plot.width, height = 7)
}
if (indicator.type == "U5MR") {
  for (c in 1:mcmc.meta$data.all$C){
    if (separate.plots.by.country){
      if(file.format=="png"){
        if((main.plot + zoom + add.legend)==3){
          png(file = file.path(fig.dir, paste0(runname, indicator.type, "Data",  mcmc.meta$data$iso.c[c], ".png")), width=24.75,height=8.25,units="in", res=250)   ##when there are three parts in the plot   kai made changes 04/09/2018
        } else if((main.plot + zoom + add.legend)==2){png(file = file.path(fig.dir, paste0(runname, indicator.type, " Data Plot ",  mcmc.meta$data$iso.c[c], ".png")), width=16.5,height=8.25,units="in", res=250)   ##when there are two random parts in the plot   kai made changes 04/09/2018
        } else {png(file = file.path(fig.dir, paste0(runname, indicator.type, "Data",  mcmc.meta$data$iso.c[c], ".png")), width=8.25,height=8.25,units="in", res=250)}  ##when there is one random part in the plot   kai made changes 04/09/2018
      } else if(file.format=="pdf"){
          pdf(file = file.path(fig.dir, paste0(runname, indicator.type, "Data",  mcmc.meta$data$iso.c[c], ".pdf")), width = plot.width , height = 7)   ##pdf width is determined by pdf.width which is given above     kai made changes 04/09/2018
      }
    }
    PlotDataAndEstimates(data = mcmc.meta$data,
                         data.all = mcmc.meta$data.all,
                         add.legend=add.legend,
                         main.plot=main.plot,
                         zoom=zoom,
                         c = c,
                         indirect_series_visibility=indirect_series_visibility, #####kai added an option whether to include indirect series   05/22/2018
                         ylab = mcmc.meta$settings$indicator.type,
                         plot.se = TRUE,
                         year.end=year.end)
  if (separate.plots.by.country)
    dev.off()
  }
  if (!separate.plots.by.country)
    dev.off()
} else if (indicator.type == "IMR") {
  for (c in 1:mcmc.meta$data.all$C){
    if (separate.plots.by.country){
      if(file.format=="png"){
        if((main.plot + zoom + add.legend)==3){
          png(file = file.path(fig.dir, paste0(runname, indicator.type, " Data Plot ",  mcmc.meta$data.all$iso.c[c], ".png")), width=24.75,height=8.25,units="in", res=250)   ##when there are three parts in the plot   kai made changes 04/09/2018
        } else if((main.plot + zoom + add.legend)==2){png(file = file.path(fig.dir, paste0(runname, indicator.type, " Data Plot ",  mcmc.meta$data.all$iso.c[c], ".png")), width=16.5,height=8.25,units="in", res=250)   ##when there are two random parts in the plot   kai made changes 04/09/2018
        } else {png(file = file.path(fig.dir, paste0(runname, indicator.type, " Data Plot ",  mcmc.meta$data.all$iso.c[c], ".png")), width=8.25,height=8.25,units="in", res=250)}  ##when there is one random part in the plot   kai made changes 04/09/2018
      } else if(file.format=="pdf"){
          pdf(file = file.path(fig.dir, paste0(runname, indicator.type, " Data Plot ",  mcmc.meta$data.all$iso.c[c], ".pdf")), width = plot.width , height = 7)   ##pdf width is determined by pdf.width which is given above     kai made changes 04/09/2018
      }
    }
    PlotDataAndEstimates(data = mcmc.meta$data,
                         data.all = mcmc.meta$data.all,
                         add.legend=add.legend,
                         main.plot=main.plot,
                         zoom=zoom,
                         c = c,
                         ylab = mcmc.meta$settings$indicator.type,
                         legendfull = mcmc.meta$data.all$imrmethod.c[c],
                         indirect_series_visibility=indirect_series_visibility,    #####kai added an option whether to include indirect series   05/22/2018
                         plot.se = TRUE,
                         year.end=year.end)
  if (separate.plots.by.country)
    dev.off()
  }
  if (!separate.plots.by.country)
    dev.off()
}
return(invisible())
}